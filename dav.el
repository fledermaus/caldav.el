;; -*- lexical-binding: t -*-
;;; dav.el --- WebDAV support

;; Copyright © 2001, 2004-2012  Free Software Foundation, Inc.
;; Copyright © 2013 Vivek Dasmohapatra <vivek@etla.org>

;; Author: Bill Perry <wmperry@gnu.org>
;; Author: Vivek Dasmohapatra <vivek@etla.org>
;; Maintainer: Vivek Dasmohapatra <vivek@etla.org>
;; Keywords: url, vc

;; This file is NOT part of GNU Emacs.

;; dav.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; dav.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with dav.el.  If not, see <http://www.gnu.org/licenses/>.

;; DAV is in RFC 2518.

;;; Commentary:
;; This is a version of url-dav modified to a) work (the shipped url-dav.el
;; appears to be based on a version of xml.el that has a different API to 
;; the current implementation) and b) have async versions of the important
;; functions

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'xml)
(require 'url-util)
(require 'url-handlers)

(defvar dav-supported-protocols '(1 2)
  "List of supported DAV versions.")

(defvar url-http-content-type)
(defvar url-http-response-status)
(defvar url-http-end-of-headers)
(defvar dav-response-dav-headers)

(defun dav-intersection (l1 l2)
  "Return a list of the elements occurring in both of the lists L1 and L2."
  (if (null l2)
      l2
    (let (result)
      (while l1
        (if (member (car l1) l2)
            (setq result (cons (pop l1) result))
          (pop l1)))
      (nreverse result))))

 ;;;###autoload
 (defun dav-supported-p (url)
   "Return WebDAV protocol version supported by URL.
 Returns nil if WebDAV is not supported."
   (dav-intersection dav-supported-protocols
             (plist-get (url-http-options url) 'dav)))

 (defun dav-node-text (node)
   "Return the text data from the XML node NODE."
   (mapconcat (lambda (txt)
                (if (stringp txt) txt ""))
              (xml-node-children node) " "))

 
 ;;; Parsing routines for the actual node contents.
 ;;
 ;; I am not incredibly happy with how this code looks/works right
 ;; now, but it DOES work, and if we get the API right, our callers
 ;; won't have to worry about the internal representation.

 (defconst dav-datatype-attribute
   'urn:uuid:c2f41010-65b3-11d1-a29f-00aa00c14882/dt)

 (defun dav-process-integer-property (node)
   (truncate (string-to-number (dav-node-text node))))

 (defun dav-process-number-property (node)
   (string-to-number (dav-node-text node)))

(defconst dav-ns-joint (format "%c" #xa0))

(defun dav-node-name (node)
  (setq node (xml-node-name node))
  (if (consp node)
      (if (equal "DAV:" (car node))
          (setq node (intern (concat (car node) (cdr node))))
        (setq node (intern (concat (car node) dav-ns-joint (cdr node))))))
  node)

(defun dav-get-child (name list)
   (if (symbolp name)
       (setq name (cons "DAV:" (symbol-name name))))
   (assoc name list))

(defconst dav-iso8601-regexp
  (let* ((dash           "-?")
         (colon          ":?")
         (4digit         "\\([0-9][0-9][0-9][0-9]\\)")
         (2digit         "\\([0-9][0-9]\\)")
         (date-fullyear  4digit)
         (date-month     2digit)
         (date-mday      2digit)
         (time-hour      2digit)
         (time-minute    2digit)
         (time-second    2digit)
         (time-secfrac   "\\(\\.[0-9]+\\)?")
         (time-numoffset (concat "[-+]\\(" time-hour "\\):" time-minute))
         (partial-time   (concat time-hour colon time-minute colon time-second
                                 time-secfrac))
         (full-date      (concat date-fullyear dash date-month dash date-mday)))
    (list (concat "^" full-date)
          (concat "T" partial-time)
          (concat "Z" time-numoffset)))
  "List of regular expressions matching ISO 8601 dates.
1st regular expression matches the date.
2nd regular expression matches the time.
3rd regular expression matches the (optional) timezone specification.")

(defun dav-process-date-property (node)
  (require 'parse-time)
  (let* ((date-re     (nth 0 dav-iso8601-regexp))
         (time-re     (nth 1 dav-iso8601-regexp))
         (tz-re       (nth 2 dav-iso8601-regexp))
         (date-string (dav-node-text node))
         re-start
         time seconds minute hour
         day month year day-of-week dst tz)
    ;; We need to populate 'time' with
    ;; (SEC MIN HOUR DAY MON YEAR DOW DST TZ)

    ;; Nobody else handles iso8601 correctly, let's do it ourselves.
    (when (string-match date-re date-string re-start)
      (setq year     (string-to-number (match-string 1 date-string))
            month    (string-to-number (match-string 2 date-string))
            day      (string-to-number (match-string 3 date-string))
            re-start (match-end 0))
      (when (string-match time-re date-string re-start)
        (setq hour    (string-to-number (match-string 1 date-string))
              minute  (string-to-number (match-string 2 date-string))
              seconds (string-to-number (match-string 3 date-string))
              re-start (match-end 0))
        (when (string-match tz-re date-string re-start)
          (setq tz (match-string 1 date-string)))
        (url-debug 'dav "Parsed iso8601%s date" (if tz "tz" ""))
        (setq time 
              (list seconds minute hour day month year day-of-week dst tz))))

    ;; Fall back to having Gnus do fancy things for us.
    (when (not time)
      (setq time (parse-time-string date-string)))

    (if time
        (setq time (apply 'encode-time time))
      (url-debug 'dav "Unable to decode date (%S) (%s)"
                 (xml-node-name node) date-string))
    time))

(defun dav-process-boolean-property (node)
  (/= 0 (string-to-number (dav-node-text node))))

(defun dav-process-uri-property (node)
  ;; Returns a parsed representation of the URL...
  (url-generic-parse-url (dav-node-text node)))

(defun dav-find-parser (node &optional name)
  "Find a function to parse the XML node NODE."
  (setq name (dav-node-name node))
  (or (get name :dav-parser)
      (let ((fn (intern (format "dav-process-%s" name))))
        (if (not (fboundp fn))
            (setq fn 'dav-node-text)
          (put name :dav-parser fn))
        fn)))

(defmacro dav-dispatch-node (node)
  `(let ((x ,node))
     (funcall (dav-find-parser x) x)))

(defun dav-process-DAV:prop (node)
  ;; A prop node has content model of ANY
  ;;
  ;; Some predefined nodes have special meanings though.
  ;;
  ;; DAV:supportedlock    - list of DAV:lockentry
  ;; DAV:source
  ;; DAV:iscollection     - boolean
  ;; DAV:getcontentlength - integer
  ;; DAV:ishidden         - boolean
  ;; DAV:getcontenttype   - string
  ;; DAV:resourcetype     - node who's name is the resource type
  ;; DAV:getlastmodified  - date
  ;; DAV:creationdate     - date
  ;; DAV:displayname      - string
  ;; DAV:getetag          - unknown
  (let ((children    (xml-node-children node))
        (node-type    nil)
        (props        nil)
        (value        nil))
    (when (not children)
      (error "No child nodes in DAV:prop"))

    (while children
      (setq node      (car children)
            node-type (cdr-safe (assq dav-datatype-attribute
                                      (xml-node-attributes node)))
            node-type (if node-type (intern node-type) 'unknown)
            value      nil)

      (case node-type
        ((dateTime.iso8601tz
          dateTime.iso8601
          dateTime.tz
          dateTime.rfc1123
          dateTime
          date)                         ; date is our 'special' one...
         ;; Some type of date/time string.
         (setq value (dav-process-date-property node)))
        (int
         ;; Integer type...
         (setq value (dav-process-integer-property node)))
        ((number float)
         (setq value (dav-process-number-property node)))
        (boolean
         (setq value (dav-process-boolean-property node)))
        (uri
         (setq value (dav-process-uri-property node)))
        (otherwise
         (if (not (eq node-type 'unknown))
             (url-debug 'dav "Unknown data type in dav-process-prop: %s"
                        node-type))
         (setq value (dav-dispatch-node node))))

      (setq props (plist-put props (dav-node-name node) value)
            children (cdr children)))
    props))

(defun dav-process-DAV:supportedlock (node)
  ;; DAV:supportedlock is a list of DAV:lockentry items.
  ;; DAV:lockentry in turn contains a DAV:lockscope and DAV:locktype.
  ;; The DAV:lockscope must have a single node beneath it, ditto for
  ;; DAV:locktype.
  (let ((children (xml-node-children node))
        (results nil)
        scope type)
    (while children
      (when (and (not (stringp (car children)))
                 (eq (dav-node-name (car children)) 'DAV:lockentry))
        (setq scope (dav-get-child 'lockscope
                                       (xml-node-children (car children)))
              type  (dav-get-child 'locktype
                                       (xml-node-children (car children))))
        (when (and scope type)
          (setq scope (dav-node-name (car (xml-node-children scope)))
                type (dav-node-name (car (xml-node-children type))))
          (push (cons type scope) results)))
      (setq children (cdr children)))
    results))

(defun dav-process-subnode-property (node)
  ;; Returns a list of child node names.
  (delq nil (mapcar 'car-safe (xml-node-children node))))

(defalias 'dav-process-DAV:depth            'dav-process-integer-property)
(defalias 'dav-process-DAV:resourcetype     'dav-process-subnode-property)
(defalias 'dav-process-DAV:locktype         'dav-process-subnode-property)
(defalias 'dav-process-DAV:lockscope        'dav-process-subnode-property)
(defalias 'dav-process-DAV:getcontentlength 'dav-process-integer-property)
(defalias 'dav-process-DAV:getlastmodified  'dav-process-date-property)
(defalias 'dav-process-DAV:creationdate     'dav-process-date-property)
(defalias 'dav-process-DAV:iscollection     'dav-process-boolean-property)
(defalias 'dav-process-DAV:ishidden         'dav-process-boolean-property)

(defun dav-process-DAV:locktoken (node)
  ;; DAV:locktoken can have one or more DAV:href children.
  (delq nil (mapcar
             (lambda (n)
               (if (stringp n) n (dav-dispatch-node n)))
             (xml-node-children node))))

(defun dav-process-DAV:owner (node)
  ;; DAV:owner can contain anything.
  (delq nil (mapcar (lambda (n)
                      (if (stringp n)
                          n
                        (dav-dispatch-node n)))
                    (xml-node-children node))))

(defun dav-process-DAV:activelock (node)
  ;; DAV:activelock can contain:
  ;;  DAV:lockscope
  ;;  DAV:locktype
  ;;  DAV:depth
  ;;  DAV:owner (optional)
  ;;  DAV:timeout (optional)
  ;;  DAV:locktoken (optional)
  (let ((children (xml-node-children node))
        (results nil))
    (while children
      (if (listp (car children))
          (push (cons (dav-node-name (car children))
                      (dav-dispatch-node (car children)))
                results))
      (setq children (cdr children)))
    results))

(defun dav-process-DAV:lockdiscovery (node)
  ;; Can only contain a list of DAV:activelock objects.
  (let ((children (xml-node-children node))
        (results nil))
    (while children
      (cond
       ((stringp (car children))
        ;; text node? why?
        nil)
       ((eq (dav-node-name (car children)) 'DAV:activelock)
        (push (dav-dispatch-node (car children)) results))
       (t
        ;; Ignore unknown nodes...
        nil))
      (setq children (cdr children)))
    results))

(defun dav-process-DAV:status (node)
  ;; The node contains a standard HTTP/1.1 response line... we really
  ;; only care about the numeric status code.
  (let ((status (dav-node-text node)))
    (if (string-match "\\`[ \r\t\n]*HTTP/[0-9.]+ \\([0-9]+\\)" status)
        (string-to-number (match-string 1 status))
      500)))

(defun dav-process-DAV:propstat (node)
  ;; A propstate node can have the following children...
  ;;
  ;; DAV:prop - a list of properties and values
  ;; DAV:status - An HTTP/1.1 status line
  (let ((children (xml-node-children node))
        (props nil)
        (status nil))
    (when (not children)
      (error "No child nodes in DAV:propstat"))

    (setq props  (dav-dispatch-node (dav-get-child 'prop children))
          status (dav-dispatch-node (dav-get-child 'status children)))

    ;; Need to parse out the HTTP status
    (setq props (plist-put props 'DAV:status status))
    props))

(defun dav-process-DAV:response (node)
  (let ((children (xml-node-children node))
        (propstat nil)
        (href))
    (when (not children)
      (error "No child nodes in DAV:response"))

    ;; A response node can have the following children...
    ;;
    ;; DAV:href     - URL the response is for.
    ;; DAV:propstat - see dav-process-propstat
    ;; DAV:responsedescription - text description of the response
    (setq propstat (dav-get-child 'propstat children)
          href     (dav-get-child 'href     children))

    (when (not href)
      (error "No href in DAV:response"))

    (when (not propstat)
      (error "No propstat in DAV:response"))

    (setq propstat (dav-dispatch-node propstat)
          href     (dav-dispatch-node href))
    (cons href propstat)))

(defun dav-process-DAV:multistatus (node)
  (let ((children (xml-node-children node))
        (results nil))
    (while children
      (setq results  (cons (dav-dispatch-node (car children)) results)
            children (cdr children)))
    results))


;;; DAV request/response generation/processing
(defun dav-process-response (buffer url)
  "Parse a WebDAV response from BUFFER, interpreting it relative to URL.

The buffer must have been retrieved by HTTP or HTTPS and contain an
XML document."
  (let ((tree nil)
        (top-node nil)
        (top-node-name nil)
        (overall-status nil))
    (when buffer
      (unwind-protect
          (with-current-buffer buffer
            ;;(write-region (point-min) (point-max) "/tmp/dav-response")
            (setq overall-status url-http-response-status)

            ;; XML documents can be transferred as either text/xml or
            ;; application/xml, and we are required to accept both of
            ;; them.
            (if (and
                 url-http-content-type
                 (string-match "\\`\\(text\\|application\\)/xml"
                               url-http-content-type))
                (setq tree (xml-parse-region (point) (point-max) nil nil :expand-ns))))
        ;; Clean up after ourselves.
        (kill-buffer buffer)))

    ;; We should now be
    (setq top-node (car tree) top-node-name (dav-node-name top-node))
    (if (eq top-node-name 'DAV:multistatus)
        (dav-dispatch-node top-node)
      (url-debug 'dav "Got back singleton response for URL(%S)" url)
      (let ((properties (dav-dispatch-node (car tree))))
        ;; We need to make sure we have a DAV:status node in there for
        ;; higher-level code;
        (setq properties (plist-put properties 'DAV:status overall-status))
        ;; Make this look like a DAV:multistatus parse tree so that
        ;; nobody but us needs to know the difference.
        (list (cons url properties))))))

(defun dav-process-async-response (_status url callback)
  (funcall callback (dav-process-response (current-buffer) url)))

;;;###autoload
(defun dav-request (url method tag body
                    &optional callback depth headers namespaces)
  "Perform WebDAV operation METHOD on URL.  Return the parsed responses.
Automatically creates an XML request body if TAG is non-nil.
BODY is the XML document fragment to be enclosed by <TAG></TAG>.

DEPTH is how deep the request should propagate.  Default is 0, meaning
it should apply only to URL.  A negative number means to use
`Infinity' for the depth.  Not all WebDAV servers support this depth
though.

HEADERS is an assoc list of extra headers to send in the request.

NAMESPACES is an assoc list of (NAMESPACE . EXPANSION), and these are
added to the <TAG> element.  The D=DAV: namespace is automatically
added to this list, so most requests can just pass in nil."
  ;; Take care of the default value for depth...
  (setq depth (or depth 0))

  ;; Now let's translate it into something webdav can understand.
  (if (< depth 0)
      (setq depth "Infinity")
    (setq depth (int-to-string depth)))
  (if (not (assoc "D" namespaces))
      (setq namespaces (cons '("D" . "DAV:") namespaces)))

  (let* ((url-request-extra-headers `(("Depth" . ,depth) ,@headers))
         (url-request-method method)
         (url-request-data
          (if tag
              (concat
               "<?xml version=\"1.0\" encoding=\"utf-8\" ?>\n"
               "<" (symbol-name tag) " "
               ;; add in the appropriate namespaces...
               (mapconcat (lambda (ns)
                            (concat "xmlns:" (car ns) "='" (cdr ns) "'"))
                          namespaces "\n    ")
               ">\n"
               body
               "</" (symbol-name tag) ">\n")
            body)))
    (if (not (assoc "Content-type" url-request-extra-headers))
        (setq url-request-extra-headers
              (cons '("Content-type" . "text/xml") url-request-extra-headers)))
    (if callback
        (url-retrieve url 'dav-process-async-response (list url callback))
      (dav-process-response (url-retrieve-synchronously url) url)) ))

(defun dav-get-properties (url &optional attributes depth namespaces)
  "Return properties for URL, up to DEPTH levels deep.

Returns an assoc list, where the key is the filename (possibly a full
URI), and the value is a standard property list of DAV property
names (ie: DAV:resourcetype)."
  (dav-request url "PROPFIND" 'DAV:propfind
                   (if attributes
                       (mapconcat (lambda (attr)
                                    (concat "<DAV:prop><"
                                            (symbol-name attr)
                                            "/></DAV:prop>"))
                                  attributes "\n  ")
                     "  <DAV:allprop/>")
                   depth nil namespaces))

(defmacro dav-http-success-p (status)
  "Return whether STATUS was the result of a successful DAV request."
  `(= (/ (or ,status 500) 100) 2))


;;; Locking support
(defvar dav-lock-identifier (concat "mailto:" user-mail-address)
  "URL used as contact information when creating locks in DAV.
This will be used as the contents of the DAV:owner/DAV:href tag to
identify the owner of a LOCK when requesting it.  This will be shown
to other users when the DAV:lockdiscovery property is requested, so
make sure you are comfortable with it leaking to the outside world.")

(defun dav-lock-resource (url exclusive &optional depth)
  "Request a lock on URL.  If EXCLUSIVE is non-nil, get an exclusive lock.
Optional 3rd argument DEPTH says how deep the lock should go, default is 0
\(lock only the resource and none of its children\).

Returns a cons-cell of (SUCCESSFUL-RESULTS . FAILURE-RESULTS).
SUCCESSFUL-RESULTS is a list of (URL STATUS locktoken).
FAILURE-RESULTS is a list of (URL STATUS)."
  (setq exclusive (if exclusive "<DAV:exclusive/>" "<DAV:shared/>"))
  (let* ((body
          (concat
           "  <DAV:lockscope>" exclusive "</DAV:lockscope>\n"
           "  <DAV:locktype> <DAV:write/> </DAV:locktype>\n"
           "  <DAV:owner>\n"
           "    <DAV:href>" dav-lock-identifier "</DAV:href>\n"
           "  </DAV:owner>\n"))
         (response nil)           ; Responses to the LOCK request
         (result nil)             ; For walking thru the response list
         (child-status nil)
         (failures nil)           ; List of failure cases (URL . STATUS)
         (successes nil))         ; List of success cases (URL . STATUS)
    (setq response (dav-request url "LOCK" 'DAV:lockinfo body
                                    depth '(("Timeout" . "Infinite"))))

    ;; Get the parent URL ready for expand-file-name
    (if (not (vectorp url))
        (setq url (url-generic-parse-url url)))

    ;; Walk thru the response list, fully expand the URL, and grab the
    ;; status code.
    (while response
      (setq result (pop response)
            child-status (or (plist-get result 'DAV:status) 500))
      (if (dav-http-success-p child-status)
          (push (list url child-status "huh") successes)
        (push (list url child-status) failures)))
    (cons successes failures)))

(defun dav-active-locks (url &optional depth)
  "Return an assoc list of all active locks on URL."
  (let ((response (dav-get-properties url '(DAV:lockdiscovery) depth))
        (child nil)
        (child-url nil)
        (child-results nil)
        (results nil))
    (if (not (vectorp url))
        (setq url (url-generic-parse-url url)))

    (while response
      (setq child (pop response)
            child-url (pop child)
            child-results nil)
      (when (and (dav-http-success-p (plist-get child 'DAV:status))
                 (setq child (plist-get child 'DAV:lockdiscovery)))
        ;; After our parser has had its way with it, The
        ;; DAV:lockdiscovery property is a list of DAV:activelock
        ;; objects, which are comprised of DAV:activelocks, which
        ;; assoc lists of properties and values.
        (while child
          (if (dav-get-child 'locktoken (car child))
              (let ((tokens (cdr (dav-get-child 'locktoken (car child))))
                (owners (cdr (dav-get-child 'owner     (car child)))))
                (dolist (token tokens)
                  (dolist (owner owners)
                    (push (cons token owner) child-results)))))
          (pop child)))
      (if child-results
          (push (cons (url-expand-file-name child-url url) child-results)
                results)))
    results))

(defun dav-unlock-resource (url lock-token)
  "Release the lock on URL represented by LOCK-TOKEN.
Returns t if the lock was successfully released."
  (let* ((url-request-extra-headers (list (cons "Lock-Token"
                                                (concat "<" lock-token ">"))))
         (url-request-method "UNLOCK")
         (url-request-data nil)
         (buffer (url-retrieve-synchronously url))
         (result nil))
    (when buffer
      (unwind-protect
          (with-current-buffer buffer
            (setq result (dav-http-success-p url-http-response-status)))
        (kill-buffer buffer)))
    result))


;;; file-name-handler stuff
(defun dav-file-attributes-mode-string (properties)
  (let ((modes (make-string 10 ?-))
        (supported-locks (plist-get properties 'DAV:supportedlock))
        (executable-p (equal (plist-get properties 'http://apache.org/dav/props/executable)
                             "T"))
        (directory-p (memq 'DAV:collection (plist-get properties 'DAV:resourcetype)))
        (readable t)
        (lock nil))
    ;; Assume we can read this, otherwise the PROPFIND would have
    ;; failed.
    (when readable
      (aset modes 1 ?r)
      (aset modes 4 ?r)
      (aset modes 7 ?r))

    (when directory-p
      (aset modes 0 ?d))

    (when executable-p
      (aset modes 3 ?x)
      (aset modes 6 ?x)
      (aset modes 9 ?x))

    (while supported-locks
      (setq lock (car supported-locks)
            supported-locks (cdr supported-locks))
      (case (car lock)
        (DAV:write
         (case (cdr lock)
           (DAV:shared                  ; group permissions (possibly world)
            (aset modes 5 ?w))
           (DAV:exclusive
            (aset modes 2 ?w))          ; owner permissions?
           (otherwise
            (url-debug 'dav "Unrecognized DAV:lockscope (%S)" (cdr lock)))))
        (otherwise
         (url-debug 'dav "Unrecognized DAV:locktype (%S)" (car lock)))))
    modes))

(autoload 'url-http-head-file-attributes "url-http")

(defun dav-file-attributes (url &optional id-format)
  (let ((properties (cdar (dav-get-properties url))))
    (if (and properties
             (dav-http-success-p (plist-get properties 'DAV:status)))
        ;; We got a good DAV response back..
        (list
         ;; t for directory, string for symbolic link, or nil
         ;; Need to support DAV Bindings to figure out the
         ;; symbolic link issues.
         (if (memq 'DAV:collection (plist-get properties 'DAV:resourcetype)) t nil)

         ;; Number of links to file... Needs DAV Bindings.
         1

         ;; File uid - no way to figure out?
         0

         ;; File gid - no way to figure out?
         0

         ;; Last access time - ???
         nil

         ;; Last modification time
         (plist-get properties 'DAV:getlastmodified)

         ;; Last status change time... just reuse last-modified
         ;; for now.
         (plist-get properties 'DAV:getlastmodified)

         ;; size in bytes
         (or (plist-get properties 'DAV:getcontentlength) 0)

         ;; file modes as a string like `ls -l'
         ;;
         ;; Should be able to build this up from the
         ;; DAV:supportedlock attribute pretty easily.  Getting
         ;; the group info could be impossible though.
         (dav-file-attributes-mode-string properties)

         ;; t if file's gid would change if it were deleted &
         ;; recreated.  No way for us to know that thru DAV.
         nil

         ;; inode number - meaningless
         nil

         ;; device number - meaningless
         nil)
      ;; Fall back to just the normal http way of doing things.
      (url-http-head-file-attributes url id-format))))

(defun dav-save-resource (url obj &optional callback content-type lock-token)
  "Save OBJ as URL using WebDAV.
URL must be a fully qualified URL.
OBJ may be a buffer or a string."
  (let ((url-request-extra-headers nil)
        (url-request-method "PUT")
        (url-request-data
         (cond ((bufferp obj) (with-current-buffer obj (buffer-string)))
               ((stringp obj) obj)
               (t (error "Invalid object to dav-save-resource")))))

    (if lock-token
        (push (cons "If" (concat "(<" lock-token ">)"))
              url-request-extra-headers))

    ;; Everything must always have a content-type when we submit it.
    (push (cons "Content-type" (or content-type "application/octet-stream"))
          url-request-extra-headers)

    ;; Do the save...
    (if callback
        (url-retrieve url 'dav-save-resource-response (list nil callback))
      (dav-save-resource-response (url-retrieve-synchronously url))) ))

(defun dav-save-resource-response (buffer &optional _callback result)
  (or buffer (setq buffer (current-buffer)))
  (when buffer
    (unwind-protect
        (with-current-buffer buffer
          (setq result (dav-http-success-p url-http-response-status)))
      (kill-buffer buffer)))
  result)

(eval-when-compile
  (defmacro dav-delete-something (url lock-token &rest error-checking)
    "Delete URL completely, with no sanity checking whatsoever.  DO NOT USE.
This is defined as a macro that will not be visible from compiled files.
Use with care, and even then think three times."
    `(progn
       ,@error-checking
       (dav-request ,url "DELETE" nil nil -1
                        (if ,lock-token
                            (list
                             (cons "If"
                                   (concat "(<" ,lock-token ">)"))))))))


(defun dav-delete-directory (url &optional recursive lock-token)
  "Delete the WebDAV collection URL.
If optional second argument RECURSIVE is non-nil, then delete all
files in the collection as well."
  (let ((status nil)
        (props nil))
    (setq props (dav-delete-something
                 url lock-token
                 (setq props (dav-get-properties url '(DAV:getcontenttype) 1))
                 (if (and (not recursive)
                          (/= (length props) 1))
                     (signal 'file-error (list "Removing directory"
                                               "directory not empty" url)))))

     (mapc (lambda (result)
             (setq status (plist-get (cdr result) 'DAV:status))
             (if (not (dav-http-success-p status))
                 (signal 'file-error (list "Removing directory"
                                           "Error removing"
                                           (car result) status))))
           props))
  nil)

(defun dav-delete-file (url &optional lock-token)
  "Delete file named URL."
  (let ((props nil)
        (status nil))
    (setq props (dav-delete-something
                 url lock-token
                 (setq props (dav-get-properties url))
                 (if (eq (plist-get (cdar props) 'DAV:resourcetype) 'DAV:collection)
                     (signal 'file-error (list "Removing old name" "is a collection" url)))))

    (mapc (lambda (result)
            (setq status (plist-get (cdr result) 'DAV:status))
            (if (not (dav-http-success-p status))
                (signal 'file-error (list "Removing old name"
                                          "Error removing"
                                          (car result) status))))
          props))
  nil)

(defun dav-directory-files (url &optional full match nosort files-only)
  "Return a list of names of files in URL.
There are three optional arguments:
If FULL is non-nil, return absolute URLs.  Otherwise return names
 that are relative to the specified URL.
If MATCH is non-nil, mention only file names that match the regexp MATCH.
If NOSORT is non-nil, the list is not sorted--its order is unpredictable.
 NOSORT is useful if you plan to sort the result yourself."
  (let ((properties (dav-get-properties url '(DAV:resourcetype) 1))
        (child-url nil)
        (child-props nil)
        (files nil)
        (parsed-url (url-generic-parse-url url)))

    (when (and (= (length properties) 1)
               (not (dav-file-directory-p url)))
      (signal 'file-error (list "Opening directory" "not a directory" url)))

    (while properties
      (setq child-props (pop properties)
            child-url (pop child-props))
      (if (and (eq (plist-get child-props 'DAV:resourcetype) 'DAV:collection)
               files-only)
          ;; It is a directory, and we were told to return just files.
          nil

        ;; Fully expand the URL and then rip off the beginning if we
        ;; are not supposed to return fully-qualified names.
        (setq child-url (url-expand-file-name child-url parsed-url))
        (if (not full)
            ;; Parts of the URL might be hex'ed.
            (setq child-url (substring (url-unhex-string child-url)
                                       (length url))))

        ;; We don't want '/' as the last character in filenames...
        (if (string-match "/$" child-url)
            (setq child-url (substring child-url 0 -1)))

        ;; If we have a match criteria, then apply it.
        (if (or (and match (not (string-match match child-url)))
                (string= child-url "")
                (string= child-url url))
            nil
          (push child-url files))))

    (if nosort
        files
      (sort files 'string-lessp))))

(defun dav-file-directory-p (url)
  "Return t if URL names an existing DAV collection."
  (let ((properties (cdar (dav-get-properties url '(DAV:resourcetype)))))
    (when (member 'DAV:collection (plist-get properties 'DAV:resourcetype))
      t)))

(defun dav-make-directory (url &optional _parents)
  "Create the directory DIR and any nonexistent parent dirs."
  (let* ((url-request-extra-headers nil)
         (url-request-method "MKCOL")
         (url-request-data nil)
         (buffer (url-retrieve-synchronously url))
         (result nil))
    (when buffer
      (unwind-protect
          (with-current-buffer buffer
            (case url-http-response-status
              (201                      ; Collection created in its entirety
               (setq result t))
              (403                      ; Forbidden
               nil)
              (405                      ; Method not allowed
               nil)
              (409                      ; Conflict
               nil)
              (415                      ; Unsupported media type (WTF?)
               nil)
              (507                      ; Insufficient storage
               nil)
              (otherwise
               nil)))
        (kill-buffer buffer)))
    result))

(defun dav-rename-file (oldname newname &optional overwrite)
  (if (not (and (string-match url-handler-regexp oldname)
                (string-match url-handler-regexp newname)))
      (signal 'file-error
              (list "Cannot rename between different URL backends"
                    oldname newname)))

  (let* ((headers nil)
         (props nil)
         (status nil)
         (directory-p (dav-file-directory-p oldname))
         (exists-p (url-http-file-exists-p newname)))

    (if (and exists-p
             (or
              (null overwrite)
              (and (numberp overwrite)
                   (not (yes-or-no-p
                         (format "File %s already exists; rename to it anyway? "
                                 newname))))))
        (signal 'file-already-exists (list "File already exists" newname)))

    ;; Honor the overwrite flag...
    (if overwrite (push '("Overwrite" . "T") headers))

    ;; Have to tell them where to copy it to!
    (push (cons "Destination" newname) headers)

    ;; Always send a depth of -1 in case we are moving a collection.
    (setq props (dav-request oldname "MOVE" nil nil (if directory-p -1 0)
                                 headers))

    (mapc (lambda (result)
            (setq status (plist-get (cdr result) 'DAV:status))

            (if (not (dav-http-success-p status))
                (signal 'file-error (list "Renaming" oldname newname status))))
          props)
    t))

(defun dav-file-name-all-completions (file url)
  "Return a list of all completions of file name FILE in URL.
These are all file names in URL which begin with FILE."
  (dav-directory-files url nil (concat "^" file ".*")))

(defun dav-file-name-completion (file url)
  "Complete file name FILE in URL.
Returns the longest string common to all file names in URL
that start with FILE.
If there is only one and FILE matches it exactly, returns t.
Returns nil if URL contains no name starting with FILE."
  (let ((matches (dav-file-name-all-completions file url)))
    (cond
     ((null matches)
      ;; No matches
      nil)
     ((and (= (length matches) 1)
           (string= file (car matches)))
      ;; Only one file and FILE matches it exactly...
      t)
     (t
      ;; Need to figure out the longest string that they have in common
      (setq matches (sort matches (lambda (a b) (> (length a) (length b)))))
      (let ((n (length file))
            (searching t)
            (regexp nil)
            (failed nil))
        (while (and searching
                    (< n (length (car matches))))
          (setq regexp (concat "^" (substring (car matches) 0 (1+ n)))
                failed nil)
          (dolist (potential matches)
            (if (not (string-match regexp potential))
                (setq failed t)))
          (if failed
              (setq searching nil)
            (incf n)))
        (substring (car matches) 0 n))))))

(defun dav-register-handler (op)
  (put op 'url-file-handlers (intern-soft (format "dav-%s" op))))

(mapc 'dav-register-handler
      ;; These handlers are disabled because they incorrectly presume that
      ;; the URL specifies an HTTP location and thus break FTP URLs.
      '(;; file-name-all-completions
        ;; file-name-completion
        ;; rename-file
        ;; make-directory
        ;; file-directory-p
        ;; directory-files
        ;; delete-file
        ;; delete-directory
        ;; file-attributes
        ))


;;; Version Control backend cruft

;(put 'vc-registered 'url-file-handlers 'dav-vc-registered)

;;;###autoload
(defun dav-vc-registered (url)
  (if (and (string-match "\\`https?" url)
           (plist-get (url-http-options url) 'dav))
      (progn
        (vc-file-setprop url 'vc-backend 'dav)
        t)))


;;; Miscellaneous stuff.

(provide 'dav)

;;; dav.el ends here
