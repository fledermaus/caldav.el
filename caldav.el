;; -*- lexical-binding: t -*-
;;; caldav.el --- CalDAV support

;; Copyright © 2013  Vivek Dasmohapatra <vivek@etla.org>

;; Author: Vivek Dasmohapatra <vivek@etla.org>
;; Maintainer: Vivek Dasmohapatra <vivek@etla.org>
;; Keywords: dav, caldav

;; This file is NOT part of GNU Emacs.

;; caldav.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; caldav.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with caldav.el.  If not, see <http://www.gnu.org/licenses/>.

;; DAV is in RFC 4791.

;;; Commentary:

;;; Code:
(require 'url-parse)

(defconst caldav-ns-caldav "urn:ietf:params:xml:ns:caldav")
(defconst caldav-ical-node 
  (intern (concat caldav-ns-caldav dav-ns-joint "calendar-data"))
  "The symbol which precedes the ical data blob inside `caldav-get-ical's
return value")
(defconst caldav-ns-dav    "DAV:")
(defconst caldav-namespaces  `(("C" . ,caldav-ns-caldav)
                               ("D" . ,caldav-ns-dav   )))

(defcustom caldav-default-url
  "https://sogo.collabora.co.uk/SOGo/dav/vivek/Calendar/personal"
  "The default caldav calendar URL"
  :type  (string)
  :group 'caldav)

(defvar caldav-prop-cache nil)

(defmacro caldav-aput (alist key value)
  `(let (c)
     (setq c (assoc ,key ,alist))
     (if c (setcdr c ,value)
       (setq ,alist (cons (cons ,key ,value) ,alist)))))

(defun caldav-month-start ()
  (let ((now (decode-time)))
    (encode-time 0 0 0 1 (nth 4 now) (nth 5 now))))

;; cba to do a generalised version of this
(defun caldav-one-year-later (start)
  (let ((now (decode-time start)) y)
    (setq y (1+ (nth 5 now)))
    (setf (nth 5 now) y)
    (apply 'encode-time now)))

(defun caldav-report-time (time)
  "Format a time value as an ISO-8601 date-time string."
  (format-time-string "%Y%m%dT%H%M%SUTC" time :utc))

(defun caldav-query (&optional start end)
  "Return a suitable XML fragment for fetching a caldav REPORT from the
server from START to END. The default is to fetch a 1 year ical blob
ranging from the beginning of this calendar month (in the local time zone)
to a point in time one year later (arrived at by decomposing the time, 
incrementing the year value, and recomposing it)."
  (or start (setq start (caldav-month-start)))
  (or end   (setq end   (caldav-one-year-later start)))
  (setq start (caldav-report-time start)
        end   (caldav-report-time end  ))
  (list start end
        (format "  <D:prop>
    <D:getetag />
    <C:calendar-data />
  </D:prop>
  <C:filter>
    <C:comp-filter name='VCALENDAR'>
      <C:time-range start='%s' end='%s'/>
      <C:comp-filter name='VEVENT' />
      <C:comp-filter name='VTODO'  />
      <C:comp-filter name='VALARM' />
    </C:comp-filter>
  </C:filter>\n" start end)))

(defun caldav-fetch-properties (&optional url attr depth)
  (let (props)
    (setq props (dav-get-properties (or url caldav-default-url)
                                    attr depth caldav-namespaces))
    (caldav-aput caldav-prop-cache url props)
    props))

(defun caldav-absolute-url (url &optional template)
  "If URL is absolute, return it. If it is not (ie it consists only of the
part after protocol://host.name) then merge it with TEMPLATE (an absolute
url) or, if TEMPLATE is nil, with `caldav-default-url'.\n
Returns an absolute url."
  (if (string-match "^\\(?:[a-z]+:\\)//" url)
      url
    (setq template (url-generic-parse-url (or template caldav-default-url)))
    (setf (url-filename template) url)
    (url-recreate-url template)))

(defun caldav-put-ical (url ical &optional create)
  "Given a CalDAV URL (which may be just the path component, in which case
an absolute URL is constructed by combining it with `caldav-default-url'
by way of `caldav-absolute-url'), serialise an rfc2445 object ICAL (as
returned by `caldav-fetch-ical') and write it back to the CalDAV store.
If CREATE is true, the calendar object MUST NOT already exist (the write
will fail if it does). Similarly, if CREATE is nil or omitted, the object
MUST already exist at the specified URL."
  (let (ical-text)
    (setq url       (caldav-absolute-url url)
          ical-text (icalendar--serialise-element ical))
    (dav-request url "PUT" nil ical-text
                 (lambda (&rest _args) (message "%s" (buffer-string)))
                 nil `((,(if create "If-None-Match" "If-Match") "*")))))

(defun caldav-fetch-ical (&optional url start end)
  "Return a a structure containing a list of rfc2445 ical objects,
from URL (defaulting to `caldav-default-url'), optionally limiting
your search to between START and END.\n
START defaults to the beginnin of the current month,
END defaults to one year after START.
The returned valus is an alist of the form:\n
   ((:from   start-datetime-srting)
    (:to     end-datetime-string)
    (:caldav caldav-list))\n
Where caldav-list has entries of the form:\n
    (\"/uri/path/to/ical/object.ics\"
     DAV:getetag \"identity-string\"  ;; The object's DAV strong-entity tag
     …                                ;; Any other attributes
     `caldav-ical-node' \"ICAL-DATA\" )
ICAL-DATA can typically be parsed with functions like `icalendar--read-element'"
  (let ((report-data (caldav-query start end)) from to query done)
    (setq from  (car   report-data)
          to    (cadr  report-data)
          query (caddr report-data))
    (dav-request (or url caldav-default-url) "REPORT"
                 'C:calendar-query query
                 (lambda (result)
                   (setq done (list (cons :from   from  ) 
                                    (cons :to     to    )
                                    (cons :caldav result))))
                 0 nil caldav-namespaces)
    (while (not done) (accept-process-output)) done))

(defun caldav-ical-to-alist (ical &optional absolute url)
  "Take a CalDAV/iCal data structure ICAL, as returned by `caldav-fetch-ical',
and return an alist whose keys are the urls of the iCal containers and whose
cdrs are the parsed iCal objects, as per `icalendar--read-element'.
If ABSOLUTE is true, make the keys absolute using `caldav-absolute-url'
and URL (or `caldav-default-url' if URL is nil)"
  (mapcar (lambda (x &optional ical-url ical-text ical-data)
            (setq i-url  (car x)
                  i-text (cadr (memq caldav-ical-node x))
                  i-data (with-temp-buffer
                              (insert i-text)
                              (goto-char (point-min))
                              (icalendar--read-element nil nil)))
            (if absolute
                (setq i-url
                      (caldav-absolute-url i-url (or url caldav-default-url))))
            (cons i-url i-data))
          (cdr (assq :caldav ical))))

(defun caldav-filter-apply-pred (ical-item attributes predicate)
  (let (attr-val attr-pred attr-func)
    (cond ((functionp predicate) (funcall predicate ical-item))  ;; whole item
          ((symbolp   predicate) (assq predicate attributes))    ;; has attr
          ;; attribute specific predicates
          ((and (consp predicate)
                (setq attr-val (nth 2 (assq (car predicate) attributes))))
           (setq attr-pred (cdr predicate))
           (cond ((stringp attr-pred) ;; regex or string equality
                  (if (eq (aref attr-pred 0) ?~)
                      (string-match (substring attr-pred 1) attr-val)
                    (equal attr-pred attr-val)))
                 ;; attr-specific predicate function
                 ;; plus target value for said function
                 ((consp attr-pred)
                  (setq attr-func (car attr-pred))
                  (if (functionp attr-func)
                      (funcall attr-func attr-val (cdr attr-pred))))))
          nil)))

(defun caldav-filter-apply-predicates (ical-item predicates)
  (let ((matched t) (attributes (nth 2 ical-item)))
    (while (and matched predicates)
      (setq matched
            (caldav-filter-apply-pred ical-item attributes (car predicates))
            predicates (cdr predicates)))
    matched))

(defun caldav-filter-items (ical type predicates &optional function)
  "Take a CalDAV/iCal alist, as returned by `caldav-ical-to-alist' and
filter it by TYPE (a symbol or list of symbols like 'VEVENT) and
all the entries in PREDICATES, which may be as follows:\n
    function - the function is called on the ical-data
    symbol   - the ical object (eg event) must have said attribute (eg 'DTEND)
    (symbol . attr-predicate) -
      The attribute of the ical object specified by symbol must satisfy
      attr-predicate. attr-predicate may be:
        \"~string-regex\"  - a regex which is tested with string-match
        \"string\"         - a string to match exactly
        (function . value) - a function which will be called as:
                             (function value-of-attribute value)\n
If the iCal object satisfies all predicates (regexes and strings match,
required attributes exist and function calls return a non-nil value)
then FUNCTION is called on the (url . ical-data) cons cell.\n
Omitting FUNCTION or passing nil is equivalent to passing 'identity.\n
The return value is a list consisting of the return values of all the calls
to FUNCTION."
  (apply 'append
         (mapcar
          ;; calendar is a (url . ical-data) cons
          (lambda (calendar &optional calendar-items)
            ;; calendar-items is a list of (VEVENT… ) and so forth
            ;; (ical objects) as per `icalendar--read-element'
            (setq calendar-items (nth 3 (cadr calendar)))
            (delq nil (mapcar
                       (lambda (i &optional itype)
                         (setq itype (car i))
                         (and (cond ((listp   type) (memq itype type))
                                    ((symbolp type) (eq   itype type)) (t))
                              (caldav-filter-apply-predicates i predicates)
                              (if (functionp function) (funcall function i) i)))
                       calendar-items)))
          ical)))

