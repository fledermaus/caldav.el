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
(defun caldav-fetch-ical (&optional url start end)
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
