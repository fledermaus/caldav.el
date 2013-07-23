;; -*- lexical-binding: t -*-
;;; caldav-edit.el --- CalDAV (calendaring) UI

;; Copyright © 2013  Vivek Dasmohapatra <vivek@etla.org>

;; Author: Vivek Dasmohapatra <vivek@etla.org>
;; Maintainer: Vivek Dasmohapatra <vivek@etla.org>
;; Keywords: dav, caldav

;; This file is NOT part of GNU Emacs.

;; caldav-edit.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; caldav-edit.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with caldav.el.  If not, see <http://www.gnu.org/licenses/>.
(require 'caldav)
(require 'icalendar)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ok, let's work this out:
;; start with a date range
;; fetch all VCAL blobs for that date range
;; expand all the ocurrences if there are repeating events
;; we should now have:
;; • the original alist uf URLs to VCALENDAR containers
;; • a lookup table of V* item UIDs to VCALENDAR blobs
;;   ◦ the blobs will be the sub-structures of the vcalendar containers
;; • a lookup table of UIDs to urls.
;; • a sorted list of occurrences (start end UID)
;; if we update the blobs in the second lookup table, since they are
;; references into the containers in the first lookup table, we can
;; serialise the containers in the first table when we write back
;; after changing them
;; now we traverse the time period, day by day
;; ◦ insert a day header for each new day
;; ◦ pop ocurrences off the list as long as they are in the current day
;; ◦ render each occurrence into the buffer
;; ◦ proceed to the next day
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun caldav-edit-fetch-data (&optional url start end)
  "Fetch and parse the ical data from URL (defaults to `caldav-default-url')
from START to END (`encode-time' style time values, defaulting to the
beginning of the month for START and one year after START for END).\n
Returns a 4 item vector:
  0 - a URL/vcalander struct alist, as returned by `caldav-ical-to-alist'
  1 - a UID to URL alist (keys are VEVENT etc UIDs, values are caldav endpoints)
  2 - a UID to VEVENT/etc struct alist (VTODO etc not yet included)
  3 - a sorted alist of ocurrence times (per `decode-time') to UID
The values of item #2 are shared with the overall struct in item 0, this means:
  • You can alter a value or values in #2
  • look up the URL of the altered items in #1
  • serialise the corresponding container in #0
  • write said serialised container back to the URL
Which will result in an updated calendar.\n
It's done this way because each URL can contain multiple UID-identified
VEVENT/etc items - ther server cares that things get written back to 
the right containers (if only to avoid spurious duplicates) but the user
likely does not."
  (let (url-vcal uid-item uid-url filter occur)
    (setq filter
          (lambda (url-ical &optional url zmap)
            (setq url (car url-ical))
            (setq zmap (icalendar--convert-all-timezones (cdr url-ical)))
            (mapc (lambda (ical)
                    (mapc (lambda (e &optional uid olist)
                            (setq uid (icalendar--get-event-property e 'UID)
                                  uid-item (cons (cons uid e  ) uid-item)
                                  uid-url  (cons (cons uid url) uid-url ))
                            (mapcar (lambda (o)
                                      (setq occur (cons (cons o uid) occur)))
                                    (icalendar--rr-occurrences e zmap)))
                          (icalendar--get-children ical 'VEVENT)))
             (cdr url-ical))
            url-ical))
    (setq url-vcal (caldav-ical-to-alist
                    (caldav-fetch-ical url start end) t url))
    (caldav-filter-items url-vcal '(VEVENT) nil filter)
    (setq occur
          (sort occur (lambda (x y) (icalendar--rr-date-< (car x) (car y)))))
    (vector url-vcal uid-item uid-url occur)))
