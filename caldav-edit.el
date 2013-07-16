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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ok, let's work this out:
;; start with a date range
;; fetch all VCAL blobs for that date range
;; expand all the ocurrences if there are repeating events
;; we should now have:
;; • a lookup table of V* item UIDs to VCALENDAR blobs
;; • a sorted list of occurrences (start end UID)
;; now we traverse the time period, day by day
;; ◦ insert a day header for each new day
;; ◦ pop ocurrences off the list as long as they are in the current day
;; ◦ render each occurrence into the buffer
;; ◦ proceed to the next day
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
