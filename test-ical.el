(when (or load-file-name buffer-file-name)
  (add-to-list 'load-path
               (file-name-directory (or load-file-name buffer-file-name)))
  (require 'icalendar)
  (require 'icalendar-rrule))

(defun test-ical-print-date (dt)
  (princ (format-time-string "%Y-%m-%d %T (%a) %z" (apply 'encode-time dt)))
  (princ "\n"))

(defun test-ical-print-event (event zmap &optional tz end)
  (princ "---------------------------------------------------------------\n")
  (princ (icalendar--get-event-property event 'DESCRIPTION))
  (princ "\n")
  (mapc 'test-ical-print-date (icalendar--rr-occurrences event zmap end tz))
  (princ "---------------------------------------------------------------\n"))

(defun test-ical-event-as-string (event zmap &optional tz end)
  (with-temp-buffer
    (let ((standard-output (current-buffer)))
      (test-ical-print-event event zmap tz end)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun test-ical-get-event-unpack-end-time (e)
  (let ((uid (icalendar--get-event-property e 'X-TEST-DATA)))
    (and uid (string-match "(unpack-until\\s-+\\(\\S-+\\))" uid)
         (apply 'encode-time
          (icalendar--decode-isodatetime (match-string 1 uid) nil t)))))

(defun test-ical-print-events (file)
  (let (ical-data zone-map)
    (setq ical-data (with-temp-buffer (insert-file-contents file)
                                      (goto-char (point-min))
                                      (icalendar--read-element nil nil))
          zone-map  (icalendar--convert-all-timezones ical-data))
    (mapc (lambda (e &optional uid end printed)
            (setq end     (test-ical-get-event-unpack-end-time e)
                  uid     (icalendar--get-event-property e 'UID)
                  printed (test-ical-event-as-string e zone-map
                                                     "Europe/London" end))
            (with-temp-file (format "data/unpacked/%s.txt" uid)
              (insert printed)))
          (icalendar--all-events ical-data))
    nil))

;; (test-ical-print-events "./data/rfc2445-15min-x6.ics")
;; (test-ical-print-events "./data/rfc2445-january.ics")
;; (test-ical-print-events "./data/rfc2445-monthly-pen-penultimate-day.ics")
;; (test-ical-print-events "./data/rfc2445-yearly-yeardays.ics")
;; (test-ical-print-events "./data/rfc2445-3rd-of-3-for-3-months.ics")
;; (test-ical-print-events "./data/rfc2445examples.ics")
;; (test-ical-print-events "./data/rfc2445-byday-byweekno.ics")
