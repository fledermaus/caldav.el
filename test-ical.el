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

(defun test-ical-print-events (file)
  (let (ical-data zone-map)
    (setq ical-data (with-temp-buffer (insert-file-contents file)
                                      (goto-char (point-min))
                                      (icalendar--read-element nil nil))
          zone-map  (icalendar--convert-all-timezones ical-data))
    (mapc (lambda (e)
            (test-ical-print-event e zone-map))
          (icalendar--all-events ical-data)) ))

;; (test-ical-print-events "./data/rfc2445examples.ics")
