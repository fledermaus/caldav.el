(when (or load-file-name buffer-file-name)
  (add-to-list 'load-path
               (file-name-directory (or load-file-name buffer-file-name)))
  (eval-and-compile
    (require 'caldav)
    (require 'icalendar)
    (require 'icalendar-write))

  (let (caldav ical item)
    (setq caldav (caldav-fetch-ical)
          ical   (caldav-ical-to-alist caldav)
          pred   '((SUMMARY    . "~ilg\\|intel")
                   (CATEGORIES . "~call"))
          item   (caldav-filter-items ical '(VEVENT) pred))
    (mapc
     (lambda (x &optional vcal)
       (setq vcal (cadr x))
       (icalendar--add-timezone vcal "US/Pacific")
       (mapc (lambda (event &optional dt)
               (dolist (prop '(DTSTART DTEND))
                 (setq dt (icalendar--get-event-property event prop)
                       dt (replace-regexp-in-string "T16" "T08" dt))
                 (icalendar--set-event-property event prop dt '(TZID "US/Pacific"))))
             (icalendar--get-children vcal 'VEVENT))
       (let ((url-debug t)) (caldav-put-ical (car x) (cdr x)))
       )
     item)
    t))



