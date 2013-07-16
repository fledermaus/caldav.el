;; -*- lexical-binding: t -*-
(require 'tzinfo)
(require 'icalendar-rrule)

;; ======================================================================
;; write support - updating ical contents
;; ======================================================================
(defun icalendar--set-event-property (event prop value &optional attr coerce)
  (let ((prop-list (nth 2 event)) cell cvalue old-attr)
    ;; events MUST have a UID, which is always the 1st prop in the list
    ;; so we can always splice values in at the cdr of the prop-list,
    ;; likewise since we want delq of the UID cell to be a no-op, we
    ;; can just delq without worrying about delq'ing the head of the list:
    (setq cvalue   (funcall (or coerce 'identity) value)
          cell     (assq prop prop-list)
          old-attr (cadr cell))
    (if cvalue
        (if cell
            (progn (setcdr (cdr cell) (cons cvalue nil))
                   (setcar (cdr cell) (or attr old-attr)))
          (setcdr prop-list (cons (list prop (or attr old-attr) cvalue)
                                  (cdr prop-list))))
      ;; cvalue of `nil' means remove the value
      (delq cell prop-list))))

(defun icalendar--seconds-to-offset (seconds)
  (let (hours minutes)
    (setq minutes (abs (/ seconds 60))
          minutes (% minutes 60)
          hours   (abs (truncate (/ seconds 3600.0))))
    (format "%c%02d%02d" (if (< seconds 0) ?- ?+) hours minutes)))

(defun icalendar--tzspec-to-rrule (tzdata rule)
  "Convert the time zone specification from `tzinfo-posix-string-to-data'
into an RRULE string.
TZDATA is the structure returned by `tzinfo-posix-string-to-data'
RULE is :std or :dst, indicating a standard or daylight-saving rule
should be extracted."
  (let (spec rtype n)
    (cond ((eq :std rule) (setq spec :dst-end-date))
          ((eq :dst rule) (setq spec :dst-start-date)))
    (setq spec  (cdr (assq spec tzdata))
          rtype (car spec))
    (cond ((eq :week-day rtype)
           (setq n (nth 2 spec))
           (format "FREQ=YEARLY;BYMONTH=%d;BYDAY=%d%s"
                   (nth 1 spec)
                   (if (eq 5 n) -1 n)
                   (aref icalendar--weekday-array (nth 3 spec))))
          ((eq :year-day rtype)
           (error "year-day style timezone shifts not supported yet")))))

(defun icalendar--make-timezone (tzname &optional tzid extra-props)
  "Take a time zone name and return an rfc2445 VTIMEZONE structure.
TZNAME is a tzfile(5) zone name.
TZID is an arbitrary ident string (defaulting to TZNAME).
EXTRA-PROPS is an optional list of timezone properties
  eg: '((X-LIC-LOCATION nil \"Somewhere/Rainbow\"))."
  (let ((tzdata (cadr (tzinfo-data tzname))))
    ;; must have version 2 zone data as v1 does not supply the POSIX spec:
    (if (setq tzdata (assq :zone-data tzdata)
              tzdata (assq 'posix tzdata)
              tzdata (cdr tzdata))
        (if (setq tzdata (tzinfo-posix-string-to-data tzdata))
            (let (standard std-offset std-name std-rrule std-startt end-span
                  daylight dst-offset dst-name dst-rrule dst-startt)
              (when (assq :offset tzdata)
                (setq std-offset (cdr (assq :offset tzdata))
                      dst-offset (or (cdr (assq :dst-offset tzdata)) std-offset)
                      std-offset (icalendar--seconds-to-offset std-offset)
                      dst-offset (icalendar--seconds-to-offset dst-offset)
                      std-startt (cdr (assq :dst-end-time tzdata))
                      std-startt (replace-regexp-in-string ":" "" std-startt)
                      std-startt (format "19700101T%s" std-startt)
                      std-rrule  (icalendar--tzspec-to-rrule tzdata :std)
                      end-span   "19710101T000000"
                      std-startt (icalendar--rr-apply-rrule
                                  std-rrule std-startt end-span nil tzname)
                      std-startt (car std-startt)
                      std-startt (mapcar
                                  (lambda (y) (nth y std-startt))
                                  '(5 4 3 2 1 0))
                      std-startt (apply 'format
                                        "%04d%02d%02dT%02d%02d%02d" std-startt)
                      std-name   (cdr (assq :name tzdata))
                      standard  `(STANDARD nil
                                           ((TZOFFSETFROM nil ,dst-offset)
                                            (TZOFFSETTO   nil ,std-offset)
                                            (TZNAME       nil ,std-name  )
                                            (DTSTART      nil ,std-startt)
                                            ,@(if std-rrule
                                                  `((RRULE nil ,std-rrule))))
                                           nil)))
              (when (and standard (assq :dst-offset tzdata))
                (setq dst-startt (cdr (assq :dst-start-time tzdata))
                      dst-startt (replace-regexp-in-string ":" "" dst-startt)
                      dst-startt (format "19700101T%s" dst-startt)
                      dst-rrule  (icalendar--tzspec-to-rrule tzdata :dst)
                      end-span   "19710101T000000"
                      dst-startt (icalendar--rr-apply-rrule
                                  dst-rrule dst-startt end-span nil tzname)
                      dst-startt (car dst-startt)
                      dst-startt (mapcar
                                  (lambda (y) (nth y dst-startt))
                                  '(5 4 3 2 1 0))
                      dst-startt (apply 'format
                                        "%04d%02d%02dT%02d%02d%02d" dst-startt)
                      dst-name   (cdr (assq :dst-name tzdata))
                      daylight  `(DAYLIGHT nil
                                           ((TZOFFSETFROM nil ,std-offset)
                                            (TZOFFSETTO   nil ,dst-offset)
                                            (TZNAME       nil ,dst-name  )
                                            (DTSTART      nil ,dst-startt)
                                            ,@(if dst-rrule
                                                  `((RRULE nil ,dst-rrule))))
                                           nil)))
              `(VTIMEZONE nil
                          ((TZID nil ,(or tzid tzname)) ,@extra-props)
                          (,@(if daylight (list daylight))
                           ,@(if standard (list standard)))) ))) ))

(defun icalendar--add-timezone (icalendar timezone)
  "Given an ICALENDAR data structure (ie a parsed VCALENDAR object) and
a time zone TIMEZONE (a tzile(5) name like \"America/Jamaica\" or
a timezone structure as returned by `icalendar--make-timezone'), insert
the timezone so specified into ICALENDAR.
If a timezone with the same TZID exists in ICALENDAR, it will be replaced.
ICALENDAR is modified in place.
Returns the modified ICALENDAR."
  (if (stringp timezone) (setq timezone (icalendar--make-timezone timezone)))
  (let (blobs b cell id item)
    ;; look for en existing tz entry that matches:
    (setq blobs (nth 3 icalendar)
          id    (assq 'TZID (nth 2 timezone))
          b     blobs)
    (while (and (not cell) b)
      (setq item (car b))
      (and (eq 'VTIMEZONE (car item))
           (equal (assq 'TZID (nth 2 item)) id)
           (setq cell item))
      (setq b (cdr b)))
    ;; replace existing entry if there is one, otherwise add new zone
    (if cell
        (setcdr cell (cdr timezone))
      (setf (nth 3 icalendar) (cons timezone blobs)))
    icalendar))

;; ======================================================================
;; write support - serialising an ical structure:
;; ======================================================================
(defun icalendar--serialise-element (element)
  (with-temp-buffer
    (if (listp (car element))
        (mapc 'icalendar--serialise-element-i element)
      (icalendar--serialise-element-i element))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun icalendar--serialise-props (props)
  (while props
    (insert ";" (symbol-name (car props)) "=" (cadr props))
    (setq props (cddr props))))

(defun icalendar--serialise-attribute (attr)
  (insert (symbol-name (car attr)))
  (icalendar--serialise-props (cadr attr))
  (insert ":" (nth 2 attr) "\n"))

(defun icalendar--serialise-element-i (element)
  (let (x type prop attr data)
    (setq x    element
          type (pop x)
          prop (pop x)
          attr (pop x)
          data (pop x))
    (insert "BEGIN:" (symbol-name type))
    (icalendar--serialise-props prop)
    (insert "\n")
    (mapc 'icalendar--serialise-attribute attr)
    (mapc 'icalendar--serialise-element-i data)
    (insert "END:" (symbol-name type) "\n")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'icalendar-write)
