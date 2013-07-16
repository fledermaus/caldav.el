;; -*- lexical-binding: t -*-
(require 'cl)

(require 'icalendar)

(defmacro with-timezone (tz &rest body)
  "Execute BODY with timezone TZ in effect (as set by `set-time-zone-rule').
Values for TZ include:\n
  a tzfile(5) string : eg \"Antarctica/South_Pole\"
  a POSIX TZ string  : eg \"PST8PDT,M3.2.0,M11.1.0\"
  t                  : UTC
  nil                : no effect - use the current default"
  (declare (indent 1))
  `(let ((cz (getenv "TZ")) rv)
     (set-time-zone-rule (or ,tz cz))
     (setq rv (progn ,@body))
     (set-time-zone-rule cz) rv))

(defconst icalendar--rr-freqs
  '(SECONDLY MINUTELY HOURLY DAILY WEEKLY MONTHLY YEARLY))

(defconst icalendar--rr-handlers
  '(icalendar--rr-freq
    icalendar--rr-interval
    icalendar--rr-bymonth
    icalendar--rr-byweekno
    icalendar--rr-byyearday
    icalendar--rr-bymonthday
    icalendar--rr-byday
    icalendar--rr-byhour
    icalendar--rr-byminute
    icalendar--rr-bysecond
    icalendar--rr-bysetpos
    icalendar--rr-zonekludge
    icalendar--rr-flatten))

(defconst icalendar--rr-jumpsizes
  '((SECONDLY . 1     )
    (MINUTELY . 60    )
    (HOURLY   . 3600  )
    (DAILY    . 86400 )
    (WEEKLY   . 604800)))

(defconst icalendar--rr-dow-values
  '(("SU" . 0)
    ("MO" . 1)
    ("TU" . 2)
    ("WE" . 3)
    ("TH" . 4)
    ("FR" . 5)
    ("SA" . 6)))

(defconst icalendar--rr-dt-slots '((:sec . 0) (:min . 1) (:hour . 2)
                                   (:day . 3) (:mon . 4) (:year . 5)
                                   (:dow . icalendar--rr-merge-date-dow)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utilities for rrule element parsing:
(defun icalendar--rr-decode-isodatetime (isodatetimestring)
  "Like `icalendar--decode-isodatetime', except we are explicitly
not interested in any timezone info at this point. This is because
for rrule manipulation we just want to make a series of ‘logical’
shifts (eg if that isodatetimestring says 16:00:00, we actually
want all subsequent events to be at 16:00:00 regardless of DST shifts)."
  (let ((year  (read (substring isodatetimestring 0 4)))
        (month (read (substring isodatetimestring 4 6)))
        (day   (read (substring isodatetimestring 6 8)))
        (hour   0)
        (minute 0)
        (second 0))
    (when (> (length isodatetimestring) 12)
      ;; hour/minute present
      (setq hour   (read (substring isodatetimestring 9  11)))
      (setq minute (read (substring isodatetimestring 11 13))))
    (when (> (length isodatetimestring) 14)
      ;; seconds present
      (setq second (read (substring isodatetimestring 13 15))))
    ;; create the decoded date-time
    (list second minute hour day month year 0)))


(defun icalendar--rr-ev-prop-attr (event prop attr &optional zone-map)
  "Extract a single rfc2445 event property attribute from EVENT, a parsed
ical event. PROP and ATTR are symbols.
ZONE-MAP is the map returned by `icalendar--convert-all-timezones'\n
Returns the string value of the attribute, if found, or nil.
If ZONE-MAP is supplied, and ATTR is 'TZID the timezone is looked up in
ZONE-MAP as per `icalendar--find-time-zone' and that value is returned instead."
  (let (attribs value)
    (setq attribs (icalendar--get-event-property-attributes event prop)
          value   (memq attr attribs))
    ;; found a value, and looking at TZID, and we have zone map? convert:
    (if (and value (eq 'TZID attr) zone-map)
        (icalendar--find-time-zone value zone-map)
      (cadr value))))

(defun icalendar--rr-merge-date-dow (template date &optional month)
  "Move DATE forwards, 24 hours at a time (if necessary) to
arrive at a date matching the day of week in TEMPLATE (which can be a
`decode-time' value or an integer between 0 and 6 (inclusive)).\n
If MONTH is specified, the generated date must be in MONTH (1-12)
or nil will be returned.
Retruns a decode-time value (which may be the original if no shift occurred)."
  (let ((dow (if (listp template) (nth 6 template) template))
        epoch old new-date jump)
    (if (eq (nth 6 date) dow)
        date
      (setq epoch    (float-time (apply 'encode-time date))
            old      (nth 6 date)
            jump     (- dow old)
            jump     (if (< jump 0) (+ 7 jump) jump)
            epoch    (+ (* 86400 jump) epoch)
            new-date (decode-time (seconds-to-time epoch)))
      (if month
          (if (eq (nth 4 new-date) month) new-date)
          new-date))))

(defun icalendar--rr-merge-date (template new-date &rest slots)
  "Merge two dates (`decode-time' values) : Values from TEMPLATE corresponding
to the keywords in SLOTS (:sec :min :hour :day :mon :year and :dow) are
copied into NEW-DATE.\n
In the case of :dow the date components (day month and year) are altered
to find the next date which matches the day-of-week in TEMPLATE (if the
day-of-week already matches, this is a no-op).\n
Returns a `decode-time' value, which may be the destructively altered
NEW-DATE or may be a new value, depending on what transformations were
necessary."
  (mapc (lambda (slot)
          (if (setq slot (cdr (assq slot icalendar--rr-dt-slots)))
            (if (integerp slot)
                (setf (nth slot new-date) (nth slot template))
              (setq new-date (funcall slot template new-date)))))
        slots)
  new-date)

(defun icalendar--rr-jump (n step start-time start)
  "Return a `decode-time' style value resulting from jumping N
STEP units of time into the future from START-TIME. START is the
encoded time-value corresponding to START-TIME.\n
Note that such decoded time values may be for ‘impossible’ dates
like the 31st of February - we must still generate such dates as
they provide the base-dates on which the byxxx rules operate,
which may or may not result in a more workable date value."
  (let (x y m z)
    ;; discard all zone info: we explicitly do not want to know about it
    ;; at this point:
    (if (> (length start-time) 6)
        (setq start-time
              (mapcar (lambda (n) (nth n start-time)) '(0 1 2 3 4 5))))
    (cond
     ;; no jump - DTSTART is always the returned value for this:
     ((zerop n) (setq x start-time))
     ;; the simple cases: seconds -> weeks are well defined units of time
     ((setq x (assq step icalendar--rr-jumpsizes))
      (setq x (+ (float-time start) (* n (cdr x)))
            x (seconds-to-time x)
            x (decode-time x))
      (setf (nth 8 x) 0))
     ;; into the rigidly defined areas of doubt and uncertainty:
     ;; jump by n months. What is a month anyway? "It depends"
     ((eq 'MONTHLY step)
      (setq x (copy-sequence start-time)
            m (nth 4 x))
      (setf (nth 4 x) (+ n m))
      ;; this can result in a nonsensical month (> 12):
      ;; if this is the case, tweak the year and month values
      ;; appropriately (nonsensical days we keep for now):
      (setq m (nth 4 x))
      (when (> m 12)
        (setq z (mod m 12)
              y (nth 5 x ))
        (setf (nth 4 x) (if (zerop z) 12 z))
        (setf (nth 5 x) (+ (/ m 12) y))))
     ;; jump by years. impossible days allowed, as with the MONTHLY case
     ((eq 'YEARLY step)
      (setq x (copy-sequence start-time)
            y (nth 5 x))
      (setf (nth 5 x) (+ n y)) ))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; if we crossed a DST boundary, tweak the time to compensate:
    ;; (this does not apply to recurrences of finer granularity than 1 day):
    (if (memq step '(DAILY WEEKLY MONTHLY YEARLY))
        (setq x (icalendar--rr-merge-date start-time x :sec :min :hour)))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; discard any generated zone info, which will be bogus at this point.
    (if (> (length x) 6)
        (setq x (mapcar (lambda (n) (nth n x)) '(0 1 2 3 4 5))))
    (append x (list 0))))


(defun icalendar--rr-day-possible-p (candidate-time)
  (let ((processed-time))
    (setq processed-time (decode-time (apply 'encode-time candidate-time)))
    (eq (nth 3 candidate-time)
        (nth 3 processed-time)) ))

(defun icalendar--rr-byxxx-effect (rule-data by-type)
  "Given RULE-DATA (as constructed internally by `icalendar--rr-occurrences')
return :expands or :restricts depending on whether the application of the
byxxx rule of type BY-TYPE should result in more or fewer event instances."
  (let ((r-type (cdr (assq :last-freq rule-data))))
    (if (memq by-type (memq r-type icalendar--rr-freqs)) :restricts :expands)))

(defconst icalendar--rr-ordinal-day-re
  (concat "^\\([-+]?[0-9]+\\)"
          (regexp-opt (mapcar 'car icalendar--rr-dow-values) t)))

(defun icalendar--rr-byxxx-ordinal-byday (text)
  (if (string-match icalendar--rr-ordinal-day-re text)
      (cons (string-to-number (match-string 1 text))
            (cdr (assoc (match-string 2 text) icalendar--rr-dow-values))) ))

(defun icalendar--rr-byxxx-element (text)
  (or (cdr (assoc text icalendar--rr-dow-values))
      (icalendar--rr-byxxx-ordinal-byday text)
      (string-to-number text 10)) )

(defun icalendar--rr-byxxx-to-data (text)
  (if text (mapcar 'icalendar--rr-byxxx-element (split-string text "," t)) nil))

(defun icalendar--rr-expand-occurlist (slot bset occurs)
  (let (new-occur p)
    (mapc (lambda (o)
            (mapc (lambda (x)
                    (setq p (copy-sequence o))
                    (setf (nth slot p) x)
                    (setq new-occur (cons p new-occur))) bset)) occurs)
    (nreverse new-occur)))

(defun icalendar--rr-day-to-weekno (dt &optional day0)
  (let (start target (n 1))
    (setq start  (icalendar--rr-week1-start (nth 5 dt) day0)
          start  (apply 'encode-time start)
          start  (float-time start)
          target (apply 'encode-time dt)
          target (float-time target)
          target (- target 604800))
    (while (< start target)
      (setq start (+ start 604800) n (1+ n)))
    (cons n (decode-time (seconds-to-time start))) ))

(defun icalendar--rr-weekdays-in (dt period day)
  (let (start time date finish dlist (year (nth 5 dt)) keep)
    (cond ((eq period 'YEARLY)
           (setq start  (icalendar--rr-nth-day year  1 dt)
                 finish (icalendar--rr-nth-day year -1 dt)))
          ((eq period 'MONTHLY)
           (setq start  (icalendar--rr-monthday dt  1)
                 finish (icalendar--rr-monthday dt -1)
                 keep   (nth 4 dt)))
          ((eq period 'WEEKLY)
           (setq start  (icalendar--rr-day-to-weekno dt)
                 finish (icalendar--rr-nth-week year (1+ (car start)))
                 start  (cdr start))))
    (when (and start finish)
      (setq date   (icalendar--rr-merge-date dt start :sec :min :hour)
            time   (float-time (apply 'encode-time date  ))
            finish (float-time (apply 'encode-time finish)))
      (while (<= time finish)
        (if (setq date  (decode-time (seconds-to-time time))
                  date  (icalendar--rr-merge-date-dow day date keep))
            (setq date  (icalendar--rr-merge-date dt date :sec :min :hour)
                  dlist (cons date dlist)))
        (setq time (+ 604800 time))))
    dlist))

(defun icalendar--rr-date-< (d0 d1)
  (time-less-p (apply 'encode-time d0)
               (apply 'encode-time d1)))

(defun icalendar--rr-date-= (d0 d1)
  (equal (apply 'encode-time d0)
         (apply 'encode-time d1)))

(defun icalendar--rr-nth-weekday-in (dt period day)
  (let (dlist n size)
    (setq n     (car day)
          day   (cdr day)
          dlist (icalendar--rr-weekdays-in dt period day)
          size  (length dlist))
    (if (> (abs n) size)
        nil
      (setq n     (if (< n 0) (+ size n) (1- n))
            dlist (sort dlist 'icalendar--rr-date-<))
      (list (nth n dlist))) ))

(defun icalendar--rr-expand-occurlist-byday (bset period occurs dtstart)
  (let (new-occur)
    (mapc
     (lambda (o)
       (mapc
        (lambda (x &optional dl)
          (if (setq dl (cond ((consp    x)
                              (icalendar--rr-nth-weekday-in o period x))
                             ((integerp x)
                              (icalendar--rr-weekdays-in o period x))))
              (setq dl (mapcar
                        (lambda (d)
                          (icalendar--rr-merge-date dtstart d :sec :min :hour))
                        dl)
                    new-occur (nconc dl new-occur)))) bset)) occurs)
    new-occur))

(defun icalendar--rr-restrict-occurlist-byday (bset period occurs _dtstart)
  (let (cardinal ordinal dlist maybe)
    (mapc (lambda (d)
            (if (consp d)
                (setq ordinal (cons d ordinal))
              (setq cardinal (cons d cardinal)))) bset)
    ;; simple restriction to days of week in ruleset:
    (mapc (lambda (o)
            (if (memq (nth 6 o) cardinal)
                (setq dlist (cons o dlist)))) occurs)
    (when ordinal
      (mapc (lambda (d)
              (mapc
               (lambda (o &optional t0 t1)
                 (when (setq maybe (icalendar--rr-nth-weekday-in o period d))
                   (setq t1 (decode-time (apply 'encode-time (car maybe)))
                         t0 (decode-time (apply 'encode-time o)))
                   (if (equal t0 t1)
                       (setq dlist (cons o dlist))))) occurs)) ordinal))
    dlist))

(defun icalendar--rr-month-max (dt)
  "Where DT is a `decode-time' style date (only the year and month are
inspected) return the highest valid day-of-month in that month."
  (let ((m (nth 4 dt))
        (y (nth 5 dt)))
    (cond
     ((memq m '(1 3 5 7 8 10 12)) 31)
     ((memq m '(4 6 9 11))        30)
     ((eq m 2)
      ;; Yes, I know the % 100 % 400 % 4 thing. No, I'm not using it here.
      (if (eq (nth 4 (decode-time (encode-time 0 0 0 29 2 y))) 2) 29 28)) )))

(defun icalendar--rr-monthday (dt day)
  "Adjust `decode-time' date value DT to RFC2445 month-day value DAY"
  (when (and (integerp day) (not (zerop day)) (<= (abs day) 31))
    (let ((max (icalendar--rr-month-max dt)) new-day)
      (if (< day 0) (setq day (+ 1 max day)))
      (if (or (<= day 0) (> day max))
          nil
        (setq new-day (copy-sequence dt))
        (setf (nth 3 new-day) day)
        (setq new-day (decode-time (apply 'encode-time new-day)))
        (icalendar--rr-merge-date dt new-day :sec :min :hour)) )))

(defun icalendar--rr-nth-day (year n &optional dtstart)
  (when (and (>= n -366) (<= n 366) (not (zerop n)))
    (let (origin m epoch new-date)
      (setq origin (copy-sequence dtstart)
            origin (nthcdr 6 origin))
      (if (< n 0)
          (setq origin (funcall 'encode-time 0 0 0 31 12 year t) m (1+ n))
        (setq origin (funcall 'encode-time 0 0 0 1 1 year t) m (1- n)))
      (setq origin   (float-time origin)
            epoch    (+ (* 86400 m) origin)
            new-date (decode-time (seconds-to-time epoch)))
      ;; if we jumped to a valid date, merge the time back in:
      (if (eq (nth 5 new-date) year)
          (if dtstart
              (icalendar--rr-merge-date dtstart new-date :sec :min :hour)
            new-date)
        nil) )))

(defun icalendar--rr-week1-start (year &optional day0)
  "Return a decoded time value for the ISO8601 first week of the year.
YEAR is a full 4-digit year. DAY0 defaults to 1, with 0 meaning Sunday
and 6 meaning Saturday."
  (let (new-year new-epoch new-date first-week-date first-epoch)
    (or day0 (setq day0 1))
    (setq new-year        (encode-time 0 0 0 1 1 year 0)
          new-epoch       (float-time new-year)
          new-date        (decode-time new-year)
          first-week-date new-date
          first-epoch     new-epoch)
    (while (not (eq (nth 6 first-week-date) day0))
      (setq first-epoch     (+ 86400 first-epoch)
            first-week-date (decode-time (seconds-to-time first-epoch))))
    ;; ISO 8601 says: a week with >= 4 days is part of this year.
    ;; even if said week would then actually begin in the year before:
    (if (> (nth 3 first-week-date) 4)
        (setq first-epoch (- first-epoch 604800)
              first-week-date (decode-time (seconds-to-time first-epoch))))
      first-week-date))

(defun icalendar--rr-weekN-start (year &optional day0)
  "Return a decoded time value for the ISO8601 last week of the year.
YEAR is a full 4-digit year. DAY0 defaults to 1, with 0 meaning Sunday
and 6 meaning Saturday."
  (let ((date (icalendar--rr-week1-start (1+ year) day0)))
    (decode-time
     (seconds-to-time (- (float-time (apply 'encode-time date)) 604800))) ))

(defun icalendar--rr-nth-week (year n &optional day0)
  "Return the decoded time value for week N in YEAR (as per ISO 8601).
Negative values for N count backwards from the last week of the year"
  (when (and (>= 53 n) (<= -53 n) (not (zerop n)))
    (let (start-date start result m)
      (cond ((< n 0)
             (setq m (1+ n) start-date (icalendar--rr-weekN-start year day0)))
            ((> n 0)
             (setq m (1- n) start-date (icalendar--rr-week1-start year day0))))
      (setq start  (float-time (apply 'encode-time start-date))
            result (+ (* 604800 m) start)
            result (decode-time (seconds-to-time result))) )))

(defun icalendar--rr-apply-rule-to-group (olist rule)
  (delq nil (mapcar (lambda (o) (delq nil (mapcar rule o))) olist)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; RECUR rule part parsers.
;; these parsers rely on being called on a gven event in the order
;; defined in icalendar--rr-handlers (which is the same as defined in
;; RFC2445 (http://www.ietf.org/rfc/rfc2445.txt))
;; Lasciate ogne speranza, voi ch'intrate:
(defun icalendar--rr-zonekludge (_r data _d tz _s _c _u target)
  (let (cell olist func)
    (setq cell  (assq target data)
          olist (cdr cell)
          func  (lambda (o) (setcar (last o) tz)))
    (icalendar--rr-apply-rule-to-group olist func)))

(defun icalendar--rr-flatten (_r data _d _tz _s _c _u target)
  (let (cell olist)
    (setq cell  (assq target data)
          olist (cdr cell)
          olist (delq nil (apply 'nconc olist)))
    (setcdr (assq target data) olist)))

(defun icalendar--rr-by-x (rule data _dtstart _tz _start _count _until target x)
  (let (bset action olist period slot func)
    (cond ((eq 'BYSECOND x) (setq period 'SECONDLY slot 0))
          ((eq 'BYMINUTE x) (setq period 'MINUTELY slot 1))
          ((eq 'BYHOUR   x) (setq period 'HOURLY   slot 2))
          ((eq 'BYMONTH  x) (setq period 'MONTHLY  slot 4)))
    (when (and slot (setq bset (cadr (assq x rule))))
      (setq action (icalendar--rr-byxxx-effect data period)
            bset   (icalendar--rr-byxxx-to-data bset)
            olist  (cdr (assq target data))
            func   (cond ((eq :expands action)
                          (lambda (o)
                            (icalendar--rr-expand-occurlist slot bset o)))
                         ((eq :restricts action)
                          (lambda (o)
                            (delete-if-not (lambda (dt)
                                             (memq (nth slot dt) bset)) o))))
            olist  (mapcar func olist))
      (setcdr data (cons (cons :bymonth (nconc bset action)) (cdr data)))
      (setcdr (assq :last-freq data) period)
      (setcdr (assq target     data)  olist)) ))

(defun icalendar--rr-bysecond (rule data dtstart tz start count until target)
  (icalendar--rr-by-x rule data dtstart tz start count until target 'BYSECOND))

(defun icalendar--rr-nth (n set &optional size)
  (or size (setq size (length set)))
  (cond ((> n 0) (setq n (- n    1)))
        ((< n 0) (setq n (+ size n)))
        (t (setq n nil)))
  (if (and n (>= n 0) (< n size))
      (nth n set)))

(defun icalendar--rr-subset (olist nthlist)
  "For a list of ordinal values NTHLIST (-N to 1, 1 to N)
return the members of OLIST that correspond to those offsets.
Negative offsets count from the end of OLIST.
Out-of-range values are omitted."
  (let (subset len)
    (setq len (length olist))
    (mapc (lambda (n &optional v)
            (if (setq v (icalendar--rr-nth n olist len))
                (setq subset (cons v subset)))) nthlist)
    (nreverse subset)))

(defun icalendar--rr-bysetpos (rule data _d _t _s _c _u target)
  (let (cell olist (bset (cadr (assq 'BYSETPOS rule))))
    (when bset
      (setq bset  (icalendar--rr-byxxx-to-data bset)
            cell  (assq target data)
            olist (cdr cell)
            olist (mapcar (lambda (x)
                            (setq x (sort x 'icalendar--rr-date-<))
                            (icalendar--rr-subset x bset)) olist))
      (setcdr cell olist))))

(defun icalendar--rr-byminute (rule data dtstart tz start count until target)
  (icalendar--rr-by-x rule data dtstart tz start count until target 'BYMINUTE))

(defun icalendar--rr-byhour (rule data dtstart tz start count until target)
  (icalendar--rr-by-x rule data dtstart tz start count until target 'BYHOUR))

(defun icalendar--rr-byday (rule data dtstart _tz _start _count _until target)
  (let (bset action olist period func byday)
    (when (setq bset (cadr (assq 'BYDAY rule)))
      (setq action (icalendar--rr-byxxx-effect data 'DAILY)
            period (cdr (assq :last-freq data))
            bset   (icalendar--rr-byxxx-to-data bset)
            olist  (cdr (assq target data))
            byday  (cond ((eq :expands   action)
                          'icalendar--rr-expand-occurlist-byday)
                         ((eq :restricts action)
                          'icalendar--rr-restrict-occurlist-byday))
            func   (lambda (o) (funcall byday bset period o dtstart))
            olist  (mapcar func olist))
      (setcdr data (cons (cons :byday bset) (cdr data)))
      (setcdr (assq :last-freq data) 'DAILY)
      (setcdr (assq target     data)  olist)) ))

(defun icalendar--rr-bymonthday (rule data _dtstart _tz _start _count _until target)
  (let (bset olist action func)
    (when (setq bset (cadr (assq 'BYMONTHDAY rule)))
      (setq bset   (icalendar--rr-byxxx-to-data bset)
            action (icalendar--rr-byxxx-effect data 'DAILY)
            olist  (cdr (assq target data)))

      (cond ((eq :expands   action)
             (setq func
                   (lambda (o &optional dlist)
                     (mapc
                      (lambda (day &optional new-day)
                        (if (setq new-day (icalendar--rr-monthday o day))
                            (setq dlist (cons new-day dlist)))) bset) dlist)))
            ((eq :restricts action)
             (setq func
                   (lambda (o &optional dlist)
                     (mapc (lambda (d &optional dlist)
                             (if (eq (nth 3 o) d)
                                 (setq dlist (cons o dlist)))) bset) dlist))) )
      (setq olist (icalendar--rr-apply-rule-to-group olist func)
            olist (mapcar 'car olist))
      (setcdr data (cons (cons :bymonthday bset) (cdr data)))
      (setcdr (assq :last-freq data) 'DAILY)
      (setcdr (assq target     data)  olist)) ))

;; only interpreting BYYEARDAY for YEARLY repeats. Not sure what it
;; would mean in other contexts.
(defun icalendar--rr-byyearday (rule data dtstart _tz _start _count _until target)
  (let (bset olist ylist func)
    (when (and (eq (cdr (assq :last-freq data)) 'YEARLY)
               (setq bset (cadr (assq 'BYYEARDAY rule))))

      (setq bset  (icalendar--rr-byxxx-to-data bset)
            olist (cdr (assq target data))
            func  (lambda (dt) (nth 5 dt))
            ylist (icalendar--rr-apply-rule-to-group olist func)
            ylist (mapcar 'delete-dups ylist)
            func  (lambda (y &optional dlist)
                    (mapc (lambda (d)
                              (if (setq d (icalendar--rr-nth-day y d dtstart))
                                  (setq dlist (cons d dlist)))) bset) dlist)
            ylist (icalendar--rr-apply-rule-to-group ylist func)
            ylist (mapcar 'car ylist))

      (setcdr data (cons (cons :byyearday bset) (cdr data)))
      (setcdr (assq :last-freq data) 'DAILY)
      (setcdr (assq target     data) ylist)) ))

;; byweekno only applies to yearly rules (per RFC2445)
(defun icalendar--rr-byweekno (rule data dtstart _tz _start _count _until target)
  (let (bset olist wlist func)
    (when (and (eq (cdr (assq :last-freq data)) 'YEARLY)
               (setq bset (cadr (assq 'BYWEEKNO rule))))

      (setq bset  (icalendar--rr-byxxx-to-data bset)
            olist (cdr (assq target data))
            func  (lambda (dt &optional wl)
                    (mapc (lambda (n &optional wd)
                            (setq wd (icalendar--rr-nth-week (nth 5 dt) n)
                                  wl (cons wd wlist))) bset) wl)
            wlist (mapcar 'car (icalendar--rr-apply-rule-to-group olist func))
            func  (lambda (dt)
                    (icalendar--rr-merge-date dtstart dt :sec :min :hour :dow))
            wlist (icalendar--rr-apply-rule-to-group wlist func))

      (setcdr data (cons (cons :byweekno bset) (cdr data)))
      (setcdr (assq :last-freq data) 'WEEKLY)
      (setcdr (assq target     data)   wlist)) ))

(defun icalendar--rr-bymonth (rule data dtstart start tz count until target)
  (icalendar--rr-by-x rule data dtstart tz start count until target 'BYMONTH))

(defun icalendar--rr-interval (rule data dtstart _tz start count until target)
  (let (freq interval occurs (j 0) next (last start))
    (setq interval (or (cadr (assq 'INTERVAL rule)) 1)
          freq     (cdr (assq :freq data)))
    (if (stringp interval) (setq interval (string-to-number interval 10)))
    (setcdr data (cons (cons :interval interval) (cdr data)))
    ;; we always generate all base event times for the date range in question:
    ;; even for count limited rules: we do this because we cannot know until
    ;; all the BYxxx rules are applied whether the "impossible" dates that
    ;; can be generated by this process (eg monthly with a dtstart on the 31st)
    ;; should be discarded (eg a 31st dtstart with a `BYDAY' of 15,27) will
    ;; generate acceptable dates in all months, even though there's no 31st
    ;; in half of them.
    (when count
      (setq until (icalendar--rr-jump (* count interval 2) freq dtstart start)
            until (apply 'encode-time until)))
    (while last
      (setq next (icalendar--rr-jump (* j interval) freq dtstart start)
            j    (1+ j)
            last nil)
      (if (<= (float-time (apply 'encode-time next))
              (float-time until))
          (setq occurs (cons next occurs) last next)))
    ;; store the basic list of event-occurrences
    (setcdr data (cons (cons target (mapcar 'list occurs)) (cdr data))) ))

(defun icalendar--rr-freq (rule data _dtstart _tz _start _count _until _target)
  (let (freq)
    (when (setq freq (intern-soft (cadr (assq 'FREQ rule))))
      (setcdr (assq :last-freq data) freq)
      (setcdr (assq :freq      data) freq)) ))
;; end of rrule part parser section.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun icalendar--rr-apply-rrule (rrule dtstart to
                                  &optional from zone edata slot)
  "Apply an rfc2445 repeat-rule for a given range, returning a list of
date-time values conforming to said rule.
The purpose of this function is purely to generate a recurrence list,
for use in rfc2445 event recurrence and timezone rule calculations.
As such, it (deliberately) does not enforce a few rfc2445 constraints:
• The DTSTART is NOT forced into the recurrence set even if it doesn't
  match the RRULE spec.
• The COUNT component of the RRULE is ignored (as it is not possible to
  apply said component correctly for a single RRULE component: It can only
  be meaningfully applied to the combined expansion of all four possible
  recurrence rules: RRULE, RDATE, EXRULE and EXDATE)."
  (let (start until end)
    ;; if the rrule was not already unpacked, deal with that task:
    (if (stringp rrule)
        (setq rrule (icalendar--split-value rrule)))
    ;; likewise unpack the DTSTART value if necessary:
    (if (stringp dtstart)
        (setq dtstart (icalendar--rr-decode-isodatetime dtstart)))
    (if dtstart
        (setq start (apply 'encode-time dtstart)))

    (or edata (setq edata (list t '(:freq nil) '(:last-freq nil))))
    (or slot  (setq slot  :occurs))
    (or zone  (setq zone  "UTC0"))

    (if (stringp from)
        (if (setq from (icalendar--decode-isodatetime from nil zone))
            (setq from (apply 'encode-time from))))
    (or from (setq from dtstart))

    (if (stringp to)
        (if (setq to (icalendar--decode-isodatetime to nil zone))
            (setq to (apply 'encode-time to))))

    (if (setq until (cadr (assq 'UNTIL rrule)))
        (setq until (icalendar--decode-isodatetime until nil zone)
              until (apply 'encode-time until)))

    (if (and to until)
        (setq end (if (time-less-p to until) to until))
      (setq end (or to until)))

    (with-timezone t ;; enforce UTC calculations for this block
      (mapc
       (lambda (handler)
         (funcall handler rrule edata dtstart zone start nil end slot))
       icalendar--rr-handlers))

    ;; sort the generated dates
    (let (dates cell)
      (setq cell  (assq slot edata)
            dates (cdr  cell)
            dates (sort dates 'icalendar--rr-date-<))

      ;; strip out dates before the start
      (while (icalendar--rr-date-< (car dates) from)
        (setq dates (cdr dates)))

      ;; strip out dates after the end
      (setq dates (nreverse dates)
            end   (decode-time end))
      (while (icalendar--rr-date-< end (car dates))
        (setq dates (cdr dates)))

      (nreverse dates))))

(defun icalendar--rr-occurrences (event zone-map &optional end tz)
  "Return a list of time values (as per `decode-time') at which EVENT has
an instance. If there is no recurrence rule entry, then a list with a
single entry (the DTSTART value) is returned.
ZONE-MAP should be the timezone map harvested from the calendar with
`icalendar--convert-all-timezones'.\n
If the recurrence rule exists, but specifies neither a COUNT value nor an
UNTIL entry, then instances are only generated upto END (an emacs time value
as per `encode-time').\n
If neither limiting rule part (COUNT, DTEND) is specified and END is not
supplied, a default of 1 year from the DTSTART value is assumed.\n
TZ, if supplied, gives a TZ environment variable value (either
tzfile(5) or POSIX style) in which you actually want the occurrences.\n
The default is to give you occurrences in the zone they are scheduled in.
eg: A US/Pacific daily event scheduled at 08:00:00 would come out at
either 16:00:00 or 15:00:00 (depending on the date) in Europe/London."
  (let (edata   ;; event-data - accumulate calculated/extracted info
        dtst_z  ;; start date (as per `decode-time')
        dtstart ;; start date (as per `decode-time') sans zone info
        dttext  ;;
        munge   ;; date mangling function
        start   ;; ibid       (as per `encode-time')
        zone    ;; POSIX timezone string for start date (default zone)
        count   ;; number of repetitions
        until   ;; last acceptable date (as per `encode-time')
        rrule   ;; repeat-rule, per rfc2445 split by "," into components
        rdate   ;; explicit repeat spec, per rfc2445, split by ","
        exdate  ;; explicit exclude spec, per rfc2445, split by ","
        exrule  ;; repeat-rule, per rfc2445 split by "," into components
        rd-list ;; list of explicit repeat   date-times (per `decode-time')
        ex-list ;; list of explicit excluded date-times (per `decode-time')
        rzone   ;; POSIX TZ string for rdate parsing
        xzone   ;; POSIX TZ string for exdate parsing
        olist)  ;; list of occurrences
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; the decomposed start time is necessary to fill in any time elements
    ;; which the rrule does not specify (eg if the hour is missing from the
    ;; rrule, get it from the DTSTART entry for the event))
    (setq dttext  (icalendar--get-event-property event 'DTSTART)
          zone    (icalendar--rr-ev-prop-attr event 'DTSTART 'TZID zone-map)
          dtst_z  (icalendar--decode-isodatetime dttext nil zone)
          ;; dtstart should omit TZ info, it is the human-readable parts:
          dtstart (icalendar--rr-decode-isodatetime dttext)
          start   (if dtstart (apply 'encode-time dtstart))
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; repeat rule spec + exclude rule spec
          rrule   (icalendar--get-event-property event 'RRULE)
          rrule   (icalendar--split-value rrule)
          count   (cadr (assq 'COUNT rrule))
          count   (if count (string-to-number count 10))
          exrule  (icalendar--get-event-property event 'EXRULE)
          exrule  (icalendar--split-value exrule)

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; repeate dates (fixed list)
          rdate   (icalendar--get-event-property event 'RDATE)
          rdate   (split-string (or rdate "") "," t)
          rzone   (or (icalendar--rr-ev-prop-attr event 'RDATE 'TZID zone-map)
                      zone)
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; excluded dates (fixed list)
          exdate   (icalendar--get-event-property event 'EXDATE)
          exdate   (split-string (or exdate "") "," t)
          xzone    (or (icalendar--rr-ev-prop-attr event 'EXDATE 'TZID zone-map)
                       zone))

    ;; no repeat rule and no static repeat list => there's only one occurrence
    (if (and (not rrule) (not rdate))
        (list dtst_z)
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; start with the tasks explicit `UNTIL' value, falling back to the
      ;; explicit `UNTIL' in the call to this function and then the implicit
      ;; one year span we impose (as calendar views rarely exceed one year):
      (if (setq until (cadr (assq 'UNTIL rrule)))
          (setq until (icalendar--decode-isodatetime until nil zone)
                until (apply 'encode-time until))
        (if end
            (setq until end)
          (setq end (copy-sequence dtstart))
          (incf (nth 5 end))
          (setq until (apply 'encode-time end))))

      (setq edata (list t '(:freq nil) '(:last-freq nil))
            munge (lambda (o) (decode-time (apply 'encode-time o))))

      ;; rrule and exrule are pretty similar: the parts that are not permitted
      ;; in exrules (COUNT, UNTIL etc) are not handled in the parser list so
      ;; we can just munge them both the same way here:
      (with-timezone t ;; enforce UTC calculations for this block
        (when rrule
          (mapc
           (lambda (handler)
             (funcall handler rrule edata dtstart zone start count until
                      :occurs))
           icalendar--rr-handlers))

        (when exrule
          (mapc
           (lambda (handler)
             (funcall handler exrule edata dtstart zone start count until
                      :exclude))
           icalendar--rr-handlers)))

      (when rdate
        (setq rd-list (mapcar (lambda (x)
                                (setq x (icalendar--rr-decode-isodatetime x))
                                (setcar (last x) rzone) x)
                              rdate)))

      (when exdate
        (setq ex-list (mapcar (lambda (x)
                                (setq x (icalendar--rr-decode-isodatetime x))
                                (setcar (last x) xzone) x)
                              exdate)) )

      ;; sort the generated dates
      (let (rc-dates ex-dates rcell ecell dates e0)
        (setq rcell    (assq :occurs edata)
              rc-dates (cdr rcell)
              rc-dates (sort rc-dates 'icalendar--rr-date-<))

        ;; strip out any recurrence rule dates < the start date
        (while (icalendar--rr-date-< (car rc-dates) dtstart)
          (setq rc-dates (cdr rc-dates)))
        ;; note that fixed dates are allowed to precede th start date

        (setq rc-dates (append rd-list rc-dates)
              rc-dates (sort rc-dates 'icalendar--rr-date-<))

        ;; de-duplicate the recurrence list (don't need to do this for the
        ;; exclude list, as those dupes don't show up in the output):
        (let ((tail rc-dates))
          (while tail
            (if (equal (car tail) (cadr tail)) (setcdr tail (cddr tail)))
            (setq tail (cdr tail))))

        (setq ecell    (assq :exclude edata)
              ex-dates (cdr ecell)
              ex-dates (append ex-list ex-dates)
              ex-dates (sort ex-dates 'icalendar--rr-date-<))

        ;; DTSTART is grandfathered in to the occurrence list
        ;; even if the recur ruleset would exclude it.
        ;; only EXRULE and EXDATE can remove DTSTART from the list:
        (let (adjusted-start)
          (setq adjusted-start (copy-sequence dtstart))
          (setcar (last adjusted-start) zone)
          (if (not (icalendar--rr-date-= (car rc-dates) adjusted-start))
              (setq rc-dates (cons adjusted-start rc-dates))))

        ;; push the sorted exclude dates back into the data structure:
        (if ecell (setcdr ecell ex-dates))
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; RRULE/EXRULE merging:
        ;; two sorted lists of dates: filter out the elements of rc-dates
        ;; which are also in ex-dates:
        (when (and ex-dates (setq e0 (car ex-dates)))
          (while (and rc-dates ex-dates)

            ;; copy across rc-dates that are before the first edate
            (while (and rc-dates (icalendar--rr-date-< (car rc-dates) e0))
              (setq dates (cons (car rc-dates) dates) rc-dates (cdr rc-dates)))

            ;; throw away all matches
            (while (and rc-dates (equal (car rc-dates) e0))
              (setq rc-dates (cdr rc-dates)))

            ;; now throw away e0 until it catches up
            (while (and rc-dates ex-dates
                        (icalendar--rr-date-< e0 (car rc-dates)))
              (setq ex-dates (cdr ex-dates) e0 (car ex-dates))))

          ;; ran out of exclude items before r items: just copy the rest across
          (mapc (lambda (d) (setq dates (cons d dates))) rc-dates)
          (setq rc-dates (nreverse dates)))
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; push the (possibly) reduced date list back into the data structure:
          (if rcell
              (setcdr rcell rc-dates)
            (setcdr edata (cons (cons :occurs rc-dates) (cdr edata)))))

      (setq olist (cdr (assq :occurs edata)))

      (if count
          (let (tmp)
            (while (and (< (length tmp) count) olist)
              (setq tmp   (cons (car olist) tmp)
                    olist (cdr olist)))
            (setq olist (nreverse tmp))))

      (when until
        (setq until (with-timezone t (decode-time until)))
        (setq olist
              (delete-if (lambda (o) (icalendar--rr-date-< until o)) olist)))

      (with-timezone (or tz zone)
        (setq olist (mapcar munge olist)))

      olist) ))

(provide 'icalendar-rrule)
