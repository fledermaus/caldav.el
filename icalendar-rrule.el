;; -*- lexical-binding: t -*-

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
    icalendar--rr-bysetpos))

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
(defun icalendar--rr-merge-date-dow (template date)
  "Move DATE forwards, 24 hours at a time (if necessary) to
arrive at a date matching the day of week in TEMPLATE (which can be a
`decode-time' value or an integer between 0 and 6 (inclusive)).\n
Retruns a decode-time value (which may be the original if no shift occurred)."
  (let ((dow (if (listp template) (nth 6 template) template))
        epoch old new-date)
    (if (eq (nth 6 date) dow)
        date
      (setq epoch    (float-time (apply 'encode-time date))
            old      (nth 6 date)
            jump     (- dow old)
            jump     (if (< jump 0) (+ 7 jump) jump)
            epoch    (+ (* 86400 jump) epoch)
            new-date (decode-time (seconds-to-time epoch)))
      new-date)))

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
    (cond
     ;; no jump - DTSTART is always the returned value for this:
     ((zerop n) (setq x start-time))
     ;; the simple cases: seconds -> weeks are well defined units of time
     ((setq x (assq step icalendar--rr-jumpsizes))
      (setq x (+ (float-time start) (* n (cdr x)))
            x (seconds-to-time x)
            x (decode-time x)))
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
    ;; if we crossed a DST boundary, tweak the time to compensate:
    ;; (this does not apply to recurrences of finer granularity than 1 day):
    (if (memq step '(DAILY WEEKLY MONTHLY YEARLY))
        (setq x (icalendar--rr-merge-date start-time x :sec :min :hour)))
    x))

(defun icalendar--rr-day-possible-p (candidate-time)
  (let ((processed-time))
    (setq processed-time (decode-time (apply 'encode-time candidate-time)))
    (eq (nth 3 candidate-time)
        (nth 3 processed-time)) ))

(defun icalendar--rr-byxxx-effect (rule-data by-type)
  "Given RULE-DATA (as constructed internally by `icalendar--rr-occurences')
return :expands or :restricts depending on whether the application of the
byxxx rule of type BY-TYPE should result in more or fewer event instances."
  (let ((r-type (cdr (assq :last-freq rule-data))))
    (if (memq by-type (memq r-type icalendar--rr-freqs)) :restricts :expands)))

(defun icalendar--rr-byxxx-element (text)
  (let ((tval (cdr (assoc text icalendar--rr-dow-values))))
    (if tval (cdr tval) (string-to-number text 10))))

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
    (let (origin m (zone (last dtstart)) epoch new-date)
      (setq origin (copy-sequence dtstart)
            origin (nthcdr 6 origin))
      (if (< n 0)
          (setq origin (apply 'encode-time 0 0 0 31 12 year nil) m (1+ n))
        (setq origin (apply 'encode-time 0 0 0 1 1 year nil) m (1- n)))
      (setq origin   (float-time origin)
            epoch    (+ (* 86400 m) origin)
            new-date (decode-time (seconds-to-time epoch)))
      ;; if we jumped to a valid date, merge the time back in:
      (if (eq (nth 5 dtstart) (nth 5 new-date))
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
    (let (start-date start result N)
      (cond ((< n 0)
             (setq m (1+ n) start-date (icalendar--rr-weekN-start year day0)))
            ((> n 0)
             (setq m (1- n) start-date (icalendar--rr-week1-start year day0))))
      (setq start  (float-time (apply 'encode-time start-date))
            result (+ (* 604800 m) start)
            result (decode-time (seconds-to-time result))) )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; RECUR rule part parsers.
;; these parsers rely on being called on a gven event in the order
;; defined in icalendar--rr-handlers (which is the same as defined in
;; RFC2445 (http://www.ietf.org/rfc/rfc2445.txt))
;; Lasciate ogne speranza, voi ch'intrate:

(defun icalendar--rr-bysecond   (rule data dtstart start count until))
(defun icalendar--rr-bysetpos   (rule data dtstart start count until))
(defun icalendar--rr-byminute   (rule data dtstart start count until))
(defun icalendar--rr-byhour     (rule data dtstart start count until))
(defun icalendar--rr-byday      (rule data dtstart start count until))
(defun icalendar--rr-bymonthday (rule data dtstart start count until))
(defun icalendar--rr-byyearday  (rule data dtstart start count until))
(defun icalendar--rr-by-x (rule data dtstart start count until target x)
  (let (bset action olist period slot)
    (cond ((eq 'BYSECOND x) (setq period 'SECONDLY slot 0))
          ((eq 'BYMINUTE x) (setq period 'MINUTELY slot 1))
          ((eq 'BYHOUR   x) (setq period 'HOURLY   slot 2))
          ((eq 'BYMONTH  x) (setq period 'MONTHLY  slot 4)))
    (when (and slot (setq bset (cadr (assq x rule))))
      (setq action (icalendar--rr-byxxx-effect data period)
            bset   (icalendar--rr-byxxx-to-data bset)
            olist  (cdr (assq target data))
            olist  (cond ((eq :expands action)
                          (icalendar--rr-expand-occurlist slot bset olist))
                         ((eq :restricts action)
                          (delete-if-not (lambda (dt)
                                           (memq (nth slot dt) bset)) olist))))
      (setcdr data (cons (cons :bymonth (nconc bset action)) (cdr data)))
      (setcdr (assq :last-freq data) period)
      (setcdr (assq target     data)  olist)) ))

(defun icalendar--rr-bymonthday (rule data dtstart start count until target)
  (let (bset olist dlist action)
    (when (setq bset (cadr (assq 'BYMONTHDAY rule)))
      (setq bset   (icalendar--rr-byxxx-to-data bset)
            action (icalendar--rr-byxxx-effect data 'DAILY)
            olist  (cdr (assq target data)))
      (cond ((eq :expands   action)
             (mapc
              (lambda (o)
                (mapc
                 (lambda (day &optional new-day)
                   (if (setq new-day (icalendar--rr-monthday o day))
                       (setq dlist (cons new-day dlist)))) bset)) olist))
            ((eq :restricts action)
             (mapc (lambda (o)
                     (mapc (lambda (d)
                             (if (eq (nth 3 o) d)
                                 (setq dlist (cons o dlist)))) bset)) olist)))
      (setcdr data (cons (cons :bymonthday bset) (cdr data)))
      (setcdr (assq :last-freq data) 'DAILY)
      (setcdr (assq target     data)  dlist)) ))

;; only interpreting BYYEARDAY for YEARLY repeats. Not sure what it
;; would mean in other contexts.
(defun icalendar--rr-byyearday (rule data dtstart start count until target)
  (let (bset olist ylist dlist)
    (when (and (eq (cdr (assq :last-freq data)) 'YEARLY)
               (setq bset (cadr (assq 'BYYEARDAY rule))))
      (setq bset  (icalendar--rr-byxxx-to-data bset)
            olist (cdr (assq target data))
            ylist (delete-dups (mapcar (lambda (dt) (nth 5 dt)) olist)))
      (mapc (lambda (y)
              (mapc (lambda (d)
                      (if (setq d (icalendar--rr-nth-day y d dtstart))
                          (setq dlist (cons d dlist)))) bset)) ylist)
      (setcdr data (cons (cons :byyearday bset) (cdr data)))
      (setcdr (assq :last-freq data) 'DAILY)
      (setcdr (assq target     data) dlist)) ))

;; byweekno only applies to yearly rules (per RFC2445)
(defun icalendar--rr-byweekno (rule data dtstart start count until target)
  (let (bset olist wlist)
    (when (and (eq (cdr (assq :last-freq data)) 'YEARLY)
               (setq bset (cadr (assq 'BYWEEKNO rule))))
      (setq bset  (icalendar--rr-byxxx-to-data bset)
            olist (cdr (assq target data)))
      (mapc (lambda (dt)
              (mapc (lambda (n &optional wd)
                      (setq wd (icalendar--rr-nth-week (nth 5 dt) n)
                            wlist (cons wd wlist))) bset)) olist)
      (setq wlist
            (mapcar
             (lambda (dt)
               (icalendar--rr-merge-date dtstart dt :sec :min :hour :dow))
             wlist))
      (setcdr data (cons (cons :byweekno bset) (cdr data)))
      (setcdr (assq :last-freq data) 'WEEKLY)
      (setcdr (assq target     data)   wlist)) ))

(defun icalendar--rr-bymonth (rule data dtstart start count until target)
  (icalendar--rr-by-x rule data dtstart start count until target 'BYMONTH))

(defun icalendar--rr-interval (rule data dtstart start count until target)
  (let (freq interval occurs (j 0) (o 0) next (last start))
    (setq interval (or (cadr (assq 'INTERVAL rule)) 1)
          freq     (cdr (assq :freq data)))
    (setcdr data (cons (cons :interval interval) (cdr data)))
    ;; we always generate all base event times for the date range in question:
    ;; even for count limited rules: we do this because we cannot know until
    ;; all the BYxxx rules are applied whether the "impossible" dates that
    ;; can be generated by this process (eg monthly with a dtstart on the 31st)
    ;; should be discarded (eg a 31st dtstart with a `BYDAY' of 15,27) will
    ;; generate acceptable dates in all months, even though there's no 31st
    ;; in half of them.
    ;; tl;dr → ignore `COUNT' at this stage, assume an `UNTIL' value exists:
    (while last
      (setq next (icalendar--rr-jump j freq dtstart start) j (1+ j) last nil)
      (if (<= (time-to-seconds (apply 'encode-time next))
              (time-to-seconds until))
          (setq occurs (cons next occurs) last next)))
    ;; store the basic list of event-occurrences
    (setcdr data (cons (cons target occurs) (cdr data))) ))

(defun icalendar--rr-freq (rule data dtstart start count until target)
  (let (freq)
    (when (setq freq (intern-soft (cadr (assq 'FREQ rule))))
      (setcdr (assq :last-freq data) freq)
      (setcdr (assq :freq      data) freq)) ))
;; end of rrule part parser section.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun icalendar--rr-occurences (event zone-map &optional end)
  "Return a list of time values (as per `encode-time') at which EVENT has
an instance. If there is no recurrence rule entry, then a list of length
1 (the DTSTART value) is returned.

ZONE-MAP should be the timezone map harvested from the calendar with
icalendar--rr-timezones.

If the recurrence rule exists, but specifies neither a COUNT value nor an
UNTIL entry, then instances are only generated upto END (another time value).
If neither limiting rule part (COUNT, DTSTART) is pecified and END is not
supplied, a default of 1 year from the DTSTART value is assumed."
  (let (zone dtstart eprops rrule-text rrule start edata count until)
    (setq estart     (icalendar--get-event-property event 'DTSTART)
          eprops     (icalendar--get-event-property-attributes event 'DTSTART)
          zone       (icalendar--find-time-zone eprops zone-map)
          rrule-text (icalendar--get-event-property event 'RRULE)
          rrule      (icalendar--split-value rrule-text))

    ;; get the decomposed start time (necessary to fill in any time elements
    ;; which the rrule does not specify (eg if the hour is missing from the
    ;; rrule, get it from the DTSTART entry for the event))
    ;; also work out the time-value for the start and the COUNT value (if any)
    (setq dtstart (icalendar--decode-isodatetime estart nil zone)
          start   (apply 'encode-time dtstart)
          count   (assq 'COUNT rrule))

    ;; we always need an `UNTIL' value, as we cannot otherwise work out when
    ;; to stop generating candidates.
    ;; (`COUNT' won't work for this: tl;dr: human calendars are insane)
    ;; start with the tasks explicit `UNTIL' value, falling back to the
    ;; explicit `UNTIL' in the call to this function and then the implicit
    ;; one year span we impose (as calendar views rarely exceed one year):
    (if (setq until (assq 'UNTIL rrule))
        (setq until (icalendar--decode-isodatetime until)
              until (apply 'encode-time until))
      (if end
          (setq until end)
        (setq end (copy-sequence dtstart))
        (incf (nth 5 end))
        (setq until (apply 'encode-time end))))

    (setq edata (list t '(:freq nil) '(:last-freq nil)))
    (when rrule
      (mapc (lambda (handler)
              (funcall handler rrule edata dtstart start count until :occurs)
              (message "rrule %S -> \n%S\n--\n" handler edata))
            icalendar--rr-handlers))
    (when exrule
      (mapc (lambda (handler)
              (funcall handler exrule edata dtstart start count until :exclude)
              (message "exrule %S -> \n%S\n--\n" handler edata))
            icalendar--rr-handlers))

(setq tmp-ical
      '((VCALENDAR nil
                   ((PRODID nil "-//Inverse inc./SOGo 2.0.5a//EN")
                    (VERSION nil "2.0"))
                   ((VTIMEZONE nil
                               ((TZID nil "Europe/London")
                                (X-LIC-LOCATION nil "Europe/London"))
                               ((DAYLIGHT nil
                                          ((TZOFFSETFROM nil "+0000")
                                           (TZOFFSETTO nil "+0100")
                                           (TZNAME nil "BST")
                                           (DTSTART nil "19700329T010000")
                                           (RRULE nil "FREQ=YEARLY;BYMONTH=3;BYDAY=-1SU"))
                                          nil)
                                (STANDARD nil
                                          ((TZOFFSETFROM nil "+0100")
                                           (TZOFFSETTO nil "+0000")
                                           (TZNAME nil "GMT")
                                           (DTSTART nil "19701025T020000")
                                           (RRULE nil "FREQ=YEARLY;BYMONTH=10;BYDAY=-1SU"))
                                          nil)))
                    (VEVENT nil
                            ((UID nil "5F7C-518C1800-5-7DE1DB00")
                             (SUMMARY nil "ILG Conf Call")
                             (DESCRIPTION nil "Sync call for Intel Linux Graphics/OBS Appliance\\nprojects.")
                             (CLASS nil "PUBLIC")
                             (CATEGORIES nil "Calls")
                             (CREATED nil "20130509T214308Z")
                             (DTSTAMP nil "20130509T214308Z")
                             (LAST-MODIFIED nil "20130509T214308Z")
                             (XRULE nil "FREQ=WEEKLY")
                             (RRULE nil "RRULE:FREQ=WEEKLY")
                             (DTSTART
                              (TZID "Europe/London")
                              "20130531T160000")
                             (DTEND
                              (TZID "Europe/London")
                              "20130531T164500")
                             (TRANSP nil "OPAQUE"))
                            nil))))
      tmp-event
      (car (icalendar--all-events tmp-ical)))
