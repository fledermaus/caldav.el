;; -*- lexical-binding: t -*-
;;; tzinfo.el --- Time Zone Information parser and utilities

;; Copyright © 2013  Vivek Dasmohapatra <vivek@etla.org>

;; Author: Vivek Dasmohapatra <vivek@etla.org>
;; Maintainer: Vivek Dasmohapatra <vivek@etla.org>
;; Keywords: time, timezone

;; This file is NOT part of GNU Emacs.

;; tzinfo.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; tzinfo.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with caldav.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commen]tary:

;;; Code:
(require 'bindat)

(defvar last)

(defvar tzinfo-directory "/usr/share/zoneinfo"
  "Directory where zoneinfo rules may be found")

(defvar tzinfo-data-offset 0)

(defconst tzinfo-capture-offset '((eval (setq tzinfo-data-offset bindat-idx))))

(defconst tzinfo-spec-header `((ident  str     4)
                               (version str    1)
                               (fill          15)
                               (gmt     long   1)
                               (std     long   1)
                               (leap    long   1)
                               (time    long   1)
                               (type    long   1)
                               (char    long   1)
                               ,tzinfo-capture-offset))

;; key - repeat-key (look up value in header) - format - postprocess
(defconst tzinfo-spec-list '([ time  time (long 1) tzinfo--s32  ]
                             [ index time (byte 1) nil          ]
                             [ zone  type (str  6) tzinfo--rule ]
                             [ name  char (byte 1) nil          ]
                             [ leap  leap (str  8) tzinfo--leap ]
                             [ std   std  (byte 1) tzinfo--bool ]
                             [ gmt   gmt  (byte 1) tzinfo--bool ]))

(defconst tzinfo-spec-list-2 '([ time  time (vec   8) tzinfo--s64    ]
                               [ index time (byte  1) nil            ]
                               [ zone  type (str   6) tzinfo--rule   ]
                               [ name  char (byte  1) nil            ]
                               [ leap  leap (str  12) tzinfo--leap64 ]
                               [ std   std  (byte  1) tzinfo--bool   ]
                               [ gmt   gmt  (byte  1) tzinfo--bool   ]))

(defconst tzinfo-MAX64 (expt 2.0 64))
(defconst tzinfo-MAX32 (expt 2 32))
(defconst tzinfo-max32 (- (expt 2 31) 1))
(defconst tzinfo-min32 (- (expt 2 31)))

(defconst tzinfo-MAX8  (expt 2 8))
(defconst tzinfo-max8  (- (expt 2 7) 1))
(defconst tzinfo-min8  (- (expt 2 7)))

(defun tzinfo--s32 (u)
  "Cast integer U to a signed 32 bit value.
Throw an error if out of range."
  (if (> u tzinfo-max32)
      (setq u (- u tzinfo-MAX32)))
  (if (< u tzinfo-min32)
      (error "Value %d is out of 32 bit range" u)
    u))

(defun tzinfo--s8 (u)
  "Cast integer U to a signed 8 bit value.
Throw an error if out of range."
  (if (> u tzinfo-max8)
      (setq u (- u tzinfo-MAX8)))
  (if (< u tzinfo-min8)
      (error "Value %d is out of 8 bit range" u)
    u))

(defun tzinfo--be-vec-to-long (vec &optional offset)
  "Convert a 4 byte big-endian byte vector VEC into an [unsigned] 32 bit value."
  (let ((value 0))
    (setq offset (or offset 0))
    (mapc (lambda (x)
            (setq value
                  (logior (lsh (aref vec (+ offset x)) (* (- 3 x) 8)) value)))
          '(0 1 2 3))
    value))

(defun tzinfo--s64 (vec)
  "Passed an 8 byte vector VEC, containing a 64 bit value in network order,
return a float (since we cannot represent the full signed 64 bit range
internally as integers)"
  (let (s64)
    (setq s64  (+ (* (tzinfo--be-vec-to-long vec 0) (float tzinfo-MAX32))
                  (* (tzinfo--be-vec-to-long vec 4) 1.0)))
    (if (> (aref vec 0) 127)
        (- s64 tzinfo-MAX64)
      s64)))

(defun tzinfo--bool (u)
  "Coerce a number U into a boolean."
  (not (zerop u)))

(defun tzinfo--leap (leap)
  "Parse a leap-second (LEAP, a unibyte string) part of a TZ rule."
  (setq leap (bindat-unpack '((point long) (offset long)) leap 0))
  (mapc (lambda (x) (setcdr x (tzinfo--s32 (cdr x)))) leap)
  leap)

(defun tzinfo--leap64 (leap)
  "Parse a leap-second chunk (LEAP, a unibyte string) of a version 2 TZ rule."
  (setq leap (bindat-unpack '((point vec 8) (offset long)) leap 0))
  (let (c)
    (and (setq c (assq 'point  leap)) (setcdr c (tzinfo--s64 (cdr c))))
    (and (setq c (assq 'offset leap)) (setcdr c (tzinfo--s32 (cdr c)))))
  leap)

(defun tzinfo--rule (rule)
  "Parse a zone-description (label offset, DST flag, offset from UTC)."
  (let (offcell dstcell)
    (setq rule    (bindat-unpack '((offset long) (dst byte) (eidx byte)) rule)
          offcell (assq 'offset rule)
          dstcell (assq 'dst rule))
    (if offcell (setcdr offcell (tzinfo--s32  (cdr offcell))))
    (if dstcell (setcdr dstcell (tzinfo--bool (cdr dstcell))))
    rule))

(defun tzinfo-process-blob (data &optional v2-offset)
  "Starting at the beginning of DATA (a unibyte string containing a binary
timezone specification, such as those found in /usr/share/zoneinfo/…), parse
and extract the rule specification.\n
V2-OFFSET is the offset into DATA where the version-2 TZ information begins:
It is intended for internal use and you need not normally supply it.\n
Returns a list of zone info alists (for a v1 file, there will be only one
entry, for a v2 file, there will be one entry for the v1 zone data followed
by another for the v2 zone data).\n
Each zone info alist consists of a :header entry and a :zone-data entry,
so a v2 file would give:\n
    (((:header …) (:zone-data …))  ;; the v1 data
     ((:header …) (:zone-data …))) ;; the v2 data\n
Each :header entry contains the following data:\n
   (:header
    (ident   . \"TZif\")   ;; always \"TZif\"
    (version . \"2\")      ;; a one char string containing #x00 or #x32
    (gmt     . 5)        ;; the number of GMT indicators
    (std     . 5)        ;; the number of std/wall flags
    (leap    . 0)        ;; the number oif leap second entries
    (time    . 186)      ;; the number of timezone transitions
    (type    . 5)        ;; the number of local time types (eg GMT & BST & BDST)
    (char    . 20))      ;; size in bytes of the TZ acronym list\n
Each :zone-data entry contains the following:
   (:zone-data
    (time -2717640704.0 -1633269760.0 … 2140678800.0)
    (index 2 1 … 2)
    (zone ((name . \"LMT\") (dst)     (offset . -28378))
          ((name . \"PDT\") (dst . t) (offset . -25200))
          ((name . \"PST\") (dst)     (offset . -28800))
          ((name . \"PWT\") (dst . t) (offset . -25200))
          ((name . \"PPT\") (dst . t) (offset . -25200)))
    (name . \"LMT\\0PDT\\0PST\\0PWTPPT\\0\")
    (leap ((offset . 1)  (point .   78796800.0))
          ((offset . 2)  (point .   94694401.0))
          ⋮
          ((offset . 25) (point . 1341100824.0)))
    (std nil nil nil nil t)
    (gmt nil nil nil nil t)
    (etla \"LMT\" \"PDT\" \"PST\" \"PWT\" \"PPT\")
    (posix . \"PST8PDT,M3.2.0,M11.1.0\"))\n
The index and time lists are the same length - the time list contains a list of
epoch times at which zone shifts occur, and the index list gives the zone
into which the shift is occurring at that point (the indexth entry in the
zone entry above)\n
Notes:
 - The order of entries is not guaranteed
 - \\0 above indicates a NUL character (normally displayed as ^@)
 - in the v2 entry, the time entries and the point values in leap entries
   are float values. In v1, that are integers (v1 is 32 bit only).\n
See (man \"tzfile(5)\") for more detail."
  (let (header info (tzinfo-data-offset (or v2-offset 0)) name v2-data rval)
    (setq header (bindat-unpack tzinfo-spec-header data tzinfo-data-offset))
    (mapc (lambda (x &optional key repeat spec munge val cell)
            (setq key    (aref x 0)
                  repeat (aref x 1)
                  repeat (cdr (assq repeat header))
                  spec   (aref x 2)
                  spec   (list (list key 'repeat repeat (cons t spec))
                               tzinfo-capture-offset)
                  munge  (aref x 3)
                  val    (bindat-unpack spec data tzinfo-data-offset)
                  val    (cdr (assq key val))
                  val    (mapcar 'cdar val))
            (if munge (setq val (mapcar munge val)))
            (setq info (cons (cons key val) info)))
          (if v2-offset tzinfo-spec-list-2 tzinfo-spec-list))

    ;; record the number of bytes eaten so far:
    (setq header (cons (cons 'bytes tzinfo-data-offset) header))

    ;; force the bytes in the zone-acronyms buffer into a single string:
    (if (setq name (assq 'name info))
        (setcdr name (apply 'string (cdr name))))

    (setq name (cdr name))

    ;; slice out the acronym list:
    (setq info (cons (cons 'etla (split-string name "\x00" t)) info))

    ;; copy the zone names into each zone offset entry (eidx gives the offset
    ;; at which the zonename for that offset starts):
    (mapc
     (lambda (z &optional cell)
       (setq cell (assq 'eidx z))
       (setcar cell 'name)
       (setcdr cell
               (car (split-string (substring name (cdr cell)) "[\x00]"))))
     (cdr (assq 'zone info)))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; v2 file, and we are not already parsing the v2 section:
    (if (and (not v2-offset) (equal (cdr (assq 'version header)) "2"))
        (let (cell footer offset)
          (setq v2-data (car (tzinfo-process-blob data tzinfo-data-offset))
                cell    (assq :header v2-data))
          (message "hcell - %S" v2-data)
          (setq offset  (cdr (assq 'bytes (cdr cell)))
                footer  (substring data offset))
          (when (string-match "\n\\(.*\\)\n" footer)
            (setq footer (match-string 1 footer)
                  cell   (assq :zone-data v2-data)
                  cell   (last cell))
            (setcdr cell (list (cons 'posix footer))))))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (setq rval (list (list (cons :header    (nreverse header))
                           (cons :zone-data (nreverse   info)))))
    (if v2-data (setq rval (cons v2-data rval)))
    (nreverse rval)))

(defun tzinfo-data (zone)
  "Process ZONE (a string specifying a timezone, as per the TZ environment
variable: either a standard zone such as \"Antarctica/South_Pole\" or the
full path to a TZ info zone file).\n
See also: (man \"tzfile(5)\") and `tzinfo-process-blob'."
  (let (data file tz)
    (setq file (expand-file-name zone tzinfo-directory)
          data (with-temp-buffer
                 (set-buffer-multibyte nil)
                 (ignore-errors
                   (insert-file-contents-literally file)
                   (buffer-string))))
    (if (and (stringp data) (> (length data) 44))
        (setq data (tzinfo-process-blob data))
      (message "Timezone %s (%s): Malformed rule or non-existent zone file"
               zone file))
    data))

(progn (insert (pp (tzinfo-data "right/Etc/GMT+5"))) nil)
