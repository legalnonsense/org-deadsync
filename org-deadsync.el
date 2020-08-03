;;; org-deadsync.el --- Sync deadlines in Orgmode documents -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Jeff Filipovits

;; Author: Jeff Filipovits <jrfilipovits@gmail.com>
;; Url: https://github.com/legalnonsense/elgantt
;; Version: 0.1-pre
;; Package-Requires: ((emacs "26.1") (org "9.0") (s "1.12.0") (org-ql "0.2-pre") (ts.el "0.2-pre"))
;; Keywords: Org, agenda, calendar, deadline, link

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Link two deadlines with a given offset, so that changes to one deadline automatically change all linked deadlines

;;;; Installation

;;;;; Manual

;; Install these required packages:

;; + org-ql
;; + ts
;; + hydra
;; + s

;; Then put this file in your load-path, and put this in your init
;; file:

;; (require 'org-deadsync)

;;;; Usage

;; Run this command

;; `org-deadsync-mode': Turn on deadsync mode

;;;; Tips

;; + You can customize settings in the `org-deadsync' group.

;;;; Credits

;; This package would not have been possible without the following
;; packages: org-ql, ts.el 
;;
;;  [1] https://github.com/alphapapa/ts.el
;;  [2] https://github.com/alphapapa/org-ql/
;;
;; Or the generous assistance of alphapapa: https://github.com/alphapapa/

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

;;;; Requirements

(require 's)
(require 'ts)
(require 'org-ql)
(require 'org-id)
(require 'hydra)
(require 'ov)

;;;; Customization

(defgroup org-deadsync nil
  "Options for org-deadsync."
  :tag "org-deadsync"
  :group 'org
  :group 'org-deadsync)

(defcustom org-deadsync-weekend-adjustment t
  "If t, moves any deadline that falls on the weekend to the next weekday, or
if using a negative offset, to the previous Friday."
  :type 'boolean)

(defcustom org-deadsync-files (org-agenda-files)
  "Files with linked deadlines. Defaults to all agenda files."
  :type 'list)

(defcustom org-deadsync-lock-icon ""
  "Icon displayed after locked deadlines"
  :type 'string)

(defcustom org-deadsync-master-icon "⚷"
  "Icon displayed after master deadlines."
  :type 'string)

(defcustom org-deadsync-skip-dates '()
  "List of dates (strings in the form \"YYYY-MM-DD\") to exclude as possible deadlines, e.g., holidays, birthdays.
If a deadline falls on this date, it will be moved forward to the next available date."
  :type 'list)

;;;; Variables

(defvar org-deadsync-mode nil
  "Mode variable for org-deadsync-mode")
(make-variable-buffer-local 'org-deadsync-mode)

;;;; Functions

(defun org-deadsync--ts-adjust (&rest adjustments)
  "Wrapper for `ts-adjust' to allow arguments in the form, e.g., \"+3d\"; 
accepts a list of strings in the form \"[+/-][number][d(ay), m(onth), y(ear)]\"

E.g.:
 +4m -4d
 +7d
 +1y +1m +1d"
  (let* ((ts (-last-item adjustments))
	 (adjustments (cl-loop for adj in (split-string (car (nbutlast adjustments)) " ")
                               for num = (string-to-number (substring adj 0 -1))
                               for slot = (pcase-exhaustive (substring adj -1)
                                            ("d" 'day)
                                            ("m" 'month)
                                            ("y" 'year))
                               append (list slot num))))
    (apply #'ts-adjust (append adjustments (list ts)))))

(defun org-deadsync-lock-deadline (t-or-nil)
  "Make deadline in current heading read-only if argument is non-nil"
  (interactive)
  (save-excursion
    (when (org-get-deadline-time (point))
      (outline-back-to-heading)
      (when (re-search-forward (org-re-timestamp 'deadline)
			       (line-end-position 2) t)
	(let ((inhibit-read-only t)
	      (start (match-beginning 0))
	      (end (match-end 0)))
	  (put-text-property start end 'read-only t-or-nil))))))

(defun org-deadsync-clear-overlays-this-heading ()
  "Clear the lock and key icons from this heading."
  (interactive)
  (outline-back-to-heading)
  (re-search-forward (org-re-timestamp 'deadline)
		     (line-end-position 2) t)
  (ov-clear 'after-string org-deadsync-lock-icon (match-beginning 0)
	    (match-end 0))
  (ov-clear 'after-string org-deadsync-master-icon (match-beginning 0)
	    (match-end 0)))

(defun org-deadsync-place-overlays-this-heading ()
  "Put the appropriate overlay (i.e., lock or key, or both) at
this heading based on the properties ORG-DEADSYNC-ACTIVE and
ORG-DEADSYNC-MASTER."
  (interactive)
  (org-deadsync-clear-overlays-this-heading)
  (outline-back-to-heading)
  (when(re-search-forward (org-re-timestamp 'deadline)
			  (line-end-position 2))
    (let ((start (match-beginning 0))
	  (end (match-end 0)))
      (when (org-entry-get (point) "ORG-DEADSYNC-ACTIVE" "t")
	(ov-set (ov-regexp (org-re-timestamp 'deadline) start end)
		'after-string org-deadsync-lock-icon
		'evaporate t))
      (when (org-entry-get (point) "ORG-DEADSYNC-MASTER" "t")
	(ov-set (ov-regexp (org-re-timestamp 'deadline) start end)
		'after-string org-deadsync-master-icon
		'evaporate t)))))

(defun org-deadsync--dependents-p ()
  "Return non-nil if current entry has `org-deadsync' dependents."
  (when-let* ((master-id (org-entry-get (point) "ID")))
    (org-ql-select org-deadsync-files
      `(and (property "ORG-DEADSYNC-LINK" ,master-id)
	    (property "ORG-DEADSYNC-ACTIVE" "t")))))

(defun org-deadsync--deadline-locked-p ()
  "For the current heading, returns t if deadline is locked; otherwise nil"
  (save-excursion
    (when (org-get-deadline-time (point))
      (org-back-to-heading)
      (re-search-forward (org-re-timestamp 'deadline)
			 (line-end-position 2) t)
      (text-property-any (match-beginning 0) (match-end 0) 'read-only t))))

(defun org-deadsync-toggle-lock ()
  "Toggle whether the deadline text is locked."
  (org-deadsync-lock-deadline (not (org-deadsync--deadline-locked-p))))

(defun org-deadsync-remove-dependency ()
  "Removes the dependency for the current heading"
  (interactive)
  (save-excursion
    (let ((master-id (org-entry-get (point) "ORG-DEADSYNC-LINK")))
      (org-deadsync-lock-deadline nil)
      (org-delete-property "ORG-DEADSYNC-LINK")
      (org-delete-property "ORG-DEADSYNC-OFFSET")
      (org-delete-property "ORG-DEADSYNC-ACTIVE")
      (org-deadsync-place-overlays-this-heading)
      (save-excursion
	(org-id-goto master-id)
	(unless (org-deadsync--dependents-p)
	  (org-delete-property "ORG-DEADSYNC-MASTER")
	  (org-deadsync-refresh-this-heading))))))

(defun org-deadsync-jump-to-master ()
  "Jump directly to the master deadline" 
  (interactive)
  (when-let ((target (org-entry-get (point) "ORG-DEADSYNC-LINK")))
    (org-id-goto target)))

(defun org-deadsync-show-master ()
  "Echo master deadline information"
  (interactive)
  (save-excursion
    (when-let* ((target (org-entry-get (point) "ORG-DEADSYNC-LINK")))
      (org-id-goto target)
      (message (concat "Linked to: "
		       (org-entry-get (point) "CATEGORY") " : "
		       (org-entry-get (point) "ITEM") " : DEADLINE: "
		       (org-entry-get (point) "DEADLINE"))))))

(defun org-deadsync-set-dependency ()
  "Set heading with dependent deadline"
  (interactive)
  (let ((master-deadline nil)
	(master-id nil)
	(offset (read-string "Enter offset: ")))
    (when (not (or (string= (substring offset 0 1) "+")
		   (string= (substring offset 0 1) "-")))
      (setq offset (concat "+" offset)))
    (save-excursion 
      (org-goto)
      (setq master-deadline (cdar (org-entry-properties (point) "DEADLINE")))
      (when (not master-deadline)
	(user-error "No deadline in this node"))
      (setq master-id (cdar (org-entry-properties (point) "ID")))
      (when (not master-id)
	(setq master-id (org-id-get-create)))
      (org-set-property "ORG-DEADSYNC-MASTER" "t")
      (org-deadsync-place-overlays-this-heading))
    (org-set-property "ORG-DEADSYNC-LINK" master-id)
    (org-set-property "ORG-DEADSYNC-OFFSET" offset)
    (org-set-property "ORG-DEADSYNC-ACTIVE" "t")
    (org-deadsync-lock-deadline nil)
    (org-deadline nil "2000-01-01") ; dummy deadline in case one is there already
    (org-deadsync-refresh-this-heading)))

(defun org-deadsync-refresh-all ()
  (interactive)
  (save-excursion 
    (org-with-wide-buffer
     (outline-show-all)
     (org-ql-select org-deadsync-files
	 '(property "ORG-DEADSYNC-MASTER" "t")
       :action (lambda ()
		 (org-deadsync-refresh-this-heading)
		 (org-deadsync-refresh-dependents)))))
  (unless (org-before-first-heading-p)
    (outline-hide-other)))

(defun org-deadsync-refresh-dependents ()
  (interactive)
  (when (org-entry-get (point) "ORG-DEADSYNC-MASTER" "t")
    (when-let ((master-id (org-entry-get (point) "ID")))
      (org-ql-select org-deadsync-files
	`(and (property "ORG-DEADSYNC-LINK" ,master-id)
	      (property "ORG-DEADSYNC-ACTIVE" "t"))
	:action (lambda ()
		  (org-deadsync-refresh-this-heading)
		  (org-deadsync-refresh-dependents)))))) 

(defun org-deadsync--negative-offset-p ()
  "Determines whether something like +5d -4y +49m is a positive or negative offset."
  (when-let* ((master-deadline (save-excursion (org-id-goto (org-entry-get (point) "ORG-DEADSYNC-LINK"))
					       (ts-parse-org (org-entry-get (point) "DEADLINE"))))
	      (offset (org-entry-get (point) "ORG-DEADSYNC-OFFSET"))
	      (offset-deadline (org-deadsync--ts-adjust offset master-deadline)))
    (ts< offset-deadline master-deadline)))

(defun org-deadsync-refresh-this-heading ()
  (interactive)
  (org-deadsync-place-overlays-this-heading)
  (when (and (org-entry-get (point) "ORG-DEADSYNC-LINK")
	     (org-entry-get (point) "ORG-DEADSYNC-ACTIVE" "t"))
    (progn 
      (org-deadsync-clear-overlays-this-heading) ;; Important to clear overlays before changing deadline
      (let* ((master-deadline (save-excursion    ;; otherwise, the overlays will appear in strange places
				(org-id-goto (org-entry-get (point) "ORG-DEADSYNC-LINK"))
				(ts-parse-org (org-entry-get (point) "DEADLINE"))))
	     (offset (org-entry-get (point) "ORG-DEADSYNC-OFFSET"))
	     (offset-negative-p (org-deadsync--negative-offset-p))
	     (new-deadline (org-deadsync--ts-adjust offset master-deadline)))
	(while (org-deadsync--skip-day-p new-deadline)
	  (setq new-deadline (org-deadsync--skip-date-adjust new-deadline offset-negative-p))
	  (setq new-deadline (org-deadsync--weekend-adjust new-deadline offset-negative-p)))
	(setq new-deadline (ts-format "<%Y-%m-%d %a>" new-deadline))
	(org-deadsync-lock-deadline nil)
	(org-deadline nil new-deadline)
	(org-deadsync-place-overlays-this-heading)
	(when (org-entry-get (point) "ORG-DEADSYNC-ACTIVE" "t")
	  (org-deadsync-lock-deadline t))))))

(defun org-deadsync--skip-day-p (timestamp)
  "Should this date be skipped because it is a weekend or
because it is in `org-deadsync-skip-dates'?"
  (or (and org-deadsync-weekend-adjustment
	   (pcase (ts-dow timestamp)
             (0 t)
             (6 t)))
      (member (ts-format "%Y-%m-%d" timestamp) org-deadsync-skip-dates)))

(defun org-deadsync--weekend-adjust (timestamp &optional negative)
  "Adjust deadline to next Monday if the deadline falls on a
 weekend, assuming org-deadsync-weekend-adjustment is t. If NEGATIVE,
then adjust backward to the previous Friday."
  (if org-deadsync-weekend-adjustment
      (pcase (ts-dow timestamp)
        (0 (ts-adjust 'day (if negative -2 1) timestamp))
        (6 (ts-adjust 'day (if negative -1 2) timestamp))
        (_ timestamp))
    timestamp))

(defun org-deadsync--lock-all (t-or-nil)
  (interactive)
  (org-ql-select org-deadsync-files
    '(property "ORG-DEADSYNC-ACTIVE" "t")
    :action (lambda ()
	      (org-deadsync-lock-deadline t-or-nil))))

(defun org-deadsync--skip-date-adjust (timestamp &optional negative)
  "Adjust deadline to next day if deadline falls on a date in 
`org-deadsync-skip-dates'"
  (if (member (ts-format "%Y-%m-%d" timestamp) org-deadsync-skip-dates)
      (ts-adjust 'day (if negative -1 1) timestamp)
    timestamp))

(defun org-deadsync-toggle-active ()
  "Toggle whether a dependent deadline is active or not"
  (interactive)
  (save-excursion 
  (when-let ((activep (cdar (org-entry-properties (point) "ORG-DEADSYNC-ACTIVE"))))
    (if (string= activep "t")
	(progn 
	  (org-set-property "ORG-DEADSYNC-ACTIVE" "nil")
	  (org-deadsync-lock-deadline nil))
      (org-set-property "ORG-DEADSYNC-ACTIVE" "t"))
      (org-deadsync-refresh-this-heading)
      (org-deadsync-refresh-dependents))))

(defun org-deadsync-clear-overlays ()
  (interactive)
  (ov-clear 'after-string org-deadsync-lock-icon (point-min) (point-max))
  (ov-clear 'after-string org-deadsync-master-icon (point-min) (point-max)))

(defun org-deadsync--clear-all ()
  "Remove all locks and overlays."
  (org-deadsync--lock-all nil)
  (org-deadsync-clear-overlays))

(defun org-deadsync-org-shiftdirection (direction)
  "Substitutes for org-shift<direction> when DEADSYNC mode activated."
  (if (and (org-at-timestamp-p 'agenda)
	   (save-excursion (beginning-of-line)
			   (re-search-forward (org-re-timestamp 'deadline) nil t)))
      (progn 
	(if (and (string= (org-entry-get (point) "ORG-DEADSYNC-ACTIVE") "t")
		 (get-text-property (point) 'read-only)
		 (yes-or-no-p "Do you want to deactivate this dependency?"))
	    (org-deadsync-toggle-active))))
  (cond ((eq direction 'down) (org-shiftdown))
	((eq direction 'up) (org-shiftup))
	((eq direction 'left) (org-shiftleft))
	((eq direction 'right) (org-shiftright)))
  (when (string= (org-entry-get (point) "ORG-DEADSYNC-MASTER") "t")
    (org-deadsync-refresh-dependents)))

(defun org-deadsync-org-shiftleft ()
  (interactive)
  (org-deadsync-org-shiftdirection 'left))

(defun org-deadsync-org-shiftright ()
  (interactive)
  (org-deadsync-org-shiftdirection 'right))

(defun org-deadsync-org-shiftup ()
  (interactive)
    (org-deadsync-org-shiftdirection 'up))

(defun org-deadsync-org-shiftdown ()
  (interactive)
  (org-deadsync-org-shiftdirection 'down))

(defhydra org-deadsync--hydra (:color blue :hint nil)
  "
Org Deadline Dependencies:

_s_ Set synced deadline      _j_ Jump to master deadline  _c_ Clear all overlays
_k_ Remove sync              _e_ Echo master information  _r_ Refresh all deadlines
_t_ Toggle active sync
    for this heading

_q_ Quit
"
  ("s" org-deadsync-set-dependency)
  ("j" org-deadsync-jump-to-master)
  ("e" org-deadsync-show-master)
  ("t" org-deadsync-toggle-active)
  ("k" org-deadsync-remove-dependency)
  ("r" org-deadsync-refresh-all)
  ("c" org-deadsync-clear-overlays)
  ("q" nil))

;;;###autoload
(define-minor-mode org-deadsync-mode
  "Create deadline dependencies for org headings"
  nil
  " DEADSYNC"
  (let ((org-deadsync-mode-keymap (make-sparse-keymap)))
    (define-key org-deadsync-mode-keymap (kbd "<S-right>") 'org-deadsync-org-shiftright)
    (define-key org-deadsync-mode-keymap (kbd "<S-up>") 'org-deadsync-org-shiftup)
    (define-key org-deadsync-mode-keymap (kbd "<S-down>") 'org-deadsync-org-shiftdown)
    (define-key org-deadsync-mode-keymap (kbd "<S-left>") 'org-deadsync-org-shiftleft)
    (define-key org-deadsync-mode-keymap (kbd "C-; d d") 'org-deadsync--hydra/body)
    org-deadsync-mode-keymap)
  (if org-deadsync-mode
      (progn 
	(org-deadsync-refresh-all))
    (org-deadsync--clear-all)))

(provide 'org-deadsync)

;;; org-deadsync.el ends here
