;;; org-deadsync.el --- Sync deadlines in emacs orgmode documents -*- lexical-binding: t; -*-

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

;; Run one of these commands:

;; `org-deadsync-mode': Turn on deadsync mode

;;;; Tips

;; + You can customize settings in the `deadsync' group.

;;;; Credits

;; This package would not have been possible without the following
;; packages: org-ql, ts.el 
;;
;;  [1] https://github.com/alphapapa/ts.el
;;  [2] https://github.com/alphapapa/org-ql/

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

;;;; Customization

(defvar org-deadsync-weekend-adjustment t
  "Moves a deadline that falls on the weekend to the next weekday")

(defvar org-deadsync-files (org-agenda-files)
  "Files with linked deadlines. Defaults to all agenda files.")

(setq org-deadsync-files "~/Desktop/temp.org")

(defvar org-deadsync-locked-deadline-text-properties '()
  "plist of text properties to apply to locked deadlines")

(defvar org-deadsync-face-attrs nil
  "plist of face attributes for displaying locked deadlines, e.g., '(:background \"gray\")")

(defvar org-deadsync-lock-icon "⚷"
  "Icon displayed after locked deadlines")


(defvar org-deadsync-skip-dates '()
  "List of dates (\"YYYY-MM-DD\" to exclude as possible deadlines, e.g., holidays, birthdays")

;;my customized skip dates, which are federal holidays
(setq org-deadsync-skip-dates '("2019-01-01" "2019-01-21" "2019-02-18" "2019-05-27" "2019-07-04"
				"2019-09-02" "2019-10-14" "2019-11-14" "2019-11-28" "2019-12-25"
				"2020-01-01" "2020-01-20" "2020-02-17" "2020-05-25" "2020-07-03"
				"2020-09-07" "2020-10-12" "2020-11-11" "2020-11-26" "2020-12-25"
				"2021-01-01" "2021-01-18" "2021-02-15" "2021-05-31" "2021-07-05"
				"2021-09-06" "2021-10-11" "2021-11-11" "2021-11-25" "2021-12-24"))

;;;; Functions

(defun org-deadsync--ts-adjust (&rest adjustments)
  "Wrapper for (ts-adjust) to allow arguments in the form, e.g., \"+3d\"; accepts a list of strings in the form \"[+/-][number][d(ay), m(onth), y(ear)]\""
  (let ((ts (-last-item adjustments))
	(adjustments (nbutlast adjustments)))
    (-map (lambda (adjustment)
	    (let ((slot (substring adjustment -1)))
	      (cond ((string= slot "d") (setq slot 'day))
		    ((string= slot "m") (setq slot 'month))
		    ((string= slot "y") (setq slot 'year)))
	      (setq ts (ts-adjust slot (string-to-number (substring adjustment 0 -1)) ts))))
	  adjustments)
    ts))

(defun org-deadsync-refresh-this-heading ()
  (interactive)
  (let* ((master-deadline (save-excursion
			    (org-id-goto (org-entry-get (point) "ORG-DEADSYNC-LINK"))
			    (ts-parse-org (org-entry-get (point) "DEADLINE"))))
	 (offset (org-entry-get (point) "ORG-DEADSYNC-OFFSET"))
	 (new-deadline (->> master-deadline
			    (org-deadsync--ts-adjust offset)
			    (org-deadsync--skip-date-adjust)
			    (org-deadsync--weekend-adjust)
			    (ts-format "<%Y-%m-%d %a>"))))
    (org-deadsync--lock-deadline nil)
    (org-deadline nil new-deadline)
    (org-deadsync--lock-deadline t)))


(defun org-deadsync--set-deadline (deadline)
  "Set or update the deadline at current heading"
  (if (org-deadsync--deadline-read-only-p)
      (progn (org-deadsync--deadline nil)
	     (org-deadline nil deadline)
	     (org-deadsync--lock-deadline t))
    (org-deadline nil deadline)))


;; Apologies to the reader for the ifs and progns
(defun org-deadsync-org-shiftdown ()
  "Stand-in for org-shiftdown to deal with locked deadlines"
  (interactive)
  ;; If at a deadline timestamp...
  (if (and (org-at-timestamp-p 'agenda)
	   (save-excursion (beginning-of-line)
			   (re-search-forward "DEADLINE:[[:space:]]<[[:digit:]]\\{4\\}-[[:digit:]]\\{2\\}-[[:digit:]]\\{2\\}.*?>" nil t)))
      (progn 
	;;If the deadline link is active, and prompt to deactivate
	(if (and (string= (org-entry-get (point) "ORG-DEADSYNC-ACTIVE") "t")
		 (get-text-property (point) 'read-only)
		 (yes-or-no-p "Do you wish to deactivate this dependency?"))
	    (org-deadsync-toggle-active))))
	;;If a master deadline is updated, update dependents
  (org-shiftdown)
  (when (string= (org-entry-get (point) "ORG-DEADSYNC-MASTER") "t")
    (org-deadsync-refresh-dependents)))

(defun org-deadsync-org-shiftleft ()
  "Stand-in for org-shiftleft to deal with locked deadlines"
  (interactive)
  ;; If at a deadline timestamp...
  (if (and (org-at-timestamp-p 'agenda)
	   (save-excursion (beginning-of-line)
			   (re-search-forward "DEADLINE:[[:space:]]<[[:digit:]]\\{4\\}-[[:digit:]]\\{2\\}-[[:digit:]]\\{2\\}.*?>" nil t)))
      (progn 
	;;If the deadline link is active, and prompt to deactivate
	(if (and (string= (org-entry-get (point) "ORG-DEADSYNC-ACTIVE") "t")
		 (get-text-property (point) 'read-only)
		 (yes-or-no-p "Do you wish to deactivate this dependency?"))
	    (org-deadsync-toggle-active))))
	;;If a master deadline is updated, update dependents
  (org-shiftleft)
  (when (string= (org-entry-get (point) "ORG-DEADSYNC-MASTER") "t")
    (org-deadsync-refresh-dependents)))

(defun org-deadsync-org-shiftright ()
  "Stand-in for org-shiftright to deal with locked deadlines"
  (interactive)
  ;; If at a deadline timestamp...
  (if (and (org-at-timestamp-p 'agenda)
	   (save-excursion (beginning-of-line)
			   (re-search-forward "DEADLINE:[[:space:]]<[[:digit:]]\\{4\\}-[[:digit:]]\\{2\\}-[[:digit:]]\\{2\\}.*?>" nil t)))
      (progn 
	;;If the deadline link is active, and prompt to deactivate
	(if (and (string= (org-entry-get (point) "ORG-DEADSYNC-ACTIVE") "t")
		 (get-text-property (point) 'read-only)
		 (yes-or-no-p "Do you wish to deactivate this dependency?"))
	    (org-deadsync-toggle-active))))
	;;If a master deadline is updated, update dependents
  (org-shiftright)
  (when (string= (org-entry-get (point) "ORG-DEADSYNC-MASTER") "t")
    (org-deadsync-refresh-dependents)))

(defun org-deadsync-org-shiftup ()
  "Stand-in for org-shiftup to deal with locked deadlines"
  (interactive)
  ;; If at a deadline timestamp...
  (if (and (org-at-timestamp-p 'agenda)
	   (save-excursion (beginning-of-line)
			   (re-search-forward "DEADLINE:[[:space:]]<[[:digit:]]\\{4\\}-[[:digit:]]\\{2\\}-[[:digit:]]\\{2\\}.*?>" nil t)))
      (progn 
	;;If the deadline link is active, and prompt to deactivate
	(if (and (string= (org-entry-get (point) "ORG-DEADSYNC-ACTIVE") "t")
		 (get-text-property (point) 'read-only)
		 (yes-or-no-p "Do you wish to deactivate this dependency?"))
	    (org-deadsync-toggle-active))))
	;;If a master deadline is updated, update dependents
  (org-shiftup)
  (when (string= (org-entry-get (point) "ORG-DEADSYNC-MASTER") "t")
    (org-deadsync-refresh-dependents)))

(defun org-deadsync--dependentsp ()
  "Does the current heading have dependents?"
  (when-let ((master-id (org-entry-get (point) "ID")))
    (if (org-ql-select org-deadsync-files
	  `(and (property "ORG-DEADSYNC-LINK" ,master-id)
		(property "ORG-DEADSYNC-ACTIVE" "t")))
	t
      nil)))

(defun org-deadsync--deadline-locked-p ()
  "For the current heading, returns t if deadline is locked; otherwise nil"
  (save-excursion
    (when (org-get-deadline-time (point))
      (end-of-visual-line)
      (re-search-backward "^\\*+[[:space:]]")
      (re-search-forward "DEADLINE:[[:space:]]<[[:digit:]]\\{4\\}-[[:digit:]]\\{2\\}-[[:digit:]]\\{2\\}.*?>" nil t)
      (if (text-property-any (match-beginning 0) (match-end 0) 'read-only t)
	  't
	nil))))

(defun org-deadsync--toggle-lock ()
  "Use org-deadsync--toggle-active."
  (org-deadsync--lock-deadline (not (org-deadsync-deadline-locked-p))))

(defun org-deadsync--lock-deadline (t-or-nil)
  "Make deadline in current heading read-only if argument is non-nil"
  (save-excursion
    (when (org-get-deadline-time (point))
      (end-of-visual-line)
      (re-search-backward "^\\*+[[:space:]]")
      (when (re-search-forward "DEADLINE:[[:space:]]<[[:digit:]]\\{4\\}-[[:digit:]]\\{2\\}-[[:digit:]]\\{2\\}.*?>" nil t)
	(let ((inhibit-read-only t)
	      (start (match-beginning 0))
	      (end (match-end 0)))
	  (if t-or-nil
	      (overlay-put (make-overlay start end) 'after-string org-deadsync-lock-icon)
	    (remove-overlays (save-excursion (beginning-of-line) (point))
			     (save-excursion (end-of-line) (point))
			     'after-string org-deadsync-lock-icon))
	  (put-text-property start end 'read-only t-or-nil))))))



;;; Replaced by previous function.

;; (defun org-deadsync--lock-deadline (t-or-nil)
;;   "Make deadline in current heading read-only if argument is non-nil"
;;   (save-excursion
;;     (when (org-get-deadline-time (point))
;;       (end-of-visual-line)
;;       (re-search-backward "^\\*+[[:space:]]")
;;       (re-search-forward "DEADLINE:[[:space:]]<[[:digit:]]\\{4\\}-[[:digit:]]\\{2\\}-[[:digit:]]\\{2\\}.*?>" nil t)
;;       (let ((inhibit-read-only t))
;; 	(put-text-property (match-beginning 0) (match-end 0) 'read-only t-or-nil))
;;       (if t-or-nil
;; 	  (progn
;; 	    (when org-deadsync-face-attrs
;; 	      (overlay-put (make-overlay (match-beginning 0) (match-end 0)) 'face org-deadsync-face-attrs))
;; 	    (when org-deadsync-lock-icon
;; 	      (overlay-put (make-overlay (match-beginning 0) (match-end 0)) 'after-string org-deadsync-lock-icon)))
;; 	(remove-overlays (match-beginning 0) (+ (length org-deadsync-lock-icon) (match-end 0)))))))

(defun org-deadsync-remove-dependency ()
  "Removes the dependency for the current heading"
  (interactive)
  (save-excursion
    (let ((master-id (org-entry-get (point) "ORG-DEADSYNC-LINK")))
      (org-deadsync--lock-deadline nil)
      (org-delete-property "ORG-DEADSYNC-LINK")
      (org-delete-property "ORG-DEADSYNC-OFFSET")
      (org-delete-property "ORG-DEADSYNC-ACTIVE")
      (save-excursion
	(org-id-goto master-id)
	(unless (org-deadsync--dependentsp)
	    (org-delete-property "ORG-DEADSYNC-MASTER"))))))

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
    (when (not (and (string= (substring offset 0) "+")
		   (string= (substring offset 0) "-")))
      (setq offset (concat "+" offset)))
    (save-excursion 
      (org-goto)
      (setq master-deadline (cdar (org-entry-properties (point) "DEADLINE")))
      (when (not master-deadline)
	(user-error "No deadline in this node"))
      (setq master-id (cadr (org-entry-properties (point) "ID")))
      (when (not master-id)
	(setq master-id (org-id-get-create)))
      (org-set-property "ORG-DEADSYNC-MASTER" "t"))
  (org-set-property "ORG-DEADSYNC-LINK" master-id)
  (org-set-property "ORG-DEADSYNC-OFFSET" offset)
  (org-set-property "ORG-DEADSYNC-ACTIVE" "t")
  (org-deadline nil "2019-01-01") ; dummy deadline
  (org-deadsync--lock-deadline t)
  (org-deadsync-refresh-this-heading)))

(defun org-deadsync-refresh-all ()
  (interactive)
  (org-ql-select org-deadsync-files
    '(property "ORG-DEADSYNC-MASTER" "t")
    :action (lambda ()
	      (org-deadsync-refresh-dependents))))

(defun org-deadsync-refresh-dependents ()
  (interactive)
  (when (org-entry-get (point) "ORG-DEADSYNC-MASTER" "t")
    (let ((master-id (org-entry-get (point) "ID")))
      (org-ql-select org-deadsync-files
	`(and (property "ORG-DEADSYNC-LINK" ,master-id)
	      (property "ORG-DEADSYNC-ACTIVE" "t"))
	:action (lambda ()
		  (org-deadsync-refresh-this-heading)
		  (org-deadsync-refresh-dependents))))))
		      
(defun org-deadsync--weekend-adjust (timestamp)
  "Adjust deadline to next Monday if the deadline falls on a weekend, assuming org-deadsync-weekend-adjustment is t"
  (if org-deadsync-weekend-adjustment
      (cond ((string= (ts-format "%a" timestamp) "Sun")
	     (ts-adjust 'day 1 timestamp))
	    ((string= (ts-format "%a" timestamp) "Sat")
	     (ts-adjust 'day 2 timestamp))
	    (t timestamp))
    timestamp))
  
(defun org-deadsync--lock-all (t-or-nil)
  (interactive)
  (org-ql-select org-deadsync-files
    '(property "ORG-DEADSYNC-ACTIVE" "t")
    :action (lambda ()
	      (org-deadsync--lock-deadline t-or-nil))))

(defun org-deadsync--skip-date-adjust (timestamp)
  "Adjust deadline to next day if deadline falls on a holiday"
  (if (member (ts-format "%Y-%m-%d" timestamp) org-deadsync-skip-dates)
      (ts-adjust 'day 1 timestamp)
    timestamp))

(defun org-deadsync-toggle-active ()
  "Toggle whether a dependent deadline is active or not"
  (interactive)
  (when-let ((activep (cdar (org-entry-properties (point) "ORG-DEADSYNC-ACTIVE"))))
    (if (string= activep "t")
	(progn 
	  (org-set-property "ORG-DEADSYNC-ACTIVE" "nil")
	  (org-deadsync--lock-deadline nil))
      (org-set-property "ORG-DEADSYNC-ACTIVE" "t")
      (org-deadsync-refresh-this-heading)
      (org-deadsync-refresh-dependents))))

(defvar org-deadsync-mode nil
  "Mode variable for org-deadsync-mode")
(make-variable-buffer-local 'org-deadsync-mode)

(defhydra org-deadsync--hydra (:color blue :hint nil)
  "
Org Deadline Dependencies:

_s_ Set dependence           _j_ Jump to master deadline  _T_ Turn off minor mode
_k_ Remove dependency        _e_ Echo master information  _t_ Toggel syncing on off for heading

_r_ Refresh
_q_ Quit
"
  ("s" org-deadsync-set-dependency)
  ("j" org-deadsync-jump-to-master)
  ("e" org-deadsync-show-master)
  ("T" org-deadsync-mode)
  ("t" org-deadsync-toggle-active)
  ("k" org-deadsync-remove-dependency)
  ("r" org-deadsync-refresh)
  ("q" nil))

(defun org-deadsync-mode (&optional arg)
  "Enable deadline-dependency mode"
  (interactive)
  (setq org-deadsync-mode (not org-deadsync-mode))

  (if org-deadsync-mode
      (org-deadsync-refresh-all)
    (org-deadsync--lock-all nil)))
  
(if (not (assq 'org-deadsync-mode minor-mode-alist))
    (setq minor-mode-alist
	  (cons '(org-deadsync-mode " DEADSYNC")
		minor-mode-alist)))

;;;###autoload
(define-minor-mode org-deadsync-mode
  "Create deadline dependencies for org headings"
  :lighter " DEADSYNC"
  :keymap (let ((org-deadsync-mode-keymap (make-sparse-keymap)))
	    (define-key org-deadsync-mode-keymap (kbd "<S-right>") 'org-deadsync-org-shiftright)
	    (define-key org-deadsync-mode-keymap (kbd "<S-up>") 'org-deadsync-org-shiftup)
	    (define-key org-deadsync-mode-keymap (kbd "<S-down>") 'org-deadsync-org-shiftdown)
	    (define-key org-deadsync-mode-keymap (kbd "<S-left>") 'org-deadsync-org-shiftleft)
	    (define-key org-deadsync-mode-keymap (kbd "C-c d d") 'org-deadsync--hydra/body)
	    org-deadsync-mode-keymap))

(provide 'org-deadsync)

  

  

  
