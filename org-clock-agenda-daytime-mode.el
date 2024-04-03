;;; org-clock-agenda-daytime-mode.el --- Display the time clocked today in the modeline  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Arne Babenhauserheide

;; Author: Arne Babenhauserheide <arne_bab@web.de>
;; Keywords: org, lisp, clock, time, agenda
;; Version: 0.0.2
;; Package-Requires: ((org "9.6.18") (emacs "26.1"))
;; URL: https://www.draketo.de/software/emacs-daytime

;; This file is NOT part of GNU Emacs.

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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

;; This is a minor mode that show a modeline entry with the time
;; clocked today in your org-agenda-files.

;; The goal of this mode is to show how much you worked today, so that
;; you see when you worked your daily worktime.

;; Enable the mode with (org-clock-agenda-daytime-mode 1).

;; When you reach a target work time (default: 8 hours), the time
;; display turns green, after reaching a maximum work time (default:
;; 10 hours) it turns red. The values and faces are are customizable
;; in the `org-clock' group (use M-x customize-group org-clock).

;; The time calculation caches results for one minute to avoid slowing
;; down interaction.

;;; Code:

;; add total task time from plan.org to modeline
(defvar org-clock-agenda-daytime--last-time (float-time))
(defvar org-clock-agenda-daytime--last-res "")
(defvar org-clock-agenda-daytime-modeline-entry)

(defcustom org-clock-agenda-daytime-target-work-time-minutes (* 60 8)
  "The daily worktime is marked as enough after this many minutes.

 To specify the minutes more conveniently you can use a simple
 sexp like `(* 60 8)' for a 40 hour week or a more complex one
 like `(+ 24 (* 60 7))' for a 37 hour week"
  :group 'org-clock
  :type '(natnum sexp))
(defcustom org-clock-agenda-daytime--maximum-work-time-minutes (* 60 10)
  "The daily worktime is marked as too long after this many minutes.

You can use a sexp like `(* 60 10)' to specify the minutes more
 conveniently."
  :group 'org-clock
  :type '(natnum sexp))
(defcustom org-clock-agenda-daytime-target-work-time-reached-face 'org-done
  "The face to use to mark the modeline entry when the target time is reached."
  :group 'org-clock
  :type 'face)
(defcustom org-clock-agenda-daytime--maximum-work-time-reached-face 'org-mode-line-clock-overrun
  "The face to use to mark the modeline entry when the maximmum time is reached."
  :group 'org-clock
  :type 'face)


(require 'org)
(defun org-clock-agenda-daytime--org-total-time ()
  "Show the total time clocked for today."
  (interactive)
  ;; cache for a minute
  (if (> 60 (- (float-time) org-clock-agenda-daytime--last-time))
      org-clock-agenda-daytime--last-res
    (setq org-clock-agenda-daytime--last-time (float-time))
    (let* ((daytime-minutes (apply #'+ (mapcar
                                        (lambda (current-file)
                                          (with-current-buffer
                                              (find-file-noselect current-file)
                                            (org-clock-sum-today)))
                                        org-agenda-files)))
           (text (format " [%s] "
                         (org-duration-from-minutes daytime-minutes))))
      (setq org-clock-agenda-daytime--last-res
            (cond
             ((> org-clock-agenda-daytime-target-work-time-minutes daytime-minutes)
              text)
             ((> org-clock-agenda-daytime--maximum-work-time-minutes daytime-minutes)
              (propertize text
                          'face
                          org-clock-agenda-daytime-target-work-time-reached-face))
             (t
              (propertize text
                          'face
                          org-clock-agenda-daytime--maximum-work-time-reached-face)))))))

;;;###autoload
(define-minor-mode org-clock-agenda-daytime-mode
  "Toggle display of the total time clocked today in the modeline."
  :global t :group 'org-clock
  (or global-mode-string (setq global-mode-string '("")))
  (setq org-clock-agenda-daytime-modeline-entry
        '(:eval (org-clock-agenda-daytime--org-total-time)))
  (if org-clock-agenda-daytime-mode
	  (or (memq org-clock-agenda-daytime-modeline-entry global-mode-string)
	      (setq global-mode-string
		        (append global-mode-string (list org-clock-agenda-daytime-modeline-entry))))
    (and (memq org-clock-agenda-daytime-modeline-entry global-mode-string)
         (setq global-mode-string
               (remove org-clock-agenda-daytime-modeline-entry global-mode-string)))))

(provide 'org-clock-agenda-daytime-mode)
;;; org-clock-agenda-daytime-mode.el ends here
