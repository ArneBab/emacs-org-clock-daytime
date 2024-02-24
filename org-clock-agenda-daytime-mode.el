;;; org-clock-agenda-daytime-mode --- display the time clocked today in the modeline -*- lexical-binding: t -*-

;; Copyright (C) 2024 Arne Babenhauserheide
;; Keywords: org, lisp, clock, time, agenda
;; Version: 0.0.1
;; Requires: org

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

;;; Commentary:

;; This is a minor mode that show a modeline entry with the time
;; clocked today in your org-agenda-files.

;; The goal of this mode is to show how much you worked today, so that
;; you see when you worked your daily worktime.

;; Enable the mode with (org-clock-agenda-daytime-mode 1).

;; The time calculation caches results for one minute to avoid slowing
;; down interaction.

;;; Code:

;; add total task time from plan.org to modeline
(defvar org-total-time--last-time (float-time))
(defvar org-total-time--last-res "")
(defvar org-clock-agenda-daytime-modeline-entry)

(require 'org)
(defun org-total-time ()
  "Show the total time clocked for today."
  (interactive)
  ;; cache for a minute
  (if (> 60 (- (float-time) org-total-time--last-time))
      org-total-time--last-res
    (setq org-total-time--last-time (float-time))
    (setq org-total-time--last-res
            (format " [%s] "
                    (org-duration-from-minutes
                     (apply '+ (mapcar
                                (lambda (current-file)
                                  (with-current-buffer
                                      (find-file-noselect current-file)
                                    (org-clock-sum-today)))
                                org-agenda-files)))))
      org-total-time--last-res))

(define-minor-mode org-clock-agenda-daytime-mode
  "Toggle display of the total time clocked today in the modeline."
  :global t :group 'org-clock
  (or global-mode-string (setq global-mode-string '("")))
  (setq org-clock-agenda-daytime-modeline-entry
        '(:eval (org-total-time)))
  (if org-clock-agenda-daytime-mode
	    (or (memq org-clock-agenda-daytime-modeline-entry global-mode-string)
	        (setq global-mode-string
		          (append global-mode-string (list org-clock-agenda-daytime-modeline-entry))))
    (and (memq org-clock-agenda-daytime-modeline-entry global-mode-string)
         (setq global-mode-string
               (remove org-clock-agenda-daytime-modeline-entry global-mode-string)))))

(provide 'org-clock-agenda-daytime-mode)
;;; org-clock-agenda-daytime-mode.el ends here
