;;; system-stats.el --- Display ystem and Emacs stats in the mode-line.  -*- coding: mule-utf-8 -*-

;; Copyright (C) 2012 Kajetan Rzepecki

;; Author: Kajetan Rzepecki

;; Created: 13 Sep 2012

;; Keywords: hardware

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.


;;; Usage:

;; (require 'system-stats)
;; (system-stats-start)


;; There are a few variables to tweak:
;;   `misc-update-interval' - Time interval after which the stats are updated.
;;   `misc-emacs-uptime-format' - Emacs uptime format string used in the mode-line.
;;   `misc-system-uptime-format' - System uptime format string.
;;   `misc-time-format' - Used to format current time.
;;   `misc-date-format' - Used to format current date.
;;   `misc-stats-format' - Used to glue all this stuff together. Supports:
;;       %E - Emacs uptime according to `misc-emacs-uptime-format'.
;;       %S - System uptime according to `misc-system-uptime-format'.
;;       %l# - System load average (# minutes, where # is either 1, 5 or 15).
;;       %L - System load average triple (like \"%l1 %l5 %l15\").
;;       %T - Current time according to `format-time-string' called with `misc-time-format'.
;;       %D - Like %T, separated for convinience."

;;; TODO:
;; - Move to mode-line-stats

;;; Code:

(require 'cl)
(require 'misc-utils)

(defgroup misc-stats nil
  "Display various system stats in the mode-line."
  :group 'misc-stats)

(defcustom misc-update-interval 2
  "Number of seconds between system stats recalculation."
  :type 'number
  :group 'misc-stats)

(defcustom misc-emacs-uptime-format "%h:%.2m"
  "Format string conforming to `format-seconds'. Uses `emacs-uptime'."
  :type 'string
  :group 'misc-stats)

(defcustom misc-system-uptime-format "%h:%.2m"
  "Conforms to `format-seconds'."
  :type 'string
  :group 'misc-stats)

(defcustom misc-time-format "%H:%M:%S"
  "Conforms to `format-time-string'."
  :type 'string
  :group 'misc-stats)

(defcustom misc-date-format "%Y-%m-%d"
  "Conforms to `format-time-string'."
  :type 'string
  :group 'misc-stats)

(defcustom misc-stats-format "Emacs uptime: %E System uptime: %S Load averages: %L"
  "Format string:
%E - Emacs uptime according to `misc-emacs-uptime-format'.
%S - System uptime according to `misc-system-uptime-format'.
%l# - System load average (# minutes, where # is either 1, 5 or 15).
%L - System load average triple (like \"%l1 %l5 %l15\").
%T - Current time according to `format-time-string' called with `misc-time-format'.
%D - Like %T, separated for convinience.")

(defvar misc-stats-mode-line-string "")
(defvar misc-stats-timer nil)
(defvar misc-stats-formatters nil)

(defun misc-stats-start ()
  "Start displaying misc stats in the mode-line."
  (interactive)
  (add-to-list 'global-mode-string 'misc-stats-mode-line-string t)
  (and misc-stats-timer (cancel-timer misc-stats-timer))
  (setq misc-stats-mode-line-string "")
  (setq misc-stats-timer (run-at-time misc-update-interval
                                      misc-update-interval
                                      (lambda ()
                                        (setq misc-stats-mode-line-string (make-misc-stats))
                                        (force-mode-line-update)
                                        (sit-for 0)))))

(defun misc-stats-stop ()
  "Stop displaying misc system stats in the mode-line."
  (interactive)
  (setq misc-stats-mode-line-string "")
  (setq global-mode-string (delq 'misc-stats-mode-line-string
                                 global-mode-string))
  (setq misc-stats-timer
        (and misc-stats-timer (cancel-timer misc-stats-timer))))

(defun make-misc-stats ()
  (let* ((load (map 'list
                    (lambda (x) (/ x 100.0))
                    (load-average)))
         (boot-time (string-to-number
                      (shell-command-to-string
                        "cat /proc/stat | awk '{if(NR == 6) print $2}'")))
         (emacs-uptime (emacs-uptime misc-emacs-uptime-format))
         (system-uptime (format-seconds misc-system-uptime-format
                                        (- (float-time (current-time))
                                           boot-time))))
    (format-expand misc-stats-formatters
                   misc-stats-format
                   (list load boot-time emacs-uptime system-uptime))))

(setq misc-stats-formatters
  (list ;; FIXME `replace-match' errors with `args-out-of-range'
        ;; FIXME As a workaround precalculate uptimes in `make-misc-stats'
        ;; FIXME and pass as the optional argument to `format-expand'.
        (cons "E" (lambda (stats)
                    ;; (emacs-uptime misc-emacs-uptime-format)
                    (nth 2 stats)))
        (cons "S" (lambda (stats)
                    ;; (format-seconds misc-system-uptime-format
                    ;;                 (- (float-time (current-time))
                    ;;                    (cadr stats)))
                    (nth 3 stats)))

        (cons "L15" (lambda (stats)
                      (format "%.2f" (nth 2 (car stats)))))
        (cons "L1" (lambda (stats)
                     (format "%.2f" (nth 0 (car stats)))))
        (cons "L5" (lambda (stats)
                     (format "%.2f" (nth 1 (car stats)))))
        (cons "L" (lambda (stats)
                    (apply #'format "%.2f %.2f %.2f" (car stats))))

        (cons "T" (lambda (stats)
                    (format-time-string misc-time-format)))
        (cons "D" (lambda (stats)
                    (format-time-string misc-date-format)))
  ))

(provide 'misc-stats)

;;; file ends here
