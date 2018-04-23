;; Various utilities

(require 'cl)

(defun format-expand (formatters format &optional arg)
  "Formats `format' according to `formatters' passing `arg' as an optional argument."
  (save-match-data
    (let ((regex (concat "%\\("
                         (reduce (lambda (a b)
                                   (concat a "\\|" b))
                                 (mapcar #'car formatters))
                         "\\)")))
      (replace-regexp-in-string regex
                                (lambda (str)
                                  (let ((fun (assoc (substring str 1)
                                                    formatters)))
                                    (if fun
                                        (funcall (cdr fun) arg)
                                        (error "Unrecognized format sequence: %s" str))))
                                format t t))))

(defvar notify-send-last-msg '())

;; Send notifications using libnotify.
(defun notify-send (summary &optional text icon)
  "Show a notification and ding for good measure."
  (interactive)
  (let ((command (concat "notify-send --hint=int:transient:1"
                         (if icon (concat " --icon=" (expand-file-name icon)) "")
                         " " (shell-quote-argument summary) " "
                         (if text (shell-quote-argument text) ""))))
    (shell-command command)
    (my-play-sound (concat my-stuff-dir "ding.wav")))
  (setq notify-send-last-msg (list summary text icon)))

(defun notify-send-last ()
  "Show the last notification."
  (interactive)
  (if (null notify-send-last-msg)
    (message "No previous notification.")
    (apply #'notify-send notify-send-last-msg)))

(defun my-play-sound (sound)
  "Plays a sound."
  (start-process "play" nil "play" sound)
  "Done!")

(defun split-string-on-case (string &optional down-p)
  (let* ((len (length string))
         (start (- len 1))
         (end len)
         (matches ())
         (cas (if down-p
                  #'downcase
                  #'upcase)))
    (while (/= start -1)
      (let ((char (aref string start)))
        (when (= char (funcall cas char))
          (setq matches (cons (substring string start end)
                              matches))
          (setq end start)))
      (setq start (- start 1)))
    (if (/= end 0)
        (cons (substring string 0 end)
              matches)
        matches)))

(defun replace-pairs-in-string (str pairs)
  (if (null pairs)
      str
      (let* ((pair (car pairs))
             (string (car pair))
             (replacement (cdr pair)))
        (replace-pairs-in-string (replace-regexp-in-string string replacement str)
                                 (cdr pairs)))))

(defun html2text-string (str)
  (replace-pairs-in-string str html2text-replace-list))

(defun format-sut-string (format &optional time)
  "Like `format-time-string' only glues SUT time at the end."
  (let* ((base-time (truncate
                     (float-time
                      (date-to-time
                       (format-time-string "%Y-%m-%d 00:00:00"
                                           (or time (current-time))
                                           t)))))
         (curr-time (truncate
                     (float-time
                      (date-to-time
                       (format-time-string "%Y-%m-%d %H:%M:%S"
                                           (or time (current-time))
                                           t)))))
         (sut-time (* (- curr-time base-time) (/ (* 10.0 100 100) (* 24 60 60))))
         (sut-h (/ sut-time (* 100 100)))
         (sut-m (mod (/ sut-time 100) 100))
         (sut-s (mod sut-time 100)))
    (concat (format-time-string format)
            (format " %d" sut-h)
            (format ":%02d" sut-m)
            (format ":%02d" sut-s))))

(defun pl-income-tax-free-sum (base)
  (cond ((< base 6600.00) 1188.00)
        ((< base 11000.00) (- 1188.00 (/ (* 631.98 (- base 6600.00)) 4400.00)))
        ((< base 85528.00) 556.02)
        ((< base 127000.00) (- 556.02 (/ (* 556.02 (- base 85528.00)) 41472.00)))
        (:else 0)))

(defun pl-income-tax (income cost social-security health-insurance)
  (let ((base (round (- income cost social-security)))
        (threshold 85528))
    (round (max 0
                (- (+ (* 0.18 (min base threshold))
                      (* 0.32 (max 0 (- base threshold))))
                   (pl-income-tax-free-sum base)
                   health-insurance)))))

(defun format-consultancy-hours (format date start end break)
  (if (equal date "")
      (format-seconds format 0)
    (format-seconds format (- (cadr (time-subtract (date-to-time (concat date " " end))
                                                   (date-to-time (concat date " " start))))
                            (* (string-to-number break) 60)))))

(provide 'misc-utils)
