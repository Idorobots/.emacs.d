;;; my-org-config.el --- My Org-Mode config.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GENERAL
;;;;;;;;;;;;;;;;;;;;

(require 'gamify)

(setq gamify-notification-function 'notify-send)
(setq gamify-skip-levels '("Dabbling" "Novice" "Adequate"))
(setq gamify-update-interval 1)
(setq gamify-rusty-time (* 60 60 24 30 3))
(setq gamify-very-rusty-time (* 60 60 24 30 6))
(setq gamify-stats-file (expand-file-name "~/gamify/gamify"))
(setq gamify-low-exp 10)
(setq gamify-low-exp-delta 5)
(setq gamify-format "%XP %Lc %T %xp")
(setq gamify-org-p t)

(setq gamify-stat-name-translation-alist
      '(("Cpp" . "C++")
        ("ClientServer" . "Client/Server")))

(setq gamify-stat-levels
      '((0 . "Dabbling")
        (200 . "Novice")
        (1200 . "Adequate")
        (3700 . "Competent")
        (8500 . "Skilled")
        (16600 . "Experienced")
        (28900 . "Proficient")
        (46400 . "Adept")
        (70200 . "Professional")
        (101400 . "Expert")
        (141200 . "Accomplished")
        (190800 . "Great")
        (251400 . "Master")
        (324200 . "HighMaster")
        (410500 . "GrandMaster")
        (511700 . "Legendary")
        (845033 . "Tesla")))

(setq gamify-dot-min-font-size 12.0)
(setq gamify-dot-max-font-size 42.0)
(setq gamify-dot-min-node-size 1.0)
(setq gamify-dot-max-node-size 5.0)
(setq gamify-dot-border-size 7)
(setq gamify-dot-node-shape "circle")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COLOR THEMES
;;;;;;;;;;;;;;;;;;;;

(defun my-gamify-dark-theme ()
  (interactive)
  (setq gamify-dot-node-fill-color "#0a0a0a")
  (setq gamify-dot-edge-color "#0a0a0a")
  (setq gamify-dot-default-node-color "#454545")
  (setq gamify-dot-default-font-color "#454545")
  (setq gamify-dot-font-color "#7f7f7f")
  (setq gamify-dot-achievement-box-color "#9b7f30")
  (setq gamify-dot-rusty-font-color "#9b7f30")
  (setq gamify-dot-very-rusty-font-color "#ed3519")
  (setq gamify-dot-background-color "#000000") ; "#00000000"

  (setq gamify-dot-level-colors
        '(("Dabbling" . "#3d3d3d") ;
          ("Novice" . "#e7e7e7") ;
          ("Adequate" . "#e7e7e7")
          ("Competent" . "#e7e7e7")
          ("Skilled" . "#1eff00") ;
          ("Experienced" . "#1eff00")
          ("Proficient" . "#1eff00")
          ("Adept" . "#0070ff") ;
          ("Professional" . "#0070ff")
          ("Expert" . "#0070ff")
          ("Accomplished" . "#a335ee") ;
          ("Great" . "#a335ee")
          ("Master" . "#a335ee")
          ("HighMaster" . "#ff8000") ;
          ("GrandMaster" . "#ff8000")
          ("Legendary" . "#ff8000")
          ("Tesla" . "#dc143b"))) ;
  )

(defun my-gamify-light-theme ()
  (interactive)
  (setq gamify-dot-node-fill-color "#f7f7f7")
  (setq gamify-dot-edge-color "#8f8f8f")
  (setq gamify-dot-default-node-color "#e8e8e8")
  (setq gamify-dot-default-font-color "#a8a8a8")
  (setq gamify-dot-font-color "#2f2f2f")
  (setq gamify-dot-achievement-box-color "#9b7f30")
  (setq gamify-dot-rusty-font-color "#9b7f30")
  (setq gamify-dot-very-rusty-font-color "#ed3519")
  (setq gamify-dot-background-color "#ffffff") ; "#ffffff00"

  (setq gamify-dot-level-colors
        '(("Dabbling" . "#e0e0e0") ;
          ("Novice" . "#989898") ;
          ("Adequate" . "#989898")
          ("Competent" . "#989898")
          ("Skilled" . "#1eff00") ;
          ("Experienced" . "#1eff00")
          ("Proficient" . "#1eff00")
          ("Adept" . "#0070ff") ;
          ("Professional" . "#0070ff")
          ("Expert" . "#0070ff")
          ("Accomplished" . "#a335ee")
          ("Great" . "#a335ee")
          ("Master" . "#a335ee")
          ("HighMaster" . "#ff8000") ;
          ("GrandMaster" . "#ff8000")
          ("Legendary" . "#ff8000")
          ("Tesla" . "#dc143b"))) ;
  )

(my-gamify-dark-theme) ; Set the dark theme by default.
(gamify-start) ; Start the thing.

(provide 'my-gamify-config)
