;;; my-nethack-config.el --- My Nethack config

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GENERAL
;;;;;;;;;;;;;;;;;;;;

(require 'nethack)

;; Show timestamps:
(defun nethack-x-timestamp-message ()
  "Add a time-stamp to every message.

Add the following to your ~/.emacs

  (add-hook 'nethack-before-print-message-hook
            'nethack-x-timestamp-message)"
  (insert (format "(%d) " (elt nh-status-attribute-T 0))))

(add-hook 'nethack-before-print-message-hook 'nethack-x-timestamp-message)

;; Show item properties
(defvar nethack-x-highlights '((" blessed " . nethack-green-face)
                   (" holy " . nethack-green-face)
                   (" cursed " . nethack-red-face)
                   (" unholy " . nethack-purple-face)
                   (" cursed .* (being worn)" . nethack-orange-face))
  "An assoc of regexps and font colors")

(defun nethack-x-highlight-option ()
  "Highlight a nethack menu option based on a regexp."
  ;; Move to the beginning of the option just added
  (save-excursion
    (let (start
      (end (point)))
    (forward-line -1)
    (forward-line 0)
    ;; A mini-hack so the option accelerator doesn't get highlighted
    (setq start (+ (point) 4))
    (mapc (lambda (x)
        (if (re-search-forward (car x) nil t)
        (put-text-property start end 'face (cdr x))))
      nethack-x-highlights))))

(add-hook 'nethack-add-menu-hook 'nethack-x-highlight-option)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; KEYBINDINGS
;;;;;;;;;;;;;;;;;;;;

(defmacro nh-bind-keys (&rest bindings)
  (append '(lambda ())
          (map 'list
               (lambda (binding)
                 `(local-set-key (kbd ,(car binding))
                                 ',(intern (format "nethack-command-%s" (cadr binding)))))
               bindings)))

(add-hook 'nethack-map-mode-hook
  (nh-bind-keys
    ;; Movement:
    ("8"       north)
    ("<up>"    north)
    ("2"       south)
    ("<down>"  south)
    ("6"       east)
    ("<right>" east)
    ("4"       west)
    ("<left>"  west)
    ("3"       southeast)
    ("1"       southwest)
    ("9"       northeast)
    ("7"       northwest)
    ("<prior>" up)
    ("<next>"  down)
    ;; Meta stuff:
    ("M-e"     show-all-equipment-in-use)
    ("M-t"     show-tool-in-use)
    ("M-n"     show-worn-amulet)
    ("M-r"     show-worn-rings)
    ("M-a"     show-worn-armor)
    ("M-w"     show-wielded-weapon)
    ("M-d"     show-discoveries)
    ("M-m"     what-is-map-piece)
    ("M-p"     toggle-pickup)
    ;; Character actions:
    ("C-l"     search)
    ("L"       look-here)
    ("l"       what-is-symbol)
    ("R"       read)
    ("r"       rest-one-move)
    ("P"       pray)
    ("p"       pick-up)
    ("C-s"     swap-weapons)
    ("S"       list-known-spells)
    ("s"       cast-spell)
    ("z"       zap-wand)
    ("T"       identify-trap)
    ("t"       throw)
    ("o"       open)
    ("c"       close-door)
    ("$"       count-gold)
    ("C-o"     offer)
    ("C-p"     pay)
    ("q"       quaff)
    ("I"       type-inventory)
    ("i"       inventory)
    ("C-I"     identify-all-items)
    ("m"       move)
    ("M"       create-monster)
    ("a"       force-fight)
    ("A"       apply)
    ("C-a"     select-ammo-for-quiver)
    ("f"       fire)
    ("e"       eat)
    ("E"       engrave)
    ("D"       drop)
    ("d"       drop-specific-item)
    ("C-d"     remove-accessory)
    ("C-R"     remove-all-armor)
    ("C-r"     remove-single-armor)
    ("k"       kick)
    ("W"       wear-armor)
    ("C-w"     wield)
    ("w"       put-on)
    ("<return>" redo-previous)
    ;; Other:
    ("C-e"     explore-mode)
;;    ("C-g"     cancel)
    ("C-x C-s" save-game)
    ("C-q"     quit)
    ("C-S"     settings)
    ("C-r"     redraw-scene)
    ("C-m"     previous-message)
    ("?"       help)
    ("!"       shell-escape)
    ("v"       version)
    ("V"       version-and-history)
))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EVENT HOOKS
;;;;;;;;;;;;;;;;;;;;

;; Warn about low HP:
(defun nethack-x-warn-low-hp (attr new old)
  "Print a message in `nh-message-buffer' when hitpoints get low."
  (if (and (string-equal attr "HP")
       (< new old)
       (< (/ new (float (car nh-status-attribute-HPmax))) 0.20))
      (nhapi-message 'atr-blink "Hitpoints below 20%")))

(add-hook 'nethack-status-attribute-change-functions 'nethack-x-warn-low-hp)

(provide 'my-nethack-config)
