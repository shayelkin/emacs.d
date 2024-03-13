;;;
;;; key-bindings.el --- global key bindings
;;;


;;; The most important binding is set outside of Emacs: remapping Caps Lock to Control.

;;; As I'm using use-package, I prefer where possible to set keys in a use-package
;;; deceleration, so they could be listed by (describe-personal-keybindings).

;;; TODO: Build equivalent to (describe-personal-keybindings) to keys bound without the use
;;;       of use-package.


(keymap-global-set "M-<return>" 'fill-paragraph)

;;; Probably the only CUA bindings I use.
;;; As a bonus, C-z overrides the default binding of suspend-frame, which is useless imo.
(keymap-global-set "C-z" 'undo)
(keymap-global-set "C-." 'completion-at-point)

;;; I never delete a single character: C-w, and retyping the word is faster than navigating
;;; to a specific character in it.
(keymap-global-set "C-w" 'backward-kill-word)

;;; Move kill-region (cut) to C-c C-k.
(keymap-global-set "C-c C-k" 'kill-region)

;;; C-x m to open links in web browsers.
(keymap-global-set "C-x m" 'browse-url-at-point)

;;; Join this line and the next, regardless on where the point is in the line.
(keymap-global-set "M-j" (lambda ()
                           (interactive)
                           (join-line -1)))

;;; Change the font scale: by default, this is bound to C-wheel, which cause many accidental
;;; font size changes. Instead, bind it to C-S with the +/- keys, or C-S-0 to reset it (the
;;; latter is bound by default to paredit-forward-slurp-sexp, but that command is also bound
;;; to C-<right>, which is what I use).
(keymap-global-unset "C-<wheel-up>")
(keymap-global-unset "C-<wheel-down>")
(keymap-global-set   "C-+" 'text-scale-increase)
(keymap-global-set   "C-_" 'text-scale-decrease)
(keymap-global-set   "C-)" (lambda ()
                             (interactive)
                             (text-scale-increase 0)))


;;; Make M-S-. undo M-., going back to to where M-. was invoked from.
(keymap-global-set "M->" 'pop-tag-mark)

;;; Command key shortcuts for macOS (Emacs maps command to super).
(when on-mac-window-system
  ;;; Emulate a 3-button mouse
  ;;(keymap-set key-translation-map "s-<mouse-1>" "<mouse-2>")
  ;;; Maximize the frame (or restore to pre-maximized value)
  (keymap-global-set    "s-RET" 'toggle-frame-maximized)
  ;;;  Disable command-t, command-q, command-w, and rebind the latter to copy (M-w).
  (keymap-global-unset  "s-t")
  (keymap-global-unset  "s-w")
  (keymap-global-set    "s-w" 'kill-ring-save)
  (keymap-global-unset  "s-q"))


;;; I would like to use indent-whole-buffer as a save-hook, but can't because too many files
;;; are not correctly indented throughout.
(add-hook 'prog-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-i") 'indent-whole-buffer)))
