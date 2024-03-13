;;;
;;; hooks.el --- hooks that aren't specific to a certain package
;;;

;;; after-make-frame-function should be set in early-init.el, to so it'll take effect for the
;;; initial frame as well.

;;; Automatically break lines in text modes.
(add-hook 'text-mode-hook
          'turn-on-auto-fill)


;;; Automatically insert closing parenthesis.
(add-hook 'prog-mode-hook
          (lambda ()
            (electric-pair-mode 1)))

;;; Delete trailing whitespace before saving.
(add-hook 'before-save-hook
          'delete-trailing-whitespace)

;;; This will effectively stop garbage collection while the minibuffer is in use.
(add-hook 'minibuffer-setup-hook
          (lambda ()
            (setq gc-cons-threshold most-positive-fixnum)))

;;; And resume it when the we exit the minibuffer.
(add-hook 'minibuffer-exit-hook
          (lambda ()
            (setq gc-cons-threshold (* 10 1000 1000))))
