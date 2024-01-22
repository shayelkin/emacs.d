;;;
;;; early-init.el ---
;;; <https://www.gnu.org/software/emacs/manual/html_node/emacs/Early-Init-File.html>
;;;


;;; Increase the interpreter limits, as the defaults are pretty low.
(setq max-lisp-eval-depth 5000)
(when (version< emacs-version "29.1")
  ;; In 29.1+, increasing max-lisp-eval-depth should take care of it
  (setq max-specpdl-size 5000))


;;; Raise gc-cons-threshold a lot to avoid GC pauses during startups, lower it after (but not as
;;; low as the 800Kb default).
(setq gc-cons-threshold (* 1000 1000 1000))
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold (* 10 1000 1000))))


;;; Do frame-chrome changing stuff before the first frame is created (disabling the toolbar after
;;; the frame is created takes ~0.2s).
(add-to-list 'default-frame-alist '(tool-bar-lines . 0))
(add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))
;;; I have a really big screen, so a maximized frame is too tall and too wide for my taste. 200 by
;;; 50 covers most, but not all, of a 13" laptop screen (which is the smallest I currently use).
(add-to-list 'default-frame-alist '(width . 200))
(add-to-list 'default-frame-alist '(height . 50))


;;; Load customization early. This helps a few things:
;;; 1. It does chrome changing stuff.
;;; 2. If customizing a package, this would have the values set before whatever startup code for it
;;;    is executed with the default values.
;;; 3. Values explicitly set in init won't be overridden by auto-saved customized values.
(setq custom-file
      (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  ;; strip the extension, so the compiled file will be loaded (if exists)
  (load (file-name-sans-extension custom-file)))


;;; idk if Emacs require or load this file, but adding a provide won't hurt.
(provide 'early-init)
