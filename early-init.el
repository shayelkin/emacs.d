;;; early-init.el --- https://www.gnu.org/software/emacs/manual/html_node/emacs/Early-Init-File.html


;; GC threshold default is 800KB. Raise it high enough to avoid during startup,
;; then lower it but not as low
(setq gc-cons-threshold (* 200 1000 1000))
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold (* 8 1000 1000))))

;; Do frame-chrome changing stuff before the first frame is created
;; (dsabling the toolbar after the frame is created takes ~0.2s)
(add-to-list 'default-frame-alist '(tool-bar-lines . 0))
(add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))
(add-to-list 'default-frame-alist '(height . 50))
(add-to-list 'default-frame-alist '(width  . 200))

(provide 'early-init) ;; idk if Emacs require or load this file?
