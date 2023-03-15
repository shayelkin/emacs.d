;;; local.el

(add-to-list 'load-path (expand-file-name "~/src/emacs-lisp"))

(setq-default line-spacing 1)

;; Always delete trailing whitespaces before saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Can't enable globally as lots of Confluent code doesn't have final newline
(setq require-final-newline t)

;(split-window-horizontally)

;;;
;;; Colors
;;;

(defvar light-theme 'adwaita)
(defvar dark-theme  'misterioso)

(defun toggle-theme ()
  (interactive)
  (if (eq (car custom-enabled-themes) light-theme)
      (load-theme dark-theme)
    (load-theme light-theme)))

(global-set-key (kbd "<f5>") 'toggle-theme)
