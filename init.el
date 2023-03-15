;; init.el --- For Emacs 27+

;; Don't bother with backwards compatibility
(when (version< emacs-version "27")
  (error "Time to upgrade this Emacs installation!"))

(defconst on-mac-window-system (memq window-system '(mac ns))
  "Non-nil if running on a macOS Desktop.")

(let ((site-functions-file
       (expand-file-name "site-functions.el" user-emacs-directory)))
  (if (file-exists-p site-functions-file)
      (load site-functions-file)
    (message "site-functions.el missing (maybe unlinked?). Some commands would be broken.")))

;;; Workaround for "Invalid image type 'svg'"
;;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=59081
(when (and on-mac-window-system (version< emacs-version "29"))
  (defun image-type-available-p (type)
  "Return t if image type TYPE is available.
Image types are symbols like `xbm' or `jpeg'."
  (if (eq 'svg type)
      nil
    (and (fboundp 'init-image-library)
         (init-image-library type)))))

(blink-cursor-mode                0)
(show-paren-mode                  t)
(column-number-mode               t)
(electric-indent-mode             t)            ;; Automatically reindent
(global-display-line-numbers-mode t)

(defalias 'yes-or-no-p 'y-or-n-p)

(windmove-default-keybindings)                  ;; Shift+Arrows switch windows

(add-hook 'text-mode-hook 'turn-on-auto-fill)   ;; Auto fill in text-mode

(set-variable 'shell-font-lock-keywords nil)    ;; Don't highlight in shell buffers

(setq-default indent-tabs-mode nil)

(setq
 use-dialog-box                        nil
 inhibit-startup-message               t
 frame-title-format                    "%b"
 mac-option-modifier                   'meta
 ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=36810
 gnutls-algorithm-priority             "NORMAL:-VERS-TLS1.3"
 read-file-name-completion-ignore-case t
 initial-major-mode                    'literate-calc-mode)

(add-hook 'prog-mode-hook
          (lambda ()
            (electric-pair-mode 1)
            (local-set-key (kbd "C-c C-i") 'indent-whole-buffer)))

;;;
;;; Key Bindings:
;;;

;; Function keys
;; F7 - literate-calc-eval-buffer
(global-set-key (kbd "<f8>") 'ispell-word)
;; F12 - vterm-other-window

;; Other keys
(global-set-key (kbd "C-z")  'undo)
(global-set-key (kbd "C-w")  'backward-kill-word)

;; M-x alternatives
(global-set-key (kbd "C-c C-c")    'execute-extended-command)
(global-set-key (kbd "<C-return>") 'execute-extended-command)

;; Move kill-region (cut) to C-c C-k.
(global-set-key (kbd "C-c C-k") 'kill-region)

;; C-x m to open links in web browsers
(global-set-key (kbd "C-x m") 'browse-url-at-point)

;; Join this line and the next, regardless on where the point is in the line
(global-set-key (kbd "M-j") (lambda () (interactive) (join-line -1)))

;; C-S with the +/- keys to changes font size
(global-set-key (kbd "C-+") (lambda () (interactive) (text-scale-increase +1)))
(global-set-key (kbd "C-_") (lambda () (interactive) (text-scale-increase -1)))

;; M-S-.: Go back to where M-. was invoked from
(global-set-key (kbd "M->") 'pop-tag-mark)

;; macOS: Disable command-{t,q,w}, rebind the latter to copy (M-w)
(when on-mac-window-system
  (global-unset-key (kbd "s-t"))
  (global-unset-key (kbd "s-w"))
  (global-set-key   (kbd "s-w") 'kill-ring-save)
  (global-unset-key (kbd "s-q")))

;;;
;;; Packages, organized with https://github.com/jwiegley/use-package:
;;;

(setq package-archives '(("gnu"          . "https://elpa.gnu.org/packages/")
			 ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("melpa"        . "https://melpa.org/packages/"))
      package-archive-priorities '(("melpa-stable" . 2)
                                   ("gnu"          . 1)
                                   ("melpa"        . 0)))

(eval-when-compile
  (require 'use-package))

(require 'bind-key)
(require 'use-package-ensure)
(setq use-package-always-ensure t)

(use-package server
  :if window-system
  :hook
  (after-init . (lambda ()
                  (unless (server-running-p)
                    (server-start)))))

(use-package hl-line
  :hook
  ((prog-mode text-mode literate-calc-mode) . hl-line-mode))

(use-package flycheck
  :init
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  :config
  (global-flycheck-mode)
  ;; https://github.com/flycheck/flycheck/issues/1523
  (let ((govet (flycheck-checker-get 'go-vet 'command)))
    (when (equal (cadr govet) "tool")
      (setf (cdr govet) (cddr govet)))))

(use-package flycheck-popup-tip
  :hook
  (flycheck-mode . flycheck-popup-tip-mode))

(use-package flyspell
  :hook
  ((text-mode . flyspell-mode)
   (prog-mode . flyspell-prog-mode)))

(use-package exec-path-from-shell
  :if on-mac-window-system
  :config
  (exec-path-from-shell-initialize))

(use-package doom-modeline
  :init
  (setq doom-modeline-buffer-encoding 'nondefault)
  :config
  (doom-modeline-mode 1))

(use-package which-key
  :config
  (which-key-mode))

(use-package auto-dim-other-buffers
  :config
  (auto-dim-other-buffers-mode))

(use-package markdown-mode
  :commands
  (markdown-mode gfm-mode)
  :mode
  (("README\\.md\\'" . gfm-mode)
   ("\\.md\\'"       . markdown-mode)
   ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command
        "pandoc --quiet -f markdown -t html -s --mathjax --highlight-style=pygments"))

(use-package helm
  :config
  (helm-mode 1))

(use-package helm-swoop
  :after
  (helm)
  :init
  (setq helm-swoop-split-direction 'split-window-horizontally)
  :bind
  (("C-s"   . helm-swoop)
   ("C-x i" . helm-multi-swoop-all)
   ("M-x"   . helm-M-x)
   :map helm-swoop-map
   ("M-i" . helm-multi-swoop-all-from-helm-swoop)))

(use-package magit
  :bind
  ("C-x C-g" . magit-status))

(use-package eglot
  :hook
  ((rust-mode
    go-mode)  . eglot-ensure))

(use-package vterm
  :init
  (setq vterm-max-scrollback 100000)
  :bind
  ("<f12>" . vterm-other-window))

(use-package paredit
  :hook
  ((cider-repl-mode
    clojure-mode
    emacs-lisp-mode
    eval-expression-minibuffer-setup
    ielm-mode
    lisp-interaction-mode
    lisp-mode
    scheme-mode)
   . enable-paredit-mode))

(use-package literate-calc-mode
  :bind
  ("<f7>" . literate-calc-eval-buffer)
  ("<S-f7>" . literate-calc-minor-mode))

;; https://github.com/alphapapa/prism.el
(use-package prism
  :hook
  (((c-mode
     cider-repl-mode
     clojure-mode
     emacs-lisp-mode
     eval-expression-minibuffer-setup
     java-mode
     ielm-mode
     lisp-interaction-mode
     lisp-mode
     scheme-mode)
    . prism-mode)
   (python-mode . prism-whitespace-mode)))

(use-package dash-at-point
  :if on-mac-window-system
  :bind
  ("C-?" . dash-at-point))

;;;
;;;
;;;

;; Keep a separate file with machine specific configuration. It is called last,
;; so variables set there would override those set here.
(let ((local-init-file
       (expand-file-name "local.el" user-emacs-directory)))
  (when (file-exists-p local-init-file)
    (load local-init-file)))

;; Custom is last. It is also machine specific.
(setq custom-file
      (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))
