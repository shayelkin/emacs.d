;;;
;;; my-packages.el --- use use-package to install and configure packages
;;;

;; Package sources. Prefer stable to unstable sources.

(setq package-archives '(("gnu"          . "https://elpa.gnu.org/packages/")
			 ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("melpa"        . "https://melpa.org/packages/"))
      package-archive-priorities '(("melpa-stable" . 2)
                                   ("gnu"          . 1)
                                   ("melpa"        . 0)))

;;; Support for packages (installation and use), utilizing use-package
;;; <https://github.com/jwiegley/use-package>
(eval-when-compile
  (require 'use-package))
(require 'bind-key)

;;; This will download and install packages mentions below (but won't upgrade existing
;;; ones). It's ok as I only run Emacs on handful of desktops, and don't change the packages
;;; used too often.
(require 'use-package-ensure)
(setq use-package-always-ensure t)


;;; I use use-package for built in packages too. This keeps the per-package configuration
;;; neatly organized.


;;; Always server-start on a graphical system.
(use-package server
  :if window-system
  :hook
  (after-init . (lambda ()
                  (unless (server-running-p)
                    (server-start)))))


;;; Limit hl-line (highlight the line at point) to text and programming mode, as it breaks
;;;  sone other interactive modes.
(use-package hl-line
  :hook
  ((prog-mode
    text-mode
    literate-calc-mode) . hl-line-mode))


(use-package flycheck
  :init
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  :config
  (global-flycheck-mode)
  ;; <https://github.com/flycheck/flycheck/issues/1523>
  (let ((govet (flycheck-checker-get 'go-vet 'command)))
    (when (equal (cadr govet) "tool")
      (setf (cdr govet) (cddr govet)))))


(use-package flycheck-popup-tip
  :hook
  (flycheck-mode . flycheck-popup-tip-mode))


;;; Magit in itself is a reason to use Emacs. It is the best Git interface available
;;; anywhere.
(use-package magit
  :bind
  ("C-x C-g" . magit-status))


;;; <https://github.com/dgutov/diff-hl>
(use-package diff-hl
  :after
  (magit)
  :config
  (global-diff-hl-mode)
  :hook
  ((magit-pre-refresh . diff-hl-magit-pre-refresh)
   (magit-post-refresh . diff-hl-magit-post-refresh)))


;;; <https://github.com/Olivia5k/makefile-executor.el>
(use-package makefile-executor
  :hook
  (makefile-mode . makefile-executor-mode))


;;; Before I found Vterm, I used to have an Emacs and a Terminal.app open side-by-side, as
;;; no terminal in Emacs was usable for anything more than a quick ls. Vterm is actually
;;; good (I still keep a Terminal.app window open though, because old habits die hard).
(use-package vterm
  :init
  (setq vterm-max-scrollback 100000)
  :bind
  ("<f12>" . vterm-other-window))


;;; Vertico and marginalia need :init, so they won't be lazy loaded, but available from the
;;; start.

;;; Use vertico to display and navigate completions in the minibuffer
(use-package vertico
  :init
  (vertico-mode))


;;; Enable rich annotations in minibuffer completions
(use-package marginalia
  :bind
  (:map minibuffer-local-map ("M-A" . marginalia-cycle))
  (:map completion-list-mode-map ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))


(use-package consult
  :bind
  (("C-x C-b" . consult-buffer-other-window)
   ("M-y"     . consult-yank-pop)
   ("M-g g"   . consult-goto-line)
   ("M-g M-g" . consult-goto-line)))


;;; <https://github.com/magnars/expand-region.el>
(use-package expand-region
  :bind
  (("C-c n" . er/expand-region)))


;;; A modeline replacement with sensible defaults.
;;; <https://git.tty.dog/jessieh/mood-line>
(use-package mood-line
  :config
  ;; This is the only default mood-line gets wrong: by default, it doesn't utilize the full
  ;; pallette of graphical symbols available in Unicode.
  (setq mood-line-glyph-alist mood-line-glyphs-unicode)
  (mood-line-mode))


;;; <https://github.com/justbur/emacs-which-key>
(use-package which-key
  :config
  (which-key-mode))


;;; Add colors and single line progress update to compilation-mode
;;; <https://codeberg.org/ideasman42/emacs-fancy-compilation>
(use-package fancy-compilation
  :config
  (fancy-compilation-mode 1))


;;; macOS compatability
(when on-mac-window-system
  ;;; I used to dislike dark UIs, but it is growing on me. This makes Emacs
  ;;; match the system mode, but only
  (when on-homebrew-emacs-plus
    (use-package auto-dark
      :after (prism)
      :hook
      ((audo-dark-dark-mode . prism-set-colors)
       (auto-dark-light-mode . prism-set-colors))
      :config
      (auto-dark-mode t)))

  ;;; Emacs Plus injects the PATH automatically, so no need for exec-path-from-shell there.
  (use-package exec-path-from-shell
    :if (not on-homebrew-emacs-plus)
    :init
    (setq exec-path-from-shell-check-startup-files nil)
    :config
    (exec-path-from-shell-initialize))

  ;;; For invoking <https://kapeli.com/dash>, the offline API documentation
  ;;; browser from Emacs.
  (use-package dash-at-point
    :bind
    ("C-?" . dash-at-point)))

;;; To build the package using quelpa:
;; (quelpa '(copilot :fetcher github
;;          :repo "copilot-emacs/copilot.el"
;;          :branch "main"
;;          :files ("dist" "*.el")))

(use-package copilot
  :init
  (setq copilot-indent-offset-warning-disable t)
  :bind
  (("<f9>"       . copilot-mode)
   ("C-<return>" . copilot-accept-completion)))


;;; Use tree-sitter when possible
(use-package treesit-auto
  :config
  (global-treesit-auto-mode))


;;; Most code highlighting is based on identifiers, which to me, doesn't provide much
;;; usefullness, but confusion, with every word in a different vibrant color. Prism
;;; highlights code based on depth, so blocks and scope are easy to identify.
;;; I often describe it as living in the year 3000 for highlighting.
;;; <https://github.com/alphapapa/prism.el>
(use-package prism
  :hook
  ((prog-mode . prism-mode)
   (python-ts-mode . prism-whitespace-mode)))


;;; Like Magit, Paredit is a reason enough to use Emacs. I imagine that a lot
;;; of the hate towards s-expression stems from people that never used
;;; Paredit. Programming in Lisps with and without it is a world of difference.
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


(use-package cider)


(use-package markdown-mode
  :commands
  (markdown-mode gfm-mode)
  :mode
  (("README\\.md\\'" . gfm-mode)
   ("\\.md\\'"       . markdown-mode)
   ("\\.markdown\\'" . markdown-mode))
  :init
  ;; Use pandoc to render markdown.
  (setq markdown-command
        "pandoc --quiet -f markdown_github -t html -s --mathjax --highlight-style=pygments"))


;;; The child of literate programming and Calc mode. Inline arithmetic in text buffers,
;;; super useful for writing reports, or just trying to do my
;;; budget. <https://github.com/sulami/literate-calc-mode.el>
(use-package literate-calc-mode)
