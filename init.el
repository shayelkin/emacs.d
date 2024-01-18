;;;
;;; init.el --- For Emacs 27+
;;;


;;; Require Emacs from this decade, don't bother with backwards compatibility.
(when (version< emacs-version "27")
  (error "Time to upgrade this Emacs installation!"))


;;; Useful stuff that isn't necessarily customizations.
(load
 (expand-file-name "my-functions" user-emacs-directory))


;;; Local packages.
(add-to-list 'load-path (expand-file-name "~/src/emacs-lisp"))


;;;
;;; - FIX DEFAULT CUSTOMIZATIONS
;;;


;;; Workaround for "Invalid image type 'svg'"
;;; <https://debbugs.gnu.org/cgi/bugreport.cgi?bug=59081>
(when (and on-mac-window-system (version< emacs-version "29"))
  (defun image-type-available-p (type)
    "Return t if image type TYPE is available.
Image types are symbols like `xbm' or `jpeg'."
    (if (eq 'svg type)
        nil
      (and (fboundp 'init-image-library)
           (init-image-library type)))))


;;; That Blink-Cursor is enabled by default should be considered a crime.
(blink-cursor-mode 0)

;;; Highlight matching parenthesis.
(show-paren-mode t)

;;; Show line numbers in buffer, column number in the modeline.
(global-display-line-numbers-mode t)
(column-number-mode               t)

;;; Never expect me to type a full word when a single letter will do.
;;; TO-DO: Emacs 28 introduces use-short-answers which can be used once this file
;;;        drops support for Emacs 27.
(defalias 'yes-or-no-p 'y-or-n-p)

;;; Shift+Arrows switch the active window.
(windmove-default-keybindings)

;;; Automatically break lines in text modes.
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;;; Disable highlighting in shell buffers (both slow and annoying).
(set-variable 'shell-font-lock-keywords nil)

;;; Most of those are self-explanatory:
(setq-default
 fill-column      99
 indent-tabs-mode nil)

(setq
 delete-by-moving-to-trash             t
 fast-but-imprecise-scrolling          t
 frame-title-format                    "%b"
 inhibit-startup-message               t
 mac-option-modifier                   'meta
 read-file-name-completion-ignore-case t
 ;; Chunk size from reading from a subprocess. Set to 1Mb.
 read-process-output-max               (* 1000 1000)
 show-trailing-whitespace              t
 use-dialog-box                        nil)

(set-charset-priority 'unicode)


;;; Make URLs underlines and click-able.
(global-goto-address-mode 1)

;;; (so-long-commentary) describes this mode.
(global-so-long-mode)


;;;
;;; - HOOKS
;;;

;;; after-make-frame-function is set in early-init.el, to so it'll take effect for the initial
;;; frame as well.

;;; Automatically insert closing parenthesis.
(add-hook 'prog-mode-hook
          (lambda ()
            (electric-pair-mode 1)))


;;; Delete trailing whitespace before saving.
(add-hook 'before-save-hook
          (lambda ()
            (delete-trailing-whitespace)))


;;;
;;; - KEY BINDINGS
;;;


;;; The most important binding is set outside of Emacs: remapping Caps Lock to Control.

;;; As I'm using use-package, I prefer where possible to set keys in a use-package deceleration, so
;;; they could be listed by (describe-personal-keybindings).

;;; TO-DO: Build equivalent to (describe-personal-keybindings) to keys bound without the use of
;;;        use-package.

;;; TO-DO: In Emacs 29, global-set-key and global-unset-key were deprecated in favor of
;;;        keymap-global-set and keymap-global-unset. Refactor to use them once I drop support for
;;;        Emacs 28.


(global-set-key (kbd "M-<return>") 'fill-paragraph)


;;; Probably the only CUA bindings I use.
;;; As a bonus, C-z overrides the default binding of suspend-frame, which is useless imo.
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-.") 'completion-at-point)


;;; I never delete a single character: C-w, and retyping the word is faster than navigating to a
;;; specific character in it.
(global-set-key (kbd "C-w") 'backward-kill-word)


;;; Move kill-region (cut) to C-c C-k.
(global-set-key (kbd "C-c C-k") 'kill-region)


;;; C-x m to open links in web browsers.
(global-set-key (kbd "C-x m") 'browse-url-at-point)


;;; Join this line and the next, regardless on where the point is in the line.
(global-set-key (kbd "M-j") (lambda () (interactive) (join-line -1)))


;;; Change the font scale: by default, this is bound to C-wheel, which cause many accidental font
;;; size changes. Instead, bind it to C-S with the +/- keys, or C-S-0 to reset it (the latter is
;;; bound by default to paredit-forward-slurp-sexp, but that command is also bound to C-<right>,
;;; which is what I use).
(global-unset-key (kbd "<C-wheel-up>"))
(global-unset-key (kbd "<C-wheel-down>"))
(global-set-key   (kbd "C-+") 'text-scale-increase)
(global-set-key   (kbd "C-_") 'text-scale-decrease)
(global-set-key   (kbd "C-)") (lambda () (interactive) (text-scale-increase 0)))


;;; Make M-S-. undo M-., going back to to where M-. was invoked from.
(global-set-key (kbd "M->") 'pop-tag-mark)


;;; Command key shortcuts for macOS (Emacs maps command to super).
(when on-mac-window-system
  ;; Maximize the frame (or restore to pre-maximized value)
  (global-set-key   (kbd "s-<return>") 'toggle-frame-maximized)
  ;;  Disable command-t, command-q, command-w, and rebind the latter to copy (M-w).
  (global-unset-key (kbd "s-t"))
  (global-unset-key (kbd "s-w"))
  (global-set-key   (kbd "s-w") 'kill-ring-save)
  (global-unset-key (kbd "s-q")))


;;; I would like to use indent-whole-buffer as a save-hook, but can't because too many files are
;;; not correctly indented throughout.
(add-hook 'prog-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-i") 'indent-whole-buffer)))


;;;
;;; - PACKAGE MANAGEMENT:
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

;;; This will download and install packages mentions below (but won't upgrade existing ones). It's
;;; ok as I only run Emacs on handful of desktops, and don't change the packages used too often.
(require 'use-package-ensure)
(setq use-package-always-ensure t)


;;;
;;; - BUILT IN PACKAGES:
;;;

;;; I use use-package for built in packages too. This keeps the per-package configuration neatly
;;; organized.


;;; Always server-start on a graphical system.
(use-package server
  :if window-system
  :hook
  (after-init . (lambda ()
                  (unless (server-running-p)
                    (server-start)))))


;;; Limit hl-line (highlight the line at point) to text and programming mode, as it sometimes break
;;; the more interactive modes.
(use-package hl-line
  :hook
  ((prog-mode
    text-mode
    literate-calc-mode) . hl-line-mode))


;;; At work, I started using GoLand so I'd have a similar environment to my colleagues, and I don't
;;; get to program in Rust as much as I would like to. As such, I don't use LSP much.
(use-package eglot
  :hook
  ((rust-mode
    go-mode)  . eglot-ensure))


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


;;;
;;; - SOURCE CONTROL AND BUILD SUPPORT:
;;;


;;; Magit in itself is a reason to use Emacs. It is the best Git interface available anywhere.
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
  :config
  (add-hook 'makefile-mode-hook 'makefile-executor-mode))


;;; Before I found Vterm, I used to have an Emacs and a Terminal.app open
;;; side-by-side, as no terminal in Emacs was usable for anything more than a
;;; quick ls. Vterm is actually good (I still keep a Terminal.app window open
;;; though, because old habits die hard).
(use-package vterm
  :init
  (setq vterm-max-scrollback 100000)
  :bind
  ("<f12>" . vterm-other-window))


;;;
;;; - IN-BUFFER NAVIGATION:
;;;

;;; I've tried a few incremental completion packages, and I currently like Helm (and Swoop, for
;;; searching in buffers) the most.
(use-package helm
  :config
  (helm-mode 1))

(use-package helm-swoop
  :after
  (helm)
  :bind
  (("C-s"        . helm-swoop)
   ("C-x i"      . helm-multi-swoop-all)
   ("M-x"        . helm-M-x)
   ("C-c C-c"    . helm-M-x)
   :map helm-swoop-map
   ("M-i"        . helm-multi-swoop-all-from-helm-swoop)))

;;; <https://github.com/magnars/expand-region.el>
(use-package expand-region
  :bind
  (("C-c n" . er/expand-region)))


;;;
;;; - VISUAL ENHANCEMENTS:
;;;


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



;;;
;;; - MACOS COMPATIBILITY:
;;;

;;; <https://github.com/purcell/exec-path-from-shell>
(use-package exec-path-from-shell
  :if on-mac-window-system
  :init
  (setq exec-path-from-shell-check-startup-files nil)
  :config
  (exec-path-from-shell-initialize))


;;; For invoking <https://kapeli.com/dash>, the offline API documentation
;;; browser from Emacs.
(use-package dash-at-point
  :if on-mac-window-system
  :bind
  ("C-?" . dash-at-point))


;;; I used to dislike dark UIs, but it is growing on me. This makes Emacs
;;; match the system mode.
(use-package auto-dark
  :init
  ;; Need to enable osascript, as the default Emacs macOS builds do not define
  ;; ns-do-applescript.
  (setq auto-dark-allow-osascript t
        auto-dark-detection-method 'osascript)
  :config
  (auto-dark-mode t))


;;;
;;; COPILOT:
;;;

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


;;;
;;; LANGUAGE SUPPORT:
;;;


;;; Wucuo is a faster alternative to flyspell <https://github.com/redguardtoo/wucuo>
(use-package wucuo  
  :hook
  ((text-mode . wucuo-start)
   (prog-mode . wucuo-start)))


;;; Most code highlighting is based on identifiers, which to me, is redundant
;;; and confusing, with every word in a different vibrant color. Prism
;;; highlights code based on depth, so blocks and scope are easy to
;;; identify. It is living in the year 3000 for code
;;; highlighting. <https://github.com/alphapapa/prism.el>
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
(use-package dockerfile-mode)
(use-package protobuf-mode)
(use-package rust-mode)
(use-package terraform-mode)
(use-package yaml-mode)


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
        "pandoc --quiet -f markdown -t html -s --mathjax --highlight-style=pygments"))


(use-package go-mode
  :hook
  (go-mode     . lsp-deferred)
  (before-save . lsp-format-buffer)
  (before-save . lsp-organize-imports))


;;; The child of literate programming and Calc mode. Inline arithmetic in text
;;; buffers, super useful for writing reports, or just trying to figure out my
;;; own personal budget. <https://github.com/sulami/literate-calc-mode.el>
(use-package literate-calc-mode)



;;;
;;; 🐷: "Th-Th-The, Th-Th-The, Th-Th... That's all, folks!"
;;;
