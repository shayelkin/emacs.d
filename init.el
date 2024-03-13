;;;
;;; init.el --- For Emacs 29.1+
;;;


;;; Require Emacs from this decade, don't bother with backwards compatibility.
(when (version< emacs-version "29.1")
  (error "Time to upgrade this Emacs installation!"))


;;; Useful stuff that isn't necessarily customizations.
(load
 (expand-file-name "my-functions" user-emacs-directory))


;;; Local packages.
(add-to-list 'load-path (expand-file-name "~/src/emacs-lisp"))


;;;
;;; - FIX DEFAULTS
;;;


;;; That Blink-Cursor is enabled by default should be considered a crime.
(blink-cursor-mode 0)

;;; Highlight matching parenthesis.
(show-paren-mode t)

;;; Show line numbers in buffer, column number in the modeline.
(global-display-line-numbers-mode t)
(column-number-mode               t)

;;; Shift+Arrows switch the active window.
(windmove-default-keybindings)

;;; Disable highlighting in shell buffers (both slow and annoying).
(set-variable 'shell-font-lock-keywords nil)

;;; Most of those are self-explanatory:
(setq-default
 fill-column      92
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
 use-dialog-box                        nil
 ;; use a single letter instead of a full "yes" or "no"
 use-short-answers                     t)


;;; Make URLs underlined and click-able.
(global-goto-address-mode 1)

;;; Performance mitigration for long lines (see 'M-x so-long-commentary').
(global-so-long-mode)

(set-charset-priority 'unicode)

;;; Use Noto Symbols as fallback for those less-common pictographs
(set-fontset-font t nil (font-spec :family "Noto Sans Symbols 2") nil :append)


;;;
;;; - HOOKS
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

;;;
;;; - KEY BINDINGS
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

;;; This will download and install packages mentions below (but won't upgrade existing
;;; ones). It's ok as I only run Emacs on handful of desktops, and don't change the packages
;;; used too often.
(require 'use-package-ensure)
(setq use-package-always-ensure t)


;;;
;;; - BUILT IN PACKAGES:
;;;

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


;;;
;;; - NAVIGATION:
;;;

;; Most of those packages need :init rather than config, as they should be enable from the
;; start, and not lazy loaded.


;; Use vertico to display and navigate completions in the minibuffer
(use-package vertico
  :init
  (vertico-mode))


;; Enable rich annotations in minibuffer completions
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
;;; super useful for writing reports, or just trying to do my budget. <https://github.com/sulami/literate-calc-mode.el>
(use-package literate-calc-mode)


;;;
;;; 🐷: "Th-Th-The, Th-Th-The, Th-Th... That's all, folks!"
;;;
