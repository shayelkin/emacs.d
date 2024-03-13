;;;
;;; fix-defaults.el --- Change Emacs default settings that seem wrong (to me)
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
