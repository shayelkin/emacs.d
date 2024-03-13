;;;
;;; init.el --- For Emacs 29.1+
;;;


;;; Require Emacs 29.1, don't bother with backwards compatibility.
(when (version< emacs-version "29.1")
  (error "Time to upgrade this Emacs installation!"))

;;; We can't do this in early-init.el, as we need a frame for this
(split-window-right)

;;; Local packages
(add-to-list 'load-path (expand-file-name "~/src/emacs-lisp"))


(dolist (f
         '("my-functions" "my-fix-defaults" "my-hooks" "my-key-bindings" "my-packages"))
  (load (expand-file-name f user-emacs-directory)))


;;; 🐷 "Th-Th-The, Th-Th-The, Th-Th... That's all, folks!"
