;;;
;;; my-functions.el --- Useful stuff that isn't just customizations.
;;;


(defconst on-mac-window-system (memq window-system '(mac ns))
  "Non-nil if running on a macOS desktop.")

(defconst on-homebrew-emacs-plus
  (cl-search "emacs-plus" system-configuration-options)
  "Non-nil if this Emacs originated in the Emacs is from the Emacs Plus package
 (https://github.com/d12frosted/homebrew-emacs-plus)")

(defun indent-whole-buffer ()
  "Indent the whole buffer."
  (interactive)
  (indent-region (point-min) (point-max) nil))


(defun rename-file-and-buffer (new-name)
  "Renames both the current buffer and the file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))
