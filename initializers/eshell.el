;; Eshell Hooks
(defun eshell-settings ()
  (progn
    (setq show-trailing-whitespace nil)))

(add-hook 'eshell-mode-hook 'eshell-settings)

;; Eshell Commands
(defun eshell/clear ()
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))
