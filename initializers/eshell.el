;; Eshell Hooks
(defun eshell-settings ()
  (progn
    (setq show-trailing-whitespace nil)))

(add-hook 'eshell-mode-hook 'eshell-settings)
