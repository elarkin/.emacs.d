;; Smart Mode
(require 'eshell)
(require 'em-smart)
(setq eshell-where-to-jump 'begin)
(setq eshell-review-quick-commands t)
(setq eshell-smart-space-goes-to-end t)

;; Eshell Hooks
(defun eshell-settings ()
  (progn
    (setq show-trailing-whitespace nil)
    (eshell-smart-initialize)))

(add-hook 'eshell-mode-hook 'eshell-settings)

;; Eshell Commands
(defun eshell/clear ()
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))
