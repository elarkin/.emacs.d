;; Ruby Modes
(defun ruby-minor-modes ()
  (progn
    (rinari-minor-mode t)
    (rainbow-delimiters-mode t)))

(add-hook 'ruby-mode-hook 'ruby-minor-modes)
