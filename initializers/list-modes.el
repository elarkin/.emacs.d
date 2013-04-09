;; Lisp Modes
(defun lisp-minor-modes ()
  (progn
    (paredit-mode t)
    (rainbow-delimiters-mode t)))

(add-hook 'emacs-lisp-mode-hook 'lisp-minor-modes)
(add-hook 'lisp-mode-hook 'lisp-minor-modes)
(add-hook 'lisp-interaction-mode-hook 'lisp-minor-modes)
(add-hook 'clojure-mode-hook 'lisp-minor-modes)
(add-hook 'nrepl-mode-hook 'lisp-minor-modes)
