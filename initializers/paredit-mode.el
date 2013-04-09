;; Paredit
(require 'paredit)
(define-key paredit-mode-map "\M-[" 'paredit-wrap-square)
(define-key paredit-mode-map "\M-{" 'paredit-wrap-curly)
