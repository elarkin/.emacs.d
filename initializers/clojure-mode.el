;; Clojure Mode
(add-hook 'clojure-mode-hook 'nrepl-interaction-mode)
(add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)
