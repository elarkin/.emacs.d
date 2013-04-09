;; Evil mode needs undo tree to fully emulate undo-redo
(add-hook 'evil-mode-hook
          (lambda ()
            (undo-tree-mode t)))
