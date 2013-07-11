;; Ruby Modes
(defun ruby-minor-modes ()
  (progn
    (rinari-minor-mode t)
    (rainbow-delimiters-mode t)
    (make-local-variable 'reindent-or-completion-completion)
    (setq reindent-or-completion-completion 'dabbrev-expand)
    (local-set-key (kbd "<tab>") 'reindent-or-completion)
    (local-set-key (kbd "<C-tab>") 'indent-for-tab-command)))

(add-hook 'ruby-mode-hook 'ruby-minor-modes)

;; ruby mode should apply to all ruby files
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rabl$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.irbrc$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile$" . ruby-mode))
