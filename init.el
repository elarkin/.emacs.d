;; fix the PATH variable
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (shell-command-to-string "$SHELL -i -c 'echo $PATH'")))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(setenv "PAGER" "cat")

(when window-system (set-exec-path-from-shell-PATH))

;; use the marmalade repository
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;; install these packages if they are not there.
(defvar my-packages '(clojure-mode
                      clojure-test-mode
                      nrepl
                      paredit
                      rainbow-delimiters
                      rinari
                      evil
                      magit
                      color-theme
                      color-theme-solarized))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; OSX stuff
(setq mac-option-modifier 'super)
(setq mac-command-modifier 'meta)

;; graphical UI
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(color-theme-solarized-dark)
(set-fringe-mode -1)

;; clipboard
(setq x-select-enable-clipboard t)

;; Basic Settings
(setq inhibit-startup-message t)
(global-undo-tree-mode -1)
(show-paren-mode -1)
(column-number-mode t)
(global-font-lock-mode t)
(setq-default show-trailing-whitespace t)
(server-mode 1)

;; Display The Time In The Mode Line
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(display-time)

;; Colliding Buffer Names Should Have Their Directory Names in them
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Buffer Switching Settings
(setq nrepl-hide-special-buffers t)

;; Caret Settings
(blink-cursor-mode -1)
(setq-default cursor-type 'box)

;; Completion Settings
(ido-mode t)
(ido-everywhere t)

;; Indentation Settings
(setq-default indent-tabs-mode nil) ; use spaces
(setq-default tab-width 2)

;; reload buffers changed on disk without unsaved changes
(setq auto-revert-verbose nil)
(setq auto-revert-interval 3)
(setq global-auto-revert-non-file-buffers t)
(global-auto-revert-mode 1)

;; Backups should not clutter my file system
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Org-Mode settings
(setq org-agenda-files '("~/org"))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-completion-use-ido t)

;; Mark settings
(transient-mark-mode -1)

;; Hooks
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

;; Clojure Mode
(add-hook 'clojure-mode-hook 'nrepl-interaction-mode)
(add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)

;; Ruby Modes
(defun ruby-minor-modes ()
  (progn
    (rinari-minor-mode t)
    (rainbow-delimiters-mode t)))

(add-hook 'ruby-mode-hook 'ruby-minor-modes)

;; Keybindings
;; Paredit
(require 'paredit)
(define-key paredit-mode-map "\M-[" 'paredit-wrap-square)
(define-key paredit-mode-map "\M-{" 'paredit-wrap-curly)

;; EmacsClient should be quit with C-c C-c instead of C-x #
(add-hook 'server-switch-hook
          (lambda ()
            (when (current-local-map)
              (use-local-map (copy-keymap (current-local-map))))
            (when server-buffer-clients
              (local-set-key (kbd "C-c C-c") 'server-edit))))

;; Evil mode needs undo tree to fully emulate undo-redo
(add-hook 'evil-mode-hook
          (lambda ()
            (undo-tree-mode t)))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files (quote ("/Users/evan.larkin/org/notes.org"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
