;; fix the PATH variable
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (shell-command-to-string "$SHELL -i -c 'echo $PATH'")))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

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

;; clipboard
(setq x-select-enable-clipboard t)

;; Basic Settings
(setq inhibit-startup-message t)
(show-paren-mode -1)
(column-number-mode t)
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(display-time)
(color-theme-solarized-dark)
(global-font-lock-mode t)
(setq-default show-trailing-whitespace t)

;; Caret Settings
(blink-cursor-mode -1)
(setq-default cursor-type 'box)

;; Completion Settings
(ido-mode t)
(ido-everywhere t)

;; Indentation Settings
(setq-default indent-tabs-mode nil) ; use spaces

;; reload buffers changed on disk without unsaved changes
(setq auto-revert-verbose nil)
(setq auto-revert-interval 3)
(global-auto-revert-mode 1)

;; Mark settings
(transient-mark-mode -1)

;; Hooks
(defun lisp-minor-modes ()
  (progn
    (paredit-mode t)
    (rainbow-delimiters-mode t)))

(add-hook 'emacs-lisp-mode-hook 'lisp-minor-modes)
(add-hook 'lisp-mode-hook 'lisp-minor-modes)
(add-hook 'lisp-interaction-mode-hook 'lisp-minor-modes)
(add-hook 'clojure-mode-hook 'lisp-minor-modes)
(add-hook 'nrepl-mode-hook 'lisp-minor-modes)

;; Keybindings
;; Paredit
(require 'paredit)
(define-key paredit-mode-map "\M-[" 'paredit-wrap-square)
(define-key paredit-mode-map "\M-{" 'paredit-wrap-curly)


