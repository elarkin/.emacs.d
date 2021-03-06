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
(global-undo-tree-mode -1)              ;evil mode turns on undo-tree. turning it off restores default emacs undo behavior
(show-paren-mode -1)                    ;show paren mode flashes matching parens; I prefer rainbow parens
(column-number-mode t)                  ;show column number in the mode line
(global-font-lock-mode t)
(server-mode 1)                         ;make emacs behave as a server. This allows you to use emacsclient
                                        ;to connect to your running emacs without incurring a startup cost
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
(setq ido-enable-flex-matching t)

;; Indentation Settings
(setq-default indent-tabs-mode nil) ; use spaces
(setq-default tab-width 2)
(setq js-indent-level 2)
(setq css-indent-offset 2)

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

;; Mark settings
(transient-mark-mode -1)                ;Don't hilight after marking. Mark twice to create an active (hilighted) region

;; Reindent-or-completion
;modes that use a different command for indenting can override this var
(setq reindent-or-completion-indent 'indent-for-tab-command)
;modes that use a different command for completion can override this var
(setq reindent-or-completion-completion 'complete-symbol)

(defun reindent-or-completion (arg)
  "Between the beginning of line and start of text, (indent-for-tab-command). Anywhere else (completion-at-point)"
  (interactive "P")
  (let ((position (point)) start-of-text)
    (save-excursion
      (back-to-indentation)
      (setq start-of-text (point)))
    (if (<= position start-of-text)
        (funcall reindent-or-completion-indent)
      (funcall reindent-or-completion-completion arg))))

;; Load mode-specific initialization files
(require 'cl)

(defun elisp-file? (filename)
  (string-match-p "^[a-zA-Z0-9\-]+.el$" filename))

(setq init-dir "~/.emacs.d/initializers")

(when (file-exists-p init-dir)
  (let* ((initializer-files (remove-if-not 'elisp-file? (directory-files init-dir)))
         (initializers (mapcar (lambda (filename) (expand-file-name filename init-dir)) initializer-files)))
    (dolist (initializer initializers)
      (load initializer))))
