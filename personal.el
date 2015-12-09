(require 'package)
(add-to-list 'package-archives
 '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
 '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; first window settings
(setq initial-frame-alist
      '((menu-bar-lines . 0)
        (top . 0)
        (left-fringe 0)
        (right-fringe 0)
        (toolbar-lines . 0)
        (width . 80)
        (height . 54)
        (background-color . "black")
        (foreground-color . "white")
        (cursor-color . "red")))

;; default window settings
(setq default-frame-alist
      '((menu-bar-lines . 1)
        (left-fringe 0)
        (right-fringe 0)
        (tool-bar-lines . 0)
        (width . 80)
        (height . 54)
        (background-color . "black")
        (foreground-color . "white")
        (cursor-color . "red")))

(require 'flymake-ruby)
(set-face-background 'flymake-errline "red5")
(set-face-background 'flymake-warnline "dark slate blue")
(defun flymake-ruby-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (flymake-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list "ruby" (list "-c" local-file))))

(push '(".+\\.rb$" flymake-ruby-init) flymake-allowed-file-name-masks)
(push '("Rakefile$" flymake-ruby-init) flymake-allowed-file-name-masks)

(push '("^\\(.*\\):\\([0-9]+\\): \\(.*\\)$" 1 2 nil 3) flymake-err-line-patterns)
(add-hook 'ruby-mode-hook 'flymake-ruby-load)
(autoload 'ruby-mode "ruby-mode" "Major mode for editing ruby scripts." t)
(setq auto-mode-alist  (cons '(".rb$" . ruby-mode) auto-mode-alist))
(global-set-key (kbd "C-c r r") 'inf-ruby)

(require 'rvm)
(rvm-use-default)

(setq standard-indent 2)
(setq scroll-step 1)
(mouse-wheel-mode t)
(line-number-mode 1)
(column-number-mode 1)
(global-linum-mode)
(set-default-font "Monospace-10")
(set-cursor-color "red")
(set-mouse-color "goldenrod")
(set-face-background 'region "blue")
(global-set-key (kbd "C-x C-b") 'ibuffer)
(autoload 'ibuffer "ibuffer" "List buffers." t)
(setq inhibit-startup-message nil)
(scroll-bar-mode -1)

(require 'whitespace)
(setq whitespace-style '(face empty tabs lines-tail trailing))
(global-whitespace-mode t)
(setq delete-trailing-lines nil)
(setq require-final-newline t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; highlight blank space, tabs and lines over 80 chars.
(add-hook 'font-lock-mode-hook
          (function
           (lambda ()
             (setq font-lock-keywords
                   (append font-lock-keywords
                           '(("\t+"          (0 'my-tab-face t))
                             ("^.\\{81,\\}$" (0 'my-long-line-face t))
                             ("[ \t]+$"      (0 'my-trailing-space-face t)))))))
)

;;save desktop between sessions
(require 'desktop)
(desktop-save-mode 1)

;;ibuffer
(eval-after-load "ibuf-ext"
  '(define-ibuffer-filter filename
     "toggle current view to buffers with file or directory name matching QUALIFIER."
     (:description "filename"
      :reader (read-from-minibuffer "filter by file/directory name (regexp): "))
     (ibuffer-awhen (or (buffer-local-value 'buffer-file-name buf)
                        (buffer-local-value 'dired-directory buf))
                    (string-match qualifier it))))
(setq ibuffer-saved-filter-groups
     (quote (("default"
              ("dired" (mode . dired-mode))
	      ("ruby"  (mode . ruby-mode))
              ("emacs" (or
                        (name . "^\\*scratch\\*$")
                        (name . "^\\*Messages\\*$")
                        (name . "^\\*personal\\*$"))
              ("pegasus" (name . "^\\*pegasus\\*$"))
              ("gemini" (name . "^\\*gemini*\\$"))
              ("marty_demo" (name . "^\\*marty_demo*\\$"))))))
)
(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))

;This function can be used to split the current window into N windows, you can
;type "C-u 3 M-x wenshan-split-window-vertical" to achieve what you want.
(defun wenshan-split-window-vertical (&optional wenshan-number)
"Split the current window into `wenshan-number' windows"
  (interactive "P")
  (setq wenshan-number (if wenshan-number
                           (prefix-numeric-value wenshan-number)
                         2))
  (while (> wenshan-number 1)
    (split-window-right)
    (setq wenshan-number (- wenshan-number 1)))
  (balance-windows))

(global-set-key (kbd "C-c 3") 'wenshan-split-window-vertical)

(require 'ido)
(ido-mode t)

(require 'direx)
(global-set-key (kbd "C-x C-j") 'direx:jump-to-directory)

;; save cursor location  for each file each time visited
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file "~/.emacs.d/saved-places")
(setq save-place-forget-unreadable-files nil)

;; backup files into system temp directory instead of working dir
(setq backup-directory-alist
      '((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      '((".*" ,temporary-file-directory t )))

;; remove temp files more than a week old
(message "Deleting old backup files...")
(let ((week (* 60 60 24 7))
      (current (float-time (current-time))))
  (dolist (file (directory-files temporary-file-directory t))
    (when (and (backup-file-name-p file)
               (> (- current (float-time (fifth (file-attributes file))))
                  week))
      (message "%s" file)
      (delete-file file))))
