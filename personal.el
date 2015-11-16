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
(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(require 'whitespace)
(setq whitespace-style '(face empty tabs lines-tail trailing))
(global-whitespace-mode t)

;; highlight blank space, tabs and lilnes over 80 chars.
(custom-set-faces
 '(my-tab-face            ((((class color)) (:background "red"))) t)
 '(my-trailing-space-face ((((class color)) (:background "red"))) t)
 '(my-long-line-face      ((((class color)) (:background "red"))) t))

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
(setq history-length 250)
(add-to-list 'desktop-globals-to-save 'file-name-history)
(setq desktop-buffers-not-to-save
      (concat "\\("
              "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS"
              "\\|\\.emacs.*\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb"
              "\\)$"))
(add-to-list 'desktop-modes-not-to-save 'dired-mode)
(add-to-list 'desktop-modes-not-to-save 'Info-mode)
(add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
(add-to-list 'desktop-modes-not-to-save 'fundamental-mode)

(desktop-save-mode 1)
(defun my-desktop-save ()
  (interactive)
  ;; Don't call desktop-save-in-desktop-dir, as it prints a message.
  (if (eq (desktop-owner) (emacs-pid))
      (desktop-save desktop-dirname)))
(add-hook 'auto-save-hook 'my-desktop-save)

;;; desktop-override-stal-locks.el begins here.
(defun emacs-process-p (pid)
  "If pid is the process ID of an emacs process, return it, else nil."
  (when pid
    (let* ((cmdline-file (concat "/proc/" (int-to-string pid) "/cmdline")))
      (when (file-exists-p cmdline-file)
        (with-temp-buffer
          (insert-file-contents-literally cmdline-file)
          (goto-char (point-min))
          (search-forward "emacs" nil t)
          pid)))))
(defadvice desktop-owner (after pry-from-cold-dead-hands activate)
 "Don't allow dead emacsen to own the desktop file."
 (when (not (emacs-process-p ad-return-value))
   (setq ad-return-value nil)))
;;; desktop-override-stale-locks.el ends here

;;ibuffer
(setq ibuffer-saved-filter-groups
     (quote (("default"
              ("dired" (mode . dired-mode))
              ("pegasus" (name . "^\\*pegasus\\*$"))
              ("gemini" (name . "^\\*gemini*\\$"))
              ("marty_demo" (name . "^\\*marty_demo*\\$"))))))

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
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq require-final-newline t)

(require 'ido)
(ido-mode t)
