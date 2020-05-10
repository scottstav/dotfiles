;; Add MELPA
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/lisp/")

;; dont remember what this was for...
(eval-after-load 'gnutls
  '(add-to-list 'gnutls-trustfiles "/etc/ssl/cert.pem"))

;; Buffers
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Directory
(defun file-info ()
  (interactive)
  (let ((dired-listing-switches "-alh"))
    (dired-other-window buffer-file-name)))

(global-set-key (kbd "C-x C-b") 'ibuffer)

(dolist (package '(desktop+ google-this))
  (unless (package-installed-p package)
    (package-install package))
  (require package))

(google-this-mode 1)

(when (fboundp 'winner-mode)
      (winner-mode 1))
;; setup 'use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(require 'use-package)

(global-set-key "\C-cy"
		'(lambda ()
		   (interactive)
		   (popup-menu 'yank-menu)))



;; org mode
(require 'org)

(setq org-directory "~/Dropbox/org")
(setq org-default-notes-file (concat org-directory "/General.org"))
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(setq org-agenda-files (directory-files-recursively (concat org-directory "") "\\.org$"))
(setq org-log-done t)

(add-hook 'org-mode-hook '(lambda () (setq fill-column 80)))
(add-hook 'org-mode-hook 'turn-on-auto-fill)

(defun org-capture-journal-location ()
    "go to journal.org and find the subtree by date"
    (interactive "P")
    (let* ((heading (format-time-string "%Y-%m-%d %A")))
      (find-file (concat org-directory "/journal.org"))
      (goto-char 0)
      (unless (search-forward (format "* %s" heading) nil t)
        (insert (format "* %s\n" heading))
        (goto-line -1))))

(setq org-capture-templates
  '(
    ("j" "Journal Entry"
     entry (function org-capture-journal-location))
    )
  )
;;------------------language config------------------------------

(show-paren-mode 1)

;; install language config packages
(dolist (package '(yaml-mode flymake-ruby inf-ruby company robe web-mode magit))
  (unless (package-installed-p package)
    (package-install package))
  (require package))

(defun web-mode-init-hook ()
  "Hooks for web-mode"
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-markup-indent-offset 2))

(add-hook 'web-mode-hook  'web-mode-init-hook)

(setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))
(add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css?$" . web-mode))

;; json/yaml
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-hook 'json-mode-hook
	  (lambda ()
	    (make-local-variable 'js-indent-level)
	    (setq js-indent-level 2)))

;; ruby
(add-hook 'ruby-mode-hook 'robe-mode)
(add-hook 'ruby-mode-hook 'flymake-ruby-load)

;; markdown
;; may need to:
;; * brew install markdown
;; * brew install pandoc
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))
(setq markdown-command "pandoc")

;; auto-complete
(global-company-mode)
(add-hook 'after-init-hook 'global-company-mode)
(eval-after-load 'company
  '(push 'company-robe company-backends))

;;---------------------------------------------------------------

(defun connect-remote ()
  (interactive)
  (dired "/ec2-user@minecraft.scotty.dance:~"))

;; Shell
  (defun with-face (str &rest face-plist)
    (propertize str 'face face-plist))

  (defun shk-eshell-prompt ()
    (let ((header-bg "#cae682"))
      (concat
       (with-face (concat (eshell/pwd) " ") :background header-bg :foreground "#e5786d")
       (with-face (format-time-string "(%Y-%m-%d %H:%M) " (current-time)) :background header-bg :foreground "#888")
       (with-face
        (or (ignore-errors (format "(%s)" (vc-responsible-backend default-directory))) "")
        :background header-bg)
       (with-face "\n" :background header-bg)
       (with-face user-login-name :foreground "#cae682")
       (if (= (user-uid) 0)
           (with-face " #" :foreground "red")
         " $")
       " ")))
  (setq eshell-prompt-function 'shk-eshell-prompt)
  (setq eshell-highlight-prompt nil)


;;------------------save config------------------------------

;; save backups in .emacs.d/backups
(setq backup-directory-alist
      `(("." . ,(expand-file-name
		 (concat user-emacs-directory "backups/")))))
;; save auto-saves in .emacs.d/auto-save
;; this is not working
(setq auto-save-file-name-transforms
      `((".*" ,(concat user-emacs-directory "auto-save/") t)))

(setq backup-by-copying t)

(setq version-control t     ;; Use version numbers for backups.
      kept-new-versions 10  ;; Number of newest versions to keep.
      kept-old-versions 0   ;; Number of oldest versions to keep.
      delete-old-versions t ;; Don't ask to delete excess backup versions.
      backup-by-copying t)  ;; Copy all files, don't rename them.

;; remove trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;---------------------------------------------------------------

(ido-mode 1) ; file search magic

;; stupid bell
(setq ring-bell-function 'ignore)

(add-to-list 'default-frame-alist '(font . "Menlo-18" ))

(when (string= system-type "darwin") ; avoid warn when opening dir on macOS
  (setq dired-use-ls-dired nil))

(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(custom-enabled-themes (quote (wombat)))
 '(org-agenda-files
   (quote
    ("~/Dropbox/org/General.org" "~/Dropbox/org/movement.org" "~/Dropbox/org/shopping.org" "/Users/scottstav/Dropbox/org/dotfiles.org" "/Users/scottstav/Dropbox/org/poetry.org" "/Users/scottstav/Dropbox/org/posts.org" "/Users/scottstav/Dropbox/org/projects.org" "/Users/scottstav/Dropbox/org/sensunDnD.org")))
 '(package-selected-packages
   (quote
    (google-this desktop+ magit git js2-mode flymake-ruby robe inf-ruby flycheck web-mode json-mode groovy-mode gradle-mode use-package markdown-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
