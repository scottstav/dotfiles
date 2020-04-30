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

(desktop-save-mode 1)
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

(customize-set-variable 'org-journal-dir (concat org-directory ""))
(customize-set-variable 'org-journal-file-type 'weekly)
(customize-set-variable 'org-journal-file-format "journal-%Y%m%d.org")
(dolist (package '(org-journal))
  (unless (package-installed-p package)
    (package-install package))
  (require package))
;;------------------language config------------------------------

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

(when (string= system-type "darwin") ; avoid warn when opening dir on macOS
  (setq dired-use-ls-dired nil))

(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files
   (quote
    ("~/Dropbox/org/General.org" "~/Dropbox/org/movement.org" "~/Dropbox/org/shopping.org" "/Users/scottstav/Dropbox/org/dotfiles.org" "/Users/scottstav/Dropbox/org/journal-20200413.org" "/Users/scottstav/Dropbox/org/poetry.org" "/Users/scottstav/Dropbox/org/posts.org" "/Users/scottstav/Dropbox/org/projects.org" "/Users/scottstav/Dropbox/org/sensunDnD.org")))
 '(package-selected-packages
   (quote
    (org-journal magit git js2-mode flymake-ruby robe inf-ruby flycheck web-mode json-mode groovy-mode gradle-mode use-package markdown-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
