;; Add MELPA
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu/mu4e")
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/lisp/")

;; MAIL
(require 'mu4e)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))
(setq mu4e-mu-binary (executable-find "mu"))
;; enable inline images
(setq mu4e-view-show-images t)
;; use imagemagick, if available
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))
;; END MAIL

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

;; (use-package treemacs
;;   :ensure t
;;   :defer t
;;   :init
;;   (with-eval-after-load 'winum
;;     (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
;;   :config
;;   (progn
;;     (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
;;           treemacs-deferred-git-apply-delay      0.5
;;           treemacs-directory-name-transformer    #'identity
;;           treemacs-display-in-side-window        t
;;           treemacs-eldoc-display                 t
;;           treemacs-file-event-delay              5000
;;           treemacs-file-extension-regex          treemacs-last-period-regex-value
;;           treemacs-file-follow-delay             0.2
;;           treemacs-file-name-transformer         #'identity
;;           treemacs-follow-after-init             t
;;           treemacs-git-command-pipe              ""
;;           treemacs-goto-tag-strategy             'refetch-index
;;           treemacs-indentation                   2
;;           treemacs-indentation-string            " "
;;           treemacs-is-never-other-window         nil
;;           treemacs-max-git-entries               5000
;;           treemacs-missing-project-action        'ask
;;           treemacs-move-forward-on-expand        nil
;;           treemacs-no-png-images                 nil
;;           treemacs-no-delete-other-windows       t
;;           treemacs-project-follow-cleanup        nil
;;           treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
;;           treemacs-position                      'left
;;           treemacs-recenter-distance             0.1
;;           treemacs-recenter-after-file-follow    nil
;;           treemacs-recenter-after-tag-follow     nil
;;           treemacs-recenter-after-project-jump   'always
;;           treemacs-recenter-after-project-expand 'on-distance
;;           treemacs-show-cursor                   nil
;;           treemacs-show-hidden-files             t
;;           treemacs-silent-filewatch              nil
;;           treemacs-silent-refresh                nil
;;           treemacs-sorting                       'alphabetic-asc
;;           treemacs-space-between-root-nodes      t
;;           treemacs-tag-follow-cleanup            t
;;           treemacs-tag-follow-delay              1.5
;;           treemacs-user-mode-line-format         nil
;;           treemacs-user-header-line-format       nil
;;           treemacs-width                         35)

;;     ;; The default width and height of the icons is 22 pixels. If you are
;;     ;; using a Hi-DPI display, uncomment this to double the icon size.
;;     ;;(treemacs-resize-icons 44)

;;     (treemacs-follow-mode t)
;;     (treemacs-filewatch-mode t)
;;     (treemacs-fringe-indicator-mode t)
;;     (pcase (cons (not (null (executable-find "git")))
;;                  (not (null treemacs-python-executable)))
;;       (`(t . t)
;;        (treemacs-git-mode 'deferred))
;;       (`(t . _)
;;        (treemacs-git-mode 'simple))))
;;   :bind
;;   (:map global-map
;;         ("M-0"       . treemacs-select-window)
;;         ("C-x t 1"   . treemacs-delete-other-windows)
;;         ("C-x t t"   . treemacs)
;;         ("C-x t B"   . treemacs-bookmark)
;;         ("C-x t C-t" . treemacs-find-file)
;;         ("C-x t M-t" . treemacs-find-tag)))


;; (use-package treemacs-projectile
;;   :after treemacs projectile
;;   :ensure t)

;; (use-package treemacs-icons-dired
;;   :after treemacs dired
;;   :ensure t
;;   :config (treemacs-icons-dired-mode))

;; (use-package treemacs-magit
;;   :after treemacs magit
;;   :ensure t)

;; (use-package treemacs-persp
;;   :after treemacs persp-mode
;;   :ensure t
;;   :config (treemacs-set-scope-type 'Perspectives))

(global-set-key (kbd "C-x C-b") 'ibuffer)

;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time

(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling

(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

(setq scroll-step 1) ;; keyboard scroll one line at a time

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
;;store org-mode links to messages
(require 'org-mu4e)
;;store link to message if in header view, not to header query
(setq org-mu4e-link-query-in-headers-mode nil)
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
      `(
	("j" "Journal Entry"
	 entry (function org-capture-journal-location))
	("t" "Todo"
	 entry (file ,(concat org-directory "/General.org"))
	 "* TODO %?\n  %iSCHEDULED: %^t\n  %a")
      	)
      )

;;------------------language config------------------------------

(show-paren-mode 1)

;; install language config packages
(dolist (package '(yaml-mode flymake-ruby inf-ruby company robe web-mode magit))
  (unless (package-installed-p package)
    (package-install package))
  (require package))

(use-package elpy
  :ensure t
  :init
  (elpy-enable))

(setq elpy-rpc-virtualenv-path 'current)
(setq elpy-rpc-python-command "python3")

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

(defun connect-remote-minecraft ()
  (interactive)
  (dired "/ec2-user@minecraft.scotty.dance:~"))

(defun connect-remote-arch ()
  (interactive)
  (dired "/root@10.0.1.41:/"))

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

(setq next-line-add-newlines t)
(ido-mode 1) ; file search magic

;; stupid bell
(setq ring-bell-function 'ignore)

(add-to-list 'default-frame-alist '(font . "Menlo-20" ))

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
    ("~/Dropbox/org/birthdays.org" "~/Dropbox/org/General.org" "~/Dropbox/org/movement.org" "~/Dropbox/org/shopping.org" "/Users/scottstav/Dropbox/org/dotfiles.org" "/Users/scottstav/Dropbox/org/poetry.org" "/Users/scottstav/Dropbox/org/posts.org" "/Users/scottstav/Dropbox/org/projects.org" "/Users/scottstav/Dropbox/org/sensunDnD.org")))
 '(package-selected-packages
   (quote
    (treemacs-persp treemacs-magit treemacs-icons-dired treemacs-projectile treemacs elpy exec-path-from-shell google-this desktop+ magit git js2-mode flymake-ruby robe inf-ruby flycheck web-mode json-mode groovy-mode gradle-mode use-package markdown-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
