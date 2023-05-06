;; Add MELPA
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)


;;(server-start)

;; setup 'use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(require 'use-package)

(add-to-list 'load-path "~/.emacs.d/elisp/")

;; start in home dir
(setq default-directory "~/")

;; local cert for http requests
(eval-after-load 'gnutls
  '(add-to-list 'gnutls-trustfiles "/etc/ssl/cert.pem"))

;; constants
(defconst EMACS27+   (> emacs-major-version 26))
(defconst EMACS28+   (> emacs-major-version 27))
(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-LINUX   (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))
(defconst IS-BSD     (or IS-MAC (eq system-type 'berkeley-unix)))

;; mail

(autoload 'notmuch "notmuch" "notmuch mail" t)
(setq send-mail-function 'sendmail-send-it
      sendmail-program "/usr/bin/msmtp"
      mail-specify-envelope-from t
      message-sendmail-envelope-from 'header
      mail-envelope-from 'header)

(setq mail-user-agent 'message-user-agent)

(org-msg-mode)


(use-package vterm
  :ensure t
  :init (add-hook 'vterm-mode-hook (lambda ()
				     (display-line-numbers-mode -1)
				     (setq mode-line-format "  --- vterm ---")
				     )))

(global-visual-line-mode 1)

;; auth
(setq auth-sources '("~/.authinfo"))


;; random annoying message
(setq byte-compile-warnings '(cl-functions))

(global-unset-key [(control z)])
(global-unset-key [(control x)(control z)])

;; Use the whole screen
(setq frame-resize-pixelwise t)
(toggle-frame-maximized)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; multiple cursors                                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq default-frame-alist '((cursor-color . "white")))
(use-package multiple-cursors
  :ensure)
(global-set-key (kbd "C-c m c") 'mc/edit-lines)
(global-set-key (kbd "C-=")  'mc/mark-next-like-this)
(global-set-key (kbd "C--")  'mc/skip-to-next-like-this)
(global-set-key (kbd "C-<")  'mc/mark-previous-like-this)

;; OS level keybinds
(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'super)
(global-set-key (kbd "s-v")  'yank)
(global-set-key (kbd "s-c")  'kill-ring-save)

;; Contrary to what many Emacs users have in their configs, you really don't
;; need more than this to make UTF-8 the default coding system:
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))       ; pretty
(prefer-coding-system 'utf-8)            ; pretty
(setq locale-coding-system 'utf-8)       ; please
;; The clipboard's on Windows could be in an encoding that's wider (or thinner)
;; than utf-8, so let Emacs/the OS decide what encoding to use there.
(unless IS-WINDOWS
  (setq selection-coding-system 'utf-8)) ; with sugar on top

;; Less noise at startup. The dashboard/empty scratch buffer is good enough.
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t
      ;; Avoid pulling in many packages by starting the scratch buffer in
      ;; `fundamental-mode', rather than, say, `org-mode or `text-mode'.
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil)

;; Get rid of "For information about GNU Emacs..." message at startup, unless
;; we're in a daemon session, where it'll say "Starting Emacs daemon." instead,
;; which isn't so bad.
(unless (daemonp)
  (advice-add #'display-startup-echo-area-message :override #'ignore))

(use-package modus-themes
  :ensure                         ; omit this to use the built-in themes
  :init
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs nil
        modus-themes-region '(bg-only no-extend)
	modus-themes-subtle-line-numbers t)

  ;; Load the theme files before enabling a theme (else you get an error).
  (modus-themes-load-themes)
  :config
  ;; Load the theme of your choice:
  (modus-themes-load-vivendi) ;; OR (modus-themes-load-vivendi)
  :bind ("<f5>" . modus-themes-toggle))

;; transparency
(set-frame-parameter (selected-frame) 'alpha '(90 90))
(add-to-list 'default-frame-alist '(alpha 90 90))

;; never used
(tool-bar-mode -1)
(menu-bar-mode -1)

;; ------------- stolen from DOOM ----------------------------------------;

;; mode line
(use-package mood-line
  :ensure)
(mood-line-mode)
(set-face-attribute 'mode-line nil
                    :box nil)


;; expand-region
(use-package expand-region
  :ensure)
(global-set-key (kbd "C-\\") 'er/expand-region)

;; misc functions
(defun untabify-buffer ()
  "De-indent current buffer."
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  "Indent the entire buffer according to current mode."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer.
Including indent-buffer, which should not be called automatically on save."
  (interactive)
  (untabify-buffer)
  (delete-trailing-whitespace)
  (indent-buffer))

(defun crontab-e ()
    "Run `crontab -e' in a Emacs buffer."
    (interactive)
    (with-editor-async-shell-command "crontab -e"))

;; dired
(require 'dired-x)
(put 'dired-find-alternate-file 'disabled nil)
(add-hook 'dired-mode-hook 'dired-hide-details-mode)

(setq dired-guess-shell-alist-user '(("\\.jpeg\\'" "sxiv")
                                     ("\\.doc\\'" "libreoffice")
                                     ("\\.docx\\'" "libreoffice")
                                     ("\\.ppt\\'" "libreoffice")
                                     ("\\.pptx\\'" "libreoffice")
                                     ("\\.xls\\'" "libreoffice")
                                     ("\\.xlsx\\'" "libreoffice")
                                     ("\\.jpg\\'" "pinta")
                                     ("\\.png\\'" "pinta")
                                     ("\\.jpeg\\'" "sxiv")))


(global-unset-key (kbd "C-x C-r"))
(global-set-key (kbd "C-x C-r") 'counsel-recentf)

(defun dired-get-size ()
  (interactive)
  (let ((files (dired-get-marked-files)))
    (with-temp-buffer
      (apply 'call-process "/usr/bin/du" nil t nil "-sch" files)
      (message "Size of all marked files: %s"
               (progn
                 (re-search-backward "\\(^[0-9.,]+[A-Za-z]+\\).*total$")
                 (match-string 1))))))

(define-key dired-mode-map (kbd "?") 'dired-get-size)
;;(define-key dired-mode-map (kbd "V") 'dired-get-size)

(defun file-info ()
  "Show the info for just the current file."
  (interactive)
  (let ((dired-listing-switches "-alh"))
    (dired-other-window buffer-file-name)))

;; change from list-buffer to ibuffer
;; ibuffer allows you to do things on buffers
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; better scrolling config
(toggle-scroll-bar -1)
(scroll-bar-mode -1)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time

(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling

(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

(setq scroll-step 1) ;; keyboard scroll one line at a time
;(global-set-key (kbd "M-l") 'centered-cursor-mode)

;; url

;; org-mode
(require 'org)
(setq diary-file "~/Dropbox/org/personal/diary")
(setq org-directory "~/Dropbox/org")

;;(setq org-default-notes-file (concat org-directory "/General.org"))
(setq org-agenda-files (list "~/Dropbox/org/roam/daily" "~/Dropbox/org/roam/"))
(setq org-modules
      (quote
       (ol-bbdb ol-bibtex ol-docview ol-eww ol-gnus ol-info ol-irc ol-mhe ol-rmail ol-w3m org-mac-iCal org-mac-link)))

(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)
(setq org-clock-sound "~/Dropbox/Sounds/win.wav")

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)

(setq org-log-done nil)

(setq org-export-with-section-numbers nil)

;; go to first heading when opening org files
(add-hook 'org-mode-hook (lambda () (org-next-visible-heading 1)))

;;roam
(use-package org-roam
  :ensure
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n I" . org-roam-node-insert-immediate)
         :map org-mode-map
         ("C-M-i" . completion-at-point)
         :map org-roam-dailies-map
         ("Y" . org-roam-dailies-capture-yesterday)
         ("T" . org-roam-dailies-capture-tomorrow))
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :config
  (setq org-roam-directory (file-truename "~/Dropbox/org/roam"))
  (org-roam-db-autosync-mode)
  (require 'org-roam-dailies)

  (setq org-roam-capture-templates '(("p" "project" plain
				      "\n%?"
				      :if-new (file+head "%<%Y.%m.%d>-${slug}.org" "#+TITLE: ${title}")
				      :unnarrowed t)
				     ("w" "work" plain
				      "\n%?"
				      :if-new (file+head "%<%Y.%m.%d>-${slug}.org" "#+TITLE: ${title}")
				      :unnarrowed t))))


(setq org-roam-dailies-capture-templates
      '(("d" "default" entry
         "\n* %?"
         :target (file+head "%<%Y-%m-%d>.org" "#+TITLE: %<%Y-%m-%d>"))))
;; publishing
(setq org-html-metadata-timestamp-format "%a %Y/%m/%d")
(setq org-html-postamble-format
      '(("en"
	 "<p class=\"date\">Created: %d </p><p class=\"updated\">Last Updated: %C</p><p class=\"creator\">Generated by %c</p>")))
(setq org-html-postamble t)

;; ;; org babel
(use-package ob-http :ensure)
(use-package ob-mongo :ensure)
(use-package ob-graphql :ensure)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((shell . t)
   (emacs-lisp . t)
   (latex . t)
   (js . t)
   (python . t)
   ))


(defun my-org-confirm-babel-evaluate (lang body)
  (not (member lang '("node" "http" "python" "emacs-lisp" "graphql" "sh" "bash" "js" "shell"))))

(setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)

;; ivy
(use-package ivy
  :ensure
  :config
  (ivy-mode 1)
  (setq ivy-height 10)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (setq ivy-re-builders-alist '((t . orderless-ivy-re-builder))))

(use-package counsel
  :ensure)

(global-set-key (kbd "M-i") 'counsel-ag)

(use-package swiper
  :ensure
  :config
  (global-set-key "\C-s" 'swiper))

(use-package wgrep
  :ensure)

;; this ensures code highlighting on export... and probably other stuff
(use-package htmlize
  :ensure)

(require 'ox-publish)
(setq org-publish-project-alist
      '(
	;; static files that should be at the base of the website
	("org-static"
	 :base-directory "~/projects/scotty.dance/static/"
	 :base-extension any
	 :publishing-directory "~/public_html/"
	 :recursive t
	 :publishing-function org-publish-attachment
	 )
	;; convert org documents to pdfs
	("org-pdfs"
	 :base-directory "~/projects/scotty.dance/pdfs/"
	 :base-extension "org"
	 :publishing-directory "~/public_html/"
	 :recursive t
	 :publishing-function org-latex-publish-to-pdf
	 )
	;; all other org files in nested directories i.e. posts/
	("org-pages"
	 :base-directory "~/projects/scotty.dance/"
	 :base-extension "org"
	 :publishing-directory "~/public_html/"
	 :recursive t
	 :publishing-function org-html-publish-to-html
	 :headline-levels 4
	 :auto-preamble t
	 )
	("org-media"
	 :base-directory "~/projects/scotty.dance/media/"
	 :base-extension any
	 :publishing-directory "~/public_html/media/"
	 :recursive t
	 :publishing-function org-publish-attachment
	 )
	("my-website" :components ("org-media" "org-pages" "org-static" "org-pdfs"))
	))

;; jira (ifit)


;; this doesnt work
;; (defun org-capture-journal-location ()
;;   "Go to journal.org and find the subtree by date."
;;   (interactive "P")
;;   (let* ((heading (format-time-string "%Y-%m-%d %A")))
;;     (find-file (concat org-directory "/journal.org"))
;;     (goto-char 0)
;;     (unless (search-forward (format "* %s" heading) nil t)
;;       (insert (format "* %s\n" heading))
;;       (goto-line -1)))
;;   )

(setq org-agenda-include-diary t)


;;------------------language config------------------------------

(use-package graphql-mode
  :ensure)
(use-package request
  :ensure)

(use-package prettier-js
  :ensure)

;; magit / git

(use-package magit
  :ensure)

(use-package blamer
  :ensure t
  :bind (("s-i" . blamer-show-commit-info))
  :defer 20
  :custom
  (blamer-idle-time 0.3)
  (blamer-min-offset 70)
  :custom-face
  (blamer-face ((t :foreground "#7a88cf"
                    :background nil
                    :height 140
                    :italic t)))
  :config
;  (global-blamer-mode 1)
  )

(use-package browse-at-remote :ensure)
(setq magit-display-buffer-function
      (lambda (buffer)
        (display-buffer
         buffer (if (and (derived-mode-p 'magit-mode)
                         (memq (with-current-buffer buffer major-mode)
                               '(magit-process-mode
                                 magit-revision-mode
                                 magit-diff-mode
                                 magit-stash-mode
                                 magit-status-mode)))
                    nil
                  '(display-buffer-same-window)))))
(global-set-key (kbd "C-x g") 'magit-status)
(use-package forge
  :ensure
  :after magit)
(setq forge-owned-accounts '(("scottstav")))

(use-package git-gutter+
  :ensure
  :config
  (define-key git-gutter+-mode-map (kbd "M-g k") 'git-gutter+-revert-hunk)
  (define-key git-gutter+-mode-map (kbd "M-g n") 'git-gutter+-next-hunk))
(global-git-gutter+-mode)
(show-paren-mode 1)
(electric-pair-mode 1)

(use-package tide
  :ensure t)

(use-package add-node-modules-path
  :ensure)

(defun setup-tide-mode ()
  "Setup function for tide."
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (company-mode +1)
  (tide-hl-identifier-mode +1)
  ;;(add-hook 'before-save-hook 'tide-format-before-save)
  (prettier-js-mode +1)
  (add-node-modules-path)
  (local-set-key "\C-c\C-d" 'tide-documentation-at-point)
  (local-set-key "\C-c\C-r" 'tide-references)
  (local-set-key "\C-c\C-f" 'tide-rename-file)
  (local-set-key "\C-c\C-s" 'tide-rename-symbol)
  (setq tide-native-json-parsing t)
  (centered-cursor-mode 1)
  (subword-mode 1))

(global-set-key "\C-c\C-u" 'uncomment-region)
(global-set-key "\C-c\C-p" 'comment-region)

;; formats the buffer before saving
;;(add-hook 'before-save-hook 'tide-format-before-save)


(defun setup-js-mode ()
  "Setup function for tide."
  (interactive)
  (flycheck-mode +1)
  (local-set-key "\C-c\C-r" 'lsp-find-references)
  (local-set-key "\M-." 'lsp-find-definition)
  (lsp)
  )

(add-hook 'typescript-mode-hook #'setup-tide-mode)
;(add-hook 'js-mode-hook #'setup-tide-mode)

(use-package exec-path-from-shell :ensure)
(when (daemonp)
  (exec-path-from-shell-initialize))

(setq exec-path (append exec-path '("~/.nvm/versions/node/v14.20.0/bin")))

(use-package nodejs-repl
  :ensure)

(use-package jest-test-mode :ensure t :defer t :commands jest-test-mode :init
  (add-hook 'typescript-mode-hook 'jest-test-mode)
  (add-hook 'js-mode-hook 'jest-test-mode)
  (add-hook 'typescript-tsx-mode-hook 'jest-test-mode))

(setq js-indent-level 2)
(setq typescript-indent-level 2)

(use-package paredit
  :ensure)

;; shell / vterm
(setq vterm-max-scrollback 100000)

(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)
(add-hook 'go-mode-hook #'lsp-deferred)

;; lsp-mode
(use-package lsp-mode
  :ensure)

(use-package lsp-ui :ensure)
(lsp-ui-mode)

(defun my/align-whitespace (start end)
  "Align columns by whitespace"
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\)\\s-" 1 0 t))



(use-package elpy
  :ensure t
  :init
  (elpy-enable))

(setq elpy-rpc-backend "jedi")

;; json/yaml
(use-package yaml-mode
  :ensure)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-hook 'yaml-mode-hook
          (lambda ()
            (define-key yaml-mode-map "\C-m" 'newline-and-indent)))
(add-hook 'json-mode-hook
          (lambda ()
            (setq standard-indent 2)))

;; ruby
(use-package flymake-ruby
  :ensure)
(use-package robe
  :ensure)
(add-hook 'ruby-mode-hook 'flymake-ruby-load)
(add-hook 'ruby-mode-hook 'robe-mode)

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

(use-package impatient-mode
  :ensure)

;; auto-completen
(setq company-idle-delay 0)
(setq company-minimum-prefix-length 1)
(setq company-dabbrev-downcase nil)

(use-package orderless
  :ensure t
  :custom (completion-styles '(orderless)))

;(use-package yasnippet
;  :ensure
;  :config
;  (setq yas-snippet-dirs '("~/Dropbox/config/emacs/snippets"))
;  (yas-global-mode 1))


;;---------------------------------------------------------------
;; Projectile
(use-package projectile
  :ensure)
(use-package counsel-projectile
  :ensure
  )


(projectile-mode +1)

(setq projectile-switch-project-action 'magit-status)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
;;(setq projectile-indexing-method 'native)
(setq projectile-indexing-method 'alien)

(defun connect-remote-minecraft ()
  (interactive)
  (dired "/ssh:ec2-user@minecraft.scotty.dance:~"))

;; Shell
(defun with-face (str &rest face-plist)
  (propertize str 'face face-plist))

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

;; text navigation
(use-package ace-jump-mode
  :ensure)

(use-package ace-window
  :ensure)
(global-set-key (kbd "C-x o") 'ace-window)
(global-set-key (kbd "M-o") 'other-window)
(define-key global-map (kbd "C-;") 'ace-jump-mode)
(use-package golden-ratio
  :ensure)
(golden-ratio-mode 1)

(delete-selection-mode 1)

(use-package which-key
  :ensure)
(which-key-mode 1)

(use-package helpful
  :ensure
  :config
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  (global-set-key (kbd "C-c C-d") #'helpful-at-point)
  )

(use-package key-chord
  :ensure)
(key-chord-mode 1)

(key-chord-define-global ";;" "\C-e;")
(key-chord-define-global "vv" "\C-^")
;; (key-chord-define-global "hj"     'undo)
;; (key-chord-define-global [?h ?j]  'undo)  ; the same
;; (key-chord-define-global "jk"     'dabbrev-expand)
;; (key-chord-define-global "cv"     'reindent-then-newline-and-indent)
;; (key-chord-define-global "4r"     "$")

;; restclient-mode
(use-package restclient :ensure)
(add-to-list 'auto-mode-alist '("\\.rest-client\\'" . restclient-mode))

;; keybindings
(global-set-key (kbd "C-M-<return>") 'org-insert-subheading)
(global-unset-key (kbd "s-w"))
(global-set-key (kbd "C-c C-k") 'paredit-splice-sexp)
(use-package dockerfile-mode
  :ensure)

(use-package terraform-mode
  :ensure
  :config
  (defun tf-before-save ()
    (when (eq major-mode 'terraform-mode)
      (message (concat "Running tf format " buffer-file-name))
      (call-process-shell-command (concat "terraform fmt -list=false -write=true " buffer-file-name "&"))))
  (add-hook 'before-save-hook #'tf-before-save)
  )

;; scroll other window
(define-key global-map [(meta p)] '(lambda() (interactive) (scroll-other-window -1)))
(define-key global-map [(meta n)] '(lambda() (interactive) (scroll-other-window 1)))

;; stupid bell
(setq ring-bell-function 'ignore)

(when (string= system-type "darwin") ; avoid warn when opening dir on macOS
  (setq dired-use-ls-dired nil))

(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))

(setq next-line-add-newlines nil)

(setq calendar-latitude 29.8940761)
(setq calendar-longitude -98.3503236)
(setq calendar-locpation-name "Spring Branch, TX")

;; never used
(tool-bar-mode -1)
(menu-bar-mode -1)

(add-to-list 'default-frame-alist '(font . "Iosevka Extended 14" ))
(set-frame-font "Iosevka Extended 14" nil t)


;; some bug fix for org mode src blocks: https://emacs.stackexchange.com/questions/64886/indentation-in-org-mode-source-block-with-return
(with-eval-after-load "org"
  (when (version-list-= (version-to-list org-version) '(9 4 3))
    (defun org-return-fix (fun &rest args)
      "Fix https://emacs.stackexchange.com/questions/64886."
      (let* ((context (if org-return-follows-link (org-element-context)
            (org-element-at-point)))
             (element-type (org-element-type context)))
    (if (eq element-type 'src-block)
        (apply #'org--newline args)
      (apply fun args))))
    (advice-add 'org-return :around #'org-return-fix)))

(defun name-of-the-file ()
  "Gets the name of the file the current buffer is based on."
  (interactive)
  (message (buffer-file-name)))
(global-unset-key (kbd "C-c C-/"))
(global-set-key (kbd "C-c C-/") 'name-of-the-file)

(with-eval-after-load "org-src"
  (when (version-list-= (version-to-list org-version) '(9 4 3))
    (defun org-src--contents-for-write-back ()
      "Return buffer contents in a format appropriate for write back.
Assume point is in the corresponding edit buffer."
      (let ((indentation-offset
         (if org-src--preserve-indentation 0
           (+ (or org-src--block-indentation 0)
          (if (memq org-src--source-type '(example-block src-block))
              org-src--content-indentation
            0))))
        (use-tabs? (and (> org-src--tab-width 0) t))
        (source-tab-width org-src--tab-width)
        (contents (org-with-wide-buffer (buffer-string)))
        (write-back org-src--allow-write-back))
    (with-temp-buffer
      ;; Reproduce indentation parameters from source buffer.
      (setq indent-tabs-mode use-tabs?)
      (when (> source-tab-width 0) (setq tab-width source-tab-width))
      ;; Apply WRITE-BACK function on edit buffer contents.
      (insert (org-no-properties contents))
      (goto-char (point-min))
      (when (functionp write-back) (save-excursion (funcall write-back)))
      ;; Add INDENTATION-OFFSET to every non-empty line in buffer,
      ;; unless indentation is meant to be preserved.
      (when (> indentation-offset 0)
        (while (not (eobp))
          (skip-chars-forward " \t")
          ;; (unless (eolp)     ;ignore blank lines
          (let ((i (current-column)))
        (delete-region (line-beginning-position) (point))
        (indent-to (+ i indentation-offset)))
          ;;)
          (forward-line)))
      (buffer-string))))))

;; customize keep
(setq show-paren-mode t)
(setq global-display-line-numbers-mode t)
(setq markdown-command "/usr/local/bin/pandoc")
(setq global-visual-line-mode t)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blamer-face ((t :foreground "#7a88cf" :background nil :height 140 :italic t)) t))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(warning-suppress-log-types
   '((use-package)
     (auto-save)
     (auto-save)
     (auto-save)
     (auto-save)
     (auto-save)
     (auto-save)
     (auto-save)
     (auto-save)
     (auto-save)
     (auto-save)
     (auto-save)))
 '(warning-suppress-types
   '((auto-save)
     (auto-save)
     (auto-save)
     (auto-save)
     (auto-save)
     (auto-save)
     (auto-save)
     (auto-save)
     (auto-save)
     (auto-save)
     (auto-save))))
