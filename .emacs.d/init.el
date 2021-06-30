;; Add MELPA
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

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

(use-package vterm
  :ensure t
  :init (add-hook 'vterm-mode-hook (lambda ()
				     (display-line-numbers-mode -1)
				     (setq mode-line-format nil)
				     )))


(global-unset-key [(control z)])
(global-unset-key [(control x)(control z)])

;; font
(set-frame-font "Iosevka 24" nil t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; multiple cursors                                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;; ----------------- stolen from DOOM ----------------------------------------;
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

;; Emacs is essentially one huge security vulnerability, what with all the
;; dependencies it pulls in from all corners of the globe. Let's try to be at
;; least a little more discerning.
(setq gnutls-verify-error (not (getenv "INSECURE"))
      gnutls-algorithm-priority
      (when (boundp 'libgnutls-version)
        (concat "SECURE128:+SECURE192:-VERS-ALL"
                (if (and (not IS-WINDOWS)
                         (not (version< emacs-version "26.3"))
                         (>= libgnutls-version 30605))
                    ":+VERS-TLS1.3")
                ":+VERS-TLS1.2"))
      ;; `gnutls-min-prime-bits' is set based on recommendations from
      ;; https://www.keylength.com/en/4/
      gnutls-min-prime-bits 3072
      tls-checktrust gnutls-verify-error
      ;; Emacs is built with `gnutls' by default, so `tls-program' would not be
      ;; used in that case. Otherwise, people have reasons to not go with
      ;; `gnutls', we use `openssl' instead. For more details, see
      ;; https://redd.it/8sykl1
      tls-program '("openssl s_client -connect %h:%p -CAfile %t -nbio -no_ssl3 -no_tls1 -no_tls1_1 -ign_eof"
                    "gnutls-cli -p %p --dh-bits=3072 --ocsp --x509cafile=%t \
--strict-tofu --priority='SECURE192:+SECURE128:-VERS-ALL:+VERS-TLS1.2:+VERS-TLS1.3' %h"
                    ;; compatibility fallbacks
                    "gnutls-cli -p %p %h"))

;; themes
(use-package doom-themes
  :ensure
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-dark+ t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; mode line
(use-package all-the-icons)

(display-battery-mode 1)
(display-time-mode 1)
(setq display-time-default-load-average nil)
(setq doom-modeline-bar-width 5)
(setq doom-modeline-height 5)
(setq doom-modeline-buffer-encoding nil)
(setq doom-modeline-window-width-limit fill-column)
(setq doom-modeline-vcs-max-length 40)
(setq doom-modeline-percent-position nil)
(setq doom-modeline-battery-status t)
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

;; ----------------- END stolen from DOOM ----------------------------------------;
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
(add-hook 'dired-mode-hook 'dired-hide-details-mode)

(defun dired-get-size ()
  (interactive)
  (let ((files (dired-get-marked-files)))
    (with-temp-buffer
      (apply 'call-process "/usr/bin/du" nil t nil "-sch" files)
      (message "Size of all marked files: %s"
               (progn
                 (re-search-backward "\\(^[0-9.,]+[A-Za-z]+\\).*total$")
                 (match-string 1))))))

;; (define-key dired-mode-map (kbd "?") 'dired-get-size)


(defun file-info ()
  "Show the info for just the current file."
  (interactive)
  (let ((dired-listing-switches "-alh"))
    (dired-other-window buffer-file-name)))

;; change from list-buffer to ibuffer
;; ibuffer allows you to do things on buffers
(use-package helm
  :ensure)
(use-package helm-ag
  :ensure)
(use-package helm-swoop
  :ensure)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "C-c y") 'helm-show-kill-ring)
(global-set-key (kbd "M-x") 'helm-M-x)
(setq helm-swoop-pre-input-function
      (lambda () nil))
(global-set-key (kbd "C-s") 'helm-swoop)
(setq helm-swoop-split-with-multiple-windows t)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t
      helm-echo-input-in-header-line t)

(helm-mode 1)

;; better scrolling config
(toggle-scroll-bar -1)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time

(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling

(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

(setq scroll-step 1) ;; keyboard scroll one line at a time
(global-set-key (kbd "M-l") 'centered-cursor-mode)

(use-package yascroll
  :ensure)
(global-yascroll-bar-mode 1)

;; url

;; setup 'use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(require 'use-package)

;; desktop mode
(desktop-save-mode 1)
(setq desktop-path '("~/.emacs.d/desktops"))

;; spotify
(use-package oauth2
  :ensure)
;;(add-to-list 'load-path "~/.emacs.d/elisp/spotify/")
;;(require 'spotify)

;; Settings
;; import auth credentionls
;; (load-file "./auth/spotify-credentials.el")
;; (setq spotify-oauth2-client-secret spotify-client-secret)
;; (setq spotify-oauth2-client-id spotify-client-id)
;; (define-key spotify-mode-map (kbd "C-c .") 'spotify-command-map)
;; ;; the minibuf message is annoying so..
;; (setq spotify-player-status-refresh-interval 5)
;; (setq spotify-transport 'connect)

;; define-word
(add-hook 'text-mode-hook
          (lambda () (local-set-key (kbd "C-c C-d") #'define-word-at-point)))

;; org-mode
(require 'org)
(setq diary-file "~/Dropbox/org/personal/diary")
(setq org-directory "~/Dropbox/org")
(setq org-capture-templates nil)
(add-to-list 'org-capture-templates
             '("x" "Template name" plain
               (file (lambda () (expand-file-name
                     (format-time-string "%Y-%m-%d.org")
                     org-directory)))
               ""))
;;(setq org-default-notes-file (concat org-directory "/General.org"))
(setq org-agenda-files (list "~/Dropbox/org/personal/inbox.org" "~/Dropbox/org/personal/marathon.org" "~/Dropbox/org/personal/birthdays.org" "~/Dropbox/org/personal/General.org"))
(setq org-modules
      (quote
       (ol-bbdb ol-bibtex ol-docview ol-eww ol-gnus ol-info ol-irc ol-mhe ol-rmail ol-w3m org-mac-iCal org-mac-link)))

(setq org-capture-templates
      '(("i" "Inbox" entry (file "~/Dropbox/org/personal/inbox.org")
         "* TODO %?\nEntered on %U")
        ))

(define-key global-map (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)

(setq org-log-done nil)

(setq org-export-with-section-numbers nil)

;; these two things auto-beak lines when they become too long
(add-hook 'org-mode-hook '(lambda () (setq fill-column 80)))
(add-hook 'org-mode-hook 'turn-on-auto-fill)

;; publishing
(setq org-html-metadata-timestamp-format "%a %Y/%m/%d")
(setq org-html-postamble-format
      '(("en"
	 "<p class=\"date\">Created: %d </p><p class=\"updated\">Last Updated: %C</p><p class=\"creator\">Generated by %c</p>")))
(setq org-html-postamble t)

;; this ensures code highlighting on export... and probably other stuff
(use-package htmlize
  :ensure)

(require 'ox-publish)
(setq org-publish-project-alist
      '(
	("org-static"
	 :base-directory "~/projects/scotty.dance/static/"
	 :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|sh\\|txt\\|m4a\\|html"
	 :publishing-directory "~/public_html/"
	 :recursive t
	 :publishing-function org-publish-attachment
	 )
	("org-pages"
	 :base-directory "~/projects/scotty.dance/"
	 :base-extension "org"
	 :publishing-directory "~/public_html/"
	 :recursive t
	 :publishing-function org-html-publish-to-html
	 :headline-levels 4             ; Just the default for this project.
	 :auto-preamble t
	 )
	("org" :components ("org-pages" "org-static"))
	))

;; jira (ifit)
(use-package org-jira
  :ensure
  :init
  (setq jiralib-url "https://ifitdev.atlassian.net"))


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

;; mail
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu/mu4e")
(require 'mu4e)

;;------------------language config------------------------------

(use-package graphql-mode
  :ensure)
(use-package request
  :ensure)

(use-package prettier-js
  :ensure)

;; magit / git
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

(defun setup-tide-mode ()
  "Setup function for tide."
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (add-hook 'before-save-hook 'tide-format-before-save)
  (prettier-js-mode +1)
  (local-set-key "\C-c\C-d" 'tide-documentation-at-point)
  (local-set-key "\C-c\C-r" 'tide-references)
  (local-set-key "\C-c\C-f" 'tide-rename-file)
  (local-set-key "\C-c\C-s" 'tide-rename-symbol)
  (setq tide-native-json-parsing t))

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
(add-hook 'js-mode-hook #'setup-tide-mode)

(setq exec-path (append exec-path '("~/.nvm/versions/node/v14.16.0/bin")))
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

(use-package exec-path-from-shell :ensure)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; shell / vterm
(setq vterm-max-scrollback 100000)

(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$"
                          ""
                          (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq eshell-path-env path-from-shell) ; for eshell users
    (setq exec-path (split-string path-from-shell path-separator))))


(when window-system (set-exec-path-from-shell-PATH))


;; go-lang

(setenv "GOPATH" "/Users/scottstav/go")
;; i dont know why this has to be set manually
(setenv "GOROOT" "/usr/local/opt/go/libexec")

(defun my-go-mode-hook ()
  ; Call Gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)
  ; Godef jump key binding
  (local-set-key (kbd "M-.") 'godef-jump)
  (local-set-key (kbd "M-,") 'pop-tag-mark)
  (local-set-key (kbd "M-p") 'compile)            ; Invoke compiler
  (local-set-key (kbd "M-P") 'recompile)          ; Redo most recent compile cmd
  (local-set-key (kbd "M-]") 'next-error)         ; Go to next error (or msg)
  (local-set-key (kbd "M-[") 'previous-error)     ; Go to previous error or msg
  (go-eldoc-setup)
  (lsp)
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet"))
  )


;; lsp-mode
(use-package lsp-mode
  :ensure)

(use-package lsp-ui :ensure)
(lsp-ui-mode)


(use-package helm-lsp
  :ensure)

(define-key lsp-mode-map [remap xref-find-apropos] #'helm-lsp-workspace-symbol)

(defun my/align-whitespace (start end)
  "Align columns by whitespace"
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\)\\s-" 1 0 t))

(add-hook 'go-mode-hook 'my-go-mode-hook)

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

;; auto-completen
(global-company-mode 1)
(use-package yasnippet
  :ensure
  :config
  (setq yas-snippet-dirs '("~/Dropbox/config/emacs/snippets"))
  (yas-global-mode 1))

(use-package helm-c-yasnippet
  :ensure
  :config
  (setq helm-yas-space-match-any-greedy t)
  (global-set-key (kbd "C-c C-y") 'helm-yas-complete))





;;---------------------------------------------------------------
;; Projectile
(use-package projectile
  :ensure)
(use-package helm-projectile
  :ensure)

(projectile-mode +1)
(setq projectile-switch-project-action 'magit-status)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
;;(setq projectile-indexing-method 'native)
(setq projectile-indexing-method 'alien)
(helm-projectile-on)


(defun connect-remote-minecraft ()
  (interactive)
  (dired "/ssh:ec2-user@minecraft.scotty.dance:~"))

;; Shell
(defun with-face (str &rest face-plist)
  (propertize str 'face face-plist))


;;------------------save config------------------------------

;; my functions
(defun my/big-small-window ()
  "Expand current window to use half of the other window's lines."
  (interactive)
  (enlarge-window (/ (window-height (next-window)) 2)))

(global-set-key (kbd "C-c w") 'my/big-small-window)

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
(global-set-key (kbd "M-o") 'ace-window)
(define-key global-map (kbd "C-;") 'ace-jump-mode)

(delete-selection-mode 1)

(use-package which-key
  :ensure)
(which-key-mode 1)

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

;; windows, purpose
(use-package window-purpose
  :ensure)
(add-to-list 'purpose-user-mode-purposes '(js-mode . edit))
(add-to-list 'purpose-user-mode-purposes '(vterm-mode . shell))
(add-to-list 'purpose-user-mode-purposes '(magit-mode . help))
(purpose-mode)
(purpose-compile-user-configuration)

;; keybindings
(global-set-key (kbd "C-M-<return>") 'org-insert-subheading)
(global-set-key (kbd "M-i") 'helm-do-ag-project-root)
(global-unset-key (kbd "s-w"))
(global-set-key (kbd "C-c C-k") 'paredit-splice-sexp)

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

;; never used
(tool-bar-mode -1)
(menu-bar-mode -1)


;; customize keep
(setq show-paren-mode t)
(setq global-display-line-numbers-mode t)
(setq helm-ag-base-command "ag --nocolor --nogroup --ignore-case --path-to-ignore ~/.ignore")
(setq helm-ag-insert-at-point (quote symbol))
(setq markdown-command "/usr/local/bin/pandoc")
(setq global-visual-line-mode t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files
   '("~/Dropbox/org/personal/work.org" "~/Dropbox/org/personal/inbox.org" "~/Dropbox/org/personal/marathon.org" "~/Dropbox/org/personal/birthdays.org" "~/Dropbox/org/personal/General.org"))
 '(org-agenda-window-setup 'other-frame)
 '(package-selected-packages
   '(helm-c-yasnippet yascroll center-scroll-mode ace-window centered-cursor-mode jade-mode lsp-ui which-key key-chord key-chord-mode ace-jump-mode frame-purpose window-purpose helm-swoop yaml-mode restclient nvm expand-region helm-ag browse-at-remote vterm helm-projectile projectile elpy lsp-treemacs helm-lsp lsp-mode exec-path-from-shell paredit jest-test-mode nodejs-repl tide git-gutter+ forge prettier-js graphql-mode org-jira htmlize oauth2 helm doom-modeline doom-themes multiple-cursors emojify use-package))
 '(send-mail-function 'smtpmail-send-it)
 '(smtpmail-smtp-server "smtp.gmail.com")
 '(smtpmail-smtp-service 587))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
