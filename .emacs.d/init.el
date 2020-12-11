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


(use-package emojify
  :ensure
  :hook (after-init . global-emojify-mode))

;; font
(add-to-list 'default-frame-alist '(font . "Menlo-20" ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; multiple cursors                                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package multiple-cursors)
(global-set-key (kbd "C-c m c") 'mc/edit-lines)
(global-set-key (kbd "C-=")  'mc/mark-next-like-this)
(global-set-key (kbd "C--")  'mc/skip-to-next-like-this)
(global-set-key (kbd "C-<")  'mc/mark-previous-like-this)

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
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; mode line
(use-package all-the-icons)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

;; ----------------- END stolen from DOOM ----------------------------------------;
;; expand-region
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
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; better scrolling config
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time

(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling

(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

(setq scroll-step 1) ;; keyboard scroll one line at a time

;; url


;; Begin installed packages





;; setup 'use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(require 'use-package)

;; misc install
(dolist (package '(desktop+ helm yaml-mode flymake-ruby inf-ruby company robe magit npm-mode exec-path-from-shell jedi go-mode terraform-mode define-word helm-projectile ag helm-ag go-dlv expand-region lsp-mode browse-at-remote terraform-mode go-mode restclient vterm jedi  multiple-cursors projectile doom-themes urlenc ruby-refactor treemacs-persp treemacs-magit treemacs-icons-dired treemacs-projectile treemacs elpy exec-path-from-shell magit git js2-mode flymake-ruby robe inf-ruby flycheck json-mode markdown-mode))
  (unless (package-installed-p package)
    (package-install package))
  (require package))

(use-package google-this
  :ensure google-this)

;; desktop mode
(desktop-save-mode 1)
(setq desktop-path '("~/.emacs.d/desktops"))

;; google this (set to C-c / ENTER)
(google-this-mode 1)

;; keychain history
(global-set-key "\C-cy"
		'(lambda ()
		   (interactive)
		   (popup-menu 'yank-menu)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Treemacs                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-directory-name-transformer    #'identity
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-extension-regex          treemacs-last-period-regex-value
          treemacs-file-follow-delay             0.2
          treemacs-file-name-transformer         #'identity
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         t
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-move-forward-on-expand        nil
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                      'left
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-asc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-user-mode-line-format         nil
          treemacs-user-header-line-format       nil
          treemacs-width                         35
          treemacs-workspace-switch-cleanup      nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))


(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package treemacs-icons-dired
  :after treemacs dired
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)

;; END Treemacs ;;;;;;;;;


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
	 :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|sh\\|txt\\|m4a"
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

;;------------------language config------------------------------

;;(setq flycheck-javascript-standard-executable "/usr/local/bin/standardx")
(use-package graphql-mode
  :ensure)
(use-package request
  :ensure)

;;???
;;(use-package indium
  ;;:ensure)

(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1)
  :config
  (add-to-list 'yas-snippet-dirs (locate-user-emacs-file "snippets")))

;; magit / git
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

(use-package lsp-sourcekit
  :ensure
  :after lsp-mode
  :config
  (setq lsp-sourcekit-executable "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/sourcekit-lsp"))

(use-package swift-mode
  :ensure
  :hook (swift-mode . (lambda () (lsp))))

(use-package tide
  :ensure t)

(defun setup-tide-mode ()
  "Setup function for tide."
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  ;;(setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1)
  (local-set-key "\C-c\C-d" 'tide-documentation-at-point)
  (local-set-key "\C-c\C-r" 'tide-references)
  (local-set-key "\C-c\C-f" 'tide-rename-file)
  (setq tide-native-json-parsing t))

(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

;;(eval-after-load 'flycheck
  ;;'(add-hook 'flycheck-mode-hook #'flycheck-typescript-tslint-setup))

(add-hook 'typescript-mode-hook #'setup-tide-mode)
(add-hook 'js-mode-hook #'setup-tide-mode)


(use-package nodejs-repl
  :ensure)

(use-package jest-test-mode :ensure t :defer t :commands jest-test-mode :init
  (add-hook 'typescript-mode-hook 'jest-test-mode)
  (add-hook 'js-mode-hook 'jest-test-mode)
  (add-hook 'typescript-tsx-mode-hook 'jest-test-mode))


;; (setq inferior-js-program-command "/usr/bin/java org.mozilla.javascript.tools.shell.Main")
;; (add-hook 'js-mode-hook '(lambda ()
;; 			    (local-set-key "\C-x\C-e" 'js-send-last-sexp)
;; 			    (local-set-key "\C-\M-x" 'js-send-last-sexp-and-go)
;; 			    (local-set-key "\C-cb" 'js-send-buffer)
;; 			    (local-set-key "\C-c\C-b" 'js-send-buffer-and-go)
;; 			    (local-set-key "\C-cl" 'js-load-file-and-go)
;; 			    (run-js)
;; 			    ))


(setq js-indent-level 2)
(setq typescript-indent-level 2)

(use-package paredit
  :ensure)

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

(defun my/align-whitespace (start end)
  "Align columns by whitespace"
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\)\\s-" 1 0 t))

(defun my/mc-lsp-highlight-symbol ()
  (interactive)
  (mc/remove-fake-cursors)

  (-when-let ((((&hash? "range" (&hash? "start"))) highlights)
              (->> (lsp-request "textDocument/documentHighlight"
                                (lsp--text-document-position-params))
                   (-separate
                    (-lambda ((&hash "range"))
                      (lsp--point-in-bounds-p (lsp--range-to-region range))))))
    (goto-char (lsp--position-to-point start))
    (push-mark)

    (mapc (-lambda ((&hash? "range" (&hash? "start")))
            (goto-char (lsp--position-to-point start))
            (push-mark)
            (mc/create-fake-cursor-at-point))
          highlights)

    (mc/maybe-multiple-cursors-mode)
    (->> start
         (lsp--position-to-point)
         (goto-char))
    (push-mark)))

(require 'go-projectile)
(add-hook 'go-mode-hook 'my-go-mode-hook)

(use-package elpy
  :ensure t
  :init
  (elpy-enable))

(setq elpy-rpc-backend "jedi")

;; json/yaml
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
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

;; auto-complete
(global-company-mode)
(add-hook 'after-init-hook 'global-company-mode)
(eval-after-load 'company
  '(push 'company-robe company-backends))
;;(add-hook 'after-init-hook #'global-flycheck-mode)
;;---------------------------------------------------------------
;; Projectile

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

;; restclient-mode
(add-to-list 'auto-mode-alist '("\\.rest-client\\'" . restclient-mode))

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
(ido-mode 1) ; file search magic

;; never used
(tool-bar-mode -1)
(menu-bar-mode -1)
(toggle-scroll-bar -1)

;; customize keep
(setq show-paren-mode t)
(setq doom-modeline-bar-width 8)
(setq doom-modeline-height 15)
(setq global-display-line-numbers-mode t)
(setq helm-ag-base-command "ag --nocolor --nogroup --ignore-case --path-to-ignore ~/.ignore")
(setq helm-ag-insert-at-point (quote symbol))
(setq markdown-command "/usr/local/bin/pandoc")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(global-visual-line-mode t)
 '(package-selected-packages
   '(ts-comint nodejs-repl indium jest-test-mode emojify htmlize url-util paredit graphql-mode org-jira git-gutter+ git-gitter+ forge tide yaml-mode vterm use-package urlenc treemacs-projectile treemacs-persp treemacs-magit treemacs-icons-dired terraform-mode swift-mode ruby-refactor robe restclient org-roam oauth2 npm-mode multiple-cursors lsp-sourcekit json-mode js2-mode js-comint jedi helm-projectile helm-ag google-this go-projectile go-dlv git flymake-ruby flycheck expand-region exec-path-from-shell elpy doom-themes doom-modeline desktop+ define-word browse-at-remote ag))
 '(safe-local-variable-values
   '((nil
      (eval add-hook 'after-save-hook
	    '(lambda nil
	       (org-publish-project "org"))
	    nil t))
     (eval add-hook 'after-save-hook
	   '(lambda nil
	      (org-publish-project "org"))
	   nil t))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
