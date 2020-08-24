;; Add MELPA
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu/mu4e")
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/elisp/")


;; dont remember what this was for...
(eval-after-load 'gnutls
  '(add-to-list 'gnutls-trustfiles "/etc/ssl/cert.pem"))

(defconst EMACS27+   (> emacs-major-version 26))
(defconst EMACS28+   (> emacs-major-version 27))
(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-LINUX   (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))
(defconst IS-BSD     (or IS-MAC (eq system-type 'berkeley-unix)))

(add-to-list 'default-frame-alist '(font . "Menlo-20" ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; multiple cursors                                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package multiple-cursors)
(global-set-key (kbd "C-c m c") 'mc/edit-lines)

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
      ;; `fundamental-mode', rather than, say, `org-mode' or `text-mode'.
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
  (load-theme 'doom-snazzy t)

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


;; this	is a mess i dont know where to put anything!!!
(global-set-key (kbd "C-\\") 'er/expand-region)

(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
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
    "Run `crontab -e' in a emacs buffer."
    (interactive)
    (with-editor-async-shell-command "crontab -e"))

;; Buffers
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Directory
(defun file-info ()
  (interactive)
  (let ((dired-listing-switches "-alh"))
    (dired-other-window buffer-file-name)))

(global-set-key (kbd "C-x C-b") 'ibuffer)

;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time

(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling

(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

(setq scroll-step 1) ;; keyboard scroll one line at a time

(dolist (package '(desktop+ google-this helm))
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
          treemacs-is-never-other-window         nil
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


;; god i dont know anymore somebody make me organize my config
(add-hook 'text-mode-hook
          (lambda () (local-set-key (kbd "C-c C-d") #'define-word-at-point)))

;; org mode

(setq diary-file "~/Dropbox/org/diary")

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
      (goto-line -1)))
  )

(defun find-journal-date ()
  "Search the journal file for the current date"
  (interactive "P")
  )

(setq org-capture-templates
      `(
	("j" "Journal Entry"
	 entry (function org-capture-journal-location))
	("t" "Todo"
	 entry (file ,(concat org-directory "/General.org"))
	 "* TODO %?\n  %iSCHEDULED: %^t\n  %a")
      	)
      )

(require 'org-mac-link)
(add-hook 'org-mode-hook (lambda ()
  (define-key org-mode-map (kbd "C-c g") 'org-mac-grab-link)))

(require 'org-mac-iCal)

(setq org-agenda-include-diary t)
;;------------------language config------------------------------

(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1)
  :config
  (add-to-list 'yas-snippet-dirs (locate-user-emacs-file "snippets")))



(global-set-key (kbd "C-x g") 'magit-status)
(show-paren-mode 1)
(electric-pair-mode 1)

;; install language config packages
(dolist (package '(yaml-mode flymake-ruby inf-ruby company robe web-mode magit npm-mode exec-path-from-shell jedi go-mode terraform-mode))
  (unless (package-installed-p package)
    (package-install package))
  (require package))

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

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

(require 'go-projectile)
(add-hook 'go-mode-hook 'my-go-mode-hook)

(use-package elpy
  :ensure t
  :init
  (elpy-enable))

(setq elpy-rpc-backend "jedi")

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



;; auto-complete
(global-company-mode)
(add-hook 'after-init-hook 'global-company-mode)
(eval-after-load 'company
  '(push 'company-robe company-backends))
(add-hook 'after-init-hook #'global-flycheck-mode)
;;---------------------------------------------------------------
;; Projectile
(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(setq projectile-indexing-method 'native)

(defun connect-remote-minecraft ()
  (interactive)
  (dired "/ssh:ec2-user@minecraft.scotty.dance:~"))

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

;; keybindings
(global-set-key (kbd "C-M-<return>") 'org-insert-subheading)


(setq next-line-add-newlines t)
(ido-mode 1) ; file search magic

;; helm-ag
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
 '(doom-modeline-bar-width 8)
 '(doom-modeline-height 15)
 '(fci-rule-color "#383a42")
 '(global-display-line-numbers-mode t)
 '(helm-ag-base-command "ag --nocolor --nogroup --ignore-case --path-to-ignore ~/.ignore")
 '(helm-ag-insert-at-point (quote symbol))
 '(jdee-db-active-breakpoint-face-colors (cons "#f0f0f0" "#4078f2"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#f0f0f0" "#50a14f"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#f0f0f0" "#9ca0a4"))
 '(markdown-command "/usr/local/bin/pandoc")
 '(objed-cursor-color "#e45649")
 '(org-agenda-files
   (quote
    ("~/Dropbox/org/marathon.org" "~/Dropbox/org/birthdays.org" "~/Dropbox/org/General.org")))
 '(org-modules
   (quote
    (ol-bbdb ol-bibtex ol-docview ol-eww ol-gnus ol-info ol-irc ol-mhe ol-rmail ol-w3m org-mac-iCal org-mac-link)))
 '(package-selected-packages
   (quote
    (define-word helm-projectile ag helm-ag go-dlv expand-region lsp-mode browse-at-remote terraform-mode go-mode treemacs-evil restclient vterm jedi speed-type npm-mode multiple-cursors projectile doom-themes impatient-mode gdscript-mode urlenc ruby-refactor treemacs-persp treemacs-magit treemacs-icons-dired treemacs-projectile treemacs elpy exec-path-from-shell google-this desktop+ magit git js2-mode flymake-ruby robe inf-ruby flycheck web-mode json-mode groovy-mode gradle-mode use-package markdown-mode)))
 '(pdf-view-midnight-colors (cons "#383a42" "#fafafa"))
 '(rustic-ansi-faces
   ["#fafafa" "#e45649" "#50a14f" "#986801" "#4078f2" "#a626a4" "#0184bc" "#383a42"])
 '(send-mail-function (quote sendmail-send-it))
 '(show-paren-mode t)
 '(vc-annotate-background "#fafafa")
 '(vc-annotate-color-map
   (list
    (cons 20 "#50a14f")
    (cons 40 "#688e35")
    (cons 60 "#807b1b")
    (cons 80 "#986801")
    (cons 100 "#ae7118")
    (cons 120 "#c37b30")
    (cons 140 "#da8548")
    (cons 160 "#c86566")
    (cons 180 "#b74585")
    (cons 200 "#a626a4")
    (cons 220 "#ba3685")
    (cons 240 "#cf4667")
    (cons 260 "#e45649")
    (cons 280 "#d2685f")
    (cons 300 "#c07b76")
    (cons 320 "#ae8d8d")
    (cons 340 "#383a42")
    (cons 360 "#383a42")))
 '(vc-annotate-very-old-color nil))

(global-set-key (kbd "M-i") 'helm-do-ag-project-root)


;; stupid bell
(setq ring-bell-function 'ignore)

(when (string= system-type "darwin") ; avoid warn when opening dir on macOS
  (setq dired-use-ls-dired nil))

(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))



(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'downcase-region 'disabled nil)
