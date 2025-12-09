(setq user-full-name "Scott Stavinoha"
      user-mail-address "scottstavinoha@gmail.com")

;; Package settings
(eval-and-compile
  (setq load-prefer-newer t
        package-user-dir "~/.emacs.d/elpa"
        package--init-file-ensured t
        package-enable-at-startup nil)
  (unless (file-directory-p package-user-dir)
    (make-directory package-user-dir t))
  (setq load-path (append load-path (directory-files package-user-dir t "^[^.]" t))))

;; Initialize package system and use-package
(eval-when-compile
  (require 'package)
  (package-initialize)
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
  (unless (package-installed-p 'bind-key)
    (package-refresh-contents)
    (package-install 'bind-key))
  (require 'use-package)
  (require 'bind-key)
  (setq use-package-always-ensure t))

;; Package archives
(unless (assoc-default "gnu" package-archives)
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t))
(unless (assoc-default "melpa" package-archives)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t))

;; Quelpa
(use-package quelpa :ensure)
(use-package quelpa-use-package
  :demand
  :config (quelpa-use-package-activate-advice))

;; Straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Startup
(setq inhibit-default-init t
      inhibit-startup-echo-area-message t
      inhibit-startup-screen t
      initial-scratch-message nil
      initial-major-mode 'org-mode)

;; UI
(mapc (lambda (mode) (when (fboundp mode) (funcall mode -1)))
      '(menu-bar-mode tool-bar-mode))
(scroll-bar-mode 0)
(setq ring-bell-function 'ignore)
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)
(fset 'yes-or-no-p 'y-or-n-p)
(global-visual-line-mode)

;; Scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; Files & Backups
(setq backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      version-control t
      kept-new-versions 10
      kept-old-versions 0
      delete-old-versions t
      backup-by-copying t
      vc-make-backup-files t
      save-interprogram-paste-before-kill nil
      large-file-warning-threshold 100000000)

(add-to-list 'backup-directory-alist (cons tramp-file-name-regexp nil))
(global-auto-revert-mode t)

;; Custom file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file) (load custom-file))

;; Savefile dir
(defconst gas-savefile-dir (expand-file-name "savefile" user-emacs-directory))
(unless (file-exists-p gas-savefile-dir)
  (make-directory gas-savefile-dir))

;; Browser & misc
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "firefox"
      byte-compile-warnings '(cl-functions))

;; Auth
(require 'auth-source)
(setq auth-sources '("~/.authinfo.gpg")
      epg-gpg-program "gpg")
(setf epa-pinentry-mode 'loopback)

;; Frame settings
(setq default-frame-alist '((cursor-color . "white")
                             (alpha-background . 80)))
(set-frame-parameter nil 'alpha-background 80)

;; Indentation
(setq-default indent-tabs-mode nil)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Parens & pairs
(show-paren-mode 1)
(electric-pair-mode 1)
(delete-selection-mode 1)

;; Vertico (minibuffer completion)
(use-package vertico
  :init (vertico-mode))

(use-package savehist
  :init (savehist-mode))

(use-package orderless
  :custom (completion-styles '(orderless basic))
  :init
  (setq completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion))
                                         (eglot (styles orderless)))))

;; Corfu (in-buffer completion)
(use-package corfu
  :custom (corfu-auto t)
  :hook ((prog-mode . corfu-mode)
         (shell-mode . corfu-mode)
         (eshell-mode . corfu-mode)))

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block))

;; Annotations & UI
(use-package marginalia
  :config (marginalia-mode))

(use-package embark
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim)
         ("C-h B" . embark-bindings))
  :init (setq prefix-help-command #'embark-prefix-help-command)
  :config (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target))

(use-package embark-consult
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;; Consult
(use-package consult
  :bind (("C-x b" . consult-buffer)
         ("M-i" . consult-ripgrep)
         ("C-s" . consult-line))
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 0.4 any))
  (setq consult-narrow-key "<"))

(use-package org
  :delight org-mode "✎"
  :pin gnu
  :defer t
  :config
  (define-key org-mode-map (kbd "C-c C-r") verb-command-map)
  (setq org-directory "~/Dropbox/org"
        org-agenda-files (list "~/Dropbox/org/roam/daily" "~/Dropbox/org/roam/")
        org-modules '(ol-bbdb ol-bibtex ol-docview ol-eww ol-gnus ol-info ol-irc ol-mhe ol-rmail ol-w3m)
        org-clock-persist 'history
        org-export-with-section-numbers nil
        org-hide-block-startup t
        org-src-window-setup 'current-window
        org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)
  (org-clock-persistence-insinuate))

;; Org babel
(use-package ob-http :ensure t)
(use-package ob-mongo :ensure t)
(use-package ob-graphql :ensure t)
(use-package ox-gfm :ensure t)

(require 'org-tempo)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((shell . t) (emacs-lisp . t) (latex . t) (js . t) (python . t) (http . t)))

(defun my-org-confirm-babel-evaluate (lang body)
  (not (member lang '("node" "http" "python" "emacs-lisp" "graphql" "sh" "bash" "js" "shell" "javascript" "inline-js"))))

(add-to-list 'org-src-lang-modes '("inline-js" . javascript))
(defvar org-babel-default-header-args:inline-js
  '((:results . "html") (:exports . "results")))
(defun org-babel-execute:inline-js (body _params)
  (format "<script type=\"text/javascript\">\n%s\n</script>" body))

;; Org-roam
(defun my/org-roam-filter (node)
  (let ((tags (org-roam-node-tags node)))
    (not (member "ATTACH" tags))))

(defun my/org-roam-node-find ()
  (interactive)
  (if (equal current-prefix-arg nil)
      (org-roam-node-find)
    (org-roam-node-find t nil 'my/org-roam-filter)))

(use-package org-roam
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . my/org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n I" . org-roam-node-insert-immediate)
         :map org-mode-map ("C-M-i" . completion-at-point))
  :bind-keymap ("C-c n d" . org-roam-dailies-map)
  :config
  (setq org-roam-directory (file-truename "~/Dropbox/org/roam"))
  (org-roam-db-autosync-mode)
  (require 'org-roam-dailies)
  (setq org-roam-capture-templates
        '(("p" "project" plain "\n%?"
           :if-new (file+head "%<%Y.%m.%d>-${slug}.org" "#+TITLE: ${title}")
           :unnarrowed t)
          ("w" "work" plain "\n%?"
           :if-new (file+head "%<%Y.%m.%d>-${slug}.org" "#+TITLE: ${title}")
           :unnarrowed t)))
  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry "\n\n\n\n%?"
           :target (file+head "%<%Y-%m-%d>.org" "#+TITLE: %<%Y-%m-%d>\n\n* %<%Y-%m-%d>"))
          ("w" "work" entry "\n* %?"
           :target (file+head "./work/%<%Y-%m-%d>.org" "#+TITLE: %<%Y-%m-%d>")))))

;; Org publishing
(setq org-html-metadata-timestamp-format "%a %Y/%m/%d"
      org-html-prefer-user-labels t
      org-export-with-broken-links t
      org-html-postamble t
      org-html-postamble-format
      '(("en" "<p class=\"date\">Created: %d </p><p class=\"updated\">Last Updated: %C</p><p class=\"creator\">Generated by %c</p>")))

(setq org-publish-project-alist
      '(("org-pages" :base-directory "~/Dropbox/org/roam/daily"
         :base-extension "org"
         :publishing-directory "/ssh:root@lantana.io:/var/www/html/org/daily"
         :recursive t :publishing-function org-html-publish-to-html
         :headline-levels 4 :auto-preamble t
         :exclude "daily/work/\\|iFIT\\|lantana.index.org"
         :auto-sitemap t :sitemap-filename "index.org"
         :sitemap-title "Hello" :sitemap-sort-files anti-chronologically
         :html-head-include-scripts t
         :html-head-extra "<script src=\"../../test.js\"></script>")
        ("lantana-index" :base-directory "~/Dropbox/org/"
         :base-extension "org"
         :publishing-directory "/ssh:root@lantana.io:/var/www/html/"
         :publishing-function org-html-publish-to-html)
        ("js" :base-directory "~/Dropbox/org/js"
         :base-extension "js"
         :publishing-directory "/ssh:root@lantana.io:/var/www/html/"
         :recursive t :publishing-function org-publish-attachment)
        ("org-attachments" :base-directory "~/Dropbox/org/roam/daily/data"
         :base-extension "png\\|jpg\\|gif\\|css\\|js"
         :publishing-directory "/ssh:root@lantana.io:/var/www/html/org/daily/data"
         :publishing-function org-publish-attachment :recursive t)
        ("LANtana" :components ("org-pages" "lantana-index" "org-attachments"))))

;; Org-download
(use-package org-download
  :init
  (setq org-download-method 'attach
        org-download-screenshot-method "grim -g \"$(slurp)\" /tmp/temp.png && convert -filter Cubic -resize 500 /tmp/temp.png %s"))

;; GPTel
  (auth-source-forget-all-cached)
  (use-package gptel
    :config
    (setq gptel-default-mode 'org-mode
          gptel-use-tools 'ask
          gptel-track-media t
          gptel-model 'claude-3-5-haiku-20241022
          gptel-backend
          (gptel-make-anthropic "Claude"
            :key (shell-command-to-string "bw get password 5322bf93-eb74-4ba6-bf47-b3950038928a"))))

  ;; MCP servers
  (use-package mcp
    :after gptel
    :custom
    (mcp-hub-servers
     `(("github" . (:command "docker"
                    :args ("run" "-i" "--rm" "-e" "GITHUB_PERSONAL_ACCESS_TOKEN"
                           "ghcr.io/github/github-mcp-server")
                    :env (:GITHUB_PERSONAL_ACCESS_TOKEN
                          ,(shell-command-to-string "bw get password 396252b4-ca8d-40db-9afe-b3950038c380"))))
       ("context7" . (:url "https://mcp.context7.com/mcp"))
       ("brave-search" . (:command "docker"
                          :args ("run" "-i" "--rm" "-e" "BRAVE_API_KEY"
                                 "docker.io/mcp/brave-search")
                          :env (:BRAVE_API_KEY
                                ,(shell-command-to-string "bw get password 8e6bcb23-28f5-4a86-9efb-b3950038dcca"))))
       ("browser-control" . (:command "node"
                             :args ("/home/ifit/browser-control-mcp/mcp-server/dist/server.js")
                             :env (:EXTENSION_SECRET "2ed34b9e-2d45-46e3-991c-e4bec401943f"
                                   :EXTENSION_PORT "8079")))
       ("filesystem" . (:command "npx"
                        :args ("-y" "@modelcontextprotocol/server-filesystem" "~/Dropbox")
                        :roots ("/home/ifit/")))))
    :config (require 'mcp-hub)
    :hook (after-init . mcp-hub-start-all-server))

  (require 'gptel-integrations)
  (gptel-mcp-connect)

  ;; Claude Code
  (use-package inheritenv
    :vc (:url "https://github.com/purcell/inheritenv" :rev :newest))

  (use-package claude-code
    :config (claude-code-mode)
    :bind-keymap ("C-c c" . claude-code-command-map))

  ;; GPTel presets
  (gptel-make-preset 'coder
    :description "A preset optimized for coding tasks"
    :backend "Claude"
    :model 'claude-opus-4-1-20250805
    :system "You are a programmer.  Follow my instructions and return relevant code snippets.
- Generate ONLY code as output, without any explanation or markdown code fences. Do not place in source blocks.
- Generate only new code, do not repeat context.
- Do not ask for further clarification, and make any assumptions you need to follow instructions."
    :tools '("mcp-context7"))

  (gptel-make-preset 'qq
    :description "For quick questions. Small responses to and from minibuf."
    :backend "Claude"
    :model 'claude-3-5-haiku-20241022
    :system "You are a robot that provides information. You can search the web with the tool but only if you need to. Be as quick as possible. Limit your responses to 110 characters"
    :tools '("get_file_info" "search_files" "directory_tree"
             "list_directory_with_sizes" "read_multiple_files"
             "read_media_file" "read_text_file" "list_directory" "read_file"
             "list_allowed_directories" "get-library-docs" "resolve-library-id"
             "brave_summarizer" "brave_news_search" "brave_image_search"
             "brave_video_search" "brave_local_search" "brave_web_search")
    :stream t :temperature 1.0 :max-tokens nil :use-context 'user
    :track-media t :include-reasoning t)

  ;; Copilot
  (use-package copilot
    :quelpa (copilot :fetcher github
             :repo "copilot-emacs/copilot.el"
             :branch "main"
             :files ("*.el"))
    :hook (prog-mode . copilot-mode)
    :bind (:map prog-mode-map ("<backtab>" . copilot-accept-completion)))

;; Magit
(use-package magit
  :bind ("C-x g" . magit-status)
  :config
  (setq magit-display-buffer-function
        (lambda (buffer)
          (display-buffer
           buffer (if (and (derived-mode-p 'magit-mode)
                          (memq (with-current-buffer buffer major-mode)
                                '(magit-process-mode magit-revision-mode
                                  magit-diff-mode magit-stash-mode
                                  magit-status-mode)))
                     nil
                   '(display-buffer-same-window))))
        magit-list-refs-sortby "-creatordate"))

;; Other git packages
(use-package browse-at-remote :ensure)
(use-package forge
  :after magit
  :config (setq forge-owned-accounts '(("scottstav"))))
(use-package git-gutter
  :config (global-git-gutter-mode +1))

;; LSP
(use-package lsp-mode
  :custom (lsp-completion-provider :none)
  :init
  (setq lsp-keymap-prefix "C-c l"
        lsp-response-timeout 1)
  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)))
  :hook (lsp-completion-mode . my/lsp-mode-setup-completion)
  :commands lsp)

(use-package lsp-ui :commands lsp-ui-mode)
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)

;; Tree-sitter
(use-package tree-sitter-langs)
(use-package tree-sitter
  :config
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (cmake "https://github.com/uyha/tree-sitter-cmake")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (rust "https://github.com/tree-sitter/tree-sitter-rust" "master" "src")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (make "https://github.com/alemuller/tree-sitter-make")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

;; Code formatting
(use-package apheleia
  :pin "melpa")

;; Snippets
(use-package yasnippet
  :init
  (setq yas-snippet-dirs '("~/Dropbox/config/emacs/snippets")
        markdown-fontify-code-blocks-natively t)
  :bind ("C-c i" . yas-insert-snippet)
  :config (yas-global-mode 1))

;; Projectile
(use-package projectile
  :config
  (projectile-mode +1)
  (setq projectile-switch-project-action 'magit-status
        projectile-indexing-method 'alien)
  :bind-keymap ("C-c p" . projectile-command-map)
  :bind (:map projectile-command-map ("x v" . projectile-run-vterm-other-window)))

;; Eldoc
(use-package eldoc
  :defer t
  :diminish eldoc-mode)

(use-package add-node-modules-path)
(use-package prettier-js)

(defun setup-typescript-mode ()
  (interactive)
  (add-node-modules-path)
  (subword-mode)
  (display-line-numbers-mode)
  (apheleia-mode)
  (lsp))

(add-hook 'typescript-mode-hook 'setup-typescript-mode)

(use-package jest-test-mode
  :defer t
  :commands jest-test-mode
  :init
  (add-hook 'typescript-mode-hook 'jest-test-mode)
  (add-hook 'typescript-ts-mode-hook 'jest-test-mode)
  (add-hook 'js-mode-hook 'jest-test-mode)
  (add-hook 'typescript-tsx-mode-hook 'jest-test-mode))

(setq js-indent-level 2
      typescript-indent-level 2)

(add-hook 'python-mode-hook
          (lambda () (lsp) (display-line-numbers-mode)))

(use-package go-mode
  :config (add-hook 'go-mode-hook 'eglot-ensure))

(use-package rust-mode
  :init (setq rust-mode-treesitter-derive t
              rust-format-on-save t)
  :hook ((rust-mode . (lambda ()
                        (setq indent-tabs-mode nil)
                        (display-line-numbers-mode)))
         (rust-mode . lsp)))

;; C
(add-hook 'c-mode-common-hook 'lsp)

;; CSS
(use-package css-mode
  :config (setq css-indent-offset 2))

;; JSON
(use-package json-mode
  :bind (:map json-mode-map ("C-c i" . json-mode-beautify))
  :mode ("\\.\\(json\\)$" . json-mode))

;; YAML
(use-package yaml-mode
  :mode ("\\.\\(yml\\|yaml\\|\\config\\|sls\\)$" . yaml-mode))

;; GraphQL
(use-package graphql-mode)

;; Terraform
(use-package terraform-mode
  :config
  (defun tf-before-save ()
    (when (eq major-mode 'terraform-mode)
      (message (concat "Running tf format " buffer-file-name))
      (call-process-shell-command
       (concat "terraform fmt -list=false -write=true " buffer-file-name "&"))))
  (add-hook 'before-save-hook #'tf-before-save))

;; Markdown
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "pandoc"))

;; Docker
(use-package dockerfile-mode)

;; Theme
(use-package modus-themes
  :init (load-theme 'modus-vivendi :no-confirm))

;; Modeline
(use-package minions
  :config (minions-mode))

(use-package mood-line
  :config (mood-line-mode))

;; Rainbow delimiters
(use-package rainbow-delimiters)
(use-package rainbow-mode :delight)

;; Which-key
(use-package which-key
  :config (which-key-mode 1))

;; Helpful
(use-package helpful
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-c C-d" . helpful-at-point)))

;; Terminal
(setq vterm-always-compile-module t)
(use-package vterm)

;; ERC (IRC)
(require 'erc)
(require 'erc-desktop-notifications)
(require 'erc-track)
(require 'notifications)
(erc-track-mode)
(setq erc-track-position-in-mode-line 't)

(defun my-on-action-function (id key)
  (message "Message %d, key \"%s\" pressed" id key))
(defun my-on-close-function (id reason)
  (message "Message %d, closed due to \"%s\"" id reason))
(defun erc-notifications-notify (nick msg)
  (interactive)
  (notifications-notify
   :title nick :body msg
   :actions '("Confirm" "Reply" "Refuse" "Close")
   :on-action 'my-on-action-function
   :on-close 'my-on-close-function))

;; Other utilities
(use-package htmlize)
(use-package wgrep)
(use-package ace-jump-mode
  :bind ("C-;" . ace-jump-mode))
(use-package whisper
  :vc (:url "https://github.com/natrys/whisper.el" :branch "master")
  :bind ("C-c r" . whisper-run))
(use-package expand-region
  :bind ("C-\\" . er/expand-region))
(use-package multiple-cursors
  :init (multiple-cursors-mode)
  :bind (("C-c m c" . mc/edit-lines)
         ("C-=" . mc/mark-next-like-this)
         ("C--" . mc/skip-to-next-like-this)
         ("C-<" . mc/mark-previous-like-this)))

;; Global keybindings
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "C-c C-u") 'uncomment-region)
(global-set-key (kbd "C-c C-p") 'comment-region)
(global-set-key (kbd "C-c C-k") 'paredit-splice-sexp)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Custom delete functions
(defun delete-word(arg)
  "Delete characters forward until encountering the end of a word."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

(defun backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word."
  (interactive "p")
  (delete-word (- arg)))

(global-set-key (kbd "M-d") 'delete-word)
(global-set-key (kbd "M-DEL") 'backward-delete-word)

;; Bean minor mode
(defvar bean-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c RET") 'gptel-send)
    map)
  "bean-minor-mode keymap.")

(define-minor-mode bean-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  :init-value t
  :lighter " bean")

(bean-minor-mode 1)

;; Display buffer settings
(setq display-buffer-alist
      '(((major-mode . vterm-mode)
         (display-buffer-reuse-mode-window display-buffer-at-bottom))
        ((or . ((derived-mode . flymake-diagnostics-buffer-mode)
                (derived-mode . flymake-project-diagnostics-mode)
                (derived-mode . messages-buffer-mode)))
         (display-buffer-reuse-mode-window display-buffer-at-bottom)
         (window-height . 0.3)
         (dedicated . t)
         (preserve-size . (t . t)))))

;; Async shell
(add-to-list 'display-buffer-alist
             '("*Async Shell Command*" display-buffer-no-window (nil)))

(defun dired-get-size ()
  (interactive)
  (let ((files (dired-get-marked-files)))
    (with-temp-buffer
      (apply 'call-process "/usr/bin/du" nil t nil "-sch" files)
      (message "Size of all marked files: %s"
               (progn
                 (re-search-backward "\\(^[0-9.,]+[A-Za-z]+\\).*total$")
                 (match-string 1))))))

(defun open-file-or-xdg-open ()
  "Open file with xdg-open if it's a video file, otherwise open in Emacs."
  (interactive)
  (let ((file (dired-get-file-for-visit)))
    (if (or (string-suffix-p ".mkv" file) (string-suffix-p ".mp4" file))
        (call-process "xdg-open" nil 0 nil file)
      (dired-find-file))))

(eval-after-load "dired"
  '(progn
     (define-key dired-mode-map [return] 'open-file-or-xdg-open)
     (define-key dired-mode-map (kbd "<mouse-2>") 'open-file-or-xdg-open)))

(defun file-info ()
  "Show the info for just the current file."
  (interactive)
  (let ((dired-listing-switches "-alh"))
    (dired-other-window buffer-file-name)))

(setq dired-listing-switches "-alht")

(defun save-buffer-always ()
  "Save the buffer even if it is not modified."
  (interactive)
  (set-buffer-modified-p t)
  (save-buffer))

(defun untabify-buffer ()
  "De-indent current buffer."
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  "Indent the entire buffer according to current mode."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun cleanup-buffer ()
  "Perform whitespace cleanup operations on a buffer."
  (interactive)
  (untabify-buffer)
  (delete-trailing-whitespace)
  (indent-buffer))

(defun crontab-e ()
  "Run crontab -e in an Emacs buffer."
  (interactive)
  (with-editor-async-shell-command "crontab -e"))

(defun gptel-send-to-new-session (text)
  "Send TEXT to a new gptel session and submit it."
  (let* ((session-name (format "*gptel-%s*" (substring text 0 (min 8 (length text)))))
         (gptel-buffer (gptel session-name)))
    (with-current-buffer gptel-buffer
      (goto-char (point-max))
      (insert text)
      (gptel-send))
    (switch-to-buffer-other-frame gptel-buffer)))
