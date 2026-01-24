;; Bootstrap straight.el
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

;; Install use-package via straight
(straight-use-package 'use-package)

;; Make use-package use straight by default
(setq straight-use-package-by-default t)
(straight-use-package 'org)

(global-set-key (kbd "M-o") 'other-window)

(scroll-bar-mode -1)                     ; Disable visible scrollbar
(menu-bar-mode -1)                       ; Disable the menu bar
(tool-bar-mode -1)                       ; Disable the toolbar

;; Start dired with details hidden and fit window to longest filename
(add-hook 'dired-mode-hook #'dired-hide-details-mode)

(setq fit-window-to-buffer-horizontally t)

(set-face-attribute 'mode-line nil :height 0.8)
(set-face-attribute 'mode-line-inactive nil :height 0.8)

(load-theme 'deeper-blue t)

(setq make-backup-files nil)       ; Stop creating ~ backup files
(setq auto-save-default nil)       ; Stop creating #autosave# files

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(global-set-key (kbd "C-c C-u") 'uncomment-region)
(global-set-key (kbd "C-c C-p") 'comment-region)

;; Enable visual line mode globally
(global-visual-line-mode 1)
;; Add some extra line spacing for better readability of wrapped lines
(setq-default line-spacing 0.2)

(use-package expand-region
  :bind ("C-\\" . er/expand-region))

(use-package multiple-cursors
  :init (multiple-cursors-mode)
  :bind (("C-c m c" . mc/edit-lines)
         ("C-=" . mc/mark-next-like-this)
         ("C--" . mc/skip-to-next-like-this)
         ("C-<" . mc/mark-previous-like-this)))

;;; completion-and-project.el --- Modern completion + project setup -*- lexical-binding: t; -*-


;;; Minibuffer Completion (Vertico + Orderless)
(use-package vertico
  :ensure t
  :init (vertico-mode)
  :custom
  (vertico-cycle t)
  (vertico-count 12)
  :config
  ;; Load the directory extension (bundled with vertico)
  (require 'vertico-directory)
  :bind (:map vertico-map
              ("RET"   . vertico-directory-enter)
              ("DEL"   . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package savehist
  :ensure nil  ; built-in
  :init (savehist-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;;; In-Buffer Completion (Corfu)
(use-package corfu
  :ensure t
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 2)
  (corfu-cycle t)
  (corfu-quit-no-match 'separator)
  :init (global-corfu-mode)
  :bind (:map corfu-map
              ("C-n" . corfu-next)
              ("C-p" . corfu-previous)
              ("C-g" . corfu-quit)))

;; Corfu in terminal
(use-package corfu-terminal
  :ensure t
  :unless (display-graphic-p)
  :after corfu
  :init (corfu-terminal-mode))

;;; Annotations (Marginalia)
(use-package marginalia
  :ensure t
  :init (marginalia-mode))

;;; Enhanced Commands (Consult)
(use-package consult
  :ensure t
  :bind
  (;; Global bindings
   ("C-s"     . consult-line)
   ("C-x b"   . consult-buffer)
   ("C-x C-b"   . ibuffer)
   ("C-x r b" . consult-bookmark)
   ("M-y"     . consult-yank-pop)
   ("M-g g"   . consult-goto-line)
   ("M-g M-g" . consult-goto-line)
   ("M-g o"   . consult-outline)
   ("M-g i"   . consult-imenu)
   ("M-i"   . consult-ripgrep)
   ("M-s f"   . consult-fd)
   ("C-c p r" . consult-ripgrep)
   ;; Project bindings (C-x p prefix)
   ("C-c p b" . consult-project-buffer)
   ("C-c p f" . consult-fd))
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  (setq consult-fd-args
    "fd --full-path --color=never --hidden")
  (advice-add #'register-preview :override #'consult-register-window)
  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep consult-fd
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 0.4 any))
  (setq consult-narrow-key "<")
  (setq consult-ripgrep-args
  "rg --null --line-buffered --color=never --max-columns=1000 --path-separator / --smart-case --no-heading --with-filename --line-number --hidden"))

;;; Actions (Embark)
(use-package embark
  :ensure t
  :bind
  (("C-."   . embark-act)
   ("C-;"   . embark-dwim)
   ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(defun my-project-shell ()
  "Start an inferior shell in the current project's root directory.
If a buffer already exists for running a shell in the project's root,
switch to it.  Otherwise, create a new shell buffer.
With \\[universal-argument] prefix arg, create a new inferior shell buffer even
if one already exists."
  (interactive)
  (require 'comint)
  (let* ((default-directory (project-root (project-current t)))
         (default-project-shell-name (project-prefixed-buffer-name "shell"))
         (shell-buffer (get-buffer default-project-shell-name)))
    (if (and shell-buffer (not current-prefix-arg))
        (if (comint-check-proc shell-buffer)
            (pop-to-buffer shell-buffer (bound-and-true-p display-comint-buffer-action))
          (vterm shell-buffer))
      (vterm (generate-new-buffer-name default-project-shell-name)))))

(advice-add 'project-shell :override #'my-project-shell)

;;; Project Management (project.el)
(use-package project
  :ensure nil  ; built-in
  :bind-keymap ("C-c p" . project-prefix-map)
  :bind
  (:map project-prefix-map
        ;; Override some defaults with consult variants
        ("b" . consult-project-buffer)
        ("r" . consult-ripgrep)
        ("f" . consult-fd)
	("v" . project-shell))
  :custom
  ;; Where to store known projects
  (project-list-file (expand-file-name "projects" user-emacs-directory))
  ;; Commands available when switching projects
  (project-switch-commands
   '((consult-fd "Find file" ?f)
     (consult-ripgrep "Ripgrep" ?r)
     (project-dired "Dired" ?d)
     (magit-project-status "Magit" ?m)
     (project-eshell "Eshell" ?e))))

;; Remember recent files for consult-recent-file
(use-package recentf
  :ensure nil
  :init (recentf-mode)
  :custom
  (recentf-max-saved-items 200)
  (recentf-exclude '("/tmp/" "/ssh:" "COMMIT_EDITMSG")))

;;; Optional: Better candidate info in corfu
(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))
(use-package wgrep)

(use-package gptel
    :config
    (setq gptel-default-mode 'org-mode
          gptel-use-tools 'ask
          gptel-track-media t
          gptel-model 'claude-sonnet-4-5-20250929
          gptel-backend
          (gptel-make-anthropic "Claude"
            :key (shell-command-to-string "bw get password 5322bf93-eb74-4ba6-bf47-b3950038928a"))))

;; set a global keybind for gptel send to C-c enter
(global-set-key (kbd "C-c <return>") 'gptel-send)

(defun gptel-generate-code-here (prompt)
  "Generate code at point from PROMPT with file context."
  (interactive "sCode to generate: ")
  (gptel-request
    (format "File: %s\nLanguage: %s\n\nGenerate: %s"
            (or (buffer-file-name) "unknown")
            (symbol-name major-mode)
            prompt)
    :system "You are a code generator. Return ONLY the requested code. No markdown fences, no explanations, no prose. Raw code only, ready to be inserted directly into a source file."
    :position (point)
    :stream t))

;; todo

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
     ))
  :config (require 'mcp-hub)
  :hook (after-init . mcp-hub-start-all-server))
(require 'gptel-integrations)
(gptel-mcp-connect)

(use-package gptel-agent
  :config (gptel-agent-update))

(use-package gptel-magit
  :hook (magit-mode . gptel-magit-install))

;; todo

(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :hook (prog-mode . copilot-mode)
  :bind (:map prog-mode-map ("<backtab>" . copilot-accept-completion)))

(setq copilot-indent-offset-warning-disable t)

(use-package magit
  :bind ("C-x g" . magit-status))

(use-package browse-at-remote)

(use-package forge
  :ensure t
  :after magit)

(use-package git-gutter
  :config (global-git-gutter-mode +1))

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)  ; or t for automatic
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package eglot
  :hook ((python-ts-mode . eglot-ensure)
         (js-ts-mode . eglot-ensure)
         (typescript-ts-mode . eglot-ensure)
         (rust-ts-mode . eglot-ensure)
         (go-ts-mode . eglot-ensure)))

(use-package platformio-mode
  :ensure t
  :hook (c++-mode . platformio-conditionally-enable))

(use-package org
  :straight nil
  :config
  (setq org-directory "~/Dropbox/org"
        org-agenda-files (list "~/Dropbox/org/denote/"
			       "~/Dropbox/org/denote/journal")
        org-clock-persist 'history
        org-export-with-section-numbers nil
        org-hide-block-startup t
        org-src-window-setup 'current-window
        org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate))

;; Org babel
(use-package ob-http)
(use-package ox-gfm :ensure t)

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

(defun my/gptel-image-download-setup ()
  (interactive)
  (when (derived-mode-p 'org-mode)
    (with-eval-after-load 'org-download
      (setq-local org-download-image-dir
		  (file-name-as-directory (concat (file-name-as-directory temporary-file-directory) "gptel")))
      (setq-local org-download-heading-lvl nil)
      )
    )
  )
(add-hook 'gptel-mode-hook #'my/gptel-image-download-setup)

(use-package denote
    :straight t
    :bind (("C-c n n" . denote)              ; Create new note
           ("C-c n f" . denote-open-or-create) ; Find/create note
           ("C-c n l" . denote-link)           ; Insert link
           ("C-c n L" . denote-backlinks)      ; Show backlinks
           ("C-c n r" . denote-rename-file)    ; Rename with new title/keywords
           ("C-c n R" . denote-rename-file-using-front-matter))
    :config
    ;; Main notes directory (keeping separate from journal)
    (setq denote-directory (expand-file-name "~/Dropbox/org/denote"))

    ;; File naming
    (setq denote-file-type 'org)           ; Use org format
    (setq denote-known-keywords '("project" "ifit" "work" "personal"))
    (setq denote-infer-keywords t)         ; Suggest keywords from existing files
    (setq denote-prompts '(title keywords)) ; What to ask for when creating


    ;; Allow subdirectories
    (setq denote-allow-subdirectories t)

    ;; History
    (add-to-list 'savehist-additional-variables 'denote-history))


  ;; Optional: Work notes
  (defun my/denote-work ()
    "Create or open today's work journal entry."
    (interactive)
    (let ((denote-directory (expand-file-name "~/Dropbox/org/denote/work"))
          (denote-file-name-title "work"))
      (denote-create-note)))

  (global-set-key (kbd "C-c n W") #'my/denote-work)

  (use-package denote-journal
    :straight t
    :bind (("C-c n J" . denote-journal-new-entry)     ; Today's journal
           ("C-c n j" . denote-journal-new-or-existing-entry)) ; Find journal entry
    :config
    ;; Journal lives in separate directory
    (setq denote-journal-directory (expand-file-name "~/Dropbox/org/denote/journal"))

    ;; Simpler journal file names (no signature, no keywords by default)
    (setq denote-journal-file-name-title "journal")

    ;; Daily journal (could be 'weekly or 'monthly)
    (setq denote-journal-frequency 'daily))


  ;; Optional: Work journal (separate from personal)
  ;; You can call this function or bind it
  (defun my/denote-journal-work ()
    "Create or open today's work journal entry."
    (interactive)
    (let ((denote-journal-directory (expand-file-name "~/Dropbox/org/denote/work/journal"))
          (denote-journal-file-name-title "work"))
      (denote-journal-new-or-existing-entry)))

  (global-set-key (kbd "C-c n w") #'my/denote-journal-work)


  ;; Consult integration (if you use consult)
  (with-eval-after-load 'consult
    (defun my/consult-denote ()
      "Find denote file with consult."
      (interactive)
      (consult-find denote-directory))

    (defun my/consult-denote-grep ()
      "Grep denote files with consult."
      (interactive)
      (consult-ripgrep denote-directory)))

  ;; Quick access to iFIT work service files
  (defun my/find-ifit-service ()
    "Find and open an iFIT service file from denote/work.
Shows clean service names for completion (e.g., 'user-service')."
    (interactive)
    (let* ((work-dir (expand-file-name "~/Dropbox/org/denote/work"))
           (files (directory-files work-dir t "\\.org$"))
           ;; Filter out journal files
           (service-files (seq-remove
                           (lambda (f) (string-match-p "journal" f))
                           files))
           ;; Build alist of (display-name . filepath)
           (candidates
            (mapcar
             (lambda (filepath)
               (let* ((filename (file-name-nondirectory filepath))
                      ;; Extract title part: 20230725T120000--user-service__ifit.org -> user-service
                      (title (if (string-match "--\\([^_]+\\)__" filename)
                                 (match-string 1 filename)
                               filename)))
                 (cons title filepath)))
             service-files))
           ;; Let user choose
           (chosen (completing-read "iFIT Service: " candidates nil t))
           (filepath (cdr (assoc chosen candidates))))
      (when filepath
        (find-file filepath))))

  (global-set-key (kbd "C-c n i") #'my/find-ifit-service)

(use-package vterm)

(use-package openwith
  :config
  (setq openwith-associations '(("\\.\\(mkv\\|mp4\\|avi\\|webm\\)\\'" "mpv" (file))))
  (openwith-mode t))

(setq large-file-warning-threshold nil)
