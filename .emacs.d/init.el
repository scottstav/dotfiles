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

(add-hook 'before-save-hook 'delete-trailing-whitespace)

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
   ("C-x r b" . consult-bookmark)
   ("M-y"     . consult-yank-pop)
   ("M-g g"   . consult-goto-line)
   ("M-g M-g" . consult-goto-line)
   ("M-g o"   . consult-outline)
   ("M-g i"   . consult-imenu)
   ("M-i"   . consult-ripgrep)
   ("M-s f"   . consult-fd)
   ("C-x p r" . consult-ripgrep)
   ;; Project bindings (C-x p prefix)
   ("C-x p b" . consult-project-buffer)     
   ("C-x p f" . consult-fd))
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

;;; Project Management (project.el)
(use-package project
  :ensure nil  ; built-in
  :bind-keymap ("C-x p" . project-prefix-map)
  :bind
  (:map project-prefix-map
        ;; Override some defaults with consult variants
        ("b" . consult-project-buffer)
        ("r" . consult-ripgrep)
        ("f" . consult-fd))
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

;;; Summary of key bindings:
;; 
;; Global:
;;   C-s       → search current buffer (consult-line)
;;   C-x b     → switch buffer (consult-buffer, includes recent files)
;;   M-y       → yank from kill ring with preview
;;   M-s r     → ripgrep (anywhere)
;;   M-s f     → find file (anywhere)
;;   M-g i     → jump to symbol in buffer (imenu)
;;   C-.       → embark actions on thing at point
;;
;; Project (C-x p prefix):
;;   C-x p p   → switch project
;;   C-x p f   → find file in project
;;   C-x p r   → ripgrep in project
;;   C-x p b   → switch buffer (project only)
;;   C-x p d   → dired at project root
;;   C-x p e   → eshell at project root
;;   C-x p k   → kill project buffers


(use-package wgrep)

(use-package gptel
    :config
    (setq gptel-default-mode 'org-mode
          gptel-use-tools 'ask
          gptel-track-media t
          gptel-model 'claude-3-5-haiku-20241022
          gptel-backend
          (gptel-make-anthropic "Claude"
            :key (shell-command-to-string "bw get password 5322bf93-eb74-4ba6-bf47-b3950038928a"))))

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
     ("brave-search" . (:command "docker"
				 :args ("run" "-i" "--rm" "-e" "BRAVE_API_KEY"
					"docker.io/mcp/brave-search")
				 :env (:BRAVE_API_KEY
                                       ,(shell-command-to-string "bw get password 8e6bcb23-28f5-4a86-9efb-b3950038dcca"))))
     ("browser-control" . (:command "node"
				    :args (,(expand-file-name "~/browser-control-mcp/mcp-server/dist/server.js"))
				    :env (:EXTENSION_SECRET "2ed34b9e-2d45-46e3-991c-e4bec401943f"
							    :EXTENSION_PORT "8079")))
     ("filesystem" . (:command "npx"
                               :args ("-y" "@modelcontextprotocol/server-filesystem" "~/Dropbox")
                               :roots ("~")))))
  :config (require 'mcp-hub)
  :hook (after-init . mcp-hub-start-all-server))
(require 'gptel-integrations)
(gptel-mcp-connect)

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
  :system "You are a robot that provides information. You can search the web with the tool but only if you need to. Be as quick as possible. Limit your responses to 110 characters."
  :tools '("get_file_info" "search_files" "directory_tree"
           "list_directory_with_sizes" "read_multiple_files"
           "read_media_file" "read_text_file" "list_directory" "read_file"
           "list_allowed_directories" "get-library-docs" "resolve-library-id"
           "brave_summarizer" "brave_news_search" "brave_image_search"
           "brave_video_search" "brave_local_search" "brave_web_search")
  :stream t :temperature 1.0 :max-tokens nil :use-context 'user
  :track-media t :include-reasoning t)

(use-package magit
  :bind ("C-x g" . magit-status))

(use-package browse-at-remote)
(use-package forge
  :after magit
  :config (setq forge-owned-accounts '(("scottstav"))))
(use-package git-gutter
  :config (global-git-gutter-mode +1))
