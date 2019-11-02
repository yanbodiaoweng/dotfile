;; fix the PATH variable
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (shell-command-to-string "TERM=vt100 $SHELL -i -c 'echo $PATH'")))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))
(when window-system (set-exec-path-from-shell-PATH))

(require 'package)
(setq package-archives '(("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
                         ("gnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("org" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/")))

(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

(use-package all-the-icons
  :ensure t)

(use-package lazycat-theme
  :load-path "lisp"
  :config
  (set-frame-font "Hack-14"))

;; (use-package darcula-theme
;;   :ensure t
;;   :config
;;   ;; your preferred main font face here
;;   (set-frame-font "Hack-14"))

(use-package awesome-tray
  :load-path "lisp"
  :config
  (awesome-tray-mode 1)
  (setq awesome-tray-active-modules
        '("evil" "parent-dir" "git" "mode-name" "date")))

(use-package evil-magit
  :ensure t
  :after evil magit)

(use-package evil
  :ensure t
  :init ;; tweak evil's configuration before loading it
  (setq evil-search-module 'evil-search)
  (setq evil-ex-complete-emacs-commands nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (setq evil-shift-round nil)
  (setq evil-want-C-u-scroll t)
  :config ;; tweak evil after loading it
  (evil-mode))

(use-package evil-org
  :ensure t
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme)))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
  (setq evil-want-C-i-jump nil))

(use-package aggressive-indent
  :ensure t
  :config
  (global-aggressive-indent-mode 1))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;; interactive completion
(use-package helm
  :ensure t
  :diminish helm-mode
  :init
  (setq helm-autoresize-max-height 30
        helm-display-header-line nil
        helm-always-two-windows t
        helm-split-window-inside-p t
        helm-move-to-line-cycle-in-source nil
        helm-ff-search-library-in-sexp t
        helm-ff-file-name-history-use-recentf t
        helm-comp-read-mode-line ""
        helm-read-file-name-mode-line-string ""
        helm-mode-line-string "")
  ;; enable fuzzy matching
  (setq helm-buffers-fuzzy-matching t
        helm-completion-in-region-fuzzy-match t
        helm-M-x-fuzzy-match t
        helm-apropos-fuzzy-match t
        helm-imenu-fuzzy-match t
        helm-lisp-fuzzy-completion t
        helm-locate-fuzzy-match t
        helm-mode-fuzzy-match t
        helm-recentf-fuzzy-match t
        helm-semantic-fuzzy-match t)
  :config
  (require 'helm-config)
  (helm-mode 1)
  (helm-autoresize-mode 1)
  (helm-adaptive-mode 1))

;; fuzzier matching for helm
(use-package helm-flx
  :ensure t
  :after helm
  :config
  (helm-flx-mode +1))

(use-package helm-swoop
  :ensure t)

(use-package helm-projectile
  :ensure t)

(use-package expand-region
  :ensure t
  :commands er/expand-region
  :init
  (setq-default
   expand-region-contract-fast-key "V"
   expand-region-reset-fast-key "r"))

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
;;           treemacs-display-in-side-window        t
;;           treemacs-eldoc-display                 t
;;           treemacs-file-event-delay              5000
;;           treemacs-file-follow-delay             0.2
;;           treemacs-follow-after-init             t
;;           treemacs-git-command-pipe              ""
;;           treemacs-goto-tag-strategy             'refetch-index
;;           treemacs-indentation                   2
;;           treemacs-indentation-string            " "
;;           treemacs-is-never-other-window         nil
;;           treemacs-max-git-entries               5000
;;           treemacs-missing-project-action        'ask
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
;;           treemacs-sorting                       'alphabetic-desc
;;           treemacs-space-between-root-nodes      t
;;           treemacs-tag-follow-cleanup            t
;;           treemacs-tag-follow-delay              1.5
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

;; (use-package treemacs-evil
;;   :after treemacs evil
;;   :ensure t)

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


(use-package ace-window
  :ensure t)

(use-package ranger
  :ensure t
  :config
  (ranger-override-dired-mode t)
  (setq ranger-cleanup-on-disable t)
  (setq ranger-show-hidden t)
  (setq ranger-parent-depth 2)
  (setq ranger-preview-file t)
  (setq ranger-show-literal t)
  (setq ranger-excluded-extensions '("mkv" "iso" "mp4"))
  (setq ranger-max-preview-size 10)
  (setq ranger-dont-show-binary t)
  )

(use-package page-break-lines
  :ensure t)

(use-package projectile
  :ensure t
  :config
  (projectile-mode))

(use-package evil-nerd-commenter
  :ensure t)

;; clean up obsolete buffers
(use-package midnight
  :ensure t)

(use-package paradox
  :ensure t
  :commands paradox-list-packages
  :init
  (setq paradox-github-token t)
  :config
  (evilified-state-evilify paradox-menu-mode paradox-menu-mode-map))

(use-package json-mode
  :ensure t
  :mode ("\\.json\\'" . json-mode)
  :init
  (add-hook 'json-mode-hook
            (lambda ()
              (make-local-variable 'js-indent-level)
              (setq js-indent-level 2))))

;; interact with HTTP APIs
(use-package restclient
  :ensure t
  :mode ("\\.http\\'" . restclient-mode))



(use-package org
  :mode ("\\.\\(org\\|org_archive\\|txt\\)\\'" . org-mode)
  :init
  (setq
   ;; log time when task is finished
   org-log-done 'time
   ;; create a logbook entry
   org-log-into-drawer t
   ;; org directory and agenda files
   org-directory "~/org"
   org-agenda-files (list org-directory)
   org-startup-indented t
   ;; org keywords and faces
   org-todo-keywords '((sequence "TODO(t)" "|" "DONE(d)")
                       (sequence "DOING(i)" "WAITING(b@/!)" "|" "ABORTED(a@/!)" )
                       )
   ;; dracula-themed faces
   org-todo-keyword-faces (quote
                           (("TODO" :foreground "#ff5555" :weight bold)
                            ("DOING" :foreground "#8be9fd" :weight bold)
                            ("DONE" :foreground "#50fa7b" :weight bold)
                            ("WAITING" :foreground "#ffb86c" :weight bold)
                            ("HOLD" :foreground "#bd93f9" :weight bold)
                            ("ABORTED" :foreground "#50fa7b" :weight bold)))
   ;; state triggers
   org-todo-state-tags-triggers (quote
                                 (("CANCELED" ("CANCELED" . t))
                                  ("WAITING"  ("WAITING"  . t))
                                  ("HOLD"     ("WAITING"  . t) ("HOLD" . t))
                                  (done       ("WAITING")      ("HOLD"))
                                  ("TODO"     ("WAITING")      ("CANCELED") ("HOLD"))
                                  ("STARTED"  ("WAITING")      ("CANCELED") ("HOLD"))
                                  ("DONE"     ("WAITING")      ("CANCELED") ("HOLD"))))
   ;; use fast todo selection
   org-use-fast-todo-selection t
   org-treat-S-cursor-todo-selection-as-state-change nil
   ;; handle empty lines
   org-cycle-separator-lines 0
   org-blank-before-new-entry (quote ((heading)
                                      (plain-list-item . auto)))
   ;; targets complete directly with ido
   org-outline-path-complete-in-steps nil
   org-src-fontify-natively t
   ;; use ido for both buffer and file completion and ido-everywhere to t
   ;;org-completion-use-ido t
   org-capture-templates
   '(("j" "随手记" entry (file+olp+datetree
                          (lambda()(concat org-directory "/journal.org")))
      "* %?\n%U\n" :clock-keep t)
     ("t" "todo" entry (file+olp+datetree
                        (lambda()(concat org-directory "/todo.org")))
      "* TODO %?\n%U\n")
     ))
  :config
  (set-face-attribute 'org-level-1 nil :height 1.0)
  (set-face-attribute 'org-level-2 nil :height 1.0))

(use-package org-bullets
  :ensure t
  :after org
  :init
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; (use-package ibuffer-vc
;;   :ensure t
;;   :defer 5
;;   :init
;;   ;; Include version control status info in the ibuffer list.
;;   (setq ibuffer-formats
;;         '((mark modified read-only vc-status-mini " "
;;                 (name 18 18 :left :elide)
;;                 " "
;;                 (size 9 -1 :right)
;;                 " "
;;                 (mode 16 16 :left :elide)
;;                 " "
;;                 (vc-status 16 16 :left)
;;                 " "
;;                 filename-and-process)))
;;   :config (add-hook 'ibuffer-hook (lambda()
;;                                     (ibuffer-vc-set-filter-groups-by-vc-root)
;;                                     (unless (eq ibuffer-sorting-mode 'alphabetic)
;;                                       (ibuffer-do-sort-by-alphabetic)))))


(use-package helm-hunks
  :ensure t
  :commands (helm-hunks
             helm-hunks-current-buffer
             helm-hunks-staged
             helm-hunks-staged-current-buffer))

(use-package paredit
  :ensure t
  :bind (:map paredit-mode-map
              ("M-s" . paredit-splice-sexp)
              ("M-S" . paredit-split-sexp)
              ("M-j" . paredit-join-sexps)

              ("M-o" . paredit-forward-down)
              ("M-O" . paredit-forward-up)
              ("M-u" . paredit-backward-down)
              ("M-U" . paredit-backward-up)

              ("M-l" . paredit-forward)
              ("M-h" . paredit-backward)
              ("M-k" . paredit-kill)
              ("M-(" . backward-barf-sexp)
              ("M-)" . forward-barf-sexp)
              ("C-(" . backward-slurp-sexp)
              ("C-)" . forward-slurp-sexp))
  :config
  ;; Some enhanced fns I found somewhere.
  (defun forward-barf-sexp (prefix)
    "Calls `paredit-forward-barf-sexp', unless PREFIX is non nil.
               With prefix it calls `paredit-barf-all-the-way-forward'"
    (interactive "P")
    (if prefix
        (paredit-barf-all-the-way-forward)
      (paredit-forward-barf-sexp)))

  (defun forward-slurp-sexp (prefix)
    "Calls `paredit-forward-slurp-sexp`, unless PREFIX is non nil.
               With prefix it calls `paredit-slurp-all-the-way-forward'"
    (interactive "P")
    (if prefix
        (paredit-slurp-all-the-way-forward)
      (paredit-forward-slurp-sexp)))

  (defun backward-barf-sexp (prefix)
    "Calls `paredit-backward-barf-sexp', unless PREFIX is non nil.
               With prefix it calls `paredit-barf-all-the-way-backward'"
    (interactive "P")
    (if prefix
        (paredit-barf-all-the-way-backward)
      (paredit-backward-barf-sexp)))

  (defun backward-slurp-sexp (prefix)
    "Calls `paredit-backward-slurp-sexp', unless PREFIX is non nil.
               With prefix it calls `paredit-slurp-all-the-way-backward'"
    (interactive "P")
    (if prefix
        (paredit-slurp-all-the-way-backward)
      (paredit-backward-slurp-sexp)))

  ;; Enable Paredit in the minibuffer.
  (defvar paredit-minibuffer-commands '(eval-expression
                                        pp-eval-expression
                                        eval-expression-with-eldoc
                                        ibuffer-do-eval
                                        ibuffer-do-view-and-eval)
    "Interactive commands for which paredit should be enabled in the minibuffer.")

  (add-hook 'minibuffer-setup-hook
            (lambda ()
              (if (memq this-command paredit-minibuffer-commands)
                  (enable-paredit-mode)))))


(use-package company
  :ensure t
  :diminish company-mode
  :init
  (setq company-minimum-prefix-length 2
        company-selection-wrap-around t
        company-idle-delay 0
        company-tooltip-align-annotations t)
  :config
  (add-hook 'after-init-hook 'global-company-mode))

(use-package diff-hl
  :ensure t
  :init
  (setq diff-hl-side 'right)
  (add-hook 'prog-mode-hook 'diff-hl-flydiff-mode)
  (add-hook 'prog-mode-hook 'diff-hl-margin-mode)
  ;; (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))
;; (use-package git-gutter-fringe
;;   :ensure t
;;   :defer 5
;;   :diminish git-gutter-mode
;;   :config (global-git-gutter-mode))

(use-package dashboard
  :ensure t
  :config
  (setq show-week-agenda-p t)
  (setq dashboard-items '((recents . 15) (projects . 5) (agenda . 5)))
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-startup-banner 3)
  (setq dashboard-center-content t)
  (setq dashboard-navigator-buttons
        `(;; line1
          ((,(all-the-icons-octicon "mark-github" :height 1.1 :v-adjust 0.0)
            "Homepage"
            "Browse homepage"
            (lambda (&rest _) (browse-url "homepage")))
           ("★" "Star" "Show stars" (lambda (&rest _) (show-stars)) warning)
           ("?" "" "?/h" #'show-help nil "<" ">"))
          ;; line 2
          ((,(all-the-icons-faicon "linkedin" :height 1.1 :v-adjust 0.0)
            "Linkedin"
            ""
            (lambda (&rest _) (browse-url "homepage")))
           ("⚑" nil "Show flags" (lambda (&rest _) (message "flag")) error))))
  (setq dashboard-banner-logo-title "emacs")
  (dashboard-setup-startup-hook)
  )
;; (defvar --backup-directory "~/.backups")
;; (if (not (file-exists-p --backup-directory))
;;         (make-directory --backup-directory t))
;; (setq backup-directory-alist `(("." . ,--backup-directory)))
(setq make-backup-files t               ; backup of a file the first time it is saved.
      backup-by-copying t               ; don't clobber symlinks
      version-control t                 ; version numbers for backup files
      delete-old-versions t             ; delete excess backup files silently
      delete-by-moving-to-trash t
      kept-old-versions 6               ; oldest versions to keep when a new numbered backup is made (default: 2)
      kept-new-versions 9               ; newest versions to keep when a new numbered backup is made (default: 2)
      auto-save-default t               ; auto-save every buffer that visits a file
      auto-save-timeout 20              ; number of seconds idle time before auto-save (default: 30)
      auto-save-interval 200            ; number of keystrokes between auto-saves (default: 300)
      vc-make-backup-files t
      )
(setq backup-directory-alist '(("" . "~/.emacs.d/backup/per-save")))

(defun force-backup-of-buffer ()
  ;; Make a special "per session" backup at the first save of each
  ;; emacs session.
  (when (not buffer-backed-up)
    ;; Override the default parameters for per-session backups.
    (let ((backup-directory-alist '(("" . "~/.emacs.d/backup/per-session")))
          (kept-new-versions 3))
      (backup-buffer)))
  ;; Make a "per save" backup on each save.  The first save results in
  ;; both a per-session and a per-save backup, to keep the numbering
  ;; of per-save backups consistent.
  (let ((buffer-backed-up nil))
    (backup-buffer)))

(add-hook 'before-save-hook  'force-backup-of-buffer)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))


(defmacro csetq (variable value)
  `(funcall (or (get ',variable 'custom-set) 'set-default) ',variable ,value))
;;** decorations
(csetq tool-bar-mode nil)
(csetq menu-bar-mode nil)
(csetq scroll-bar-mode nil)
(csetq truncate-lines t)
(csetq inhibit-startup-screen t)
(setq initial-scratch-message "")
(csetq text-quoting-style 'grave)
(csetq line-number-display-limit-width 2000000)
;;** navigation within buffer
(csetq next-screen-context-lines 5)
(csetq recenter-positions '(top middle bottom))
(csetq ring-bell-function 'ignore)
(csetq highlight-nonselected-windows nil)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq kill-buffer-query-functions nil)
;;** internals
(csetq gc-cons-threshold (* 10 1024 1024))
(csetq ad-redefinition-action 'accept)
;;** time display
(csetq display-time-24hr-format t)
(csetq display-time-default-load-average nil)
(csetq display-time-format "")
(add-hook 'prog-mode-hook 'linum-mode)
(setq linum-format "%4d \u2502 ")
(toggle-frame-maximized)
(setq inhibit-startup-screen t)
(global-hl-line-mode)
(show-paren-mode t)
(setq show-paren-delay 0
      show-paren-style 'parenthesis)
(setq uniquify-buffer-name-style 'post-forward)

(use-package keybinds
  :load-path "init")

(use-package golang
  :load-path "init")
