;; ;; fix the PATH variable
;; (defun set-exec-path-from-shell-PATH ()
;;   (let ((path-from-shell (shell-command-to-string "TERM=vt100 $SHELL -i -c 'echo $PATH'")))
;;     (setenv "PATH" path-from-shell)
;;     (setq exec-path (split-string path-from-shell path-separator))))
;; (when window-system (set-exec-path-from-shell-PATH))


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

(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns x))
  :config
  (exec-path-from-shell-initialize))

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
   org-log-done 'note
   ;; create a logbook entry
   org-log-into-drawer t
   ;; org directory and agenda files
   org-directory "~/org"
   org-agenda-files (list org-directory)
   org-agenda-span 'day
   org-startup-indented t
   ;; org keywords and faces
   org-todo-keywords '((sequence "TODO(t)" "DOING(i)" "WAITING(b@/!)" "|" "DONE(d)" "ABORTED(a@/!)")
                       (sequence "IDEA(z)" "IDEAING(x)" "|" "IDEAED(c@/!)")
                       (sequence "TASK(v)" "TASKING(b)" "|" "TASKED(n@/!)")
                       (sequence  "|" "ABORTED(a@/!)")
                       )
   ;; dracula-themed faces
   org-todo-keyword-faces (quote
                           (("TODO" :foreground "#ff5555" :weight bold)
                            ("DOING" :foreground "#8be9fd" :weight bold)
                            ("DONE" :foreground "#50fa7b" :weight bold)
                            ("WAITING" :foreground "#ffb86c" :weight bold)
                            ("IDEA"  :foreground "#f5ce42" :weight bold)
                            ("TASKING"  :foreground "#42f56f" :weight bold)
                            ("TASKED"  :foreground "#50fa7b" :weight bold)
                            ("TASK"  :foreground "#d93c1c" :weight bold)
                            ("IDEAING"  :foreground "#246142" :weight bold)
                            ("IDEAED"  :foreground "#50fa7b" :weight bold)
                            ("ABORTED" :foreground "#5274a3" :weight bold)))
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
  (set-face-attribute 'org-level-2 nil :height 1.0)
  (setq org-agenda-time-grid
        (quote
         ((today)
          (900 1100 1300 1500 1700)
          "......" "----------------"))))

(use-package org-bullets
  :ensure t
  :after org
  :init
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; (use-package org-habit
;;   :ensure t)

;; (use-package org-super-agenda
;;   :ensure t
;;   :after org
;;   :hook ((org-agenda-mode . org-super-agenda-mode))
;;   :init
;;   (require 'org-habit)
;;   (setq ((org-super-agenda-groups
;;           '(;; Each group has an implicit boolean OR operator between its selectors.
;;             (:name "Today"  ; Optionally specify section name
;;                    :time-grid t  ; Items that appear on the time grid
;;                    :todo "TODAY")  ; Items that have this TODO keyword
;;             (:name "Important"
;;                    ;; Single arguments given alone
;;                    :tag "bills"
;;                    :priority "A")
;;             ;; Set order of multiple groups at once
;;             (:order-multi (2 (:name "Shopping in town"
;;                                     ;; Boolean AND group matches items that match all subgroups
;;                                     :and (:tag "shopping" :tag "@town"))
;;                              (:name "Food-related"
;;                                     ;; Multiple args given in list with implicit OR
;;                                     :tag ("food" "dinner"))
;;                              (:name "Personal"
;;                                     :habit t
;;                                     :tag "personal")
;;                              (:name "Space-related (non-moon-or-planet-related)"
;;                                     ;; Regexps match case-insensitively on the entire entry
;;                                     :and (:regexp ("space" "NASA")
;;                                                   ;; Boolean NOT also has implicit OR between selectors
;;                                                   :not (:regexp "moon" :tag "planet")))))
;;             ;; Groups supply their own section names when none are given
;;             (:todo "WAITING" :order 8)  ; Set order of this section
;;             (:todo ("SOMEDAY" "TO-READ" "CHECK" "TO-WATCH" "WATCHING")
;;                    ;; Show this group at the end of the agenda (since it has the
;;                    ;; highest number). If you specified this group last, items
;;                    ;; with these todo keywords that e.g. have priority A would be
;;                    ;; displayed in that group instead, because items are grouped
;;                    ;; out in the order the groups are listed.
;;                    :order 9)
;;             (:priority<= "B"
;;                          ;; Show this section after "Today" and "Important", because
;;                          ;; their order is unspecified, defaulting to 0. Sections
;;                          ;; are displayed lowest-number-first.
;;                          :order 1)
;;             ;; After the last group, the agenda will display items that didn't
;;             ;; match any of these groups, with the default order position of 99
;;             )))
;;         (org-agenda nil "a")))
;; ;; (use-package ibuffer-vc
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

(custom-set-faces
 '(default ((t (:background "black" :foreground "#137D11")))))

(if (featurep 'cocoa)
    (progn
      ;; 在Mac平台, Emacs不能进入Mac原生的全屏模式,否则会导致 `make-frame' 创建时也集成原生全屏属性后造成白屏和左右滑动现象.
      ;; 所以先设置 `ns-use-native-fullscreen' 和 `ns-use-fullscreen-animation' 禁止Emacs使用Mac原生的全屏模式.
      ;; 而是采用传统的全屏模式, 传统的全屏模式, 只会在当前工作区全屏,而不是切换到Mac那种单独的全屏工作区,
      ;; 这样执行 `make-frame' 先关代码或插件时,就不会因为Mac单独工作区左右滑动产生的bug.
      ;;
      ;; Mac平台下,不能直接使用 `set-frame-parameter' 和 `fullboth' 来设置全屏,
      ;; 那样也会导致Mac窗口管理器直接把Emacs窗口扔到单独的工作区, 从而对 `make-frame' 产生同样的Bug.
      ;; 所以, 启动的时候通过 `set-frame-parameter' 和 `maximized' 先设置Emacs为最大化窗口状态, 启动5秒以后再设置成全屏状态,
      ;; Mac就不会移动Emacs窗口到单独的工作区, 最终解决Mac平台下原生全屏窗口导致 `make-frame' 左右滑动闪烁的问题.
      (setq ns-use-native-fullscreen nil)
      (setq ns-use-fullscreen-animation nil)

      ;; 默认先最大化。
      (set-frame-parameter (selected-frame) 'fullscreen 'maximized)

      (run-at-time "2sec" nil
                   (lambda ()
                     (toggle-frame-fullscreen)
                     ))
      ))
(transient-mark-mode 1)                 ;标记高亮
(setq mouse-yank-at-point t)            ;粘贴于光标处,而不是鼠标指针处
(setq default-major-mode 'text-mode)    ;设置默认地主模式为TEXT模式
(setq inhibit-compacting-font-caches t) ;使用字体缓存，避免卡顿
(setq frame-resize-pixelwise t) ;设置缩放的模式,避免Mac平台最大化窗口以后右边和下边有空隙
;; 不显示 *scratch*
(defun remove-scratch-buffer ()
  (if (get-buffer "*scratch*")
      (kill-buffer "*scratch*")))
(add-hook 'after-change-major-mode-hook 'remove-scratch-buffer)

(use-package keybinds
  :load-path "init")

(use-package golang
  :load-path "init")
