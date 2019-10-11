
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

(use-package general
  :ensure t
  :config
  (progn
    (general-evil-setup t)
    (defvar zzh-global-leader "SPC")
    (defvar zzh-second-leader-key "/")
    (general-define-key
     :states '(normal visual motion)
     :prefix zzh-global-leader
     "" nil
     ;; buffer
     "b" '(:ignore t :which-key "Buffer")
     "bb" '(helm-mini :which-key "buffers list")
     "bn" '(new-empty-buffer :which-key "new buffer")
     "bd" '(kill-buffer-and-window :which-key "kill buffer")
     "bf" '(ranger :which-key "open file")
     ;;git
     "g" '(:ignore t :wk "git")
     "gs" '(magit-status :wk "status")
     ;;org
     "o" '(:ignore t :which-key "Org")
     "oc" '(org-capture :which-key "capture")
     "oa" '(org-agenda :which-key "agenda")
     "ot" '(org-todo-list :which-key "todo-list")
     ;;project
     "p" '(:ignore t :which-key "Project")
     "pp" '(helm-projectile-switch-project :wk "project list")
     "pa" '(projectile-add-known-project :wk "add project")
     "ps" '(helm-projectile-rg :wk "search in project")
     ;;search
     "f" '(:ignore t :which-key "Search")
     "ff" '(helm-swoop :wk "search in buffer")
     "fs" '(helm-rg :wk "search in directory")
     "fp" '(helm-rg :wk "search in directory")
     ;;windows
     "w" '(:ignore t :which-key "Windows")
     "wo" '(switch-window :which-key "switch window")
     "wd" '(delete-window :which-key "delete window")
     "wD" '(ace-delete-window :which-key "delete other")
     "ws" '(evil-window-vsplit :wk "split v")
     "wv" '(evil-window-split :wk "split")
     ;; others
     "s" '(helm-swoop :which-key "swoop")
     "j" '(ranger :which-key "jump")
     "\\" '(eshell :wk "shell")
     "0" '(treemacs :which-key "treemacs")
     ";" '(evilnc-comment-or-uncomment-lines :which-key "Comment")
     "r" '(helm-mini :which-key "Recent")
     "v" '(er/expand-region :wk "expand")
     "TAB" '("SPC r RET" :which-key "Last buffer")
     "SPC" '(helm-M-x :which-key "Command")
     "q" '(save-buffers-kill-terminal :which-key "Quit"))

    (general-define-key
     :states '(normal visual motion)
     :keymaps 'org-mode-map
     ",," 'org-capture-finalize
     "tc" 'org-toggle-checkbox
     "tt" 'org-todo)

    (general-define-key
     :states '(normal visual)
     :keymaps 'go-mode-map
     ";" 'lsp-find-definition
     "r" 'lsp-find-references
     "s" 'lsp-find-implementation
     ;;TODO 跳到或者生成test
     ",t" 'go-gen-test-dwim
     ",m" 'lsp-ui-imenu
     ",f" 'go-fill-struct)

    (defvar my-keys-minor-mode-map (make-keymap) "my-minor-mode keymap")

    (general-define-key
     :states '(insert visual)
     :keymaps 'my-keys-minor-mode-map)
    ;; "TAB" 'tab-to-tab-stop)

    (general-define-key
     :states '(normal visual motion)
     :keymaps 'my-keys-minor-mode-map
     "C-j" 'evil-jump-forward
     "C-k" 'evil-jump-backward
     "S-SPC" 'set-mark-command)

    (define-minor-mode my-keys-minor-mode
      "A minor mode so that my key settings override annoying major modes."
      t " ZZH" 'my-keys-minor-mode-map)))

(defun new-empty-buffer ()
  "Opens a new empty buffer."
  (interactive)
  (switch-to-buffer (generate-new-buffer "untitled"))
  (funcall 'org-mode)
  (put 'buffer-offer-save 'permanent-local t)
  (setq buffer-offer-save t))




; START TABS CONFIG
;; Create a variable for our preferred tab width
 (setq custom-tab-width 2)

;; Two callable functions for enabling/disabling tabs in Emacs
(defun disable-tabs () (setq indent-tabs-mode nil))
(defun enable-tabs  ()
  (local-set-key (kbd "TAB") 'tab-to-tab-stop)
  (setq indent-tabs-mode t)
  (setq tab-width custom-tab-width))

;; Hooks to Enable Tabs
(add-hook 'prog-mode-hook 'enable-tabs)
;; Hooks to Disable Tabs
(add-hook 'lisp-mode-hook 'disable-tabs)
(add-hook 'emacs-lisp-mode-hook 'disable-tabs)

;; Language-Specific Tweaks
(setq-default python-indent-offset custom-tab-width) ;; Python
(setq-default js-indent-level custom-tab-width)      ;; Javascript

;; Making electric-indent behave sanely
(setq-default electric-indent-inhibit t)

;; Make the backspace properly erase the tab instead of
;; removing 1 space at a time.
(setq backward-delete-char-untabify-method 'hungry)

;; (OPTIONAL) Shift width for evil-mode users
;; For the vim-like motions of ">>" and "<<".
(setq-default evil-shift-width custom-tab-width)

;; WARNING: This will change your life
;; (OPTIONAL) Visualize tabs as a pipe character - "|"
;; This will also show trailing characters as they are useful to spot.
(setq whitespace-style '(face tabs tab-mark trailing))
(custom-set-faces
 '(whitespace-tab ((t (:foreground "#636363")))))
(setq whitespace-display-mappings
  '((tab-mark 9 [124 9] [92 9]))) ; 124 is the ascii ID for '\|'
(global-whitespace-mode) ; Enable whitespace mode everywhere
  ; END TABS CONFIG


(global-set-key (kbd "C->") 'indent-rigidly-right-to-tab-stop)
(global-set-key (kbd "C-<") 'indent-rigidly-left-to-tab-stop)

(provide 'keybinds)
  
