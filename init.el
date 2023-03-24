;; NOTE: init.el is now generated from Emacs.org.  Please edit that file
;;       in Emacs and init.el will be generated automatically!

;; You will most likely need to adjust this font size for your system!
(defvar efs/default-font-size 120)
(defvar efs/default-variable-font-size 100)

;; Make frame transparency overridable
(defvar efs/frame-transparency '(100 . 100))

;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                     (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)

;; Initialize package sources
  (require 'package)
  (setq package-archives
        '(("GNU ELPA"	. "https://elpa.gnu.org/packages/")
          ("Melpa"        . "https://melpa.org/packages/") 
          ("Melpa Stable" . "https://stable.melpa.org/packages/")))
  (setq   package-archive-priorities
          '(("Melpa"        .  0)
            ("GNU ELPA"	 .  5) 
            ("Melpa Stable" .  10)
            ))
;;  (package-initialize)
;;  (unless package-archive-contents (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
;;  (unless (package-installed-p 'use-package) (package-install 'use-package))

  (require 'use-package)
  (setq use-package-always-ensure t)

  (add-to-list 'load-path "~/.emacs.d/elpa/org-contrib-0.4.1")
  ;; (add-to-list 'org-export-backends 'taskjuggler)

(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "09:00"))

;; NOTE: If you want to move everything out of the ~/.emacs.d folder
;; reliably, set `user-emacs-directory` before loading no-littering!
;(setq user-emacs-directory "~/.cache/emacs")

(use-package no-littering)

;; no-littering doesn't set this by default so we must place
;; auto save files in the same path as it uses for sessions
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 50)        ; Give some breathing room

(menu-bar-mode -1)            ; Disable the menu bar

;; Set up the visible bell
(setq visible-bell t)

(column-number-mode)
(global-display-line-numbers-mode t)

;; Set frame transparency
(set-frame-parameter (selected-frame) 'alpha efs/frame-transparency)
(add-to-list 'default-frame-alist `(alpha . ,efs/frame-transparency))
;;  (set-frame-parameter (selected-frame) 'fullscreen 'maximized)
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(setq-default fill-column 80)

(set-face-attribute 'default nil :font "Fira Code Retina" :height efs/default-font-size)
;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Fira Code Retina" :height efs/default-font-size)
;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Cantarell" :height efs/default-variable-font-size :weight 'regular)

(unless (package-installed-p 'yasnippet)
  (package-install 'yasnippet))
(require 'yasnippet)

(unless (package-installed-p 'yasnippet-snippets)
  (package-install 'yasnippet-snippets))
(require 'yasnippet-snippets)

(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"                 ;; personal snippets
        ))
(yas-global-mode 1)

(use-package undo-tree
  :init
  (global-undo-tree-mode 1))

(use-package command-log-mode
  :commands command-log-mode)

(use-package doom-themes
  :init
  (load-theme 'doom-palenight t)
  ;; (load-theme 'doom-challenger-deep)
  :config
  (doom-themes-treemacs-config)
  (doom-themes-org-config)
  )

(use-package all-the-icons)

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(use-package which-key
  :defer 
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :after ivy)

(use-package counsel
  :bind (("C-M-j" . 'counsel-switch-buffer)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
  (counsel-mode 1))

(use-package ivy-prescient
  :after counsel
  :custom
  (ivy-prescient-enable-filtering nil)
  :config
  ;; Uncomment the following line to have sorting remembered across sessions!
  (prescient-persist-mode 1)
  (ivy-prescient-mode 1))

(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                5000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

;; (use-package treemacs-evil
;;   :after (treemacs evil)
;;   :ensure t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
  :after (treemacs persp-mode) ;;or perspective vs. persp-mode
  :ensure t
  :config (treemacs-set-scope-type 'Perspectives))

(use-package treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
  :after (treemacs)
  :ensure t
  :config (treemacs-set-scope-type 'Tabs))

(use-package hydra
  :defer t)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

;; (efs/leader-keys
;;  "ts" '(hydra-text-scale/body :which-key "scale text"))

(defun efs/org-mode-setup ()
  (org-indent-mode)
  ;; (variable-pitch-mode 1)
  ;; (visual-line-mode 1)
  )

(use-package org
  :mode (("\\.org$" . org-mode))
  :bind
  (("\C-cl" . org-store-link)
  ("\C-ca" . org-agenda)
  ("\C-cb" . org-iswitchb))
  :commands (org-capture org-agenda)
  :hook (org-mode . efs/org-mode-setup)
  :config
  (efs/org-font-setup)
  (setq org-ellipsis " ▾")
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

  (setq org-directory "/mnt/c/Users/cbollinger/Daten/org-system/org-mode")
  (setq org-default-notes-file "/mnt/c/Users/cbollinger/Daten/org-system/org-mode/refile/refile.org")
  (setq org-agenda-files (quote ("/mnt/c/Users/cbollinger/Daten/org-system/org-mode/refile"
				 "/mnt/c/Users/cbollinger/Daten/org-system/org-mode/private"
				 "/mnt/c/Users/cbollinger/Daten/org-system/org-mode/gnu-software"
				 "/mnt/c/Users/cbollinger/Daten/org-system/org-mode/duagon/General"
				 "/mnt/c/Users/cbollinger/Daten/org-system/org-mode/duagon/Clients"
				 "/mnt/c/Users/cbollinger/Daten/org-system/org-mode/duagon/Products")))
  (setq org-todo-keywords
	(quote ((sequence "TODO(t)" "NEXT(n)" "ONGOING(o)" "|" "DONE(d)")
		(sequence "EC(C)" "RFEW(0)" "RFEX(1)" "G2(2)" "G2.1(3)" "G2.2(4)" "G3(5)" "|" "Abnahme(6)")
		(sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))

  (setq org-todo-keyword-faces
	(quote (("TODO"      :foreground "red"          :weight bold)
		("NEXT"      :foreground "blue"         :weight bold)
		("ONGOING"   :foreground "yellow"       :weight bold)
		("DONE"      :foreground "forest green" :weight bold)

		("EC"        :foreground "red"          :weight bold)
		("RFEW"      :foreground "blue"         :weight bold)
		("RFEX"      :foreground "magenta"      :weight bold)
		("G2"        :foreground "magenta"      :weight bold)
		("G2.1"      :foreground "yellow"       :weight bold)
		("G2.2"      :foreground "brown"        :weight bold)
		("G3"        :foreground "forest green" :weight bold)
		("Abnahme"   :foreground "green"        :weight bold)

		("WAITING"   :foreground "orange"       :weight bold)
		("HOLD"      :foreground "magenta"      :weight bold)
		("CANCELLED" :foreground "forest green" :weight bold)
		("MEETING"   :foreground "forest green" :weight bold)
		("PHONE"     :foreground "forest green" :weight bold))))

  (setq org-todo-state-tags-triggers
	(quote (("CANCELLED" ("CANCELLED" . t))
		("WAITING" ("WAITING" . t))
		("HOLD" ("WAITING") ("HOLD" . t))
		("DONE" ("WAITING") ("HOLD"))
		("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
		("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
		("ONGOING" ("WAITING") ("CANCELLED") ("HOLD"))
		("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

					;Targets include this file and any file contributing to the agenda - up to 9 levels deep
  (setq org-refile-targets (quote ((nil :maxlevel . 9)
				   (org-agenda-files :maxlevel . 9))))

					;Save Org buffers after refiling!
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  (setq org-tag-alist (quote ((:startgroup)
			      ("Projekte" . ?P)
			      (:grouptags)
			      ("D521_PDM" . ?a)
			      ("D522_BT" . ?b)
			      ("D522_NLD" . ?c)
			      ("RemoteIO" . ?c)
			      (:endgroup)
			      (:startgroup)
			      ("Private" . ?V)
			      (:grouptags)
			      ("Training" . ?t)
			      ("DSP" . ?d)
			      ("NOTE" . ?n)
			      ("ORG" . ?o)
			      ("PERSONAL" . ?p)
			      (:endgroup)
			      ("FLAGGED" . ??))))

					;Configure custom agenda views
  (setq org-agenda-custom-commands
	'(
	  ("d" "Dashboard" ((agenda "" ((org-deadline-warning-days 7)))
			    (todo "NEXT"               ((org-agenda-overriding-header "Next Tasks")))
			    (todo "ONGOING"            ((org-agenda-overriding-header "All ongoing Tasks")))
			    (todo "WAITING"            ((org-agenda-overriding-header "All Tasks external waiting")))
			    (todo "HOLD"               ((org-agenda-overriding-header "All Tasks on hold")))
			    (todo "TODO"               ((org-agenda-overriding-header "All Todo Tasks")))
			    (todo "CANCELLED"          ((org-agenda-overriding-header "Project CANCELLED")))
			    (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

	  ("c" "EC-Overview" ((agenda "" ((org-deadline-warning-days 7)))
			      (todo "EC"                   ((org-agenda-overriding-header "EC Planned")))
			      (todo "RFEW"                 ((org-agenda-overriding-header "RFEW: Request for Work signed")))
			      (todo "RFEX"                 ((org-agenda-overriding-header "RFEX: Request for Execution signed")))
			      (todo "G2"                   ((org-agenda-overriding-header "G2: Planning done")))
			      (todo "G2.1"                 ((org-agenda-overriding-header "G2.1: Development done")))
			      (todo "G2.2"                 ((org-agenda-overriding-header "G2.2: G2 Validation done")))
			      (todo "G3"                   ((org-agenda-overriding-header "G3: G3 Validation done")))
			      (todo "Abnahme"              ((org-agenda-overriding-header "Abnahmeprotokoll unterschrieben")))
			      (tags-todo "agenda/ACTIVE"   ((org-agenda-overriding-header "Active Projects")))))

	  ("n" "Agenda and all TODOs" ((agenda "") (alltodo "")))

	  ("x" "Next Tasks"
	   ((todo "NEXT" ((org-agenda-overriding-header "Next Tasks")))))

	  ("w" "Workflow Status"
	   ((todo "WAIT"
		  ((org-agenda-overriding-header "Waiting on External")
		   (org-agenda-files org-agenda-files)))
	    (todo "REVIEW"
		  ((org-agenda-overriding-header "In Review")
		   (org-agenda-files org-agenda-files)))
	    (todo "PLAN"
		  ((org-agenda-overriding-header "In Planning")
		   (org-agenda-todo-list-sublevels nil)
		   (org-agenda-files org-agenda-files)))
	    (todo "BACKLOG"
		  ((org-agenda-overriding-header "Project Backlog")
		   (org-agenda-todo-list-sublevels nil)
		   (org-agenda-files org-agenda-files)))
	    (todo "READY"
		  ((org-agenda-overriding-header "Ready for Work")
		   (org-agenda-files org-agenda-files)))
	    (todo "ACTIVE"
		  ((org-agenda-overriding-header "Active Projects")
		   (org-agenda-files org-agenda-files)))
	    (todo "COMPLETED"
		  ((org-agenda-overriding-header "Completed Projects")
		   (org-agenda-files org-agenda-files)))
	    (todo "CANCELlED"
		  ((org-agenda-overriding-header "Cancelled Projects")
		   (org-agenda-files org-agenda-files)))))

	  ("N" "Notes" tags "NOTE"
	   ( (org-agenda-overriding-header "Notes") (org-tags-match-list-sublevels t)))

	  ("h" "Habits" tags-todo "STYLE=\"habit\""
	   ((org-agenda-overriding-header "Habits")
	    (org-agenda-sorting-strategy
	     '(todo-state-down effort-up category-keep))))
	  ))

					;I use C-c c to start capture mode
  (global-set-key (kbd "C-c c") 'org-capture)
  (setq org-capture-templates
	(quote (("t" "todo" entry (file "/mnt/c/Users/cbollinger/Daten/org-system/org-mode/refile/todo.org")
		 "* TODO [#A] %?\n%U\n%a\n" :clock-in t :clock-resume t)
		("r" "respond" entry (file "/mnt/c/Users/cbollinger/Daten/org-system/org-mode/refile/refile.org")
		 "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
		("n" "note" entry (file "/mnt/c/Users/cbollinger/Daten/org-system/org-mode/refile/note.org")
		 "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
		("j" "Journal" entry (file+datetree "/mnt/c/Users/cbollinger/Daten/org-system/org-mode/refile/journal.org")
		 "* %?\n%U\n" :clock-in t :clock-resume t :tree-type month)
		("w" "org-protocol" entry (file "/mnt/c/Users/cbollinger/Daten/org-system/org-mode/refile/refile.org")
		 "* TODO Review %c\n%U\n" :immediate-finish t)
		("m" "Meeting" entry (file "/mnt/c/Users/cbollinger/Daten/org-system/org-mode/refile/meeting.org")
		 "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
		("p" "Phone call" entry (file "/mnt/c/Users/cbollinger/Daten/org-system/org-mode/refile/phone.org")
		 "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
		("h" "Habit" entry (file "/mnt/c/Users/cbollinger/Daten/org-system/org-mode/refile/habit.org")
		 "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n"))))

					;Allow setting single tags without the menu
  (setq org-fast-tag-selection-single-key (quote expert))
					;For tag searches ignore tasks with scheduled and deadline dates
  (setq org-agenda-tags-todo-honor-ignore-options t)
					;Spell checker
					;flyspell mode for spell checking everywhere
  (add-hook 'org-mode-hook 'turn-on-flyspell 'append)

					;Setting up spell checking with multiple dictionaries
  (with-eval-after-load "ispell"
    ;;Configure `LANG`, otherwise ispell.el cannot find a 'default
    ;;dictionary' even though multiple dictionaries will be configured
    ;;in next line.
    (setenv "LANG" "en_US.UTF-8")
    (setq ispell-program-name "hunspell")
    ;;Configure German, Swiss German, and two variants of English.
    (setq ispell-dictionary "de_CH,en_GB,en_US")
    ;;ispell-set-spellchecker-params has to be called
    ;;before ispell-hunspell-add-multi-dic will work
    (ispell-set-spellchecker-params)
    (ispell-hunspell-add-multi-dic "de_CH,en_GB,en_US")
    ;;For saving words to the personal dictionary, don't infer it from
    ;;the locale, otherwise it would save to ~/.hunspell_de_DE.
    (setq ispell-personal-dictionary "~/.hunspell_personal"))

					;Place tags close to the right-hand side of the window
  (add-hook 'org-finalize-agenda-hook 'place-agenda-tags)
  (defun place-agenda-tags ()
    "Put the agenda tags by the right border of the agenda window."
    (setq org-agenda-tags-column (- 4 (window-width)))
    (org-agenda-align-tags))

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; org-mode agenda options                                                ;;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;open agenda in current window
  (setq org-agenda-window-setup (quote current-window))
  ;;warn me of any deadlines in next 7 days
  (setq org-deadline-warning-days 7)
  ;;show me tasks scheduled or due in next fortnight
  (setq org-agenda-span (quote fortnight))
  ;;don't show tasks as scheduled if they are already shown as a deadline
  (setq org-agenda-skip-scheduled-if-deadline-is-shown t)
  ;;don't give awarning colour to tasks with impending deadlines
  ;;if they are scheduled to be done
  (setq org-agenda-skip-deadline-prewarning-if-scheduled (quote pre-scheduled))
  ;;don't show tasks that are scheduled or have deadlines in the
  ;;normal todo list
  (setq org-agenda-todo-ignore-deadlines (quote all))
  (setq org-agenda-todo-ignore-scheduled (quote all))
  ;;sort tasks in order of when they are due and then by priority
  (setq org-agenda-sorting-strategy
	(quote
	 ((agenda deadline-up priority-down)
	  (todo priority-down category-keep)
	  (tags priority-down category-keep)
	  (search category-keep))))

  ;; Disable keys in org-mode
  ;;    C-c [
  ;;    C-c ]
  ;;    C-c ;
  ;;    C-c C-x C-q  cancelling the clock (we never want this)
  (add-hook 'org-mode-hook
	    '(lambda ()
	       ;; Undefine C-c [ and C-c ] since this breaks my
	       ;; org-agenda files when directories are include It
	       ;; expands the files in the directories individually
	       (org-defkey org-mode-map "\C-c[" 'undefined)
	       (org-defkey org-mode-map "\C-c]" 'undefined)
	       (org-defkey org-mode-map "\C-c;" 'undefined)
	       (org-defkey org-mode-map "\C-c\C-x\C-q" 'undefined))
	    'append)


  ;; Download the sound at https://freesound.org/people/.Andre_Onate/sounds/484665/
  (setq org-clock-sound "~/.emacs.d/wav/mixkit-slot-machine-win-siren-1929.wav")
  )

(defun efs/org-font-setup ()
    ;; Replace list hyphen with dot
    (font-lock-add-keywords 'org-mode
                            '(("^ *\\([-]\\) "
                               (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set face for org
   (set-face-attribute 'org-document-title nil :font "Iosevka Etoile" :weight 'bold :height 2.0)
   ;; Set faces for heading levels
    (dolist (face '((org-level-1 . 1.4)
                    (org-level-2 . 1.3)
                    (org-level-3 . 1.2)
                    (org-level-4 . 1.1)
                    (org-level-5 . 1.1)
                    (org-level-6 . 1.1)
                    (org-level-7 . 1.1)
                    (org-level-8 . 1.1)))
      (set-face-attribute (car face) nil :font "Iosevka Etoile" :weight 'medium :height (cdr face)))



    ;; Ensure that anything that should be fixed-pitch in Org files appears that way
    (set-face-attribute 'org-block nil    :foreground nil :inherit 'fixed-pitch)
    (set-face-attribute 'org-table nil    :inherit 'fixed-pitch)
    (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
    (set-face-attribute 'org-code nil     :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-table nil    :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
    (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
    (set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch)
    (set-face-attribute 'line-number nil :inherit 'fixed-pitch)
    (set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch))

;; Get rid of the background on column views
;; (set-face-attribute 'org-column-title nil :background "light gray")
;; (set-face-attribute 'org-column face nil :height 180 :width normal)
;; (set-face-attribute 'org-column nil :background "light gray" :foreground "dark red")

(require 'org-habit)
(add-to-list 'org-modules 'org-habit)
(setq org-habit-graph-column 60)
;; This turns the habit display on again at 6AM each morning. 
(run-at-time "06:00" 86400 '(lambda () (setq org-habit-show-habits t)))

(use-package org-attach-screenshot
  :bind ("<f6> s" . org-attach-screenshot)
  :config (setq org-attach-screenshot-dirfunction
		(lambda () 
		  (progn (cl-assert (buffer-file-name))
			 (concat (file-name-sans-extension (buffer-file-name))
				 "-att")))
		org-attach-screenshot-command-line "gnome-screenshot -a -f %f"))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(require 'ox-latex)
(require 'ob-js)
(require 'color)

;; (unless (package-installed-p 'ob-ipython)
;;   (package-install 'ob-ipython))
;; (require 'ob-ipython)

(setq org-startup-shrink-all-tables t)
(setq org-startup-folded t)
(setq org-hide-block-startup t)

;; Make babel results blocks lowercase
(setq org-babel-results-keyword "results")

;; Do not ask when evaluating source code blocks
(defun bh/display-inline-images ()
  (condition-case nil
      (org-display-inline-images)
    (error nil)))

;; Highlight coloring export of source code block export
(add-to-list 'org-latex-packages-alist '("" "minted"))
(setq org-latex-listings 'minted)
(setq org-src-fontify-natively t)

(setq org-ditaa-jar-path "~/usr/share/ditaa/ditaa.jar")
(setq org-plantuml-jar-path "/usr/share/plantuml/plantuml.jar")
;; Use fundamental mode when editing plantuml blocks with C-c '
(add-to-list 'org-src-lang-modes (quote ("plantuml" . fundamental)))
(add-to-list 'exec-path "/usr/bin/magick")
(use-package gnuplot
  :init
)

(with-eval-after-load 'org
      (org-babel-do-load-languages
       'org-babel-load-languages
       '((emacs-lisp . t)    ; 
         (C          . t)    ; C, C++, D
         (js         . t)    ; JavaScript
         (org        , t)    ;
         (ditaa      . t)    ; ditaa
         (shell      . t)    ; shell, bash
         (lisp       . t)    ; lisp
         (latex      . t)    ; latex
         (octave     . t)    ; octave
         (gnuplot    . t)    ; gnuplot
         (python     . t)    ; pyhon
         (plantuml   . t)))  ; this line activate plantuml

      (push '("conf-unix" . conf-unix) org-src-lang-modes))

    ;; Use python lexer for ipython blocks
;;  (ipython     . t)   ; pyhon
;;  (setq python-shell-interpreter "python3")
;;  (add-to-list 'org-latex-minted-langs '(ipython "python"))  

  ;; Do not prompt to confirm evaluation
  ;; This may be dangerous - make sure you understand the consequences
  ;; of setting this -- see the docstring for details
  (setq org-confirm-babel-evaluate nil)

(unless (package-installed-p 'ox-reveal)
  (package-install 'ox-reveal))
(require 'ox-reveal)
(setq ox-reveal-always-ensure t)
(setq org-reveal-root "file:///home/christian/Daten/reveal.js")
(setq Org-Reveal-title-slide nil)

(use-package hide-mode-line
  :ensure t)

(defun my/org-tree-slide-setup ()
  (interactive)
  (org-display-inline-images)
  (hide-mode-line-mode 1)
  (setq text-scale-mode-amount 3)
  (text-scale-mode 1))

(defun my/org-tree-slide-end ()
  (interactive)
  (org-display-inline-images)
  (hide-mode-line-mode 0)
  (text-scale-mode 0)
  (org-tree-slide-mode 0))

(use-package org-tree-slide
  :ensure t
  :defer t
  :custom
  (org-image-actual-width nil)
  (org-tree-slide-activate-message "Presentation started!")
  (org-tree-slide-deactivate-message "Presentation finished!")
  :hook ((org-tree-slide-play . my/org-tree-slide-setup)
         (org-tree-slide-stop . my/org-tree-slide-end))
  :bind (:map org-tree-slide-mode-map
              ("<f6>" . org-tree-slide-move-previous-tree)
              ("<f7>" . org-tree-slide-move-next-tree)
              ("<f8>" . org-tree-slide-content)))

(defun dw/org-present-prepare-slide ()
  (org-overview)
  (org-show-entry)
  (org-show-children))

(defun dw/org-present-hook ()
  (setq-local face-remapping-alist '((default (:height 1.5) variable-pitch)
                                     (header-line (:height 4.5) variable-pitch)
                                     (org-code (:height 1.55) org-code)
                                     (org-verbatim (:height 1.55) org-verbatim)
                                     (org-block (:height 1.25) org-block)
                                     (org-block-begin-line (:height 0.7) org-block)))
  (setq header-line-format " ")
  (org-display-inline-images)
  (dw/org-present-prepare-slide))

(defun dw/org-present-quit-hook ()
  (setq-local face-remapping-alist '((default variable-pitch default)))
  (setq header-line-format nil)
  (org-present-small)
  (org-remove-inline-images))

(defun dw/org-present-prev ()
  (interactive)
  (org-present-prev)
  (dw/org-present-prepare-slide))

(defun dw/org-present-next ()
  (interactive)
  (org-present-next)
  (dw/org-present-prepare-slide))

(use-package org-present
  :bind (:map org-present-mode-keymap
         ("C-c C-j" . dw/org-present-next)
         ("C-c C-k" . dw/org-present-prev))
  :hook ((org-present-mode . dw/org-present-hook)
         (org-present-mode-quit . dw/org-present-quit-hook)))

(with-eval-after-load 'org
  ;; This is needed as of Org 9.2
  (require 'org-tempo)

  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python")))

;; Automatically tangle our Emacs.org config file when we save it
(defun efs/org-babel-tangle-config ()
  (when (string-equal (file-name-directory (buffer-file-name))
                      (expand-file-name user-emacs-directory))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))
(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'efs/org-babel-tangle-config)))

; Clocking Functions

(setq bh/keep-clock-running nil)

(defun bh/clock-in-to-next (kw)
  "Switch a task from TODO to NEXT when clocking in. Skips capture tasks, projects, and subprojects. Switch projects and subprojects from NEXT back to TODO"
  (when (not (and (boundp 'org-capture-mode) org-capture-mode))
    (cond
     ((and (member (org-get-todo-state) (list "TODO"))
           (bh/is-task-p))
      "NEXT")
     ((and (member (org-get-todo-state) (list "NEXT"))
           (bh/is-project-p))
      "TODO"))))

(defun bh/find-project-task ()
  "Move point to the parent (project) task if any"
  (save-restriction
    (widen)
    (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
      (while (org-up-heading-safe)
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq parent-task (point))))
      (goto-char parent-task)
      parent-task)))

(defun bh/punch-in (arg)
  "Start continuous clocking and set the default task to the selected task. If no task is selected set the Organization task as the default task."
  (interactive "p")
  (setq bh/keep-clock-running t)
  (if (equal major-mode 'org-agenda-mode)
      ;;
      ;; We're in the agenda
      ;;
      (let* ((marker (org-get-at-bol 'org-hd-marker))
             (tags (org-with-point-at marker (org-get-tags-at))))
        (if (and (eq arg 4) tags)
            (org-agenda-clock-in '(16))
          (bh/clock-in-organization-task-as-default)))
    ;;
    ;; We are not in the agenda
    ;;
    (save-restriction
      (widen)
                                        ; Find the tags on the current task
      (if (and (equal major-mode 'org-mode) (not (org-before-first-heading-p)) (eq arg 4))
          (org-clock-in '(16))
        (bh/clock-in-organization-task-as-default)))))

(defun bh/punch-out ()
  (interactive)
  (setq bh/keep-clock-running nil)
  (when (org-clock-is-active)
    (org-clock-out))
  (org-agenda-remove-restriction-lock))

(defun bh/clock-in-default-task ()
  (save-excursion
    (org-with-point-at org-clock-default-task
      (org-clock-in))))

(defun bh/clock-in-parent-task ()
  "Move point to the parent (project) task if any and clock in"
  (let ((parent-task))
    (save-excursion
      (save-restriction
        (widen)
        (while (and (not parent-task) (org-up-heading-safe))
          (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
            (setq parent-task (point))))
        (if parent-task
            (org-with-point-at parent-task
              (org-clock-in))
          (when bh/keep-clock-running
            (bh/clock-in-default-task)))))))

(defvar bh/organization-task-id "2cbef41d-71da-4e1f-b161-e827513fa0ae")

(defun bh/clock-in-organization-task-as-default ()
  (interactive)
  (org-with-point-at (org-id-find bh/organization-task-id 'marker)
    (org-clock-in '(16))))

(defun bh/clock-out-maybe ()
  (when (and bh/keep-clock-running
             (not org-clock-clocking-in)
             (marker-buffer org-clock-default-task)
             (not org-clock-resolving-clocks-due-to-idleness))
    (bh/clock-in-parent-task)))


(add-hook 'org-clock-out-hook 'bh/clock-out-maybe 'append)

(require 'ox-latex)
    ;; Latex search path
    (setq exec-path (append exec-path '("/usr/share/texmf")))

    (with-eval-after-load 'tex
      (add-to-list 'safe-local-variable-values
                   '(TeX-command-extra-options . "-shell-escape")))

    ;;Allow reference to figures e.g. [@fig:label]
    (setq org-latex-prefer-user-labels t)

    ;; KDE Setting  
    ;; Make org aware of the tex enginge
    ;; -8bit option avoids undifined white space characters in minted code blocks
    (setq org-latex-pdf-process
          '("xelatex -8bit -shell-escape -interaction nonstopmode -output-directory %o %f"
            "xelatex -8bit -shell-escape -interaction nonstopmode -output-directory %o %f"))


    ;; (setq org-latex-pdf-process
    ;;       '("lualatex -shell-escape -interaction nonstopmode %f"
    ;;         "lualatex -shell-escape -interaction nonstopmode %f"))

    ;; (setq org-latex-pdf-process
    ;;    '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
    ;;      "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
    ;;      "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))


    '(org-preview-latex-process-alist
      (quote
       (
        (dvipng      :programs ("lualatex" "dvipng")
                     :description "dvi > png"
                     :message "you need to install the programs: latex and dvipng."
                     :image-input-type "dvi"
                     :image-output-type "png"
                     :image-size-adjust (1.0 . 1.0)
                     :latex-compiler ("lualatex -output-format dvi -interaction nonstopmode -output-directory %o %f")
                     :image-converter ("dvipng -fg %F -bg %B -D %D -T tight -o %O %f"))

        (dvisvgm     :programs ("latex" "dvisvgm")
                     :description "dvi > svg"
                     :message "you need to install the programs: latex and dvisvgm."
                     :use-xcolor t
                     :image-input-type "xdv"
                     :image-output-type "svg"
                     :image-size-adjust (1.7 . 1.5)
                     :latex-compiler ("xelatex -no-pdf -interaction nonstopmode -output-directory %o %f")
                     :image-converter ("dvisvgm %f -n -b min -c %S -o %O"))

        (imagemagick :programs ("latex" "convert")
                     :description "pdf > png"
                     :message "you need to install the programs: latex and imagemagick."
                     :use-xcolor t
                     :image-input-type "pdf"
                     :image-output-type "png"
                     :image-size-adjust (1.0 . 1.0)
                     :latex-compiler ("xelatex -no-pdf -interaction nonstopmode -output-directory %o %f")
                     :image-converter ("convert -density %D -trim -antialias %f -quality 100 %O")))))

(eval-after-load "ox-latex"
  '(add-to-list 'org-latex-classes
                `("beamer"
                  ,(concat "\\documentclass[presentation]{beamer}\n"
                           "[DEFAULT-PACKAGES]"
                           "[PACKAGES]"
                           "[EXTRA]\n")
                  ("\\section{%s}" . "\\section*{%s}")
                  ("\\subsection{%s}" . "\\subsection*{%s}")
                  ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))


  (with-eval-after-load "ox-latex"
    (add-to-list 'org-latex-classes
              '("beamer" "\\documentclass[presentation]{beamer}
    "
                   ("\\section{%s}" . "\\section*{%s}")
                   ("\\subsection{%s}" . "\\subsection*{%s}")
                   ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))

    (with-eval-after-load "ox-latex"
      (add-to-list 'org-latex-classes
                   '("koma-article" "\\documentclass{scrartcl}
           "
                     ("\\section{%s}"       . "\\section{%s}")
                     ("\\subsection{%s}"    . "\\subsection{%s}")
                     ("\\subsubsection{%s}" . "\\subsubsection{%s}")
                     ("\\paragraph{%s}"     . "\\paragraph{%s}")
                     ("\\subparagraph{%s}"  . "\\subparagraph{%s}"))))



    (with-eval-after-load "ox-latex"
      (add-to-list 'org-latex-classes
                   '("koma-report" "\\documentclass{scrreprt}
           "
                     ("\\chapter{%s}"       . "\\chapter{%s}")
                     ("\\section{%s}"       . "\\section{%s}")
                     ("\\subsection{%s}"    . "\\subsection{%s}")
                     ("\\subsubsection{%s}" . "\\subsubsection{%s}")
                     ("\\paragraph{%s}"     . "\\paragraph{%s}")
                     ("\\subparagraph{%s}"  . "\\subparagraph{%s}"))))


    (with-eval-after-load "ox-latex"
      (add-to-list 'org-latex-classes
                   '("dg_public" "\\documentclass{duagon_public}
           "
                     ("\\section{%s}" . "\\section{%s}")
                     ("\\subsection{%s}" . "\\subsection{%s}")
                     ("\\subsubsection{%s}" . "\\subsubsection{%s}")
                     ("\\paragraph{%s}" . "\\paragraph{%s}")
                     ("\\subparagraph{%s}" . "\\subparagraph{%s}"))))

(use-package lsp-mode
    :diminish "L"
    :commands (lsp lsp-deferred)
    :init (setq lsp-keymap-prefix "C-p p"
                lsp-enable-file-watchers nil
                lsp-enable-on-type-formatting nil
                lsp-enable-snippet nil
                lsp-lens-enable nil)
    :config
    (lsp-enable-which-key-integration t)
    (setq read-process-output-max (* 1024 1024))
)

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

(use-package lsp-treemacs
  :after lsp
  :commands lsp-treemacs-references)

(use-package lsp-ivy
  :after lsp)

(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

(use-package python-mode
    :ensure nil
    :hook (python-mode . lsp-deferred)
    :custom
    ;; NOTE: Set these if Python 3 is called "python3" on your system!
    (python-shell-interpreter "python")
    (dap-python-executable "python")
    (dap-python-debugger 'debugpy)
    :config
     (require 'dap-python)
)

(setq c-default-style "linux")
(setq c-basic-offset 4)

(use-package ccls
    :init (setq ccls-sem-highlight-method 'font-lock)
    :hook ((c-mode c++-mode objc-mode cuda-mode) . (lambda () (require 'ccls) (lsp-deferred)))
)

;; JavaScript
;; JavaScript: MinorMode
(unless (package-installed-p 'js2-mode)
  (package-install 'js2-mode))
(require 'js2-mode)
;; (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
;; ;; Better imenu
;; (add-hook 'js2-mode-hook #'js2-imenu-extras-mode)

;; JavaScript: Refactor Package
(unless (package-installed-p 'js2-refactor)
  (package-install 'js2-refactor))
(require 'js2-refactor)
(unless (package-installed-p 'xref-js2)
  (package-install 'xref-js2))

;; JavaScript: Jumping to function definitions
(require 'xref-js2)
(add-hook 'js2-mode-hook #'js2-refactor-mode)
(js2r-add-keybindings-with-prefix "C-c C-r")
(define-key js2-mode-map (kbd "C-k") #'js2r-kill)
;; js-mode (which js2 is based on) binds "M-." which conflicts with xref, so
;; unbind it.
(define-key js-mode-map (kbd "M-.") nil)

(add-hook 'js2-mode-hook (lambda ()
                           (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))

;; JavaScript: Debugging aid
(unless (package-installed-p 'sourcemap)
  (package-install 'sourcemap))
(require 'sourcemap)
(setq coffee-args-compile '("-c" "-m")) ;; generating sourcemap file
(add-hook 'coffee-after-compile-hook 'sourcemap-goto-corresponding-point)


;; JavaScript: Debugging Mode and REPL
(unless (package-installed-p 'indium)
   (package-install 'indium))
(require 'indium)
(add-hook 'js-mode-hook #'indium-interaction-mode)

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

;; (use-package company-box
;;   :hook (company-mode . company-box-mode))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/Projects/Code")
    (setq projectile-project-search-path '("~/Projects/Code")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :after projectile
  :config (counsel-projectile-mode))

(use-package magit
  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; NOTE: Make sure to configure a GitHub token before using this package!
;; - https://magit.vc/manual/forge/Token-Creation.html#Token-Creation
;; - https://magit.vc/manual/ghub/Getting-Started.html#Getting-Started
(use-package forge
  :after magit)

(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package term
  :commands term
  :config
  (setq explicit-shell-file-name "bash") ;; Change this to zsh, etc
  ;;(setq explicit-zsh-args '())         ;; Use 'explicit-<shell>-args for shell-specific args

  ;; Match the default Bash shell prompt.  Update this if you have a custom prompt
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *"))

(use-package eterm-256color
  :hook (term-mode . eterm-256color-mode))

(use-package vterm
  :commands vterm
  :config
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")  ;; Set this to match your custom shell prompt
  ;;(setq vterm-shell "zsh")                       ;; Set this to customize the shell to launch
  (setq vterm-max-scrollback 10000))

(when (eq system-type 'windows-nt)
  (setq explicit-shell-file-name "powershell.exe")
  (setq explicit-powershell.exe-args '()))

(defun efs/configure-eshell ()
  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  ;; Bind some useful keys for evil-mode
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-r") 'counsel-esh-history)
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "<home>") 'eshell-bol)
  (evil-normalize-keymaps)

  (setq eshell-history-size         10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t))

(use-package eshell-git-prompt
  :after eshell)

(use-package eshell
  :hook (eshell-first-time-mode . efs/configure-eshell)
  :config

  (with-eval-after-load 'esh-opt
    (setq eshell-destroy-buffer-when-process-dies t)
    (setq eshell-visual-commands '("htop" "zsh" "vim")))

  (eshell-git-prompt-use-theme 'powerline))

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom ((dired-listing-switches "-agho --group-directories-first"))
;;   :config
;;   (evil-collection-define-key 'normal 'dired-mode-map
;;     "h" 'dired-single-up-directory
;;     "l" 'dired-single-buffer)
  )

(use-package dired-single
  :commands (dired dired-jump))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-open
  :commands (dired dired-jump)
  :config
  ;; Doesn't work as expected!
  ;;(add-to-list 'dired-open-functions #'dired-open-xdg t)
  (setq dired-open-extensions '(("png" . "feh")
                                ("mkv" . "mpv"))))

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  ;; :config
  ;; (evil-collection-define-key 'normal 'dired-mode-map
  ;;   "H" 'dired-hide-dotfiles-mode)
  )

(use-package json-mode
  :ensure t
)

(use-package request)
(use-package json)

(use-package restclient
  :ensure t
  :mode (("\\.http\\'" . restclient-mode))
   :bind (:map restclient-mode-map
               ("C-c C-f" . json-mode-beautify)))

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))

;; To make this directory available to Emacs 
(add-to-list 'Info-additional-directory-list "~/.local/share/info")

;; Configure Elfeed
(use-package elfeed
  :ensure t
  :config
  (setq elfeed-db-directory (expand-file-name "elfeed" user-emacs-directory)
        elfeed-show-entry-switch 'display-buffer)
  :bind
  ("C-x w" . elfeed ))

(use-package elfeed-org
  :ensure t
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files (list "/mnt/c/Users/cbollinger/Daten/org-system/org-mode/refile/elfeed.org"))
)
