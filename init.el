;; NOTE: init.el is now generated from Emacs.org.  Please edit that file
;;       in Emacs and init.el will be generated automatically!

;; You will most likely need to adjust this font size for your system!
(defvar chb/default-font-size 140)
(defvar chb/default-variable-font-size 140)

;; Make frame transparency overridable
(defvar chb/frame-transparency '(100 . 100))

;; Dial the GC threshold back down so that garbage collection happens more
;; frequently but in less time.
(setq gc-cons-threshold (* 50 1000 1000))

(defun chb/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                     (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'chb/display-startup-time)

;; Initialize package sources
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")))

(setq package-archive-priorities
      '(("melpa"        .  4)
        ("gnu"          .  3) 
        ("org"          .  2)
        ("nongnu" .        1)
        ("melpa-stable".   0)))


(package-initialize)

(setq warning-suppress-types '((comp)))

;; Alternatively, for more specific suppression:
(setq warning-suppress-types '((comp) (docstring)))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)
(add-to-list 'load-path "~/.emacs.d/elpa/org-contrib-0.6")

(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
    (auto-package-update-at-time "09:00"))

(use-package transient
    :defer t)

(use-package undo-tree
  :init
  (global-undo-tree-mode 1)
  (setq undo-tree-auto-save-history nil)
)

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

;; (use-package dired-single
;;   :commands (dired dired-jump))

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
(set-frame-parameter (selected-frame) 'alpha chb/frame-transparency)
(add-to-list 'default-frame-alist `(alpha . ,chb/frame-transparency))
;;  (set-frame-parameter (selected-frame) 'fullscreen 'maximized)
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(turn-on-auto-fill)
(setq-default fill-column 70)

(set-face-attribute 'default nil :font "Fira Code Retina" :height chb/default-font-size)
;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Fira Code Retina" :height chb/default-font-size)
;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Cantarell" :height chb/default-variable-font-size :weight 'regular)

(use-package nerd-icons
:if (display-graphic-p))

;; Load and configure doom-modeline
(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :init
  (setq doom-modeline-height 45)
  (setq doom-modeline-icons t)
  (setq doom-modeline-major-mode-color-icon t)
  (setq doom-modeline-time-icon t)
  (setq doom-modeline-time t)
  (setq doom-modeline-minor-modes nil)
  ;; Ensure doom-modeline faces are available
  :config (doom-modeline-mode 1)
  ;; Increase modeline width
  (setq doom-modeline-bar-width 5) ;; Adjust this value as needed

  ;; Modify segments to show essential information
  (setq doom-modeline-buffer-file-name-style 'truncate-upto-root) ;; Truncate long file names

  ;; Add or remove segments based on your preference
  (setq doom-modeline-buffer-modification-icon t) ;; Show modified indicator
  (setq doom-modeline-major-mode-icon t) ;; Show major mode icon
  (setq doom-modeline-vcs-max-length 12) ;; Limit length of VCS branch name
   )

(use-package doom-themes
  :ensure t
  :after (ivy org doom-modeline)
  :config (load-theme 'doom-palenight t))

(use-package command-log-mode
  :commands command-log-mode)

(use-package ivy
  :ensure t
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
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package counsel
  :ensure t
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :config
  (counsel-mode 1))

(use-package ivy-youtube
    :config
    (setq ivy-youtube-key "")
     (global-set-key (kbd "C-c y") 'ivy-youtube)
     (setq ivy-youtube-play-at "browser")
     (global-set-key (kbd "C-c c") 'org-capture)
     ;; set chrome as browser
     ;; (setq browse-url-browser-function 'browse-url-generic)
     ;; (setq browse-url-generic-program "google-chrome-open-url")
)

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

(use-package vertico
:ensure t
:init
(vertico-mode))

(defun chb/org-font-setup ()
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
   (set-face-attribute 'org-block nil :foreground 'unspecified :inherit 'fixed-pitch)
   (set-face-attribute 'org-table nil    :inherit 'fixed-pitch)
   (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
   (set-face-attribute 'org-code nil     :inherit '(shadow fixed-pitch))
   (set-face-attribute 'org-table nil    :inherit '(shadow fixed-pitch))
   (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
   (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
   (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
   (set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch)
   (set-face-attribute 'line-number nil :inherit 'fixed-pitch)
   (set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch)

   (set-face-attribute 'org-column nil
               :inherit 'default  ;; Inherit default face, not org-level-* faces
               :height 1.0        ;; Ensure uniform text height
               :weight 'normal    ;; Set uniform weight
               :underline nil     ;; No underline
               :box nil           ;; No box around text
               :background "#d0e4f5"  ;; Softer light blue-gray background
               :foreground "#005b96") ;; Medium blue foreground

   ;; Customize the face of the Org mode title
   (set-face-attribute 'org-document-title nil
                       :foreground "white"
                       :background "midnight blue"
                       :weight 'bold
                       :height 2.5)

)

;; ------------------------------
;; Org & Multi-dictionary Hunspell Setup
;; ------------------------------

;; --- Custom Org setup function ---
(defun chb/org-mode-setup ()
  "Custom Org-mode setup."
  (org-indent-mode 1))

;; --- Ensure phaf is never loaded ---
(setq ispell-phaf-enabled nil)
(when (featurep 'ispell-phaf)
  (unload-feature 'ispell-phaf t))

;; --- Hunspell setup ---
(setq ispell-program-name "hunspell")
(setq ispell-really-hunspell t)
(setq ispell-personal-dictionary "~/.hunspell_personal")

(setq ispell-local-dictionary-alist
      '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" t ("-d" "en_US") nil utf-8)
        ("en_GB" "[[:alpha:]]" "[^[:alpha:]]" "[']" t ("-d" "en_GB") nil utf-8)
        ("de_CH" "[[:alpha:]]" "[^[:alpha:]]" "[']" t ("-d" "de_CH") nil utf-8)
        ("multi-de-en" "[[:alpha:]]" "[^[:alpha:]]" "[']" t ("-d" "de_CH,en_GB,en_US") nil utf-8)))

;; Default dictionary for other modes
(setq ispell-dictionary "en_US")

;; --- Org-mode configuration ---
(use-package org
  :ensure t
  :mode ("\\.org$" . org-mode)
  :bind (("C-c l" . org-store-link)
         ("C-c b" . org-iswitchb))
  :hook (org-mode . chb/org-mode-setup)
  :config
  (chb/org-font-setup)
  (chb/org-mode-setup)
  (setq org-ellipsis " ▾"
        org-log-done 'time
        org-log-into-drawer t
        org-fast-tag-selection-single-key 'expert
        org-agenda-tags-todo-honor-ignore-options t
        org-clock-sound "~/.emacs.d/wav/mixkit-slot-machine-win-siren-1929.wav")
  (unbind-key "\C-c[" org-mode-map)
  (unbind-key "\C-c]" org-mode-map)
  (unbind-key "\C-c;" org-mode-map)
  (unbind-key "\C-c\C-x\C-q" org-mode-map))

;; --- Flycheck-Aspell Setup ---
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package flycheck-aspell
  :ensure t
  :after flycheck
  :config
  ;; Set multi-dictionary for Org/Text/LaTeX
  (setq flycheck-aspell-dictionary "multi-de-en"))

;; Enable Flycheck-Aspell in Org/Text/LaTeX
(add-hook 'org-mode-hook #'flycheck-mode)
(add-hook 'text-mode-hook #'flycheck-mode)
(add-hook 'latex-mode-hook #'flycheck-mode)

;; Programming modes: English only
(defun my/prog-mode-flycheck-setup ()
  "Use English dictionary for programming modes."
  (when (derived-mode-p 'prog-mode)
    (setq flycheck-aspell-dictionary "en_US")
    (flycheck-mode 1)))

(add-hook 'prog-mode-hook #'my/prog-mode-flycheck-setup)

(setq org-hide-emphasis-markers t)

(defface my-org-emphasis-bold
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :foreground "#a60000")
    (((class color) (min-colors 88) (background dark))
     :foreground "#ff8059"))
  "My bold emphasis for Org.")

(defface my-org-emphasis-italic
  '((default :inherit italic)
    (((class color) (min-colors 88) (background light))
     :foreground "#005e00")
    (((class color) (min-colors 88) (background dark))
     :foreground "#44bc44"))
  "My italic emphasis for Org.")

(defface my-org-emphasis-underline
  '((default :inherit underline)
    (((class color) (min-colors 88) (background light))
     :foreground "#813e00")
    (((class color) (min-colors 88) (background dark))
     :foreground "#d0bc00"))
  "My underline emphasis for Org.")

(defface my-org-emphasis-strike-through
  '((((class color) (min-colors 88) (background light))
     :strike-through "#972500" :foreground "#505050")
    (((class color) (min-colors 88) (background dark))
     :strike-through "#ef8b50" :foreground "#a8a8a8"))
  "My strike-through emphasis for Org.")

(setq org-emphasis-alist
      '(("*" my-org-emphasis-bold)
        ("/" my-org-emphasis-italic)
        ("_" my-org-emphasis-underline)
        ("=" org-verbatim verbatim)
        ("~" org-code verbatim)
        ("+" (:strike-through t))))

(use-package org
  :bind ("\C-ca" . org-agenda)  
  :commands org-agenda
  :config
  (setq org-agenda-start-with-log-mode nil)                          
  (setq org-agenda-window-setup (quote current-window))              ;; open agenda in current window
  (setq org-deadline-warning-days 1)                                 ;; warn me of any deadlines in next 7 days
  (setq org-agenda-span (quote week))                                ;; show me tasks scheduled or due in next week, fortnight
  (setq org-agenda-skip-scheduled-if-deadline-is-shown t)            ;; don't show tasks as scheduled if they are already shown as a deadline
  (setq org-agenda-skip-deadline-prewarning-if-scheduled             ;; don't give awarning colour to tasks with impending deadlines
        (quote pre-scheduled))                                       ;; if they are scheduled to be done

  (setq org-agenda-todo-ignore-deadlines (quote all))                ;; don't show tasks that are scheduled or have deadlines in the
  (setq org-agenda-todo-ignore-scheduled (quote all))                ;; normal todo list

  (add-hook 'org-finalize-agenda-hook 'place-agenda-tags)            ;; Place tags close to the right-hand side of the window
  (defun place-agenda-tags ()
    "Put the agenda tags by the right border of the agenda window."
    (setq org-agenda-tags-column (/(* 2 (window-width)) 4 ))
    (org-agenda-align-tags))



;; Legt die Sortierstrategie für verschiedene Org-Agenda-Ansichten fest.
(setq org-agenda-sorting-strategy
	 '(
	   ;; In der Agenda-Ansicht (d.h. Wochen-/Tagesübersicht)
	   ;; - Zuerst nach Uhrzeit aufsteigend sortieren (time-up)
	   ;; - Dann nach Priorität absteigend (A vor B vor C) (priority-down)
	   ;; - Dann nach Kategorie (Dateiname bzw. org-agenda-files Quelle) in Originalreihenfolge (category-keep)
	   (agenda time-up priority-down category-keep)

	   ;; In der TODO-Ansicht (z.B. über `org-agenda-todo`)
	   ;; - Zuerst nach Uhrzeit aufsteigend sortieren
	   ;; - Dann nach Priorität absteigend
	   ;; - Dann nach Kategorie in Originalreihenfolge
	   (todo time-up priority-down category-keep)
	   
	   ;; In der TAGS-Suche (z.B. über `org-tags-view`)
	   ;; - Gleiches Sortierverhalten wie oben
	   (tags time-up priority-down category-keep)
	   
	   ;; In der Suchansicht (z.B. `org-search-view`)
	   ;; - Nur nach Kategorie sortieren (Originalreihenfolge beibehalten)
	   (search category-keep)
	   ))
     

    (setq org-agenda-files (quote ("~/Daten/04-org-system/01-private"
				   "~/Daten/04-org-system/02-refile"
                                   "~/Daten/04-org-system/03-gnu-software"
				   "~/Daten/04-org-system/04-elfeed"
				   "~/Daten/04-org-system/05-duagon/contracts")))

  (setq org-todo-keywords
        (quote ((sequence "TODO(t)" "ONGOING(o)" "RISK(r)" "MEETING(M)" "|" "DONE(d)" "CANCELLED(C)")
                (sequence "WP(W)" "WPon(O)" "|" "WPclose(C)")
                (sequence "RFW(0)" "ROM(1)" "PDP(2)" "REQ(3)" "ARCH(4)" "DESIGN(5)" "TEST(6)" "CLOSING(7)" "|" "CLOSED(8)")
                ;; (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING")
                )))

  (setq org-todo-keyword-faces
        (quote (("TODO"      :foreground "red"          :weight bold)
                ("MEETING"   :foreground "forest green" :weight bold)
                ("NEXT"      :foreground "blue"         :weight bold)
                ("ONGOING"   :foreground "blue"         :weight bold)
                ("RISK"      :foreground "yellow"       :weight bold)
                ("DONE"      :foreground "forest green" :weight bold)
                ("CANCELLED" :foreground "forest green" :weight bold)

                ("WP"        :foreground "blue"         :weight bold)
                ("WPon"      :foreground "yellow"       :weight bold)
                ("WPclose"   :foreground "brown"        :weight bold)

                ("RFW"       :foreground "red"          :weight bold)
                ("ROM"       :foreground "blue"         :weight bold)
                ("PDP"       :foreground "magenta"      :weight bold)
                ("REQ"       :foreground "magenta"      :weight bold)
                ("ARCH"      :foreground "yellow"       :weight bold)
                ("DESIGN"    :foreground "brown"        :weight bold)
                ("TEST"      :foreground "forest green" :weight bold)
                ("CLOSING"   :foreground "green"        :weight bold)
                ("CLOSED"    :foreground "brown"        :weight bold))))

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
          ("d" "Dashboard" ((agenda "" ((org-deadline-warning-days 1)))
                            (todo "MEETING"               ((org-agenda-overriding-header "Meeting")))
                            (todo "ONGOING"            ((org-agenda-overriding-header "All ongoing Action Items")))
                            (todo "WAITING"            ((org-agenda-overriding-header "Action Items, waiting for external input")))
                            (todo "HOLD"               ((org-agenda-overriding-header "Action Items on hold")))
                            (todo "TODO"               ((org-agenda-overriding-header "Action Itmes Backlog")))
                            (todo "CANCELLED"          ((org-agenda-overriding-header "Action Item CANCELLED")))
                            (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

          ("c" "EC-Overview" ((agenda "" ((org-deadline-warning-days 1)))
                              (todo "RISK"                 ((org-agenda-overriding-header "Risk Evaluation")))
                              (todo "RFW"                  ((org-agenda-overriding-header "RFW: Work Request")))
                              (todo "ROM"                  ((org-agenda-overriding-header "ROM: Rough Order Magnitude")))
                              (todo "PDP"                  ((org-agenda-overriding-header "PDP: Product Definition Process")))
                              (todo "REQ"                  ((org-agenda-overriding-header "REQ: Requirements Definition")))
                              (todo "ARCH"                 ((org-agenda-overriding-header "ARCH: Architecture Design")))
                              (todo "DESIGN"               ((org-agenda-overriding-header "DESIGN: Implementation")))
                              (todo "TEST"                 ((org-agenda-overriding-header "TEST: Type Test")))
                              (todo "CLOSING"              ((org-agenda-overriding-header "CLOSING: Prepartion Acceptance Protocol")))
                              (todo "CLOSED"               ((org-agenda-overriding-header "Contract Closed")))
                              (tags-todo "agenda/ACTIVE"   ((org-agenda-overriding-header "Active Projects")))))

          ("n" "Agenda and all TODOs" ((agenda "") (alltodo "")))

          ("N" "Notes" tags "NOTE"
           ( (org-agenda-overriding-header "Notes") (org-tags-match-list-sublevels t)))

          ("h" "Habits" tags-todo "STYLE=\"habit\""
           ((org-agenda-overriding-header "Habits")
            (org-agenda-sorting-strategy
             '(todo-state-down effort-up category-keep))))
          )))

(use-package org
    :commands org-capture
    :config
     (setq org-directory "~/Daten/04-org-system/")
     (setq org-default-notes-file "~/Daten/04-org-system/02-refile/refile.org")

                                           ;I use C-c c to start capture mode
     (global-set-key (kbd "C-c c") 'org-capture)
     (setq org-capture-templates
           (quote (("t" "todo" entry (file "~/Daten/04-org-system/02-refile/refile.org")
                    "* TODO [#A] %?\n%U\n%a\n" :clock-in t :clock-resume t)
                   ("r" "respond" entry (file "~/Daten/04-org-system/02-refile/refile.org")
                    "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
                   ("n" "note" entry (file "~/Daten/04-org-system/02-refile/note.org")
                    "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
                   ("j" "Journal" entry (file+datetree "~/Daten/04-org-system/02-refile/journal.org")
                    "* %?\n%U\n" :clock-in t :clock-resume t :tree-type month)
                   ("w" "org-protocol" entry (file "~/Daten/04-org-system/02-refile/refile.org")
                    "* TODO Review %c\n%U\n" :immediate-finish t)
                   ("m" "Meeting" entry (file "~/Daten/04-org-system/02-refile/refile.org")
                    "* MEETING %^{Subject} :MEETING:\n%^{When}t\n:LOGBOOK:\n:END:\n%?" :clock-in t :clock-resume t)
                   ("p" "Phone call" entry (file "~/Daten/04-org-system/02-refile/refile.org")
                    "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
                   ("h" "Habit" entry (file "~/Daten/04-org-system/02-refile/refile.org")
                    "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n"))))
)

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

(use-package ox-latex
    :ensure nil
    :after org
    :config

    ;; Default packages for all LaTeX exports (depending on engine)
    (setq org-latex-default-packages-alist
          '(("AUTO" "inputenc"      t ("pdflatex"))
            ("T1" "fontenc"         t ("pdflatex"))
            ("" "fontspec"          t ("lualatex" "xelatex"))
            ("" "hyperref"          t ("pdflatex" "lualatex" "xelatex"))
            ("table, dvipsnames, svgnames" "xcolor"       t ("pdflatex" "lualatex" "xelatex"))
  	  ("" "pifont"            t ("pdflatex" "lualatex" "xelatex"))
            ("tikz" "bclogo"        t ("pdflatex" "lualatex" "xelatex"))
            ("" "lipsum"            t ("pdflatex" "lualatex" "xelatex"))
            ("" "amssymb"           t ("pdflatex" "lualatex" "xelatex"))
            ("" "enumitem"          t ("pdflatex" "lualatex" "xelatex"))
            ("" "lastpage"          t ("pdflatex" "lualatex" "xelatex"))
            ("" "rotfloat"          t ("pdflatex" "lualatex" "xelatex"))
  	  ("" "minted"            t ("pdflatex" "lualatex" "xelatex"))
  	  ("" "mathtools"         t ("pdflatex" "lualatex" "xelatex"))
  	  ("" "unicode-math"      t ("pdflatex" "lualatex" "xelatex"))
  	  ("" "comment"           t ("pdflatex" "lualatex" "xelatex"))
  	  ("" "tcolorbox"            t ("pdflatex" "lualatex" "xelatex"))
  	  ("customcolors" "hf-tikz"  t ("pdflatex" "lualatex" "xelatex"))
  	  ("headsepline=true,footsepline=true" "scrlayer-scrpage"  t ("pdflatex" "lualatex" "xelatex"))
            ))

    ;; Use minted for source code blocks
    (setq org-latex-listings 'minted)

    ;; Set default LaTeX compiler to xelatex
    (setq org-latex-compiler "xelatex")

    ;; Paths & shell escape for minted
    (add-to-list 'exec-path "/usr/share/texmf")
    (with-eval-after-load 'tex
      (add-to-list 'safe-local-variable-values
                   '(TeX-command-extra-options . "-shell-escape")))

    ;; KOMA Report Class
    (add-to-list 'org-latex-classes
                 `("koma-report"
"%%
\\documentclass[fontsize=11pt,DIV=12,headings=big]{scrreprt}
\\AtBeginDocument{%%
\\setlength\\parindent{0pt}%%
\\setcounter{secnumdepth}{5}%%
\\setminted{fontsize=\\small,breaklines=true,autogobble=true}%%
\\pagestyle{scrheadings}%%
\\renewcommand{\\chaptermark}[1]{\\markboth{#1}{}}
\\renewcommand{\\sectionmark}[1]{\\markright{#1}}
\\ihead{\\leftmark}%%
\\ohead{}%%
\\ifoot*{Ch. Bollinger}%%
\\cfoot*{\\thepage}%%
\\ofoot*{\\today}%%
}%%
"
                   ("\\chapter{%s}"       . "\\chapter*{%s}")
                   ("\\section{%s}"       . "\\section*{%s}")
                   ("\\subsection{%s}"    . "\\subsection*{%s}")
                   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                   ("\\paragraph{%s}"     . "\\paragraph*{%s}")
                   ("\\subparagraph{%s}"  . "\\subparagraph*{%s}")))

    ;; KOMA Article Class
    (add-to-list 'org-latex-classes
                 `("koma-article"
"%%
\\documentclass[fontsize=11pt,DIV=12]{scrartcl}\n\
\\pagestyle{scrheadings}\n\
\\AtBeginDocument{%%
\\setlength\\parindent{0pt}%%
\\setcounter{secnumdepth}{5}%%
\\setminted{fontsize=\\small,breaklines=true,autogobble=true}%%
\\pagestyle{scrheadings}%%
\\renewcommand{\\sectionmark}[1]{\\markboth{#1}{}}%%
\\renewcommand{\\subsectionmark}[1]{\\markright{#1}}%%
\\ihead{\\leftmark}%%
\\ohead{\\rightmark}%%
\\ifoot*{Ch. Bollinger}%%
\\cfoot*{\\thepage}%%
\\ofoot*{\\today}%%
}%%
"
                   ("\\section{%s}"       . "\\section{%s}")
                   ("\\subsection{%s}"    . "\\subsection{%s}")
                   ("\\subsubsection{%s}" . "\\subsubsection{%s}")
                   ("\\paragraph{%s}"     . "\\paragraph{%s}")
                   ("\\subparagraph{%s}"  . "\\subparagraph{%s}")))

    ;; duagon Class
    (add-to-list 'org-latex-classes
                 '("dg_public"
              	 "
                    \\documentclass{duagon_public}
                  "
                   ("\\section{%s}"       . "\\section{%s}")
                   ("\\subsection{%s}"    . "\\subsection{%s}")
                   ("\\subsubsection{%s}" . "\\subsubsection{%s}")
                   ("\\paragraph{%s}"     . "\\paragraph{%s}")
                   ("\\subparagraph{%s}"  . "\\subparagraph{%s}")))

    ;; define the minted format
    (setq org-latex-header-extra "\\setminted{fontsize=\\\small,breaklines=true,autogobble=true}")

    ;; Override Org’s hypersetup so TOC links are blue & clickable
    (setq org-latex-hyperref-template
  	"\\hypersetup{
          pdfauthor={%a},
          pdftitle={%t},
          pdfkeywords={%k},
          pdfsubject={%d},
          pdfcreator={%c},
          pdflang={%L},
          colorlinks=true,
          linkcolor=blue,
          urlcolor=blue
        }")
    
    ;; PDF export process: use xelatex + shell escape
    (setq org-latex-pdf-process
          '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
            "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

    ;; Preview settings
    (setq org-preview-latex-default-process 'xelatex)
    (setq org-preview-latex-process-alist
          '((xelatex
             :programs ("xelatex" "dvisvgm")
             :description "xdv → svg"
             :message "You need to install: xelatex and dvisvgm."
             :image-input-type "xdv"
             :image-output-type "svg"
             :image-size-adjust (1.7 . 1.5)
             :latex-compiler ("xelatex -no-pdf -shell-escape -interaction nonstopmode -output-directory %o %f")
             :image-converter ("dvisvgm %f -n -b transparent -c %S -o %O"))))

    ;; Ensure preview temp directory exists
    (make-directory (concat temporary-file-directory "org-preview-latex") t))

;; You cannot have *both* active at the same time, because
;; `org-latex-format-headline-function` can only hold one function.
;; But you *can* switch between them depending on context.

;; Define both headline formatting functions
(defun my-org-latex-format-headline-function (todo todo-type priority text tags _info)
  "Default format function for a headline.
See `org-latex-format-headline-function' for details."
  (concat
   (and todo (format "{\\bfseries\\sffamily\\color{%s} %s} "
                     (pcase todo-type
                       ('todo "red")
                       ('done "green"))
                     todo))
   (and priority (format "\\framebox{\\#%c} " priority))
   text
   (and tags
        (format "\\hfill{}\\textsc{%s}"
                (mapconcat #'org-latex--protect-text tags ":")))))

(defun my-org-koma-latex-format-headline (todo todo-type priority text tags _info)
  "Format Org headlines for LaTeX export using KOMA classes."
  (concat
   (and todo
        (format "{\\bfseries\\color{%s}\\textsf{%s}} "
                (pcase todo-type
                  ('todo "red")
                  ('done "green")
                  (_ "black"))
                todo))
   (and priority
        (format "\\textsf{\\framebox{\\#%c}} " priority))
   text
   (and tags
        (format "\\hfill{}\\normalfont\\textsc{%s}"
                (mapconcat #'org-latex--protect-text tags ":")))))

;; Hook to set correct function before export
(defun my-org-set-headline-function-based-on-class (backend)
  (when (eq backend 'latex)
    (setq org-latex-format-headline-function
          (if (and org-latex-default-class
                   (string-match "koma" org-latex-default-class))
              #'my-org-koma-latex-format-headline
            #'my-org-latex-format-headline-function))))

(add-hook 'org-export-before-processing-hook #'my-org-set-headline-function-based-on-class)

(use-package org
  :ensure t
  :config
  ;; Load org-tempo for structure template expansion
  (require 'org-tempo)

  ;; Add commonly used templates
  (add-to-list 'org-structure-template-alist '("s" . "src"))
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("js" . "src javascript"))

  ;; Other Org-mode configurations
  (setq org-src-tab-acts-natively t)
  (setq org-confirm-babel-evaluate nil)

  ;; Set up org-tempo when entering org-mode
  (add-hook 'org-mode-hook
            (lambda ()
              (org-tempo-setup))))

;; Automatically tangle our Emacs.org config file when we save it
(defun chb/org-babel-tangle-config ()
  (when (string-equal (file-name-directory (buffer-file-name))
                      (expand-file-name user-emacs-directory))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))
(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'chb/org-babel-tangle-config)))

(use-package org
  :ensure org-contrib
  :after ox-taskjugger
  :config
  (add-to-list 'org-export-backends 'ox-taskjuggler)
  )

(use-package which-key
  :defer 
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))

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
;; (setq org-babel-results-keyword "results")

;; Do not ask when evaluating source code blocks
(defun bh/display-inline-images ()
  (condition-case nil
      (org-display-inline-images)
    (error nil)))

;: Ubuntu
;; (setq org-ditaa-jar-path "~/usr/share/ditaa/ditaa.jar")
;; (setq org-plantuml-jar-path "/usr/share/plantuml/plantuml.jar")
;; Opensuse
(setq org-ditaa-jar-path "/home/christian/bin/ditaa0_9/ditaa0_9.jar")
(setq org-plantuml-jar-path "/usr/bin/plantuml")
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
(setq python-shell-interpreter "python3")
;;  (add-to-list 'org-latex-minted-langs '(ipython "python"))  

  ;; Do not prompt to confirm evaluation
  ;; This may be dangerous - make sure you understand the consequences
  ;; of setting this -- see the docstring for details
  (setq org-confirm-babel-evaluate nil)

(use-package cl-lib
  :defer t)

  (use-package htmlize
      :ensure t
      :config
      ;; Suppress cl package obsolete warnings
      (setq byte-compile-warnings '(not cl-functions obsolete)))

(use-package lsp-mode
  :ensure t
  :hook (
         ((c-mode c++-mode) . lsp)
         ((js-mode js2-mode) . lsp)    
         ((typescript-mode web-mode) . lsp)
         )
  :commands lsp
  :commands (lsp lsp-deferred)
  :init (setq lsp-keymap-prefix "C-p p"
              lsp-enable-file-watchers nil
              lsp-enable-on-type-formatting nil
              lsp-enable-snippet nil
              lsp-lens-enable t)
  :config
  (setq lsp-prefer-flymake nil) ;; Use flycheck instead of flymake
)

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-use-childframe t
        lsp-ui-doc-position 'at-point
        lsp-ui-doc-include-signature t
        lsp-ui-sideline-enable t
        lsp-ui-sideline-ignore-duplicate t))

;; Indium -- JavaScript: Debugging Mode and REPL
(use-package indium
  :ensure t
  :hook ((js-mode . indium-interaction-mode)
         (js2-mode . indium-interaction-mode))
  ;;indium-chrome-port 13840
  :config ;; (setq indium-verbosity "debug") ;; or "verbose"
           (define-key indium-interaction-mode-map (kbd "C-c C-r") 'indium-repl)
           (define-key indium-interaction-mode-map (kbd "C-c C-d") 'indium-debugger)

           (add-hook 'indium-connected-hook
                     (lambda ()
                      (message "Indium connected.")))
 )

(use-package typescript-mode
  :mode ("\\.ts\\'" . typescript-mode)
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

;; js2-mode for enhanced JavaScript editing
(use-package js2-mode
  :ensure t
  :mode ("\\.js\\'" . js2-mode)
  :config
  (setq js2-basic-offset 2
        js2-bounce-indent-p nil))

;; xref-js2 for better jump-to-definition
(use-package xref-js2
  :ensure t
  :after js2-mode
  :hook (js2-mode . (lambda ()
                     (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))
  :config (define-key js2-mode-map (kbd "M-.") nil))

;; js2-refactor for JavaScript refactoring
(use-package js2-refactor
  :ensure t
  :after js2-mode
  :hook (js2-mode . js2-refactor-mode)
  :config
  (js2r-add-keybindings-with-prefix "C-c C-r"))

(use-package js2-refactor
  :ensure t
  :after js2-mode
  :config
  ;; Bind js2-refactor keybindings
  (js2r-add-keybindings-with-prefix "C-c C-r"))

;; For example, unbind M-. in js-mode
(add-hook 'js2-mode-hook (lambda ()
   (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))

;; JavaScript: Debugging aid
(use-package sourcemap
  :ensure t
  :config
  (setq coffee-args-compile '("-c" "-m")) ;; generating sourcemap file
  (add-hook 'coffee-after-compile-hook 'sourcemap-goto-corresponding-point))

(use-package pyvenv
  :after python-mode
  :config
  (pyvenv-mode 1))

(setq c-default-style "linux")
(setq c-basic-offset 4)

(use-package lsp-clangd
  :ensure lsp-mode
  :hook ((c-mode . (lambda () (require 'lsp-clangd) (lsp)))
         (c++-mode . (lambda () (require 'lsp-clangd) (lsp))))
  )

;; Optional: Web mode for HTML and embedded JavaScript
(use-package web-mode
  :ensure t
  :mode ("\\.html\\'")
  :config
  (add-hook 'web-mode-hook (lambda ()
                             (when (string-equal "jsx" (file-name-extension buffer-file-name))
                               (setup-tide-mode)))))

(use-package company
  :ensure t
  :hook (lsp-mode . company-mode)
  :config
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.0)
  (global-company-mode t))

(use-package company-box
:hook (company-mode . company-box-mode))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :hook (lsp-mode . flycheck-mode)
  )

(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap (("C-c p" . projectile-command-map)))

(use-package counsel-projectile
  :after projectile
  :config (counsel-projectile-mode))

(use-package company-tabnine :ensure t)

(use-package magit
  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; NOTE: Make sure to configure a GitHub token before using this package!
;; - https://magit.vc/manual/forge/Token-Creation.html#Token-Creation
;; - https://magit.vc/manual/ghub/Getting-Started.html#Getting-Started
(use-package forge
  :after magit)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

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

(defun chb/configure-eshell ()
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
  :hook (eshell-first-time-mode . chb/configure-eshell)
  :config

  (with-eval-after-load 'esh-opt
    (setq eshell-destroy-buffer-when-process-dies t)
    (setq eshell-visual-commands '("htop" "zsh" "vim")))

  (eshell-git-prompt-use-theme 'powerline))

(use-package ellama
  :ensure t
  :bind ("C-c e" . ellama)
  ;; send last message in chat buffer with C-c C-c
  :hook (org-ctrl-c-ctrl-c-final . ellama-chat-send-last-message)
  :init (setopt ellama-auto-scroll t)
  :config
  ;; show ellama context in header line in all buffers
  (ellama-context-header-line-global-mode +1)
  ;; show ellama session id in header line in all buffers
  (ellama-session-header-line-global-mode +1))

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
  (setq rmh-elfeed-org-files (list "~/Daten/04-org-system/04-elfeed/elfeed.org"))
)
