(use-package vertico
:ensure t
:init
(vertico-mode))

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
  :ensure org-contrib
  :after ox-taskjugger
  :config
  (add-to-list 'org-export-backends 'ox-taskjuggler)
  )

(use-package python-mode
  :ensure t
  :hook (python-mode . lsp-deferred)
  :custom
  ;; NOTE: Set these if Python 3 is called "python3" on your system!
  (python-shell-interpreter "python3")
  (dap-python-executable "python3")
  (dap-python-debugger 'debugpy)
  :config
  (setq py-python-command "python3")

  ;; (require 'dap-python)
  ;; (setq dap-python-debugger 'debugpy)
  )

  ;; Optional: Additional configuration for Python (using pyls or other server)
(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp))))

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
