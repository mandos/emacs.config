;; Load local machine-specific configuration (secrets, credentials)
(let ((local-config (expand-file-name "config.local.el" doom-private-dir)))
  (if (file-exists-p local-config)
      (load-file local-config)
    (message "Warning: config.local.el not found. Some features may not work.
             Copy config.local.example.el to config.local.el and add your credentials.")))

;; User information is set in config.local.el
;; See config.local.example.el for template
;; (setq user-full-name "Your Full Name"
;;       user-mail-address "your.email@example.com")

;; (add-to-list 'package-archives
;;              '("gnu-devel" . "https://elpa.gnu.org/devel/") :append)

(use-package! kbd-mode)

(setq display-line-numbers 'visual)
(setq display-line-numbers-type 'visual)

(setq org-duration-format (quote h:mm))

(setq epg-pinentry-mode 'loopback)

(add-hook 'code-review-mode-hook
          (lambda ()
            ;; include *Code-Review* buffer into current workspace
            (persp-add-buffer (current-buffer))))

(remove-hook 'vterm-mode-hook #'hide-mode-line-mode)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; in config.el
;; (setq rainbow-delimiters-max-face-count 4) ; optional, it's set to 9 by default
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode) ; can also add `conf-mode-hook'

;; (map! :ni "<f5>" #'doom/leader)

;; Windows shorcuts
(map! :n "C-l" #'windmove-right)
(map! :n "C-h" #'windmove-left)
(map! :n "C-j" #'windmove-down)
(map! :n "C-k" #'windmove-up)
(map! :leader :desc "Vertical window" :n "w \\" #'+evil/window-vsplit-and-follow)
(map! :leader :desc "Horizontal window" :n "w -" #'+evil/window-split-and-follow)
(map! :leader :desc "Dirvish" :n "o m" #'dirvish)
(map! :leader :desc "Open Bibliographic note" :n "n B" #'citar-open)

(map! :n "[ e" #'flymake-goto-prev-error)
(map! :n "] e" #'flymake-goto-next-error)

(map! :n "C-k" #'windmove-up)

(map! :leader :desc "Yank link" :n "s y" #'link-hint-copy-link)

(map! :leader
      "0" #'winum-select-window-0-or-10
      "1" #'winum-select-window-1
      "2" #'winum-select-window-2
      "3" #'winum-select-window-3
      "4" #'winum-select-window-4
      "5" #'winum-select-window-5
      "6" #'winum-select-window-6
      "7" #'winum-select-window-7
      "8" #'winum-select-window-8
      "9" #'winum-select-window-9)
;; Move right in edition mode

(map! :i "C-l" #'right-char)
(map! :i "M-l" #'right-char)

(map! :i "M-RET" #'+default/newline-below)
(map! :i "C-a" #'evil-append-line);
(map! :leader :desc "Log for all branches" :g "g h" #'magit-log-all-branches)


(defun my/elisp-mode-hook()
  (add-hook 'completion-at-point-functions (cape-capf-super #'elisp-completion-at-point :with #'yasnippet-capf) nil t))
(add-hook! 'emacs-lisp-mode-hook #'my/elisp-mode-hook)

(defun my/lsp-mode-hook()
  (add-hook 'completion-at-point-functions (cape-capf-super #'lsp-completion-at-point :with #'yasnippet-capf) nil t))
(add-hook! 'lsp-mode-hook #'my/lsp-mode-hook)

(after! corfu
  (setq +corfu-want-tab-prefer-expand-snippets t)
  (setq +corfu-want-tab-prefer-navigating-snippets t)
  (setq +corfu-want-tab-prefer-navigating-org-tables t)
  (setq tab-always-indent t)
  )

(add-to-list 'completion-styles 'flex)
;; Completion
;; (setq doom-leader-key "SPC"
;;       doom-localleader-key "M")
(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 14 ))
;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-gruvbox)
;; NOTE: Fixing menu which is not readable in my system.
(set-face-attribute 'menu nil
                    :background "#32302f"
                    :foreground "#fabd2f")

(setq org-directory "~/Org/")

(after! org
  (map!
   :map org-mode-map
   :localleader
   :desc "citar-org-roam-open-current-refs" "m c" #'citar-org-roam-open-current-refs)


  (setq org-log-done 'time )
  (setq org-startup-folded 'content)
  (setq org-archive-default-command 'org-archive-to-archive-sibling)
  (setq org-tag-alist
        '(
          ;; Context
          ("@planning" . ?p)
          ("@next" . ?n)

          ;; For projects/goals
          ("%current" . ?c)
          ("%scheduled" . ?s)
          ("%suspended" . ?S)
          ("%completed" . ?C)
          ))

  (require 'org-habit)
  (add-to-list 'org-modules 'habit)
  (setq org-habit-show-habits-only-for-today t)
  (setq org-habit-show-all-today t)
  (setq org-habit-show-done-always-green nil)
  )


(after! org-agenda
  (setq org-agenda-files
        (mapcar 'abbreviate-file-name
                (append
                 (split-string (shell-command-to-string "fd --type f --extension org --exclude roam --exclude website --exclude templates . ~/Org") "\n")
                 (split-string (shell-command-to-string "rg \"%current\" ~/Org/roam/ --color never --no-config --files-with-matches") "\n"))
                ))
  ;; (add-to-list 'org-agenda-custom-commands
  (setq org-agenda-custom-commands
        '(
          ("g" "My stuff to do (GTD)"
           ((agenda "")
            (tags-todo "+@next+%current-DONE")
            (tags-todo "+@next-DONE" ((org-agenda-files '("~/Org/gtd.org"))))))
          ("w" "Weekly review"
           ((agenda "" ((org-agenda-span 'week))))
           ((org-agenda-start-with-log-mode t)
            (org-agenda-start-day "-6d")))
          )))

(after! org-roam
  (setq org-roam-directory "~/Org/roam/"))

(use-package! magit-todos
  :after magit
  :preface
  (map! :leader "p t" #'magit-todos-list)
  :config
  (setq magit-todos-keyword-suffix "\\(?:([^)]+)\\)?:?") ; make colon optional
  (define-key magit-todos-section-map "j" nil))

(after! evil-snipe
  (define-key evil-snipe-parent-transient-map (kbd ";")
              (evilem-create 'evil-snipe-repeat
                             :bind ((evil-snipe-scope 'buffer)
                                    (evil-snipe-enable-highlight)
                                    (evil-snipe-enable-incremental-highlight))))
  (setq evil-snipe-scope 'buffer))

;; Keybinding
(defun my-open-calendar ()
  (interactive)
  (cfw:open-calendar-buffer
   :contents-sources
   (list
    (cfw:org-create-source "Green")  ; org-agenda source
    ;; (cfw:org-create-file-source "cal" "/path/to/cal.org" "Cyan")  ; other org source
    ;; (cfw:howm-create-source "Blue")  ; howm source
    ;; (cfw:cal-create-source "Orange") ; diary source
    ;; (cfw:ical-create-source "Moon" "~/moon.ics" "Gray")  ; ICS source1
    ;; (cfw:ical-create-source "gcal" "https://..../basic.ics" "IndianRed") ; google calendar ICS
    )))
;; (map! :leader
;;       (:prefix-map ("o" . "open")
;;        (:prefix ("j" . "journal")
;;         :desc "today"  "t" #'org-roam-dailies-capture-today)))

;; Environment variables
;;

;; ;; (shell-command-to-string "fd --type f --extension org . ~/org") "\n")))
;;

;; (use-package! org-super-agenda
;;   :after org-agenda
;;   :init
;;   (setq org-agenda-skip-scheduled-if-done t
;;         org-agenda-skip-deadline-if-done t
;;         org-agenda-include-deadlines t
;;         org-agenda-include-diary t
;;         org-agenda-block-separator nil
;;         org-agenda-compact-blocks nil
;;         org-agenda-start-with-log-mode nil)
;;   (setq org-agenda-sorting-strategy '((agenda time-up priority-down category-keep)
;;                                       (todo priority-down category-keep)
;;                                       (tags priority-down category-keep)
;;                                       (search category-keep)))
;;   (setq org-agenda-custom-commands
;;         '(("d" "Dzisiaj"
;;            ((agenda "" (
;;                         (org-agenda-span 'week)
;;                         (org-agenda-start-day "-1d")
;;                         (org-agenda-span 3)
;;                         (org-super-agenda-groups
;;                          '(
;;                            (:name "Plan dnia"
;;                             :time-grid t
;;                             :date today
;;                             :scheduled today
;;                             :scheduled past)
;;                            ))))))
;;           ("g" "Getting Things Done"
;;            ((tags-todo "+next"
;;                        ((org-super-agenda-groups
;;                          '(
;;                            (:name "Zaplanowane na dzisiaj:"
;;                             :date today
;;                             :scheduled today
;;                             :deadline today)
;;                            (:name "Zrobić:"
;;                             :date nil)
;;                            (:name "Zaplanowane na później:"
;;                             :scheduled future
;;                             :deadline future)
;;                            ))))))

;;           ))
;;   :config
;;   (org-super-agenda-mode))

;; (setq org-agenda-time-grid
;;       (quote
;;        (()
;;         (600 700 800 900 1000 1100 1200 1300 1400 1500 1600 1700 1800 1900 2000 2100 2200)
;;         "......"
;;         "----------------")))


(after! org-journal
  (setq org-journal-file-type 'weekly)
  (setq org-journal-file-format "%F"))

(after! elfeed
  (setq elfeed-search-filter "@6-month-ago +unread")
  (setq elfeed-sort-order 'ascending)
  (add-hook! 'elfeed-search-mode-hook 'elfeed-update))

;; (setq! bibtex-completion-bibliography '("~/org/bibliography/references.bib"))
(setq! citar-bibliography '("~/Org/bibliography/zetero/zetero.bib"))

(setq! projectile-project-search-path '(("~/Workspace/" . 1)
                                        ("~/Workspace/configs/" . 1)
                                        ("~/Workspace/emacs" . 1)
                                        ("~/Workspace/nix" . 1)
                                        ("~/Workspace/priv" . 1)
                                        ("~/Workspace/projects" . 2)
                                        ))

;; (require 'org-capture)
;; (setq org-capture-templates `(
;;                               ("p" "Protocol" entry (file+headline ,(concat org-directory "notes.org") "Inbox")
;;                                "* %^{Title}\nSource: %u, %c\n #+begin_quote\n%i\n#+end_quote\n\n\n%?")
;;                               ("L" "Protocol Link" entry (file+headline ,(concat org-directory "notes.org") "Inbox")
;;                                "* %? [[%:link][%:description]] \nCaptured On: %U")
;;                               ("bt" "Tech blog entry" plane (file ""))
;;                               ("bl" "Life blog entry" plane (file "test.org"))
;;                               ))

;; (defun org-new-post ()
;;   (interactive)
;;   (setq new-blog-date-prefix (concat (format-time-string "%Y-%m-%d") "-"))
;;   (setq new-blog-post-title (read-from-minibuffer "Post name: "))
;;   (setq new-blog-post-slug (downcase (replace-regexp-in-string "[^[:alpha:][:digit:]_-]" "" (string-replace " " "-" new-blog-post-title))))
;;   (setq new-blog-post-type (read-from-minibuffer "Post type:" nil nil nil "tech"))
;;   (setq new-blog-post-file (concat (projectile-project-root) "content/" new-blog-post-type "/" new-blog-date-prefix new-blog-post-slug ".org"))

;;   (let ((org-capture-templates
;;          `(("p" "New blog post" plain (file new-blog-post-file)
;;             ,(concat "#+title: " new-blog-post-title "\n#+options: toc:nil num:nil\n#+begin_export html\n---\nlayout: post\ntitle: " new-blog-post-title "\nexcerpt: %?\ntags: \npermalink: " "blog/" new-blog-date-prefix new-blog-post-slug "\n---\n#+end_export\n")))
;;          )) (org-capture)))

;; (use-package! org-tranclusion
;;   :after org)
;; (setq! bib-files-directory (directory-files
;;     (concat (getenv "HOME") "/org/bibliography") t
;;     "^[A-Z|a-z].+.bib$"))

(after! corfu
  (add-to-list 'completion-styles 'flex)
  )

(after! csv-mode
  (add-hook 'csv-mode-hook 'csv-align-mode)
  (add-hook 'csv-mode-hook '(lambda () (interactive) (toggle-truncate-lines nil))))


;; New client frame show current workspace instead of creating new one
(after! persp-mode
  (setq persp-emacsclient-init-frame-behaviour-override
        `(+workspace-current-name))
  )

;; (add-hook! 'doom-after-init-hook #'global-visual-line-mode)
(map! :leader :desc "Soft line wripping (globally)" :n "t W" #'global-visual-line-mode)
