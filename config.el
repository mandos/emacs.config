(setq user-full-name "Marek Maksimczyk"
      user-mail-address "marek.maksimczyk@mandos.net.pl")

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

;; Move right in edition mode
(map! :i "C-l" #'right-char)
(map! :leader :desc "Log for all branches" :g "g h" #'magit-log-all-branches)

(setq tab-always-indent nil)
;; Completion
;; (setq doom-leader-key "SPC"
;;       doom-localleader-key "M")
;;
(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 13 ))

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
(setq doom-theme 'doom-one)

(setq org-directory "~/Org/")

(after! org
  (setq org-startup-folded t))

;; Ustawienia języka japońskiego
;; (use-package! mozc
;;   :init
;;   (setq default-input-method "japanese-mozc")
;;   (setq mozc-candidate-style 'overlay)
;;   (setq header-line-format "")
;;   (setq-default mozc-candidate-style 'echo-area))

;; (use-package! evil-org
;;   :when (modulep! :editor evil +everywhere)
;;   :config
;;   (map! :map evil-org-mode-map
;;         :i "RET" (cmds! mozc-mode (cmd! (mozc-handle-event last-command-event))
;;                         (cmd! (org-return electric-indent-mode)))
;;         :i [return] (cmds! mozc-mode (cmd! (mozc-handle-event last-command-event))
;;                            (cmd! (org-return
;;                                   electric-indent-mode)))))

(after! evil-snipe
  (define-key evil-snipe-parent-transient-map (kbd ";")
              (evilem-create 'evil-snipe-repeat
                             :bind ((evil-snipe-scope 'buffer)
                                    (evil-snipe-enable-highlight)
                                    (evil-snipe-enable-incremental-highlight))))
  (setq evil-snipe-scope 'buffer))

;; org-roam with friends

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

(setq org-agenda-files
      (mapcar 'abbreviate-file-name
              (append
               (split-string (shell-command-to-string "fd --type f --extension org --exclude roam . ~/Org ~/.org-jira") "\n")
               (split-string (shell-command-to-string "rg \":bieżący:projekt:\" ~/Org/roam/ --color never --no-config --files-with-matches") "\n"))
              ))
;; (shell-command-to-string "fd --type f --extension org . ~/org") "\n")))

(use-package! org-super-agenda
  :after org-agenda
  :init
  (setq org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t
        org-agenda-include-deadlines t
        org-agenda-include-diary t
        org-agenda-block-separator nil
        org-agenda-compact-blocks nil
        org-agenda-start-with-log-mode nil)
  (setq org-agenda-sorting-strategy '((agenda time-up priority-down category-keep)
                                      (todo priority-down category-keep)
                                      (tags priority-down category-keep)
                                      (search category-keep)))
  (setq org-agenda-custom-commands
        '(("d" "Dzisiaj"
           ((agenda "" (
                        (org-agenda-span 'week)
                        (org-agenda-start-day "-1d")
                        (org-agenda-span 3)
                        (org-super-agenda-groups
                         '(
                           (:name "Plan dnia"
                            :time-grid t
                            :date today
                            :scheduled today
                            :scheduled past)
                           ))))))
          ("g" "Getting Things Done"
           ((tags-todo "+next"
                       ((org-super-agenda-groups
                         '(
                           (:name "Zaplanowane na dzisiaj:"
                            :date today
                            :scheduled today
                            :deadline today)
                           (:name "Zrobić:"
                            :date nil)
                           (:name "Zaplanowane na później:"
                            :scheduled future
                            :deadline future)
                           ))))))

          ))
  :config
  (org-super-agenda-mode))

(setq org-agenda-time-grid
      (quote
       (()
        (600 700 800 900 1000 1100 1200 1300 1400 1500 1600 1700 1800 1900 2000 2100 2200)
        "......"
        "----------------")))

;; Habbit
(after! 'evil-org-agenda
  (require 'org-habit
           (add-to-list 'org-modules 'org-habit)
           (setq org-habit-show-habits-only-for-today t)))

(after! org-journal
  (setq org-journal-file-type 'weekly)
  (setq org-journal-file-format "%F"))

(after! elfeed
  (setq elfeed-search-filter "@6-month-ago +unread")
  (setq elfeed-sort-order 'ascending)
  (add-hook! 'elfeed-search-mode-hook 'elfeed-update))

(setq jiralib-url "https://cobiro.atlassian.net")

(after! org-roam
  (require 'org-roam-protocol)
  (add-to-list 'magit-section-initial-visibility-alist (cons 'org-roam-node-section 'hide))
  (setq org-roam-mode-section
        (list #'org-roam-backlinks-section :unique t
              #'org-roam-reflinks-section
              #'org-roam-unlinked-references-section
              ))
  (setq org-roam-capture-templates
        '(("d" "default" plain "%?" :target
           (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
           :unnarrowed t)
          ("y" "youtube" plain
           (file "~/Org/templates/org-roam/youtube.org")
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: youtube\n")
           :empty-lines-before 1
           :unnarrowed t)
          )))


(use-package! websocket
  :after org-roam)

(use-package! org-roam-ui
  :after org-roam ;; or :after org
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(setq org-fold-core-style 'overlays)

(map! :leader
      :desc "Org Roam UI" "n r u" #'org-roam-ui-open
      :desc "Org Roam UI" "m m u u" #'org-roam-ui-open
      :desc "Toggle folow nodes" "m m u f" #'org-roam-ui-follow-mode
      :desc "Local graph" "m m u l" #'org-roam-ui-node-local
      :desc "Add to local graph" "m m u a" #'org-roam-ui-add-to-local-graph
      :desc "Remove from local graph" "m m u r" #'org-roam-ui-remove-from-local-graph)

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

;; (defun org-jekyll-new-post ()
;;   (interactive)
;;   (let ((org-capture-templates
;;          `(("b" "New Jekyll blog post" plain (file new-blog-post-file)
;;             ,(concat "#+title: " new-blog-post-title "\n#+options: toc:nil num:nil\n#+begin_export: html\n---\nlayout: post\ntitle: " new-blog-post-title "\nexcerpt: %?\ntags: \npermalink: " new-blog-post-slug "\n---\n#+end_export\n")))
;;          )) (org-capture)))

;; (use-package! org-tranclusion
;;   :after org)
;; (setq! bib-files-directory (directory-files
;;     (concat (getenv "HOME") "/org/bibliography") t
;;     "^[A-Z|a-z].+.bib$"))
;; (setq! bibtex-completion-bibliography '("~/org/bibliography/references.bib"))
(setq! citar-bibliography '("~/Org/bibliography/zetero/zetero.bib"))
