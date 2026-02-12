;;; user/org-roam/config.el -*- lexical-binding: t; -*-

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
          ))
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
  :init

  (map! :leader
        :desc "Org Roam UI" "n r u" #'org-roam-ui-open)
  (map! :map org-mode-map
        :localleader
        (:prefix ("m")
                 (:prefix ("u" . "org-roam-ui")
                  :desc "Org Roam UI" "u" #'org-roam-ui-open
                  :desc "Toggle folow nodes" "f" #'org-roam-ui-follow-mode
                  :desc "Local graph" "l" #'org-roam-ui-node-local
                  :desc "Add to local graph" "a" #'org-roam-ui-add-to-local-graph
                  :desc "Remove from local graph" "r" #'org-roam-ui-remove-from-local-graph)))

  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t
        org-fold-core-style 'overlays))
