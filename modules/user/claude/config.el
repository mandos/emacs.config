;;; user/claude/config.el -*- lexical-binding: t; -*-


(if (modulep! +ide)
    (progn

      ;; Insert mode after switch
      (defun my/claude-code-ide-switch-to-buffer ()
        (interactive)
        (claude-code-ide-switch-to-buffer)
        (evil-insert-state))

      ;; Claude Code IDE - MCP-based bidirectional integration
      (use-package! claude-code-ide
        :config
        (setq claude-code-ide-terminal-backend 'vterm)
        (setq claude-code-ide-use-side-window t)
        (setq claude-code-ide-window-width 0.4)
        (setq claude-code-ide-window-side 'right)
        ;; (setq claude-code-ide-cli-debug t)
        ;; (setq claude-code-ide-debug t)
        (claude-code-ide-emacs-tools-setup)

        ;; Start in insert mode when switching to Claude IDE buffers
        (evil-set-initial-state 'claude-code-ide-vterm-mode 'insert))

      ;; Bind all Claude Code IDE commands to <leader> l
      (map! :leader
            (:prefix ("l" . "llm")
                     "c" #'claude-code-ide
                     "s" #'claude-code-ide-send-prompt
                     "C" #'claude-code-ide-continue
                     "R" #'claude-code-ide-resume
                     "k" #'claude-code-ide-stop
                     "b" #'my/claude-code-ide-switch-to-buffer
                     "B" #'claude-code-ide-list-sessions
                     "t" #'claude-code-ide-toggle
                     "T" #'claude-code-ide-toggle-recent
                     "r" #'claude-code-ide-insert-at-mentioned
                     "m" #'claude-code-ide-menu
                     "n" #'claude-code-ide-send-escape
                     "v" #'claude-code-ide-toggle-vterm-optimization
                     "?" #'claude-code-ide-check-status)))

  (progn
    ;; Claude Code - standard terminal wrapper integration
    (use-package! claude-code
      :config
      (claude-code-mode)
      (setq claude-code-terminal-backend 'vterm)

      ;; Display Claude Code buffer on the right side
      (set-popup-rule! "^\\*claude" :side 'right :size 0.4 :select t :quit nil :modeline t)

      ;; Start in insert mode when switching to Claude buffers
      (evil-set-initial-state 'claude-code-mode 'insert))

    ;; Bind all Claude Code commands to <leader> l
    (map! :leader
          (:prefix ("l" . "llm")
                   "/" #'claude-code-slash-commands
                   "b" #'claude-code-switch-to-buffer
                   "B" #'claude-code-select-buffer
                   "c" #'claude-code
                   "C" #'claude-code-continue
                   "R" #'claude-code-resume
                   "i" #'claude-code-new-instance
                   "d" #'claude-code-start-in-directory
                   "e" #'claude-code-fix-error-at-point
                   "k" #'claude-code-kill
                   "K" #'claude-code-kill-all
                   "m" #'claude-code-transient
                   "n" #'claude-code-send-escape
                   "f" #'claude-code-fork
                   "r" #'claude-code-send-region
                   "s" #'claude-code-send-command
                   "S" #'claude-code-sandbox
                   "t" #'claude-code-toggle
                   "x" #'claude-code-send-command-with-context
                   "y" #'claude-code-send-return
                   "z" #'claude-code-toggle-read-only-mode
                   "1" #'claude-code-send-1
                   "2" #'claude-code-send-2
                   "3" #'claude-code-send-3
                   "M" #'claude-code-cycle-mode
                   "o" #'claude-code-send-buffer-file))))
