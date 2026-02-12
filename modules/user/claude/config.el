;;; user/claude/config.el -*- lexical-binding: t; -*-

(use-package! claude-code
  :init

  :config
  (claude-code-mode)
  (setq claude-code-terminal-backend 'vterm)

  ;; Display Claude Code buffer on the right side
  (set-popup-rule! "^\\*claude" :side 'right :size 0.4 :select t :quit nil :modeline t)

  ;; Start in insert mode when switching to Claude buffers
  (evil-set-initial-state 'claude-code-mode 'insert))

;; Bind all Claude Code commands to <leader> l

(map! :leader
      ;; FIXME: I have some error when I use `:prefix ("l" . "llm")', it's working but I don't understand it
      :prefix ("l" . "llm")
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
      "o" #'claude-code-send-buffer-file)
