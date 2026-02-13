;; -*- no-byte-compile: t; -*-
;;; user/claude/packages.el

(if (modulep! +ide)
    ;; Claude Code IDE - MCP-based bidirectional integration with Claude Code CLI
    (package! claude-code-ide
      :recipe (:host github :repo "manzaltu/claude-code-ide.el")
      :pin "5f12e60c6d2d1802c8c1b7944bbdf935d5db1364")
  ;; Claude Code - Emacs client for Claude Code CLI
  (package! claude-code
    :recipe (:host github :repo "stevemolitor/claude-code.el")
    :pin "4a9914bd4161eb43f489820f9174c62390e5adc8"))
