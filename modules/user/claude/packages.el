;; -*- no-byte-compile: t; -*-
;;; user/claude/packages.el

;; Claude Code integration - Emacs client for Claude Code CLI
(package! claude-code
  :recipe (:host github
           :repo "stevemolitor/claude-code.el"))
