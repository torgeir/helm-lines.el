;;; helm-lines.el --- A helm interface for completing by lines in project -*- lexical-binding: t; -*-

;; Copyright (C) 2012-2016 Free Software Foundation, Inc.

;; Author: @torgeir
;; Version: 1.0.0
;; Keywords: helm ag git lines complete
;; Package-Requires: ((emacs "24.4") (helm "1.9.8"))

;;; Commentary:

;; A helm interface for completing by lines in project.
;;
;; Run `helm-lines' to complete the current line by other lines that
;; exist in the current git project.
;;
;; Enable `helm-follow-mode' to replace in place as you jump around
;; the suggestions.

;;; Code:

(require 'helm)
(require 'thingatpt)


(defun helm-lines--action (line)
  "Insert the selected LINE at the beginning of the current line.
Intents the line after inserting it."
  (move-beginning-of-line 1)
  (when (not (looking-at-p "$"))
    (kill-line))
  (insert line)
  (indent-for-tab-command))


(defvar helm-lines--map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map (kbd "C-n") 'helm-next-line)
    (define-key map (kbd "C-p") 'helm-previous-line)
    map)
  "Keymap used in helm project lines.")


(defun helm-lines--async-shell-command (cmd)
  "Execute `NAME'd shell `CMD' async."
  (let ((name "helm-lines"))
    (start-process-shell-command name (format "*%s*" name) cmd)))


(defun helm-lines--trim-newline (str)
  "Trim newlines from `STR'."
  (replace-regexp-in-string "\r?\n" "" str))


(defun helm-lines--candidates ()
  "Helm candidates by listing all lines under the current git root."
  (let* ((git-root (helm-lines--trim-newline (shell-command-to-string "git rev-parse --show-toplevel")))
         (query (if (string-equal helm-pattern "")
                    "^.*$"
                  helm-pattern))
         (cmd (format (concat "ag"
                              " --nocolor"
                              " --nonumbers"
                              " --nofilename"
                              " --ignore .git"
                              " --ignore target"
                              " --ignore node_modules"
                              " -i \"%s\""                ;; the pattern
                              " %s"                       ;; the folder
                              " | grep -Ev \"^$\""        ;; remove empty lines
                              " | sed -E \"s/^[ \t]*//\"" ;; remove leading ws
                              " | sort -u"                ;; unique
                              " | head -n 100")
                      (shell-quote-argument query)
                      (shell-quote-argument git-root))))
    (helm-lines--async-shell-command cmd)))


(defvar helm-lines--source
  (helm-build-async-source "Complete line in project"
    :candidates-process 'helm-lines--candidates
    :action 'helm-lines--action))


;;;###autoload
(defun helm-lines ()
  "Helm-lines entry point."
  (interactive)
  (helm :input (helm-lines--trim-newline (thing-at-point 'line t))
        :sources '(helm-lines--source)
        :keymap helm-lines--map))


(provide 'helm-lines)

;;; helm-lines.el ends here
