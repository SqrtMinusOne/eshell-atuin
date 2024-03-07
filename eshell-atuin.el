;;; eshell-atuin.el --- Intergrate eshell with atuin, a shell history tool -*- lexical-binding: t -*-

;; Copyright (C) 2024 Korytov Pavel

;; Author: Korytov Pavel <thexcloud@gmail.com>
;; Maintainer: Korytov Pavel <thexcloud@gmail.com>
;; Version: 0.2.0
;; Package-Requires: ((emacs "27.1") (compat "29.1.4.1"))
;; Homepage: https://github.com/SqrtMinusOne/eshell-atuin.el
;; Published-At: 2024-03-08

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; TODO

;;; Code:
(require 'avl-tree)
(require 'compat)
(require 'eshell)

(defgroup eshell-atuin ()
  "Intergrate eshell with atuin, a shell history tool."
  :link '(url-link :tag "GitHub" "https://github.com/SqrtMinusOne/eshell-atuin")
  :group 'eshell)

(defcustom eshell-atuin-executable (executable-find "atuin")
  "Path to the atuin executable."
  :group 'eshell-atuin
  :type 'file)

(defcustom eshell-atuin-search-fields '(time duration command)
  "List of fields to retrive from atuin.

Be sure to keep \"time\" and \"command\" here, otherwise the package
will not work."
  :group 'eshell-atuin
  :type '(repeat
          (choice
           (const time)
           (const exit)
           (const duration)
           (const command)
           (const directory)
           (const user)
           (const host)
           (const relativetime))))

(defcustom eshell-atuin-history-format "%c"
  "How to format history items.

See `format-spec' on the general syntax.  Available flags and
corresponding search fields from `eshell-atuin-search-fields':
- %t - time
- %e - exit
- %d - duration
- %c - command
- %i - directory
- %u - user
- %h - host
- %r - relativetime"
  :group 'eshell-atuin
  :type 'file)

(defcustom eshell-atuin-search-options '("--exit" "0")
  "Additional options for \\='atuin search\\='.

See \\='atuin help search\\=' for the kind of things you may want to
include here.  Some examples:
- \\='(\"--exit\" \"0\") to filter out non-zero exit codes.
- \\='(\"--exclude-cwd\" \"/some/dir\")"
  :group 'eshell-atuin
  :type '(repeat string))

(defvar-local eshell-atuin--history-id nil
  "ID of the current atuin history command.")

(defvar-local eshell-atuin--last-command-start nil
  "Start time of last eshell command.")

(defun eshell-atuin--get-input ()
  (save-excursion
    (beginning-of-line)
    (when (looking-at-p eshell-prompt-regexp)
      (substring-no-properties (eshell-get-old-input)))))

(defun eshell-atuin--pre-exec (&rest _args)
  (when-let ((input (eshell-atuin--get-input)))
    (setq eshell-atuin--history-id
          (with-temp-buffer
            (let ((ret (call-process
                        eshell-atuin-executable nil t nil
                        "history" "start" "--" input)))
              (unless (= 0 ret)
                (error "`atuin history start' retured %s: %s" ret (buffer-string)))
              (buffer-substring-no-properties
               (point-min) (point-max)))))
    (setq eshell-atuin--last-command-start (current-time))))

(defun eshell-atuin--post-exec ()
  (when eshell-atuin--history-id
    (let* ((proc-args
            `(,eshell-atuin-executable
              "history" "end"
              "--exit" ,(number-to-string eshell-last-command-status)
              ,@(when eshell-atuin--last-command-start
                  (list
                   "--duration"
                   (prog1
                       (thread-first eshell-atuin--last-command-start
                                     (time-since)
                                     (float-time)
                                     (* 1000000000)
                                     (round)
                                     (number-to-string))
                     (setq eshell-atuin--last-command-start nil))))
              ,eshell-atuin--history-id))
           (buf (generate-new-buffer "*atuin-output*"))
           (proc (with-environment-variables (("ATUIN_LOG" "error"))
                   (start-process-shell-command "atuin-history-stop" buf
                                                (string-join proc-args " ")))))
      (set-process-sentinel
       proc
       (lambda (process _msg)
         (when (eq (process-status process) 'exit)
           (unwind-protect
               (unless (= (process-exit-status process) 0)
                 (error "`atuin history end' returned %s: %s" (process-exit-status process)
                        (with-current-buffer buf (buffer-string))))
             (kill-buffer buf))))))))

(defun eshell-atuin--init-session ()
  (setenv "ATUIN_SESSION"
          (string-trim
           (with-output-to-string
             (with-current-buffer standard-output
               (call-process eshell-atuin-executable nil t nil "uuid"))))))

;;;###autoload
(define-minor-mode eshell-atuin-mode
  "TODO"
  :global t
  :group 'eshell-atuin
  :after-hook
  (progn
    (if eshell-atuin-mode
        (progn
          (eshell-atuin--init-session)
          (advice-add #'eshell-send-input :before #'eshell-atuin--pre-exec)
          (add-hook 'eshell-post-command-hook #'eshell-atuin--post-exec))
      (advice-remove #'eshell-send-input #'eshell-atuin--pre-exec)
      (remove-hook 'eshell-post-command-hook #'eshell-atuin--post-exec))))

(defun eshell-atuin--compare (a b)
  (string-lessp
   (alist-get 'time a)
   (alist-get 'time b)))

(defvar eshell-atuin--history-cache (avl-tree-create #'eshell-atuin--compare)
  "TODO.")

(defvar eshell-atuin--history-last-update nil
  "TODO.")

(defun eshell-atuin--create-format-flag ()
  (mapconcat
   (lambda (item)
     (format "{%s}" item))
   eshell-atuin-search-fields
   "\\t"))

(defun eshell-atuin--parse-history-line (line)
  (cl-loop for field in eshell-atuin-search-fields
           for value in (string-split line "\t")
           collect (cons field value)))

(defun eshell-atuin--history-list ()
  (with-temp-buffer
    (let* ((proc-args `("search" "-f" ,(eshell-atuin--create-format-flag)
                        ,@(when eshell-atuin--history-last-update
                            (list "--after"
                                  (thread-last
                                    eshell-atuin--history-last-update
                                    (time-since)
                                    (float-time)
                                    (round)
                                    (format "%s seconds ago"))))
                        ,@eshell-atuin-search-options))
           (ret (apply #'call-process eshell-atuin-executable
                       nil t nil proc-args))
           (commands (make-hash-table :test #'equal)))
      (unless (or (= 0 ret) (= 1 ret))
        (error "`atuin history list' retured %s: %s" ret (buffer-string)))
      (goto-char (point-min))
      (cl-loop while (not (eobp))
               for line = (buffer-substring-no-properties
                           (line-beginning-position)
                           (line-end-position))
               for datum = (eshell-atuin--parse-history-line line)
               if (alist-get 'command datum) collect datum
               do (forward-line 1)))))

(defun eshell-atuin--history-update ()
  (cl-loop for line in (eshell-atuin--history-list)
           do (avl-tree-enter eshell-atuin--history-cache line))
  (setq eshell-atuin--history-last-update (current-time)))

(defun eshell-atuin--history-collection ()
  (avl-tree-mapf
   (lambda (e)
     (cons
      (format-spec eshell-atuin-history-format
                   `((?t . ,(alist-get 'time e))
                     (?e . ,(alist-get 'exit e))
                     (?d . ,(alist-get 'duration e))
                     (?c . ,(alist-get 'command e))
                     (?i . ,(alist-get 'directory e))
                     (?h . ,(alist-get 'host e))
                     (?r . ,(alist-get 'relativetime e))))
      (alist-get 'command e)))
   #'cons
   eshell-atuin--history-cache))

(defun eshell-atuin-history ()
  (interactive)
  (let* ((commands (eshell-atuin--history-collection))
         (input (eshell-atuin--get-input))
         (compl (completing-read "History: " commands nil t input)))
    (eshell-bol)
    (delete-region (point) (line-end-position))
    (insert compl)))

(provide 'eshell-atuin)
;;; eshell-atuin.el ends here
