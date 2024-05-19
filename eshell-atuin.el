;;; eshell-atuin.el --- Integrate eshell with atuin, a shell history tool -*- lexical-binding: t -*-

;; Copyright (C) 2024 Korytov Pavel

;; Author: Korytov Pavel <thexcloud@gmail.com>
;; Maintainer: Korytov Pavel <thexcloud@gmail.com>
;; Version: 0.2.0
;; Package-Requires: ((emacs "27.1") (compat "29.1.4.1"))
;; Homepage: https://github.com/SqrtMinusOne/eshell-atuin
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

;; Integrate `eshell' with atuin <https://github.com/atuinsh/atuin>
;;
;; atuin stores shell history in a database, which allows for having
;; same history across multiple shells, sessions, and optionally
;; across different machines.  This package provides functionality to
;; store and browse eshell history in atuin.
;;
;; `eshell-atuin-mode' and `eshell-atuin-history' are the
;; corresponding entrypoints.
;;
;; See also the package README at
;; <https://github.com/SqrtMinusOne/eshell-atuin>


;;; Code:
(require 'compat)
(require 'em-prompt)
(require 'eshell)

(eval-when-compile (require 'subr-x))

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
will not work.

\"relativetime\" will probably not work as expected."
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
           (const relativetime)))
  :set
  (lambda (sym value)
    (set-default sym value)
    (when (fboundp #'eshell-atuin--history-reset)
      (eshell-atuin--history-reset))))

(defcustom eshell-atuin-save-duration t
  "Whether to save command duration.

Set to nil if your atuin version is less than 18."
  :group 'eshell-atuin
  :type 'boolean)

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
  :type 'file
  :set
  (lambda (sym value)
    (set-default sym value)
    (when (fboundp #'eshell-atuin--history-reset)
      (eshell-atuin--history-reset))))

(defcustom eshell-atuin-search-options '("--exit" "0")
  "Additional options for \\='atuin search\\='.

See \\='atuin help search\\=' for the kind of things you may want to
include here.  Some examples:
- \\='(\"--exit\" \"0\") to filter out non-zero exit codes.
- \\='(\"--exclude-cwd\" \"/some/dir\")"
  :group 'eshell-atuin
  :type '(repeat string)
  :set
  (lambda (sym value)
    (set-default sym value)
    (when (fboundp #'eshell-atuin--history-reset)
      (eshell-atuin--history-reset))))

(defconst eshell-atuin-filter-modes '(global host session directory)
  "List of filter modes for \\='atuin search\\='.")

(defcustom eshell-atuin-filter-mode nil
  "Default filter mode for \\'atuin search\\='."
  :group 'eshell-atuin
  :type '(choice
          (const nil :tag "Use default filter")
          (const global :tag "All hosts, all sessions, all directories")
          (const host :tag "History just from this host")
          (const session :tag "History just from this session")
          (const directory :tag "History just from this directory"))
  :set
  (lambda (sym value)
    (set-default sym value)
    (when (fboundp #'eshell-atuin--history-rotate-cache)
      (eshell-atuin--history-rotate-cache))))

(defvar-local eshell-atuin--history-id nil
  "Atuin ID of the current eshell command.")

(defvar-local eshell-atuin--last-command-start nil
  "Start time the of current eshell command.")

(defvar eshell-atuin--session-id nil
  "Current atuin session ID.")

(defun eshell-atuin--bol-ignoring-prompt ()
  (if (fboundp 'eshell-bol-ignoring-prompt)
      (eshell-bol-ignoring-prompt nil)
    (beginning-of-line)))

(defun eshell-atuin--get-input ()
  "Get eshell input string on the current line."
  (save-excursion
    (eshell-atuin--bol-ignoring-prompt)
    (when (looking-at-p eshell-prompt-regexp)
      (substring-no-properties (eshell-get-old-input)))))

(defun eshell-atuin--pre-exec (&rest _args)
  "Tell atuin that the command has been invoked.

This calls \\='atuin history start\\=', save the ID to
`eshell-atuin--history-id' and time to
`eshell-atuin--last-command-start'.  The ID will be used to save the
results of the command in `eshell-atuin--post-exec'."
  (when-let ((input (eshell-atuin--get-input)))
    (setq eshell-atuin--history-id
          (with-temp-buffer
            (with-environment-variables (("ATUIN_SESSION" eshell-atuin--session-id))
              (let ((ret (call-process
                          eshell-atuin-executable nil t nil
                          "history" "start" "--" input)))
                (unless (= 0 ret)
                  (error "`atuin history start' retured %s: %s" ret (buffer-string)))
                (buffer-substring-no-properties
                 (point-min) (point-max))))))
    (setq eshell-atuin--last-command-start (current-time))))

(defun eshell-atuin--post-exec ()
  "Tell atuin that the invoked command has finished.

This calls \\='atuin history end\\=' with the duration and exit code
of the command."
  (when eshell-atuin--history-id
    (let* ((proc-args
            `(,eshell-atuin-executable
              "history" "end"
              "--exit" ,(number-to-string eshell-last-command-status)
              ,@(when (and eshell-atuin--last-command-start
                           eshell-atuin-save-duration)
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
           ;; XXX No idea why `call-process' or `start-process' don't
           ;; work here.
           (proc (with-environment-variables
                     (("ATUIN_LOG" "error")
                      ("ATUIN_SESSION" eshell-atuin--session-id))
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
             (kill-buffer buf))))))
    (setq eshell-atuin--history-id nil)))

(defun eshell-atuin--init-session ()
  "Initialize `eshell-atuin' session, one session per Emacs."
  (setq eshell-atuin--session-id
        (string-trim
         (with-output-to-string
           (with-current-buffer standard-output
             (call-process eshell-atuin-executable nil t nil "uuid")))))
  (setenv "ATUIN_SESSION" eshell-atuin--session-id))

;;;###autoload
(define-minor-mode eshell-atuin-mode
  "Store `eshell' history in atuin."
  :global t
  :group 'eshell-atuin
  :after-hook
  (progn
    (if eshell-atuin-mode
        (progn
          (eshell-atuin--init-session)
          (eshell-atuin--history-reset)
          (advice-add #'eshell-send-input :before #'eshell-atuin--pre-exec)
          (add-hook 'eshell-post-command-hook #'eshell-atuin--post-exec))
      (advice-remove #'eshell-send-input #'eshell-atuin--pre-exec)
      (remove-hook 'eshell-post-command-hook #'eshell-atuin--post-exec))))

(defun eshell-atuin--compare (a b)
  "Compare two `eshell-atuin' history objects, A and B.

See `eshell-atuin--history-cache' for value format."
  (unless (string-equal (alist-get 'command a)
                        (alist-get 'command b))
    (string-lessp
     (alist-get 'time a)
     (alist-get 'time b))))

(defvar eshell-atuin--history-cache nil
  "A list with shell history data from atuin.

This works something like an ordered set: the list is orded by time;
there's only one item for each command.

One element is an alist with keys as configured in
`eshell-atuin-search-fields' and values as returned by \\='atuin
search\\='.  That commands returns values already sorted by time and
grouped by commands.

The cache is used to avoid unncessary parsing of the output of the
mentioned command.  `eshell-atuin--history-update' sets
`eshell-atuin--history-last-update' and queries atuin only for results
after that time.  That way, the cache is updated incrementally.

New records are added as follows.

`eshell-atuin--history-cache-index' is a hash table with commands as
keys and pointers to this list as values.  If there's already a
command with the same name as the new one, it is deleted from the
list via that pointer.

This preserves ordering and unqiness of elements without iterating
through the entire list on each update.")

(defvar eshell-atuin--history-cache-index (make-hash-table :test #'equal)
  "An \"index\" for `eshell-atuin--history-cache'.

See the variable for how it works.")

(defvar eshell-atuin--history-cache-format-index (make-hash-table :test #'equal)
  "A hash table to map formatted commands to raw commands.

The values are alists as defined in `eshell-atuin--history-cache'; the
keys are formatted values, created by `eshell-atuin-history-format'.

This is used to speed up the lookup after `competing-read' in
`eshell-atuin-history'.")

(defvar eshell-atuin--history-last-update nil
  "The time of update of `eshell-atuin--history-cache'.")

(defvar eshell-atuin--history-cache-current-filter-mode nil
  "Current filter mode for `eshell-atuin--history-cache'.")

(defvar eshell-atuin--history-cache-other-filter-modes nil
  "Values of eshell-aution cache variables for other filter modes.")

(defun eshell-atuin--history-reset ()
  "Reset `eshell-atuin' history cache."
  (setq eshell-atuin--history-cache nil)
  (setq eshell-atuin--history-last-update nil)
  (setq eshell-atuin--history-cache-index (make-hash-table :test #'equal))
  (setq eshell-atuin--history-cache-format-index (make-hash-table :test #'equal)))

(defun eshell-atuin--history-cache-p (filter-mode)
  "Return non-nil if atuin search with FILTER-MODE has to be cached."
  (memq filter-mode '(nil global host session)))

(defun eshell-atuin--history-rotate-cache ()
  "Rotate `eshell-atuin' history cache by filter mode.

When the filter mode changes, the cache is saved and the new one is
loaded, if available.

If the current filter mode should not be cached, the cache is
reset because the `eshell-atuin--history-collection' can only get
history from the cache.  Essentially, in this case the cache is reset
and refilled on each invokation of `eshell-atuin-history'"
  (unless (eq eshell-atuin-filter-mode
              eshell-atuin--history-cache-current-filter-mode)
    ;; Save current cache, if necessary.
    (when (eshell-atuin--history-cache-p
           eshell-atuin--history-cache-current-filter-mode)
      (setf (alist-get eshell-atuin--history-cache-current-filter-mode
                       eshell-atuin--history-cache-other-filter-modes)
            `((cache . ,eshell-atuin--history-cache)
              (index . ,eshell-atuin--history-cache-index)
              (format-index . ,eshell-atuin--history-cache-format-index)
              (last-update . ,eshell-atuin--history-last-update))))
    ;; Load new cache, if necessary and available.
    (if-let ((_ (eshell-atuin--history-cache-p eshell-atuin-filter-mode))
             (other-cache
              (alist-get eshell-atuin-filter-mode
                         eshell-atuin--history-cache-other-filter-modes)))
        (let-alist other-cache
          (setq eshell-atuin--history-cache .cache)
          (setq eshell-atuin--history-cache-index .index)
          (setq eshell-atuin--history-cache-format-index .format-index)
          (setq eshell-atuin--history-last-update .last-update))
      (eshell-atuin--history-reset))
    (setq eshell-atuin--history-cache-current-filter-mode eshell-atuin-filter-mode))
  ;; Reset cache if necessary.
  (unless (eshell-atuin--history-cache-p eshell-atuin-filter-mode)
    (eshell-atuin--history-reset)))

(defun eshell-atuin--create-format-flag ()
  "Format `eshell-atuin-search-fields' for usage in the -f flag."
  (mapconcat
   (lambda (item)
     (format "{%s}" item))
   eshell-atuin-search-fields
   "\\t"))

(defun eshell-atuin--parse-history-line (line)
  "Parse one LINE of \\='atuin search\\='.

This only works for lines created with the value of the -f flag from
`eshell-atuin--create-format-flag'."
  (let ((e (cl-loop for field in eshell-atuin-search-fields
                    for value in (string-split line "\t")
                    collect (cons field value))))
    (when (alist-get 'command e)
      (setf (alist-get 'formatted-value e)
            (format-spec eshell-atuin-history-format
                         `((?t . ,(alist-get 'time e ""))
                           (?e . ,(alist-get 'exit e ""))
                           (?d . ,(alist-get 'duration e ""))
                           (?c . ,(alist-get 'command e ""))
                           (?i . ,(alist-get 'directory e ""))
                           (?h . ,(alist-get 'host e ""))
                           (?r . ,(alist-get 'relativetime e ""))))))
    e))

(defun eshell-atuin--add-datum-to-cache (datum)
  "Add DATUM to `eshell-atuin' cache.

The cache is `eshell-atuin--history-cache', which see."
  (when-let ((cell (gethash (alist-get 'command datum)
                            eshell-atuin--history-cache-index)))
    (setf (cdr cell) (cddr cell))
    (puthash (alist-get 'command (cadr cell))
             cell eshell-atuin--history-cache-index))
  (when (string-equal (alist-get 'command datum)
                      (alist-get 'command (car eshell-atuin--history-cache)))
    (setq eshell-atuin--history-cache
          (cdr eshell-atuin--history-cache)))
  (push datum eshell-atuin--history-cache)
  (puthash (alist-get 'command (cadr eshell-atuin--history-cache))
           eshell-atuin--history-cache
           eshell-atuin--history-cache-index)
  (puthash (alist-get 'formatted-value datum)
           datum
           eshell-atuin--history-cache-format-index))

(defun eshell-atuin--history-update ()
  "Run \\='atuin search\\=' and update the `atuin-eshell' cache.

See `eshell-atuin--history-cache' on algorithm."
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
                        ,@(when eshell-atuin-filter-mode
                            (list "--filter-mode"
                                  (symbol-name eshell-atuin-filter-mode)))
                        ,@eshell-atuin-search-options))
           (ret (with-environment-variables
                    (("ATUIN_SESSION" eshell-atuin--session-id))
                  (apply #'call-process eshell-atuin-executable
                         nil t nil proc-args))))
      (unless (or (= 0 ret) (= 1 ret))
        (error "`atuin history list' retured %s: %s" ret (buffer-string)))
      (goto-char (point-min))
      (cl-loop
       while (not (eobp))
       for line = (buffer-substring-no-properties
                   (line-beginning-position)
                   (line-end-position))
       for datum = (eshell-atuin--parse-history-line line)
       do (eshell-atuin--add-datum-to-cache datum)
       do (forward-line 1))))
  (setq eshell-atuin--history-last-update (current-time)))

(defun eshell-atuin--history-collection ()
  "Return an alist with `eshell-atuin' history.

The keys are formatted by `eshell-atuin-history-format'; the values
are plain commands."
  (mapcar
   (lambda (e)
     (cons
      (alist-get 'formatted-value e)
      (alist-get 'command e)))
   eshell-atuin--history-cache))

(defun eshell-atuin-history (&optional arg)
  "Browse atuin history in Eshell.

`eshell-atuin-mode' enables storing eshell history in atuin in
addition to the built-in ring.  `eshell-atuin-history' opens
`completing-read' with the saved history, like the C-r shell binding
in the original tool.

ARG overrides the default filter mode (which is
`eshell-atuin-filter-mode').  The value is an index of
`eshell-atuin-filter-modes'.

By default, the completion UI shows only commands.  To change that,
add more fields to `eshell-atuin-search-fields' and use them in
`eshell-atuin-history-format'.  The default values are such for
backwards compatibility with \"non-vertical\" completion systems.

The completions are ordered; the first one is the most recent one.

Be sure to have the correct `eshell-prompt-regexp' set up!"
  (interactive "P")
  (let ((eshell-atuin-filter-mode
         (if arg
             (or (nth arg eshell-atuin-filter-modes)
                 (user-error "Invalid filter mode index: %s" arg))
           eshell-atuin-filter-mode)))
    (eshell-atuin--history-rotate-cache)
    (eshell-atuin--history-update))
  (let* ((commands (eshell-atuin--history-collection))
         (input (eshell-atuin--get-input))
         (compl (completing-read "History: " commands nil nil input))
         (command
          (alist-get 'command
                     (gethash compl eshell-atuin--history-cache-format-index))))
    (eshell-bol)
    (delete-region (point) (line-end-position))
    (insert (or command compl))))

(provide 'eshell-atuin)
;;; eshell-atuin.el ends here
