;;; eshell-atuin.el --- Intergrate eshell with atuin, a shell history tool -*- lexical-binding: t -*-

;; Copyright (C) 2024 Korytov Pavel

;; Author: Korytov Pavel <thexcloud@gmail.com>
;; Maintainer: Korytov Pavel <thexcloud@gmail.com>
;; Version: 0.2.0
;; Package-Requires: ((emacs "27.1"))
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
(require 'eshell)

(defgroup eshell-atuin ()
  "Intergrate eshell with atuin, a shell history tool."
  :link '(url-link :tag "GitHub" "https://github.com/SqrtMinusOne/eshell-atuin")
  :group 'eshell)

(defcustom eshell-atuin-executable (executable-find "atuin")
  "Path to the atuin executable."
  :group 'eshell-atuin
  :type 'file)

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
                       (concat (number-to-string
                                (round
                                 (*
                                  1000000000 ; nanoseconds
                                  (float-time
                                   (time-subtract
                                    (current-time)
                                    eshell-atuin--last-command-start))))))
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

(defun eshell-atuin--get-history ()
  (with-temp-buffer
    (let ((ret (call-process
                eshell-atuin-executable nil t nil
                "history" "list" "-f" "")))
      (unless (= 0 ret)
        (error "`atuin history list' retured %s: %s" ret (buffer-string)))
      (buffer-substring-no-properties
       (point-min) (point-max)))))

(defun eshell-atuin-history ()
  (interactive)
  )

(provide 'eshell-atuin)
;;; eshell-atuin.el ends here
