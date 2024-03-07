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
                        "history" "start" input)))
              (unless (= 0 ret)
                (error "`atuin history start' retured %s: %s" ret (buffer-string)))
              (buffer-substring-no-properties
               (point-min) (point-max)))))
    (setq eshell-atuin--last-command-start (current-time))))

(defun eshell-atuin--post-exec ()
  (let* ((proc-args `("history" "end" "-e"
                      ,(number-to-string eshell-last-command-status)
                      ,@(when eshell-atuin--last-command-start
                          (list "-d"
                                (prog1
                                    (number-to-string
                                     (float-time
                                      (time-subtract
                                       (current-time)
                                       eshell-atuin--last-command-start)))
                                  (setq eshell-atuin--last-command-start nil))))
                      ,eshell-atuin--history-id))
         (buf (generate-new-buffer "*atuin-output*"))
         (proc (apply #'start-process "atuin-history-stop" buf
                      eshell-atuin-executable proc-args)))
    (set-process-sentinel
     proc
     (lambda (process _msg)
       (pcase (process-status process)
         ('exit (kill-buffer buf))
         ('fatal (error "Error in running 'atuin history stop'.  See *atuin-output*")))))))

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

(provide 'eshell-atuin)
;;; eshell-atuin.el ends here
