;;; mise.el --- Support for `mise' cli -*- lexical-binding: t -*-

;; Copyright (C) 2024 liuyinz

;; Author: liuyinz <liuyinz95@gmail.com>
;; Maintainer: Liuyinz <liuyinz95@gmail.com>
;; Version: 0.3.0
;; Package-Requires: ((emacs "29.1") (inheritenv "0.2") (dash "2.19.1"))
;; Keywords: tools, processes
;; Homepage: https://github.com/liuyinz/mise.el

;; This file is not a part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; This file is not a part of GNU Emacs.

;;; Commentary:

;; Use mise (https://mise.jdx.dev/) to set environment variables on a
;; per-buffer basis.  This means that when you work across multiple
;; projects which have `.envrc` files, all processes launched from the
;; buffers "in" those projects will be executed with the environment
;; variables specified in those files.  This allows different versions
;; of linters and other tools to be installed in each project if
;; desired.

;; Enable `global-mise-mode' late in your startup files.  For
;; interaction with this functionality, see `mise-', and the
;; commands `mise-reload'.

;;; Code:

(require 'subr-x)
(require 'json)

(require 'inheritenv)
(require 'dash)

(defgroup mise nil
  "Apply per-buffer environment variables using the mise tool."
  :group 'processes)

(defcustom mise-executable "mise"
  "The mise executable used by default."
  :type 'string
  :group 'mise)

(defcustom mise-update-on-eshell-directory-change t
  "Whether mise will update environment when changing directory in eshell."
  :type 'boolean
  :group 'mise)

(defcustom mise-exclude-predicate
  #'mise-default-exclude
  "Predicate to decide which buffer should be excluded from `global-mise-mode'.
This function is called with no argument and should return non-nil
if the current buffer should not obey `global-mise-mode.'"
  :type '(choice
          (const :tag "Default" mise-default-exclude)
          function)
  :group 'mise)

(defcustom mise-auto-propagate-commands
  '(shell-command-to-string async-shell-command org-babel-eval)
  "A list of commands which run after propagating env in temp buffer."
  :type '(repeat function)
  :group 'mise)

(defcustom mise-lighter '(:eval (mise--lighter))
  "The mode line lighter for `mise-mode'.
You can set this to nil to disable the lighter."
  :type 'sexp)
(put 'mise-lighter 'risky-local-variable t)

(defcustom mise-debug nil
  "Whether or not to output debug messages while in operation.
Messages are written into the *mise-debug* buffer."
  :type 'boolean
  :group 'mise)


;;; Variables

(defvar mise--cache (make-hash-table :test 'equal :size 10)
  "Known mise env directories and their mise results.
The values are as produced by `mise--export'.")

(defvar-local mise--init-env nil
  "Default env info of current buffer before mise enabled.")

(defvar-local mise-mode nil)

(defvar-local mise--status 'none
  "Symbol indicating state of the current buffer's mise.
One of \\='(error none global local)")

(defvar mise-exclude-regexps
  '("\\`\\s-\\*.*" "\\`\\*\\(Messages\\|Completions\\)\\*\\'")
  "List of regexps which buffer name matched excluded from `global-mise-mode'.")

(defvar mise-exclude-modes nil
  "List of major modes excluded from `global-mise-mode'.")



;;; Debug

(defun mise--debug (msg &rest args)
  "A version of `message' which does nothing if `mise-debug' is nil.
MSG and ARGS are as for that function."
  (when mise-debug
    (with-current-buffer (get-buffer-create " *mise-debug*")
      (unless (derived-mode-p 'special-mode)
        (special-mode))
      (when mise-mode (mise-mode -1))
      (goto-char (point-max))
      (let ((inhibit-read-only t))
        (insert (apply #'format msg args))
        (newline)))))

;;; Functions

(defun mise--message (&rest args)
  "Display a message for `mise' operation.
ARGS is as same as `message'."
  (message (concat "mise: " (apply #'format args))))

(defun mise--lighter ()
  "Return a colorized version of `mise--status' for use in the mode line."
  (let ((face (pcase mise--status
                ('none 'default)
                ((or 'error 'untrust) 'error)
                ('global 'success)
                ('local 'warning))))
    `(" mise[" (:propertize ,(symbol-name mise--status) face ,face) "]")))

(defun mise--call (destination &rest args)
  "Call mise executable in global process environment.
DESTINATION says what to do with standard output, ARGS are strings passed as
command arguments to `mise'"
  (let ((exec-path (default-value 'exec-path))
        (process-environment (default-value 'process-environment)))
    (apply #'call-process mise-executable nil destination nil args)))

(defun mise--ensure ()
  "Return non-nil if `mise-mode' is prepared for current buffer."
  (let ((exec-path (default-value 'exec-path))
        (process-environment (default-value 'process-environment)))
    (catch 'unsure
      (unless (file-exists-p default-directory)
        (mise--message "parent directory %S doesn't exist, mise-mode failed" default-directory)
        (throw 'unsure nil))
      (unless (executable-find mise-executable)
        (mise--message "can not find executable of mise!")
        (throw 'unsure nil))
      (let ((output1 (with-output-to-string
                       (mise--call standard-output "settings" "get" "experimental")))
            ;; HACK output1 cannot detect mise untrust stderr output
            (output2 (with-output-to-string
                       (mise--call standard-output "env"))))
        ;; set experimental to true
        (unless (string-match-p "true" output1)
          (if (eq 0 (mise--call nil "settings" "set" "experimental" "true"))
              (mise--message "set experimental to true in gloabl config")
            (setq mise--status 'error)
            (mise--message "set experimental to true failed")
            (throw 'unsure nil)))
        ;; trust detected config
        (when (string-match-p "Config file is not trusted" output2)
          (if (eq 0 (mise--call nil "trust" "--all"))
              (mise--message "trust detected configs.")
            (setq-local mise--status 'untrust)
            (mise--message "trust detected config failed")
            (throw 'unsure nil)))
        t))))

(defun mise--detect-configs ()
  "Return a list of configs file path for mise in current directory."
  (when-let ((output (with-output-to-string
                       (mise--call standard-output "config" "ls" "--verbose"))))
    (save-match-data
      (let ((pos 0)
            matches)
        (while (string-match "\"\\(.+\\)\"" output pos)
          (push (match-string 1 output) matches)
          (setq pos (match-end 0)))
        (-map #'expand-file-name (reverse matches))))))

(defun mise--detect-dir ()
  "Return the mise closest config located directory for the current buffer."
  (when-let ((configs (mise--detect-configs)))
    (directory-file-name (file-name-directory (car configs)))))

(defun mise--cache-key (env-dir)
  "Get a hash key for the result of invoking mise in ENV-DIR.
Generate new key when mise configs files modified."
  (let* ((configs (mise--detect-configs))
         (mdtime (--map (number-to-string
                         (time-convert (file-attribute-modification-time
                                        (file-attributes it))
                                       'integer))
                        configs)))
    (concat env-dir "\0" (md5 (string-join (-concat configs mdtime))))))

(defun mise--merged-env (pairs)
  "Make a `process-environment' value that merges PROCESS-ENV with PAIRS.
PAIRS is an alist obtained from mise's output.
Values from PROCESS-ENV will be included, but their values will
be masked by Emacs' handling of `process-environment' if they
also appear in PAIRS."
  (-concat (--map (-let [(key . val) it]
                    (concat key (and val (concat "=" val))))
                  pairs)
           (plist-get mise--init-env :env)))

(defun mise--update (&optional buffer)
  "Update the BUFFER or current buffer's environment if it is managed by mise.
All mise-managed buffers with this env will have their
environments updated."
  (with-current-buffer (or buffer (current-buffer))
    (let* ((env-dir (mise--detect-dir))
           (old-status mise--status)
           (debug-notify (propertize " new!" 'face 'warning))
           cache-key cache-value)
      (unless mise--init-env
        (setq-local mise--init-env `(:env ,process-environment
                                     :path ,exec-path)))
      (if (null env-dir)
          (progn
            (setq-local mise--status 'none)
            (mise--clear))
        (if (length= (mise--detect-configs) 1)
            (setq-local mise--status 'global)
          (setq-local mise--status 'local))
        (setq cache-key (mise--cache-key env-dir))
        (setq cache-value (gethash cache-key mise--cache))
        (setq-local process-environment
                    (mise--merged-env
                     (or cache-value
                         (let ((new-val (mise--export env-dir)))
                           (--each (hash-table-keys mise--cache)
                             (and (string-prefix-p (concat env-dir "\0") it)
                                  (remhash it mise--cache)))
                           (puthash cache-key new-val mise--cache)
                           new-val))))
        (let ((path (getenv "PATH")))
          (setq-local exec-path (-map #'directory-file-name (parse-colon-path path)))
          (when (derived-mode-p 'eshell-mode)
            (if (fboundp 'eshell-set-path)
                (eshell-set-path path)
              (setq-local eshell-path-env path)))))
      (mise--debug "%-12s: <%s>\n%-12s: %s\n%-12s: %s\n%-12s: %s\n%-12s: %s\n"
                   "buffer" (propertize (buffer-name) 'face 'error)
                   "call-func" "mise--update"
                   "mise-status" (if (eq old-status mise--status)
                                     (symbol-name mise--status)
                                   (concat (symbol-name old-status)
                                           " => "
                                           (symbol-name mise--status)
                                           debug-notify))
                   "cache-key" (or (and (null cache-key) "call mise--clear already")
                                   (and cache-value cache-key)
                                   (concat cache-key debug-notify))
                   "cache-value" (or (and (null cache-value) "call mise--export already")
                                     (with-temp-buffer
                                       (insert (json-encode-alist cache-value))
                                       (json-pretty-print-buffer)
                                       (buffer-string)))))))

(defun mise--export (env-dir)
  "Export the env vars for ENV-DIR using mise.
Return value is either \\='none, \\='error, \\='global, \\='local or an alist of
environment variable names and values."
  (let ((stderr-file (make-temp-file "mise-err"))
        result)
    (unwind-protect
        (let ((default-directory env-dir))
          (with-temp-buffer
            (let ((exit-code (mise--call (list t stderr-file) "env" "--json")))
              (mise--debug
               "%-12s: <%s>\n%-12s: %s\n%-12s: %s\n%-12s: %s\n%-12s: %s\n%-12s: %s\n"
               "path" (propertize env-dir 'face 'success)
               "call-func" "mise--export"
               "run-cmd" "mise env --json"
               "exit-code" (number-to-string exit-code)
               "stderr" (with-temp-buffer
                          (insert-file-contents stderr-file)
                          (buffer-string))
               "stdout" (buffer-string))
              (if (eq 0 exit-code)
                  (if (zerop (buffer-size))
                      (setq mise--status 'none)
                    (goto-char (point-min))
                    (setq result (let ((json-key-type 'string))
                                   (json-read-object))))
                (mise--message "failed in %s" env-dir)
                (setq mise--status 'error)))))
      (delete-file stderr-file))
    result))

(defun mise--clear (&optional buf)
  "Remove any effects of `mise-mode' for BUF.
If BUF is nil, use current buffer instead."
  (with-current-buffer (or buf (current-buffer))
    (setq-local process-environment (plist-get mise--init-env :env))
    (setq-local exec-path (plist-get mise--init-env :path))
    (when (derived-mode-p 'eshell-mode)
      (if (fboundp 'eshell-set-path)
          (eshell-set-path (butlast exec-path))
        (kill-local-variable 'eshell-path-env)))))

(defun mise--managed-buffers ()
  "Return a list of all live buffers in which `mise-mode' is enabled."
  (--filter (and (buffer-live-p it)
                 (buffer-local-value 'mise-mode it))
            (buffer-list)))

(defun mise-default-exclude ()
  "Return non-nil if current buffer should not obey `global-mise-mode'."
  (or (memq major-mode mise-exclude-modes)
      (when-let ((lst mise-exclude-regexps))
        (string-match-p (string-join (--map (concat "\\(?:" it "\\)") lst) "\\|")
                        (buffer-name)))))


;;; Propagate local environment to commands that use temp buffers

(defun mise-propagate-env (orig &rest args)
  "Advice function to wrap a command ORIG and make it use our local env.
This can be used to force compliance where ORIG starts processes
in a temp buffer.  ARGS is as for ORIG."
  (if mise-mode
      (inheritenv (apply orig args))
    (apply orig args)))

(dolist (cmd mise-auto-propagate-commands)
  (advice-add cmd :around #'mise-propagate-env))


;;; Commands

(defun mise-update-buffer (buffer)
  "Update mise environment of BUFFER."
  (interactive
   (list (completing-read
          "Mise update buffer: "
          (-map #'buffer-name (mise--managed-buffers)))))
  (mise--update buffer))

(defun mise-update-dir (&optional all)
  "Update mise environment of buffers relevant to selected directory.
If optional argument ALL is non-nil, update all mise-managed buffers."
  (interactive "P")
  (when all (clrhash mise--cache))
  (let ((buffers (or (and all (mise--managed-buffers))
                     (let ((dir (completing-read
                                 "Mise update dir: "
                                 (--map (substring it 0 (- (length it) 33))
                                        (hash-table-keys mise--cache)))))
                       (--filter (with-current-buffer it
                                   (string= (mise--detect-dir) dir))
                                 (mise--managed-buffers))))))
    (-each buffers #'mise--update)))


;;; Minor mode

;;;###autoload
(define-minor-mode mise-mode
  "A local minor mode in which env vars are set by mise."
  :group 'mise
  :init-value nil
  :lighter mise-lighter
  (if mise-mode
      (progn
        (mise--update)
        (when (and (derived-mode-p 'eshell-mode)
                   mise-update-on-eshell-directory-change)
          (add-hook 'eshell-directory-change-hook #'mise--update nil t)))
    (mise--clear)
    (remove-hook 'eshell-directory-change-hook #'mise--update t)))

(defun mise-turn-on-if-enable ()
  "Turn `mise-mode' on if applicable for current buffer."
  (unless (or (minibufferp)
              (file-remote-p default-directory)
              (funcall mise-exclude-predicate))
    (and (mise--ensure) (mise-mode))))

;;;###autoload
(define-globalized-minor-mode global-mise-mode
  mise-mode
  mise-turn-on-if-enable
  :init-value nil
  :group 'mise)

(provide 'mise)
;;; mise.el ends here
