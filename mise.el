;;; mise.el --- Support for `mise' cli -*- lexical-binding: t -*-

;; Copyright (C) 2024 liuyinz

;; Author: liuyinz <liuyinz95@gmail.com>
;; Maintainer: liuyinz <liuyinz95@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
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

;;; Code:

(require 'seq)
;; (require 'cl-lib)
;; 
;; (defgroup mise nil
;;   "Apply per-buffer environment variables using the mise tool."
;;   :group 'processes)
;; 
;; (defcustom mise-debug nil
;;   "Whether or not to output debug messages while in operation.
;; Messages are written into the *mise-debug* buffer."
;;   :type 'boolean)
;; 
;; (defcustom mise-executable "mise"
;;   "The mise executable used by default."
;;   :type 'string)
;; 
;; ;; (defun mise--locate-config ()
;; ;;   "docstring"
;; ;;  
;; ;;   )
;; 
;; ;; (defcustom mise-env-update-on-eshell-directory-change t
;; ;;   "Whether mise will update environment when changing directory in eshell."
;; ;;   :type 'boolean)
;; ;;
;; ;; (defcustom mise-lighter '(:eval (mise--lighter))
;; ;;   "The mode line lighter for `mise-mode'.
;; ;; You can set this to nil to disable the lighter."
;; ;;   :type 'sexp)
;; ;; (put 'mise-lighter 'risky-local-variable t)
;; ;;
;; 
;; (defun mise-env--mode-buffers ()
;;   "docstring"
;;   (seq-filter (lambda (b) (and (buffer-live-p b)
;;                                (with-current-buffer b
;;                                  mise-env-mode)))
;;               (buffer-list)))
;; 
;; (defun mise-env--apply (buf result)
;;   "docstring"
;;   (with-current-buffer buf
;;     (setq-local mise-env--status (if (listp result) 'on result))
;; 
;;     )
;;   )
;; 
;; (defun mise-env--update-env (env-dir)
;;   "docstring"
;; 
;;   )
;; 
;; 
;; ;;;###autoload
;; (define-minor-mode mise-mode
;;   "A local minor mode in which env vars are set by mise."
;;   :init-value nil
;;   ;; :lighter mise-env-lighter
;;   ;; :keymap e-mode-map
;;   (if mise-env-mode
;;       (progn
;;         (mise-env--update)
;;         ;; (when (and (derived-mode-p 'eshell-mode)
;;         ;;            mise-env-update-on-eshell-directory-change)
;;         ;;   (add-hook 'eshell-directory-change-hook #'mise-env--update nil t))
;;         )
;;     (mise-env--clear (current-buffer))
;;     (remove-hook 'eshell-directory-change-hook #'mise-env--update t)))
;; 
;; ;;;###autoload
;; (define-globalized-minor-mode mise-env-global-mode mise-env-mode
;;   (lambda () (unless (or (minibufferp) (file-remote-p default-directory))
;;                (mise-env-mode 1))))
;; 
(provide 'mise)
;;; mise.el ends here
