;;; prettier-elisp.el --- Minor mode to format emacs lisp code  -*- lexical-binding: t -*-

;; Version: 0.1.0

;; Copyright (C) 2020 Karim Aziiev <karim.aziev@gmail.com>

;; Author: Karim Aziiev <karim.aziev@gmail.com>
;; URL: https://github.com/KarimAziev/prettier-elisp
;; Keywords: convenience edit elisp
;; Version: 0.1.1
;; Package-Requires: ((emacs "26.1"))

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Formats your elisp code using on file save.

;;; Code:

(require 'thingatpt)

(defgroup prettier-elisp nil
  "Minor mode to format Elisp code on file save."
  :group 'languages
  :prefix "prettier-elisp"
  :link '(url-link :tag "Repository" "https://github.com/KarimAziev/prettier-elisp"))

(defcustom prettier-elisp-newline-symbols '("defcustom"
                                            "defgroup"
                                            "defvar"
                                            "defun"
                                            "cl-defun"
                                            "defmacro"
                                            "defconst"
                                            "defface"
                                            "defhydra"
                                            "define-widget"
                                            "define-global-minor-mode"
                                            "define-derived-mode"
                                            "use-package"
                                            "define-minor-mode")
  "List of forms to divide with newline."
  :group 'prettier-elisp
  :type '(repeat (string  :tag "Name")))

(defcustom prettier-elisp-non-newline-symbols '(require
                                                declare-function
                                                defvar)
  "List of forms not to divide with newline."
  :group 'prettier-elisp
  :type '(repeat (symbol  :tag "Name")))

(defun prettier-elisp-re-search-forward-inner (regexp &optional bound count)
  "Helper function for `prettier-elisp-re-search-forward'."
  (let ((parse))
    (while (> count 0)
      (with-syntax-table emacs-lisp-mode-syntax-table
        (re-search-forward regexp bound)
        (setq parse (syntax-ppss))
        (cond ((and (nth 3 parse)
                    (nth 8 parse))
               (goto-char (nth 8 parse))
               (forward-sexp))
              ((and (nth 4 parse)
                    (nth 8 parse))
               (goto-char (nth 8 parse))
               (forward-line))
              (t
               (setq count (1- count)))))))
  (point))

(defun prettier-elisp-re-search-forward (regexp &optional bound noerror count)
  "Search forward from point for REGEXP ignoring comments and strings."
  (unless count (setq count 1))
  (let ((init-point (point))
        (search-fun
         (cond ((< count 0) (setq count (- count))
                #'prettier-elisp-re-search-backward-inner)
               ((> count 0) #'prettier-elisp-re-search-forward-inner)
               (t #'ignore))))
    (condition-case err
        (funcall search-fun regexp bound count)
      (search-failed
       (goto-char init-point)
       (unless noerror
         (signal (car err) (cdr err)))))))

(defun prettier-elisp-re-search-backward-inner (regexp &optional bound count)
  "Helper for `km--elisp--re-search-backward'."
  (let ((parse))
    (while (> count 0)
      (with-syntax-table emacs-lisp-mode-syntax-table
        (re-search-backward regexp bound)
        (setq parse (syntax-ppss))
        (cond ((and (or (nth 3 parse))
                    (nth 8 parse))
               (goto-char (nth 8 parse)))
              ((and (nth 4 parse)
                    (nth 8 parse))
               (goto-char (nth 8 parse)))
              (t
               (setq count (1- count)))))))
  (point))

(defun prettier-elisp-re-search-backward (regexp &optional bound noerror count)
  "Search backward from point for REGEXP ignoring strings and comments."
  (prettier-elisp-re-search-forward
   regexp bound noerror (if count (- count) -1)))

(defun prettier-elisp-backward-list (&optional arg)
  "Move backward across one balanced group of parentheses.
Return new position or nil.
With ARG, do it that many times."
  (let ((pos (point))
        (end))
    (setq end (ignore-errors
                (backward-list (or arg 1))
                (point)))
    (unless (equal pos end)
      end)))

(defun prettier-elisp-forward-list (&optional arg)
  "Move forward across one balanced group of parentheses.
Return new position or nil.
With ARG, do it that many times."
  (let ((pos (point))
        (end))
    (setq end (ignore-errors
                (forward-list (or arg 1))
                (point)))
    (unless (equal pos end)
      end)))

;;;###autoload
(defun prettier-elisp-current ()
  "Prettify current top form."
  (interactive)
  (save-excursion
    (let ((pps (syntax-ppss)))
      (when-let ((bounds (progn
                           (if (if (> (car pps) 0)
                                   (progn
                                     (goto-char (car (nth 9 pps)))
                                     t)
                                 (looking-at "[(]"))
                               (let ((col (current-column)))
                                 (when (> col 0)
                                   (let ((pos (point)))
                                     (skip-chars-backward "\s\t")
                                     (delete-region (point) pos)
                                     (when (> (current-column) 0)
                                       (insert "\n")
                                       (skip-chars-forward "\n"))))
                                 (indent-sexp)
                                 (bounds-of-thing-at-point 'list))))))
        (let ((max (cdr bounds)))
          (save-excursion
            (save-match-data
              (while (prettier-elisp-re-search-forward "^\\s-*[\n]+" max t 1)
                (delete-region (match-beginning 0) (point)))))
          (save-excursion
            (save-match-data
              (while (prettier-elisp-re-search-forward
                      "(\\([\n\t\s]+\\)[a-zZ-A0-9]" max t 1)
                (replace-match "" nil nil nil 1))))
          (save-excursion
            (save-match-data
              (while (prettier-elisp-re-search-forward
                      "[a-zZ-A0-9)]\\([\n\t\s]+\\))" max t 1)
                (when-let ((end (1- (point)))
                           (start (save-excursion
                                    (forward-char -1)
                                    (skip-chars-backward "\s\t\n")
                                    (unless (nth 4 (syntax-ppss (point)))
                                      (point)))))
                  (delete-region start end)))))
          (save-excursion
            (save-match-data
              (while (prettier-elisp-re-search-forward
                      "[a-zZ-A0-9]\\((\\)" max t 1)
                (replace-match "\s(" nil nil nil 1))))
          (when-let ((sexp (list-at-point)))
            (save-match-data
              (cond ((looking-back ";;;###autoload[\s\t\n][\s\t\n]+" 0)
                     (replace-match ";;;###autoload\n"))))))))))

(defvar prettier-elisp-newline-symbols-re nil)

;;;###autoload
(defun prettier-elisp-ensure-newlines ()
  "Add new line after every top form from variable `prettier-elisp-newline-symbols'."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (when (and (null prettier-elisp-newline-symbols-re)
               (<= 1 (length prettier-elisp-newline-symbols)))
      (setq prettier-elisp-newline-symbols-re
            (concat "\\(" "\\(^\\s-+\\)[(]" (regexp-opt
                                             prettier-elisp-newline-symbols
                                             t) "\\)")))
    (when prettier-elisp-newline-symbols-re
      (save-match-data
        (while (prettier-elisp-re-search-forward
                prettier-elisp-newline-symbols-re nil t 1)
          (when (= 1 (nth 0 (syntax-ppss (point))))
            (replace-match "" nil nil nil 2)))))
    (goto-char (point-min))
    (save-match-data
      (while (prettier-elisp-re-search-forward ")\n\n\\(\n+\\)" nil t 1)
        (replace-match "" nil nil nil 1)))
    (when prettier-elisp-newline-symbols
      (goto-char (point-min))
      (save-match-data
        (while (prettier-elisp-forward-list)
          (let ((sexp-start (save-excursion (prettier-elisp-backward-list)
                                            (point))))
            (when (and
                   (< sexp-start (line-beginning-position))
                   (looking-at "\\(\\([\s\t]+\\)?[\n]\\)\\([;(]\\)"))
              (let ((pos (point)))
                (delete-region pos (point))
                (insert "\n")))))))))

;;;###autoload
(defun prettier-elisp ()
  "Format current defun at point and multy lines in buffer."
  (interactive)
  (save-match-data (prettier-elisp-current))
  (save-match-data (prettier-elisp-ensure-newlines)))

;;;###autoload
(defun prettier-elisp-format-buffer ()
  "Format current defun at point."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-max))
      (save-match-data
        (while (and (prettier-elisp-backward-list)
                    (looking-at "[(]"))
          (prettier-elisp-current)))))
  (save-match-data (prettier-elisp-ensure-newlines)))

;;;###autoload
(define-minor-mode prettier-elisp-mode
  "Format current defun on file save when this mode is turned on."
  :lighter " Prettier"
  :global nil
  (if prettier-elisp-mode
      (add-hook 'before-save-hook 'prettier-elisp nil 'local)
    (remove-hook 'before-save-hook 'prettier-elisp 'local)))

(provide 'prettier-elisp)
;;; prettier-elisp.el ends here