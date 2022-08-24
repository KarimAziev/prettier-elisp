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

(defgroup prettier-elisp nil
  "Minor mode to format Elisp code on file save."
  :group 'languages
  :prefix "prettier-elisp"
  :link
  '(url-link
    :tag "Repository" "https://github.com/KarimAziev/prettier-elisp"))

(defcustom prettier-elisp-newline-symbols '("defcustom" "defgroup" "defvar"
                                            "defun" "cl-defun" "defmacro"
                                            "defconst" "defface" "defhydra"
                                            "define-widget"
                                            "define-global-minor-mode"
                                            "define-derived-mode" "use-package"
                                            "define-minor-mode")
  "List of forms to divide with newline."
  :group 'prettier-elisp
  :type '(repeat (string
                  :tag "Name")))

(defcustom prettier-elisp-non-newline-symbols '(require
                                                declare-function
                                                defvar)
  "List of forms not to divide with newline."
  :group 'prettier-elisp
  :type '(repeat (symbol
                  :tag "Name")))

(defun prettier-elisp-re-search-forward-inner (regexp &optional bound count)
  "Helper function for `prettier-elisp-re-search-forward'.
Arguments REGEXP, BOUND, COUNT has the same meaning as for `re-search-forward'."
  (let ((parse))
    (while (> count 0)
      (with-syntax-table emacs-lisp-mode-syntax-table
        (re-search-forward regexp bound)
        (setq parse
              (syntax-ppss))
        (cond ((and (nth 3 parse)
                    (nth 8 parse))
               (goto-char (nth 8 parse))
               (forward-sexp))
              ((and (nth 4 parse)
                    (nth 8 parse))
               (goto-char (nth 8 parse))
               (forward-line))
              (t
               (setq count
                     (1- count)))))))
  (point))

(defun prettier-elisp-re-search-forward (regexp &optional bound noerror count)
  "Search forward from point for REGEXP ignoring comments and strings.
Arguments BOUND, NOERROR, COUNT has the same meaning as in `re-search-forward'."
  (unless count
    (setq count 1))
  (let ((init-point (point))
        (search-fun
         (cond ((< count 0)
                (setq count
                      (- count))
                #'prettier-elisp-re-search-backward-inner)
               ((> count 0)
                #'prettier-elisp-re-search-forward-inner)
               (t #'ignore))))
    (condition-case err
        (funcall search-fun regexp bound count)
      (search-failed
       (goto-char init-point)
       (unless noerror
         (signal (car err)
                 (cdr err)))))))

(defun prettier-elisp-re-search-backward-inner (regexp &optional bound count)
  "Helper for `prettier-elisp-re-search-backward'.
Arguments REGEXP, BOUND, COUNT has the same meaning
as for `re-search-backward'."
  (let ((parse))
    (while (> count 0)
      (with-syntax-table emacs-lisp-mode-syntax-table
        (re-search-backward regexp bound)
        (setq parse
              (syntax-ppss))
        (cond ((and (or (nth 3 parse))
                    (nth 8 parse))
               (goto-char (nth 8 parse)))
              ((and (nth 4 parse)
                    (nth 8 parse))
               (goto-char (nth 8 parse)))
              (t
               (setq count
                     (1- count)))))))
  (point))

(defun prettier-elisp-re-search-backward (regexp &optional bound noerror count)
  "Search backward from point for REGEXP ignoring strings and comments.
Arguments REGEXP, BOUND, NOERROR, COUNT has the same meaning
as for `re-search-backward'."
  (prettier-elisp-re-search-forward
   regexp bound noerror
   (if count
       (- count)
     -1)))

(defun prettier-elisp-move-with (fn &optional n)
  "Move by calling FN N times.
Return new position if changed, nil otherwise."
  (unless (eobp)
    (unless n
      (setq n 1))
    (when-let ((str-start (nth 8
                               (syntax-ppss (point)))))
      (goto-char str-start))
    (let ((init-pos (point))
          (pos)
          (count n))
      (while
          (and (not (= count 0))
               (when-let ((end (ignore-errors
                                 (funcall fn)
                                 (point))))
                 (unless (= end
                            (or pos init-pos))
                   (setq pos end))))
        (setq count
              (1- count)))
      (if (= count 0)
          pos
        (goto-char init-pos)
        nil))))

(defun prettier-elisp-backward-sexp (&optional arg)
  "Move backward up across ARG balanced group of parentheses.
Return new position if changed, nil otherwise."
  (prettier-elisp-move-with #'backward-sexp arg))

(defun prettier-elisp-forward-sexp (&optional arg)
  "Move backward up across ARG balanced group of parentheses.
Return new position if changed, nil otherwise."
  (prettier-elisp-move-with #'forward-sexp arg))

(defun prettier-elisp-backward-list (&optional arg)
  "Move backward across one balanced group of parentheses.
Return new position or nil.
With ARG, do it that many times."
  (prettier-elisp-move-with #'backward-list arg))

(defun prettier-elisp-forward-list (&optional arg)
  "Move forward across one balanced group of parentheses.
Return new position or nil.
With ARG, do it that many times."
  (prettier-elisp-move-with #'forward-list arg))

(defun prettier-elisp-get-list-at-point ()
  "Return list at point or nil."
  (let ((l (when (looking-at "['`]?[(]" )
             (sexp-at-point))))
    (when (listp l)
      (or l '()))))

(defun prettier-elisp-bounds-of-list-at-point ()
  "Return bounds of list at point."
  (when (prettier-elisp-get-list-at-point)
    (let ((start (point)))
      (cons start
            (save-excursion
              (prettier-elisp-move-with 'forward-sexp 1))))))

(defun prettier-elisp-bounds-of-sexp-at-point ()
  "Return bounds of exp at point."
  (let ((start (point)))
    (save-excursion
      (when (prettier-elisp-move-with 'forward-sexp 1)
        (cons start (point))))))

(defun prettier-elisp-symbol-at-point ()
  "Return symbol at point."
  (when (looking-at "[.-+=*/_~!@$%^&:<>{}?A-Za-z0-9]")
    (symbol-at-point)))

(defun prettier-elisp-delete-multi-whitespace-forward ()
  "Delete whitespace at point."
  (when (looking-at "[\s\t\n\r\f]\\{2\\}")
    (let ((beg (point)))
      (delete-region beg
                     (+ (1- beg)
                        (skip-chars-forward "\s\t\n\r\f"))))))

(defun prettier-elisp-delete-whitespace-forward ()
  "Delete whitespace at point."
  (while (looking-at "[\s\t\n\r\f]")
    (delete-char 1)))

(defun prettier-elisp-new-line-and-indent ()
  "Insert a newline if none and indent."
  (unless (looking-back "\\(\\([\n]\\)[\s\t]*\\)?[\n][\s\t]?+" 0)
    (newline-and-indent)))

(defun prettier-elisp-indent-by-fill-column ()
  "If next sexp exceeds `fill-column' insert new line or fill as paragraph."
  (let ((col (current-column))
        (len)
        (beg))
    (save-excursion
      (when
          (and (looking-at "[\s\t\n\r\f]")
               (prettier-elisp-re-search-forward "[^\s\t\n\r\f]" nil t 1))
        (forward-char -1)
        (setq beg (point))
        (prettier-elisp-forward-sexp 1)
        (setq len
              (- (point)
                 beg))))
    (if (and len
             beg
             fill-column
             (> (+ len col)
                fill-column))
        (progn
          (prettier-elisp-delete-whitespace-forward)
          (if-let* ((l (save-excursion
                         (prettier-elisp-get-list-at-point)))
                    (bounds
                     (when
                         (and
                          (car l)
                          (symbolp (car l))
                          (= (length (seq-filter #'symbolp l))
                             (length l))
                          (bounds-of-thing-at-point 'sexp)))))
              (progn (insert "\s")
                     (fill-region-as-paragraph (car bounds)
                                               (cdr bounds)))
            (prettier-elisp-new-line-and-indent)))
      (prettier-elisp-delete-multi-whitespace-forward))))

(defun prettier-elisp-ensure-list-lines ()
  "Ensure thet every list is on own line."
  (while (prettier-elisp-forward-sexp 1)
    (when (looking-back "[)]" 0)
      (save-excursion (prettier-elisp-backward-sexp 1)
                      (when (prettier-elisp-move-with 'down-list 1)
                        (prettier-elisp-ensure-list-lines)))
      (when
          (save-excursion
            (when (prettier-elisp-re-search-forward "[^\s\t]" nil t 1)
              (forward-char -1)
              (not (looking-at "[)\n\r\f]"))))
        (prettier-elisp-new-line-and-indent)))
    (when-let ((symb (symbol-at-point)))
      (pcase symb
        ((or 'save-excursion 'save-restriction)
         (message "point %d" (point))))
      (when (keywordp symb)
        (if (looking-at ":")
            (prettier-elisp-new-line-and-indent)
          (prettier-elisp-backward-sexp 1)
          (prettier-elisp-new-line-and-indent))
        (prettier-elisp-forward-sexp 1)
        (prettier-elisp-indent-by-fill-column)))))

(defun prettier-elisp-indent-inner ()
  "Indent nested list at point."
  (when-let ((l (or (prettier-elisp-get-list-at-point))))
    (save-excursion
      (forward-char 1)
      (prettier-elisp-ensure-list-lines))
    (when-let* ((symbs (and (listp l)
                            (symbolp (car-safe l))
                            (seq-take-while #'symbolp l)))
                (type (car symbs))
                (name (nth 1 symbs)))
      (when (prettier-elisp-move-with 'down-list 1)
        (prettier-elisp-delete-whitespace-forward)
        (pcase type
          ((or 'defun 'cl-defun 'defhydra
               'defclass
               'cl-defclass
               'defmacro
               'cl-defmethod)
           (prettier-elisp-forward-sexp 1)
           (prettier-elisp-indent-by-fill-column)
           (prettier-elisp-forward-sexp 1)
           (prettier-elisp-indent-by-fill-column)
           (prettier-elisp-forward-sexp 1)
           (when (and fill-column
                      (> (current-column)
                         fill-column))
             (forward-sexp -1)
             (newline-and-indent)
             (prettier-elisp-forward-sexp 1))
           (prettier-elisp-delete-whitespace-forward)
           (if (not (looking-at "\""))
               (insert "\s")
             (prettier-elisp-new-line-and-indent)
             (prettier-elisp-forward-sexp 1))
           (newline-and-indent))
          ((or 'defvar
               'defvar-local
               'defcustom
               'defconst)
           (prettier-elisp-forward-sexp 1)
           (prettier-elisp-delete-multi-whitespace-forward)
           (prettier-elisp-forward-sexp 1)
           (prettier-elisp-delete-multi-whitespace-forward)
           (skip-chars-forward "\s\t")
           (when (and fill-column
                      (>= (current-column)
                          fill-column))
             (newline-and-indent))
           (when (prettier-elisp-forward-sexp 2)
             (when (looking-back "\"" 0)
               (prettier-elisp-backward-sexp 1)
               (prettier-elisp-new-line-and-indent)))))
        (while (prettier-elisp-move-with 'backward-up-list)
          (indent-sexp))))))

(defun prettier-elisp-indent-parent ()
  "Indent parent form at top level."
  (let ((pps (syntax-ppss)))
    (when (> (car pps)
             0)
      (goto-char (car (nth 9 pps)))))
  (when-let ((col (and (looking-at "[(]")
                       (current-column))))
    (when (> col 0)
      (let ((pos (point)))
        (skip-chars-backward "\s\t")
        (delete-region (point)
                       pos)
        (when (> (current-column)
                 0)
          (insert "\n")))))
  (indent-sexp))

(defun prettier-elisp-get-bounds-to-narrow ()
  "Get bounds of thing to narrow."
  (let ((pps (syntax-ppss)))
    (when (> (car pps)
             0)
      (goto-char (car (nth 9 pps)))))
  (when (looking-at "[(]")
    (prettier-elisp-bounds-of-sexp-at-point)))

;;;###autoload
(defun prettier-elisp-current ()
  "Prettify current top form."
  (interactive)
  (save-excursion
    (when-let ((bounds (progn (prettier-elisp-indent-parent)
                              (prettier-elisp-indent-inner)
                              (prettier-elisp-get-bounds-to-narrow))))
      (save-excursion
        (goto-char (car bounds))
        (save-restriction
          (narrow-to-region (car bounds)
                            (cdr bounds))
          (save-excursion
            (save-match-data
              (while (prettier-elisp-re-search-forward "^\\s-*[\n]+" nil t 1)
                (let ((beg (match-beginning 0)))
                  (delete-region beg (point))))))
          (save-excursion
            (save-match-data
              (while (prettier-elisp-re-search-forward
                      "(\\([\n\t\s]+\\)[a-zZ-A0-9]" nil t 1)
                (replace-match "" nil nil nil 1))))
          (save-excursion
            (save-match-data
              (while (prettier-elisp-re-search-forward
                      "[a-zZ-A0-9)]\\([\n\t\s]+\\))" nil t 1)
                (when-let ((end (1- (point)))
                           (start (save-excursion
                                    (forward-char -1)
                                    (skip-chars-backward "\s\t\n")
                                    (unless (nth 4
                                                 (syntax-ppss (point)))
                                      (point)))))
                  (delete-region start end)))))
          (save-excursion
            (save-match-data
              (while (prettier-elisp-re-search-forward
                      "[a-zZ-A0-9]\\((\\)" nil t 1)
                (replace-match "\s(" nil nil nil 1)))))))))

(defvar prettier-elisp-newline-symbols-re nil)

;;;###autoload
(defun prettier-elisp-ensure-newlines ()
  "Add new line after top forms defined in `prettier-elisp-newline-symbols'."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (when (and (null prettier-elisp-newline-symbols-re)
               (<= 1
                   (length prettier-elisp-newline-symbols)))
      (setq prettier-elisp-newline-symbols-re
            (concat "\\(" "\\(^\\s-+\\)[(]"
                    (regexp-opt prettier-elisp-newline-symbols t)
                    "\\)")))
    (when prettier-elisp-newline-symbols-re
      (save-match-data
        (while (prettier-elisp-re-search-forward
                prettier-elisp-newline-symbols-re nil t 1)
          (when (= 1
                   (nth 0
                        (syntax-ppss (point))))
            (replace-match "" nil nil nil 2)))))
    (goto-char (point-min))
    (save-match-data
      (while (prettier-elisp-re-search-forward ")\n\n\\(\n+\\)" nil t 1)
        (when (match-string-no-properties 1)
          (replace-match "" nil nil nil 1))))
    (when prettier-elisp-newline-symbols
      (goto-char (point-min))
      (save-match-data
        (while (let ((pos (point))
                     (end))
                 (setq end (prettier-elisp-forward-list 1))
                 (unless
                     (equal pos end)
                   end))
          (let ((sexp-start (save-excursion (prettier-elisp-backward-list)
                                            (point))))
            (when (and
                   (< sexp-start
                      (line-beginning-position))
                   (looking-at "\\(\\([\s\t]+\\)?[\n]\\)\\([;(]\\)"))
              (let ((pos (point)))
                (delete-region pos
                               (point))
                (insert "\n")))))))))

;;;###autoload
(defun prettier-elisp-string (str)
  "Format STR and return result."
  (with-temp-buffer
    (insert str)
    (delay-mode-hooks
      (emacs-lisp-mode)
      (prettier-elisp-format-buffer))
    (buffer-string)))

;;;###autoload
(defun prettier-elisp ()
  "Format current defun at point and multy lines in buffer."
  (interactive)
  (save-match-data (prettier-elisp-ensure-newlines))
  (save-match-data (prettier-elisp-current)))

;;;###autoload
(defun prettier-elisp-format-buffer ()
  "Format current defun at point."
  (interactive)
  (save-match-data
    (prettier-elisp-ensure-newlines))
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-max))
      (when (prettier-elisp-re-search-backward "[)]" nil t 1)
        (forward-char 1)
        (while (and (prettier-elisp-backward-list)
                    (looking-at "[(]"))
          (save-excursion
            (forward-char 1)
            (prettier-elisp-current)))))))

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