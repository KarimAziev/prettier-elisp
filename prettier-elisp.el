;;; prettier-elisp.el --- Minor mode to format emacs lisp code -*- lexical-binding: t -*-

;; Copyright (C) Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/prettier-elisp
;; Keywords: convenience edit elisp
;; Version: 0.2.0
;; Package-Requires: ((emacs "27.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Formats your elisp code using on file save.

;;; Code:

(eval-when-compile
  (require 'subr-x))

(declare-function replace-region-contents "subr-x"
                  (beg end replace-fn
                       &optional max-secs
                       max-costs))

(defgroup prettier-elisp nil
  "Minor mode to format Elisp code on file save."
  :group 'languages
  :prefix "prettier-elisp"
  :link
  '(url-link
    :tag "Repository" "https://github.com/KarimAziev/prettier-elisp"))

(defcustom prettier-elisp-pre-format-hooks '(whitespace-cleanup)
  "Hooks to run before formatting."
  :group 'prettier-elisp
  :type '(hook :options (whitespace-cleanup)))

(defcustom prettier-elisp-buffer-post-format-hooks
  '(prettier-elisp-indent-buffer)
  "Hooks to run after formatting the whole buffer."
  :group 'prettier-elisp
  :type '(hook :options (prettier-elisp-indent-buffer)))

(defvar prettier-elisp--errors nil
  "List of errors and warnings for the current buffer.
This is bound dynamically while the checks run.")

(defun prettier-elisp--error (line col type message)
  "Construct a datum for error at LINE and COL with TYPE and MESSAGE."
  (push (list line col type message) prettier-elisp--errors))

(defun prettier-elisp--error-at-point (type message &optional pos)
  "Construct a datum for error at POS with TYPE and MESSAGE.
POS defaults to `point'."
  (save-excursion
    (when pos
      (goto-char pos))
    (prettier-elisp--error (line-number-at-pos)
                           (- (point)
                              (line-beginning-position))
                           type message)))

(defun prettier-elisp--inside-comment-or-string-p ()
  "Return non-nil if point is inside a comment or string."
  (let ((ppss (save-match-data
                (syntax-ppss))))
    (or (nth 3 ppss)
        (nth 4 ppss))))

(defun prettier-elisp--map-regexp-match (regexp callback)
  "For every match of REGEXP, call CALLBACK with the first match group.
If callback returns non-nil, the return value - which must be a
list - will be applied to `prettier-elisp--error-at-point'.  If
REGEXP doesn't produce a match group 1, then match group
0 (ie. the whole match string string) will be passed to
CALLBACK."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (let ((sym (or (match-string-no-properties 1)
                     (match-string-no-properties 0))))
        (save-excursion
          (goto-char (or (match-beginning 1)
                         (match-beginning 0)))
          (funcall callback sym))))))

(defun prettier-elisp--fix-lonely-parens ()
  "Warn about dangling closing parens."
  (prettier-elisp--map-regexp-match
   "^\\s-*?\\()\\)"
   (lambda (_)
     ;; Allow dangling parentheses if the preceding line ends with a comment, as
     ;; it's not uncommon even in idiomatic lisp.
     (when-let* ((end (point))
                 (start
                  (save-excursion
                    (skip-chars-backward "\s\t\r\n\f")
                    (when (not (nth 4 (syntax-ppss)))
                      (point)))))
       (delete-region start end)))))

(defun prettier-elisp-re-search-forward-inner (regexp &optional bound count)
  "Helper function for `prettier-elisp-re-search-forward'.
Arguments REGEXP, BOUND, COUNT has the same meaning as for `re-search-forward'."
  (let ((parse))
    (while (> count 0)
      (with-syntax-table
          emacs-lisp-mode-syntax-table
        (re-search-forward regexp bound)
        (setq parse
              (save-excursion
                (syntax-ppss (match-beginning 0))))
        (cond ((and (nth 3 parse)
                    (nth 8 parse))
               (goto-char (nth 8 parse))
               (forward-sexp))
              ((and (nth 4 parse)
                    (nth 8 parse))
               (goto-char (nth 8 parse))
               (forward-line))
              ((and
                (not (nth 3 parse))
                (not (nth 4 parse))
                (looking-at ";"))
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
  (unless n (setq n 1))
  (when-let ((str-start (nth 8 (syntax-ppss (point)))))
    (goto-char str-start))
  (let ((init-pos (point))
        (pos)
        (count (if (> n 0) n (- n))))
    (while
        (and (not (= count 0))
             (when-let ((end (ignore-errors
                               (funcall fn (if
                                               (> n 0) 1
                                             -1))
                               (point))))
               (unless (or (= end
                              (or pos init-pos))
                           (nth 4 (syntax-ppss (point)))
                           (and (looking-at ";")
                                (nth 4 (syntax-ppss (1+ (point))))))
                 (setq pos end))))
      (setq count
            (1- count)))
    (if (= count 0)
        pos
      (goto-char init-pos)
      nil)))

(defun prettier-elisp-backward-sexp (&optional arg)
  "Move backward across ARG balanced group of parentheses.
Return new position if changed, nil otherwise."
  (prettier-elisp-move-with #'forward-sexp
                            (if (not arg)
                                -1
                              (if (> arg 0)
                                  (- arg)
                                arg))))

(defun prettier-elisp-forward-sexp (&optional arg)
  "Move forward across ARG balanced group of parentheses.
Return new position if changed, nil otherwise."
  (prettier-elisp-move-with #'forward-sexp arg))

(defun prettier-elisp-backward-up-list (&optional arg)
  "Move backward across one balanced group of parentheses.
Return new position or nil.
With ARG, do it that many times."
  (when (prettier-elisp-move-with #'backward-up-list (if (not arg)
                                                         -1
                                                       (if (> arg 0)
                                                           (- arg)
                                                         arg)))
    (forward-sexp -1)
    (point)))

(defun prettier-elisp-get-list-at-point ()
  "Return list at point or nil."
  (let* ((parse (syntax-ppss (point)))
         (l
          (when (and
                 (not (or (nth 3 parse)
                          (nth 4 parse)))
                 (looking-at "['`]?[(\\[]"))
            (sexp-at-point))))
    (when (or (listp l)
              (vectorp l))
      (or l '()))))

(defun prettier-elisp-delete-whitespace-forward-unless-comment ()
  "Delete whitespace at point if there are no comment after."
  (let ((beg (point))
        (end)
        (done))
    (setq end (+ beg (skip-chars-forward "\s\t")))
    (if (looking-at comment-start)
        (forward-comment most-positive-fixnum)
      (when (> end beg)
        (delete-region beg end)
        (setq done t))
      (setq beg (point))
      (setq end (+ beg (skip-chars-forward "\s\t\r\f\n")))
      (when (> end beg)
        (if (looking-at comment-start)
            (forward-comment most-positive-fixnum)
          (delete-region beg end)
          (setq done t)))
      done)))

(defun prettier-elisp-delete-multi-whitespace-forward ()
  "Delete consecutive whitespace ahead."
  (when (looking-at "[\s\t\n\r\f]\\{2\\}")
    (let ((beg (point)))
      (delete-region beg
                     (+ (1- beg)
                        (skip-chars-forward "\s\t\n\r\f"))))))

(defun prettier-elisp-delete-whitespace-forward ()
  "Delete whitespace ahead of the cursor."
  (delete-region (point)
                 (+ (point)
                    (skip-chars-forward "\s\t\n\r\f"))))

(defun prettier-elisp-forward-comments ()
  "Forward comments if point is on the comment start."
  (when (looking-at comment-start)
    (forward-comment most-positive-fixnum)))

(defun prettier-elisp-line-empty-p ()
  "Return t if current line is empty."
  (string-empty-p
   (string-trim
    (buffer-substring-no-properties (line-beginning-position)
                                    (line-end-position)))))

(defun prettier-elisp-new-line-and-indent ()
  "Insert a newline if none and indent."
  (unless (looking-back "\\(\\([\n]\\)[\s\t]*\\)?[\n][\s\t]*" 0)
    (when (looking-back "[@`',][\s\t]*" 0)
      (skip-chars-backward "@`',\s\t"))
    (prettier-elisp-delete-whitespace-forward-unless-comment)
    (unless (or (looking-at "\\]\\|)")
                (looking-back "\\[\\|(" 0))
      (newline-and-indent))))

(defun prettier-elisp-indent-by-fill-column ()
  "If next sexp exceeds `fill-column' insert new line or fill as paragraph."
  (let ((col (current-column))
        (len
         (save-excursion
           (when (prettier-elisp-re-search-forward "[^\s\t\n\r\f]" nil t 1)
             (forward-char -1)
             (let ((beg (point)))
               (when (prettier-elisp-forward-sexp 1)
                 (- (point)
                    beg)))))))
    (when (and len fill-column
               col
               (> (+ len col)
                  fill-column))
      (prettier-elisp-delete-multi-whitespace-forward)
      (if-let ((l (prettier-elisp-get-list-at-point)))
          (cond ((and
                  (or (symbolp (car-safe l))
                      (stringp (car-safe l)))
                  (listp (cdr-safe l))
                  (=
                   (length
                    (seq-filter
                     (lambda (it)
                       (if (stringp it)
                           (not
                            (string-match-p
                             "[\n\r\f]" it))
                         (symbolp it)))
                     l))
                   (length l)))
                 (let ((bounds
                        (bounds-of-thing-at-point 'sexp)))
                   (fill-region-as-paragraph (car bounds)
                                             (cdr bounds)))))
        (prettier-elisp-new-line-and-indent)))))

(defun prettier-elisp-join-line ()
  "Join this line to previous unless the previous one is comment."
  (when (save-excursion
          (when (= 0 (forward-line -1))
            (end-of-line)
            (not (prettier-elisp--inside-comment-or-string-p))))
    (join-line)))

(defun prettier-elisp-insert-divider (divider)
  "Insert DIVIDER."
  (when (prettier-elisp-delete-whitespace-forward-unless-comment)
    (insert divider)))

(defun prettier-elisp-looking-at-comment-p ()
  "Return non nil if next token is comment excluding whitespaces."
  (looking-at (concat "[\s\t\n\r\f]?+" comment-start)))

(defun prettier-elisp-align-pairs ()
  "Align pairs."
  (while
      (when (symbol-at-point)
        (prettier-elisp-forward-sexp 1)
        (when (prettier-elisp-looking-at-comment-p)
          (forward-comment most-positive-fixnum))
        (if (and (>= (current-column) fill-column)
                 (not (prettier-elisp-looking-at-comment-p)))
            (prettier-elisp-insert-divider "\s"))
        (if (not (prettier-elisp-get-list-at-point))
            (prettier-elisp-forward-sexp)
          (when (prettier-elisp-move-with 'down-list)
            (prettier-elisp-ensure-list-lines))
          (prettier-elisp-backward-up-list)
          (prettier-elisp-forward-sexp))
        (prettier-elisp-delete-whitespace-forward-unless-comment)
        (when (save-excursion
                (prettier-elisp-forward-sexp))
          (newline-and-indent)
          t))))

(defun prettier-elisp-consp (sexp)
  "Return non nil if SEXP is a cons pair."
  (and sexp
       (consp sexp)
       (atom (car sexp))
       (atom (cdr sexp))))

(defun prettier-elisp-all-conses ()
  "Fix all conses."
  (while (prettier-elisp-re-search-backward "\\." nil t 1)
    (when (prettier-elisp-backward-up-list)
      (let ((sexp (sexp-at-point)))
        (save-excursion
          (pcase sexp
            ((pred prettier-elisp-consp)
             (prettier-elisp-move-with 'down-list)
             (prettier-elisp-delete-whitespace-forward-unless-comment)
             (when (or (symbol-at-point)
                       (stringp (sexp-at-point)))
               (forward-sexp)
               (prettier-elisp-insert-divider "\s")
               (when (= 1 (skip-chars-forward "."))
                 (prettier-elisp-insert-divider "\s"))))))))))

(defun prettier-elisp-ensure-list-lines ()
  "Ensure thet every list is on own line."
  (while (prettier-elisp-forward-sexp 1)
    (when (looking-back "\\()\\|]\\)[\s\t]*" 0)
      (save-excursion
        (prettier-elisp-backward-sexp 1)
        (when (prettier-elisp-move-with 'down-list 1)
          (prettier-elisp-delete-whitespace-forward-unless-comment)
          (prettier-elisp-ensure-list-lines)
          (prettier-elisp-delete-whitespace-forward-unless-comment)))
      (when (save-excursion
              (when (prettier-elisp-re-search-forward "[^\s\t]" nil t 1)
                (forward-char -1)
                (and (looking-at "[\\[(]")
                     (not (looking-back "[\\[(]" 0)))))
        (prettier-elisp-new-line-and-indent)))
    (when (and (save-excursion
                 (prettier-elisp-backward-sexp 2))
               (> (current-column)
                  fill-column))
      (prettier-elisp-backward-sexp 1)
      (prettier-elisp-new-line-and-indent)
      (prettier-elisp-forward-sexp 1))
    (when-let ((symb
                (symbol-at-point)))
      (pcase symb
        ((or 'save-excursion
             'save-restriction
             'save-match-data)
         (when (and (looking-at "[\s\t]")
                    (save-excursion
                      (skip-chars-forward "\s\t")
                      (not (looking-at comment-start))))
           (prettier-elisp-delete-whitespace-forward)
           (prettier-elisp-new-line-and-indent)))
        ('pcase
            (save-excursion
              (prettier-elisp-backward-up-list 1)
              (prettier-elisp-new-line-and-indent))
          (prettier-elisp-forward-sexp 1)
          (when (symbol-at-point)
            (prettier-elisp-new-line-and-indent)))
        ((or
          'require 'let 'if-let 'when-let 'let* 'if-let* 'when-let* 'cond 'when
          'and-let 'and-let* 'unless 'defgroup 'defun 'cl-defun 'defclass
          'defmethod 'cl-defmethod 'with-eval-after-load 'defmacro
          'global-set-key 'define-key 'define-minor-mode 'defhydra
          'pretty-hydra-define 'use-package 'use-package! 'defvar-local 'defvar
          'defcustom)
         (save-excursion
           (prettier-elisp-backward-up-list 1)
           (prettier-elisp-new-line-and-indent))
         (prettier-elisp-delete-whitespace-forward)
         (save-excursion
           (insert "\s")))
        ((or
          'setq 'setq-default 'setq-local)
         (prettier-elisp-insert-divider "\s")
         (prettier-elisp-align-pairs))
        ((pred keywordp)
         (cond ((not
                 (save-excursion
                   (prettier-elisp-backward-sexp 2)))
                (prettier-elisp-backward-sexp 1)
                (when (looking-back "(\\([\n\t\s]+\\)"
                                    0)
                  (when (prettier-elisp-backward-up-list 1)
                    (forward-char 1)
                    (prettier-elisp-delete-whitespace-forward)))
                (prettier-elisp-forward-sexp 1))
               ((or
                 (save-excursion
                   (prettier-elisp-backward-up-list 1)
                   (memq
                    (car-safe
                     (prettier-elisp-get-list-at-point))
                    '(defcustom defgroup use-package! use-package
                       transient-define-argument transient-define-prefix
                       transient-define-suffix transient-define-infix)))
                 (save-excursion
                   (when (prettier-elisp-forward-sexp 1)
                     (prettier-elisp-backward-sexp 1)
                     (unless (keywordp
                              (symbol-at-point))
                       (when (prettier-elisp-forward-sexp 2)
                         (prettier-elisp-backward-sexp 1)
                         (keywordp
                          (symbol-at-point))))))
                 (save-excursion
                   (when (prettier-elisp-backward-sexp 3)
                     (keywordp
                      (symbol-at-point)))))
                (prettier-elisp-backward-sexp 1)
                (prettier-elisp-new-line-and-indent)
                (prettier-elisp-forward-sexp 1))))))))

(defun prettier-elisp-indent-inner ()
  "Indent nested list at point."
  (when-let ((l (or (prettier-elisp-get-list-at-point))))
    (save-excursion
      (forward-char 1)
      (prettier-elisp-ensure-list-lines)
      (while (prettier-elisp-move-with 'backward-up-list)
        (indent-sexp)))
    (when-let* ((symbs (and (proper-list-p l)
                            (symbolp (car-safe l))
                            (seq-take-while #'symbolp l)))
                (type (car symbs))
                (name (nth 1 symbs)))
      (when (prettier-elisp-move-with 'down-list 1)
        (prettier-elisp-delete-whitespace-forward)
        (pcase type
          ((or 'defun 'cl-defun 'defhydra 'defclass 'cl-defclass 'defmacro
               'cl-defmethod 'transient-define-infix 'transient-define-argument
               'transient-define-prefix 'transient-define-suffix)
           (prettier-elisp-forward-sexp 1)
           (prettier-elisp-indent-by-fill-column)
           (prettier-elisp-forward-sexp 1)
           (prettier-elisp-delete-whitespace-forward)
           (insert "\s")
           (prettier-elisp-indent-by-fill-column)
           (prettier-elisp-forward-sexp 1)
           (when (and fill-column
                      (> (current-column)
                         fill-column))
             (forward-sexp -1)
             (prettier-elisp-new-line-and-indent)
             (prettier-elisp-forward-sexp 1))
           (prettier-elisp-delete-whitespace-forward)
           (if (not (looking-at "\""))
               (insert "\s")
             (prettier-elisp-new-line-and-indent)
             (prettier-elisp-forward-sexp
              1))
           (prettier-elisp-new-line-and-indent))
          ((or 'defgroup 'defcustom 'define-widget)
           (prettier-elisp-forward-sexp 1)
           (prettier-elisp-delete-multi-whitespace-forward)
           (prettier-elisp-forward-sexp 1)
           (prettier-elisp-delete-multi-whitespace-forward)
           (skip-chars-forward "\s\t")
           (when (and fill-column
                      (>= (current-column)
                          fill-column))
             (prettier-elisp-new-line-and-indent))
           (when (prettier-elisp-forward-sexp 2)
             (when (looking-back "\"" 0)
               (prettier-elisp-backward-sexp 1)
               (prettier-elisp-new-line-and-indent)))))
        (while (prettier-elisp-move-with 'backward-up-list)
          (indent-sexp))))))

(defmacro prettier-elisp-with-temp-buffer (str &rest body)
  "Insert STR and evaluate BODY in temporarily buffer with elisp syntax."
  (declare (indent 0)
           (debug t))
  `(with-temp-buffer
     (erase-buffer)
     (progn
       (let ((indent-tabs-mode nil))
         (set-syntax-table emacs-lisp-mode-syntax-table)
         (setq-local syntax-propertize-function #'elisp-mode-syntax-propertize)
         (setq-local comment-start ";")
         (setq-local comment-start-skip ";+ *")
         (setq-local comment-end "")
         (setq-local open-paren-in-column-0-is-defun-start t)
         (setq parse-sexp-ignore-comments t)
         (syntax-ppss-flush-cache (point-min))
         (insert ,str)
         (funcall syntax-propertize-function (point-min)
                  (point-max))
         ,@body))))

(defun prettier-elisp-format (body)
  "Format BODY."
  (prettier-elisp-with-temp-buffer
    body
    (goto-char (point-min))
    (prettier-elisp-indent-inner)
    (save-excursion
      (save-match-data
        (while
            (prettier-elisp-re-search-forward "[(`']\\([\n\t\r\s\f]+\\)"
                                              nil t 1)
          (let ((beg (match-beginning 0)))
            (delete-region (1+ beg)
                           (point))))))
    (save-match-data
      (prettier-elisp--fix-lonely-parens))
    (save-excursion
      (save-match-data
        (while (prettier-elisp-re-search-forward
                "\\(\\]\\|)\\)[\n][\s]*\\(\\]\\|)\\)" nil t
                1)
          (forward-char -1)
          (join-line))))
    (save-excursion
      (save-match-data
        (while (prettier-elisp-re-search-forward "^\\s-*[\n]+" nil t 1)
          (let ((beg (match-beginning 0)))
            (delete-region beg (point))))))
    (save-excursion
      (save-match-data
        (while
            (prettier-elisp-re-search-forward
             "(\\([\n\t\s]+\\)[a-z0-9:.+$!-]" nil t 1)
          (replace-match "" nil nil nil 1))))
    (save-excursion
      (save-match-data
        (while
            (prettier-elisp-re-search-forward
             "[a-zZ0-9:.+$!-]\\([\n\t\s]+\\))" nil t 1)
          (when-let ((end (1- (point)))
                     (start
                      (save-excursion
                        (forward-char -1)
                        (skip-chars-backward "\s\t\n")
                        (unless (nth 4
                                     (syntax-ppss (point)))
                          (point)))))
            (delete-region start end)))))
    (save-excursion
      (save-match-data
        (while
            (prettier-elisp-re-search-forward
             "[a-z0-9:.+$!-]\\((\\)" nil t 1)
          (replace-match "\s(" nil nil nil 1))))
    (save-excursion
      (save-match-data
        (while
            (prettier-elisp-re-search-forward
             ")\\([\s\t]+\\)[)]" nil t 1)
          (replace-match "" nil nil nil 1))))
    (save-excursion
      (save-match-data
        (while (prettier-elisp-re-search-forward "\\([\s\t]+\\)[\n\r\f]"
                                                 nil t 1)
          (replace-match "" nil nil nil 1))))
    (save-excursion
      (goto-char (point-max))
      (save-match-data
        (prettier-elisp-all-conses)))
    (string-trim (buffer-string))))

(defun prettier-elisp-current-defun ()
  "Prettify current top form."
  (when-let* ((buff (current-buffer))
              (pos (point))
              (beg
               (let ((pps (syntax-ppss)))
                 (when (> (car pps) 0)
                   (car (nth 9 pps)))))
              (end
               (save-excursion
                 (goto-char beg)
                 (prettier-elisp-move-with 'forward-sexp 1))))
    (when (and beg end)
      (let ((left-body (buffer-substring-no-properties beg pos))
            (right-body (buffer-substring-no-properties pos end))
            (body)
            (replacement))
        (setq body (concat left-body right-body))
        (setq replacement (prettier-elisp-format body))
        (unless (or (not body)
                    (not replacement)
                    (string= body replacement)
                    (not
                     (prettier-elisp-compare-strings-ignore-whitespace
                      body
                      replacement)))
          (replace-region-contents
           beg end (lambda () replacement)))))))

(defun prettier-elisp-get-line-rules (sexp)
  "Return amount of lines before SEXP."
  (pcase sexp
    (`(defvar ,_name) 1)
    (`(defvar ,_name nil) 1)
    (`(defvar ,_name ,_)
     (if (>= (length (format "%s" sexp)) fill-column) 2 1))
    (`(defvar-local ,_name ,_)
     (if (>= (length (format "%s" sexp)) fill-column) 2 1))
    (`(require ,_name) 1)
    (`(require ,_name ,_file) 1)
    (`(require ,_name ,_file _no-err) 1)
    (`(provide ,_name ,_file _no-err) 1)
    (`(declare-function ,_name ,_file) 1)
    (`(declare-function ,_name ,_file ,_args) 1)
    (`(declare-function ,_name ,_file ,_args ,_fileonly) 1)
    (`(load ,_name) 1)
    (`(load ,_name ,_) 1)
    (`(load ,_name ,_ ,_) 1)
    (`(load ,_name ,_ ,_ ,_) 1)
    (`(load ,_name ,_ ,_ ,_ ,_) 1)
    (`(provide _) 1)
    (`(provide) 1)
    (`(provide-theme _) 1)
    (_ 2)))

(defun prettier-elisp-compare-strings-ignore-whitespace (string1 string2)
  "Check if two strings are equal, ignoring whitespace and newlines.
Argument STRING1 is the first string to compare.
Argument STRING2 is the second string to compare."
  (let ((clean-string1 (replace-regexp-in-string "[[:space:]\n]+" "" string1))
        (clean-string2 (replace-regexp-in-string "[[:space:]\n]+" "" string2)))
    (string= clean-string1 clean-string2)))

(defun prettier-elisp--ensure-top-level-newlines ()
  "Add new line after top forms."
  (save-excursion
    (goto-char (point-max))
    (let ((pos)
          (sexp-count))
      (while
          (when-let ((new-pos (ignore-errors
                                (let ((parse-sexp-ignore-comments
                                       t))
                                  (backward-list)))))
            (when (or (not pos)
                      (not (= new-pos pos)))
              (setq pos new-pos)))
        (when-let ((sexp (prettier-elisp-get-list-at-point)))
          (unless sexp-count
            (save-excursion
              (forward-sexp 1)
              (prettier-elisp-delete-whitespace-forward)
              (newline 1)))
          (setq sexp-count (1+ (or sexp-count 0)))
          (let ((prev-sexp
                 (when-let ((new-pos
                             (ignore-errors
                               (let ((parse-sexp-ignore-comments
                                      t))
                                 (backward-list)))))
                   (unless (= new-pos pos)
                     (prettier-elisp-get-list-at-point)))))
            (if (not prev-sexp)
                (progn
                  (goto-char pos)
                  (forward-line -1)
                  (while (looking-at ";;;###")
                    (forward-line -1))
                  (delete-blank-lines)
                  (unless (prettier-elisp-line-empty-p)
                    (newline)))
              (forward-sexp)
              (let* ((count
                      (if (and (= 1 (prettier-elisp-get-line-rules prev-sexp))
                               (eq (car-safe sexp)
                                   (car-safe prev-sexp)))
                          1
                        2)))
                (prettier-elisp-delete-whitespace-forward)
                (newline count))))))
      sexp-count)))

(defun prettier-elisp-format-all-forms ()
  "Format all top level forms in buffer."
  (save-excursion
    (goto-char (point-max))
    (let ((pos))
      (while
          (when-let ((new-pos (ignore-errors
                                (let ((parse-sexp-ignore-comments
                                       t))
                                  (backward-list)))))
            (when (or (not pos)
                      (not (= new-pos pos)))
              (setq pos new-pos)))
        (when (prettier-elisp-get-list-at-point)
          (unless (bolp)
            (let* ((end (point))
                   (beg (+ end (skip-chars-backward "\s\t"))))
              (delete-region beg end)))
          (save-excursion
            (down-list)
            (prettier-elisp-current-defun)))))))


(defun prettier-elisp-calculate-lisp-indent (&optional parse-start)
  "Calculate indentation for Lisp code.

Optional argument PARSE-START is a buffer position. If it is a marker or
integer, it starts parsing at that position. If nil, it begins at the start of
the current defun."
;; This line because `calculate-lisp-indent-last-sexp` was defined with
;; `defvar` with its value ommited, marking it special and only defining it
;; locally. So if you don't have this, you'll get a void variable error.
  (defvar calculate-lisp-indent-last-sexp)
  (save-excursion
    (beginning-of-line)
    (let ((indent-point (point))
          state
          ;; setting this to a number inhibits calling hook
          (desired-indent nil)
          (retry t)
          calculate-lisp-indent-last-sexp containing-sexp)
      (cond ((or (markerp parse-start)
                 (integerp parse-start))
             (goto-char parse-start))
            ((null parse-start)
             (beginning-of-defun))
            (t (setq state parse-start)))
      (unless state
      ;; Find outermost containing sexp
        (while (< (point) indent-point)
          (setq state (parse-partial-sexp (point) indent-point 0))))
          ;; Find innermost containing sexp
      (while (and retry
                  state
                  (> (elt state 0) 0))
        (setq retry nil)
        (setq calculate-lisp-indent-last-sexp (elt state 2))
        (setq containing-sexp (elt state 1))
        ;; Position following last unclosed open.
        (goto-char (1+ containing-sexp))
        ;; Is there a complete sexp since then?
        (if (and calculate-lisp-indent-last-sexp
                 (> calculate-lisp-indent-last-sexp (point)))
                 ;; Yes, but is there a containing sexp after that?
            (let ((peek (parse-partial-sexp calculate-lisp-indent-last-sexp
                                            indent-point 0)))
              (if (setq retry (car (cdr peek)))
                  (setq state peek)))))
      (if retry
          nil
          ;; Innermost containing sexp found
        (goto-char (1+ containing-sexp))
        (if (not calculate-lisp-indent-last-sexp)
        ;; indent-point immediately follows open paren.
        ;; Don't call hook.
            (setq desired-indent (current-column))
            ;; Find the start of first element of containing sexp.
          (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
          (cond ((looking-at "\\s(")
          ;; First element of containing sexp is a list.
          ;; Indent under that list.
                 )
                ((> (save-excursion
                      (forward-line 1)
                      (point))
                    calculate-lisp-indent-last-sexp)
                    ;; This is the first line to start within the containing sexp.
                    ;; It's almost certainly a function call.
                 (if (or
                 ;; Containing sexp has nothing before this line
                 ;; except the first element. Indent under that element.
                      (= (point) calculate-lisp-indent-last-sexp)

;; First sexp after `containing-sexp' is a keyword. This
;; condition is more debatable. It's so that I can have
;; unquoted plists in macros. It assumes that you won't
;; make a function whose name is a keyword.
                      (when-let (char-after (char-after (1+ containing-sexp)))
                        (char-equal char-after ?:))

;; Check for quotes or backquotes around.
                      (let* ((positions (elt state 9))
                             (last (car (last positions)))
                             (rest (reverse (butlast positions)))
                             (any-quoted-p nil)
                             (point nil))
                        (or
                         (when-let (char (char-before last))
                           (or (char-equal char ?')
                               (char-equal char ?`)))
                         (progn
                           (while (and rest (not any-quoted-p))
                             (setq point (pop rest))
                             (setq any-quoted-p
                                   (or
                                    (when-let (char (char-before point))
                                      (or (char-equal char ?')
                                          (char-equal char ?`)))
                                    (save-excursion
                                      (goto-char (1+ point))
                                      (looking-at-p
                                       "\\(?:back\\)?quote[\t\n\f\s]+(")))))
                           any-quoted-p))))
                           ;; Containing sexp has nothing before this line
                           ;; except the first element.  Indent under that element.
                     nil
                     ;; Skip the first element, find start of second (the first
                     ;; argument of the function call) and indent under.
                   (progn (forward-sexp 1)
                          (parse-partial-sexp (point)
                                              calculate-lisp-indent-last-sexp
                                              0 t)))
                 (backward-prefix-chars))
                (t
                ;; Indent beneath first sexp on same line as
                ;; `calculate-lisp-indent-last-sexp'.  Again, it's
                ;; almost certainly a function call.
                 (goto-char calculate-lisp-indent-last-sexp)
                 (beginning-of-line)
                 (parse-partial-sexp (point) calculate-lisp-indent-last-sexp
                                     0 t)
                 (backward-prefix-chars)))))
                 ;; Point is at the point to indent under unless we are inside a string.
                 ;; Call indentation hook except when overridden by lisp-indent-offset
                 ;; or if the desired indentation has already been computed.
      (let ((normal-indent (current-column)))
        (cond ((elt state 3)
        ;; Inside a string, don't change indentation.
               nil)
              ((and (integerp lisp-indent-offset) containing-sexp)
              ;; Indent by constant offset
               (goto-char containing-sexp)
               (+ (current-column) lisp-indent-offset))
               ;; in this case calculate-lisp-indent-last-sexp is not nil
              (calculate-lisp-indent-last-sexp
               (or
               ;; try to align the parameters of a known function
                (and lisp-indent-function
                     (not retry)
                     (funcall lisp-indent-function indent-point state))
                     ;; If the function has no special alignment
                     ;; or it does not apply to this argument,
                     ;; try to align a constant-symbol under the last
                     ;; preceding constant symbol, if there is such one of
                     ;; the last 2 preceding symbols, in the previous
                     ;; uncommented line.
                (and (save-excursion
                       (goto-char indent-point)
                       (skip-chars-forward " \t")
                       (looking-at ":"))
                       ;; The last sexp may not be at the indentation
                       ;; where it begins, so find that one, instead.
                     (save-excursion
                       (goto-char calculate-lisp-indent-last-sexp)
                       ;; Handle prefix characters and whitespace
                       ;; following an open paren.  (Bug#1012)
                       (backward-prefix-chars)
                       (while (not (or
                                    (looking-back "^[ \t]*\\|([ \t]+"
                                                  (line-beginning-position))
                                    (and containing-sexp
                                         (>= (1+ containing-sexp)
                                             (point)))))
                         (forward-sexp -1)
                         (backward-prefix-chars))
                       (setq calculate-lisp-indent-last-sexp (point)))
                     (> calculate-lisp-indent-last-sexp
                        (save-excursion
                          (goto-char (1+ containing-sexp))
                          (parse-partial-sexp (point)
                                              calculate-lisp-indent-last-sexp 0
                                              t)
                          (point)))
                     (let ((parse-sexp-ignore-comments t)
                           indent)
                       (goto-char calculate-lisp-indent-last-sexp)
                       (or (and (looking-at ":")
                                (setq indent (current-column)))
                           (and (< (line-beginning-position)
                                   (prog2 (backward-sexp)
                                       (point)))
                                (looking-at ":")
                                (setq indent (current-column))))
                       indent))
                       ;; another symbols or constants not preceded by a constant
                       ;; as defined above.
                normal-indent))
                ;; in this case calculate-lisp-indent-last-sexp is nil
              (desired-indent)
              (t
               normal-indent))))))


(defun prettier-elisp-indent-buffer ()
  "Indent whole buffer."
  (indent-region (point-min)
                 (point-max)))

;;;###autoload
(defun prettier-elisp-ensure-top-level-newlines ()
  "Format lines before and after top forms."
  (interactive)
  (prettier-elisp--ensure-top-level-newlines))

;;;###autoload
(defun prettier-elisp-string (str)
  "Format STR and return result."
  (with-temp-buffer
    (insert str)
    (let ((emacs-lisp-mode-hook nil))
      (emacs-lisp-mode)
      (dolist (fn '(prettier-elisp-format-all-forms
                    prettier-elisp--ensure-top-level-newlines))
        (funcall fn))
      (concat (string-trim (buffer-string)) "\n"))))

;;;###autoload
(defun prettier-elisp-to-string (sexp)
  "Format SEXP and return result."
  (prettier-elisp-string (prin1-to-string sexp)))

;;;###autoload
(defun prettier-elisp ()
  "Format current top level form."
  (interactive)
  (run-hooks 'prettier-elisp-pre-format-hooks)
  (prettier-elisp-current-defun))

;;;###autoload
(defun prettier-elisp-buffer ()
  "Format all buffer."
  (interactive)
  (run-hooks 'prettier-elisp-pre-format-hooks)
  (dolist (fn '(prettier-elisp--ensure-top-level-newlines
                prettier-elisp-format-all-forms))
    (funcall fn))
  (run-hooks 'prettier-elisp-buffer-post-format-hooks))

;;;###autoload
(define-minor-mode prettier-elisp-buffer-mode
  "Format whole buffer on file save when this mode is turned on."
  :lighter " Prettier"
  :global nil
  (prettier-elisp-mode -1)
  (remove-hook 'before-save-hook #'prettier-elisp-buffer 'local)
  (advice-remove #'calculate-lisp-indent #'prettier-elisp-calculate-lisp-indent)
  (when prettier-elisp-buffer-mode
    (advice-add #'calculate-lisp-indent :override
                #'prettier-elisp-calculate-lisp-indent)
    (add-hook 'before-save-hook #'prettier-elisp-buffer nil 'local)))

;;;###autoload
(define-minor-mode prettier-elisp-mode
  "Format current top level form on file save when this mode is turned on."
  :lighter " Prettier"
  :global nil
  (advice-remove #'calculate-lisp-indent #'prettier-elisp-calculate-lisp-indent)
  (remove-hook 'before-save-hook #'prettier-elisp 'local)
  (when prettier-elisp-mode
    (advice-add #'calculate-lisp-indent :override
                #'prettier-elisp-calculate-lisp-indent)
    (add-hook 'before-save-hook #'prettier-elisp nil 'local)))

(provide 'prettier-elisp)
;;; prettier-elisp.el ends here