;;; prettier-elisp.el --- Minor mode to format emacs lisp code -*- lexical-binding: t -*-

;; Copyright (C) Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/prettier-elisp
;; Keywords: convenience edit elisp
;; Version: 0.1.1
;; Package-Requires: ((emacs "27.1"))

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

(defgroup prettier-elisp nil
  "Minor mode to format Elisp code on file save."
  :group 'languages
  :prefix "prettier-elisp"
  :link
  '(url-link
    :tag "Repository" "https://github.com/KarimAziev/prettier-elisp"))

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
  "Move backward up across ARG balanced group of parentheses.
Return new position if changed, nil otherwise."
  (prettier-elisp-move-with #'forward-sexp
                            (if (not arg)
                                -1
                              (if (> arg 0)
                                  (- arg)
                                arg))))

(defun prettier-elisp-forward-sexp (&optional arg)
  "Move backward up across ARG balanced group of parentheses.
Return new position if changed, nil otherwise."
  (prettier-elisp-move-with #'forward-sexp arg))

(defun prettier-elisp-backward-list (&optional arg)
  "Move backward across one balanced group of parentheses.
Return new position or nil.
With ARG, do it that many times."
  (prettier-elisp-move-with #'backward-list (if (not arg) -1 (if (> arg 0)
                                                                 (- arg) arg))))

(defun prettier-elisp-backward-up-list (&optional arg)
  "Move backward across one balanced group of parentheses.
Return new position or nil.
With ARG, do it that many times."
  (prettier-elisp-move-with #'backward-up-list (if (not arg)
                                                   -1
                                                 (if (> arg 0)
                                                     (- arg)
                                                   arg))))

(defun prettier-elisp-forward-list (&optional arg)
  "Move forward across one balanced group of parentheses.
Return new position or nil.
With ARG, do it that many times."
  (prettier-elisp-move-with #'forward-list arg))

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

(defun prettier-elisp-bounds-of-list-at-point ()
  "Return bounds of list at point."
  (when (prettier-elisp-get-list-at-point)
    (let ((start (point)))
      (cons start
            (save-excursion
              (prettier-elisp-move-with 'forward-sexp 1))))))

(defun prettier-elisp-inside-string-or-comment-p (&optional pos)
  "Return non nil if position POS is inside in string or comment."
  (let ((pps (syntax-ppss pos)))
    (or (nth 3 pps)
        (nth 4 pps))))

(defun prettier-elisp-bounds-of-sexp-at-point ()
  "Return bounds of exp at point."
  (unless (prettier-elisp-inside-string-or-comment-p)
    (let ((start (point)))
      (save-excursion
        (when (prettier-elisp-move-with 'forward-sexp 1)
          (cons start (point)))))))

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
  (delete-region (point)
                 (+ (point)
                    (skip-chars-forward "\s\t\n\r\f"))))

(defun prettier-elisp-delete-whitespace-backward ()
  "Delete whitespace at point."
  (delete-region (point)
                 (+ (point)
                    (skip-chars-backward "\s\t\n\r\f"))))

(defun prettier-elisp-line-empty-p ()
  "Return t if current line is empty."
  (string-empty-p
   (string-trim
    (buffer-substring-no-properties (line-beginning-position)
                                    (line-end-position)))))

(defun prettier-elisp-new-line-and-indent ()
  "Insert a newline if none and indent."
  (unless (looking-back "\\(\\([\n]\\)[\s\t]*\\)?[\n][\s\t]?+" 0)
    (when (looking-back "[@`',][\s\t]?+" 0)
      (skip-chars-backward "@`',\s\t"))
    (prettier-elisp-delete-whitespace-forward)
    (unless (looking-at "\\]\\|)")
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

(defun prettier-elisp-ensure-list-lines ()
  "Ensure thet every list is on own line."
  (while (prettier-elisp-forward-sexp 1)
    (when (looking-back ")\\|]" 0)
      (save-excursion
        (prettier-elisp-backward-sexp 1)
        (when (prettier-elisp-move-with 'down-list 1)
          (prettier-elisp-delete-whitespace-forward)
          (prettier-elisp-ensure-list-lines)
          (prettier-elisp-delete-whitespace-forward)))
      (when (save-excursion
              (when (prettier-elisp-re-search-forward "[^\s\t]" nil t 1)
                (forward-char -1)
                (looking-at "[\\[(]")))
        (prettier-elisp-new-line-and-indent)))
    (when (and (save-excursion
                 (prettier-elisp-backward-sexp 2))
               (> (current-column) fill-column))
      (prettier-elisp-backward-sexp 1)
      (prettier-elisp-new-line-and-indent)
      (prettier-elisp-forward-sexp 1))
    (when-let ((symb
                (symbol-at-point)))
      (pcase symb
        ((or 'save-excursion
             'save-restriction
             'save-match-data)
         (when (looking-at "[\s\t]")
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
          'unless 'defgroup 'defun 'cl-defun 'defclass 'defmethod 'cl-defmethod
          'defmacro 'global-set-key 'define-key 'define-minor-mode 'defhydra
          'pretty-hydra-define 'use-package 'use-package! 'defvar-local 'defvar
          'defcustom)
         (save-excursion
           (prettier-elisp-backward-up-list 1)
           (prettier-elisp-new-line-and-indent))
         (prettier-elisp-delete-whitespace-forward)
         (save-excursion
           (insert "\s")))
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
      (prettier-elisp-ensure-list-lines))
    (when-let* ((symbs (and (listp l)
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
             (prettier-elisp-forward-sexp 1))
           (prettier-elisp-new-line-and-indent))
          ((or 'defgroup 'defcustom)
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

(defun prettier-elisp-indent-parent ()
  "Indent parent form at top level."
  (let* ((pps (syntax-ppss))
         (inside-comment-or-str
          (or (nth 4 pps)
              (nth 3 pps))))
    (when (> (car pps) 0)
      (goto-char (car (nth 9 pps))))
    (when-let ((col (and
                     (not
                      inside-comment-or-str)
                     (looking-at "[(]")
                     (current-column))))
      (when (> col 0)
        (let ((pos (point)))
          (skip-chars-backward "\s\t")
          (delete-region (point)
                         pos)
          (when (> (current-column)
                   0)
            (insert "\n")))))
    (unless inside-comment-or-str
      (indent-sexp))))

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
    (when-let ((bounds
                (progn (prettier-elisp-indent-parent)
                       (prettier-elisp-indent-inner)
                       (prettier-elisp-get-bounds-to-narrow))))
      (save-excursion
        (goto-char (car bounds))
        (save-restriction
          (narrow-to-region (car bounds)
                            (cdr bounds))
          (save-excursion
            (save-match-data
              (while
                  (prettier-elisp-re-search-forward "[(`']\\([\n\t\r\s\f]+\\)"
                                                    nil t 1)
                (let ((beg (match-beginning 0)))
                  (delete-region (1+ beg)
                                 (point))))))
          (save-excursion
            (save-match-data
              (while (prettier-elisp-re-search-forward
                      "\\(\\]\\|)\\)[\n][\s]?+\\(\\]\\|)\\)" nil t
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
                   "(\\([\n\t\s]+\\)[a-zZ-A0-9:.+$!-]" nil t 1)
                (replace-match "" nil nil nil 1))))
          (save-excursion
            (save-match-data
              (while
                  (prettier-elisp-re-search-forward
                   "[a-zZ-A0-9:.+$!-]\\([\n\t\s]+\\))" nil t 1)
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
                   "[a-zZ-A0-9:.+$!-]\\((\\)" nil t 1)
                (replace-match "\s(" nil nil nil 1))))
          (save-excursion
            (save-match-data
              (while (prettier-elisp-re-search-forward "\\([\s\t]+\\)[\n\r\f]"
                                                       nil t 1)
                (replace-match "" nil nil nil 1)))))
        (prettier-elisp-ensure-lines)))))

(defvar prettier-elisp-newline-symbols-re nil)
(defmacro prettier-elisp-with-every-top-form (&rest body)
  "Bind VARS and eval BODY in current buffer on every top level form."
  (declare (indent 1)
           (debug t))
  `(save-excursion
     (save-restriction
       (widen)
       (goto-char (point-max))
       (while (and (prettier-elisp-backward-sexp)
                   (looking-at "[(]"))
         (save-excursion
           ,@body)))))

(defun prettier-elisp-delete-multi-lines-backward (n)
  "Ensure N lines newlines backward exists."
  (skip-chars-backward "\s\t\n")
  (let ((start)
        (end)
        (count 0))
    (while (prettier-elisp-line-empty-p)
      (setq count (1+ count))
      (setq end (or end (point)))
      (forward-line -1))
    (cond ((< count n)
           (insert (make-string (- n count) 10)))
          ((> count n)
           (forward-line 1)
           (setq start (point))
           (delete-region start end)))))


(defun prettier-elisp-ensure-lines ()
  "Remove or add new lines at the start and end of sexp."
  (when (save-excursion (not (prettier-elisp-move-with 'backward-sexp)))
    (prettier-elisp-delete-whitespace-backward)
     (newline 2))
  (let* ((line-end-pos (line-end-position))
         (sexp-end-pos (progn (forward-sexp 1)
                              (point)))
         (count (if (> sexp-end-pos line-end-pos)
                    2
                  1)))
    (prettier-elisp-delete-whitespace-forward)
    (newline count)))

;;;###autoload
(defun prettier-elisp-ensure-top-level-newlines ()
  "Add new line after top forms."
  (interactive)
  (prettier-elisp-with-every-top-form
      (prettier-elisp-ensure-lines)))

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
(defun prettier-elisp-to-string (sexp)
  "Format SEXP and return result."
  (with-temp-buffer
    (insert (prin1-to-string sexp))
    (delay-mode-hooks
      (emacs-lisp-mode)
      (prettier-elisp-format-buffer))
    (buffer-string)))

;;;###autoload
(defun prettier-elisp ()
  "Format current defun at point and multy lines in buffer."
  (interactive)
  (save-match-data
    (prettier-elisp-current)))

;;;###autoload
(defun prettier-elisp-format-buffer ()
  "Format current defun at point."
  (interactive)
  (prettier-elisp-with-every-top-form
      (prettier-elisp-current)))

;;;###autoload
(define-minor-mode prettier-elisp-mode
  "Format current defun on file save when this mode is turned on."
  :lighter " Prettier"
  :global nil
  (if prettier-elisp-mode
      (add-hook 'before-save-hook #'prettier-elisp nil 'local)
    (remove-hook 'before-save-hook #'prettier-elisp 'local)))

(provide 'prettier-elisp)
;;; prettier-elisp.el ends here