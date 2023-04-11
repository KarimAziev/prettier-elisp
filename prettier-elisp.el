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
          'with-eval-after-load 'defmacro 'global-set-key 'define-key
          'define-minor-mode 'defhydra 'pretty-hydra-define 'use-package
          'use-package! 'defvar-local 'defvar 'defcustom)
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

(defun prettier-elisp-format (body)
  "Format BODY."
  (with-temp-buffer
    (erase-buffer)
    (let ((emacs-lisp-mode-hook nil))
      (emacs-lisp-mode)
      (insert body)
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
                    (string= body replacement))
          (replace-region-contents
           beg end (lambda () replacement)))))))

(defun prettier-elisp-get-line-rules (sexp)
  "Return amount of lines before SEXP."
  (pcase sexp
    (`(defvar ,_name)
     1)
    (`(defvar ,_name nil)
     1)
    (`(defvar ,_name ,_)
     (if (>= (length (format "%s" sexp)) fill-column)
         2
       1))
    (`(defvar-local ,_name ,_)
     (if (>= (length (format "%s" sexp)) fill-column)
         2
       1))
    (`(require ,_name)
     1)
    (`(require ,_name ,_file)
     1)
    (`(require ,_name ,_file _no-err)
     1)
    (`(provide ,_name ,_file _no-err)
     1)
    (`(declare-function ,_name
                        ,_file)
     1)
    (`(declare-function ,_name
                        ,_file
                        ,_args)
     1)
    (`(declare-function ,_name ,_file ,_args ,_fileonly)
     1)
    (`(load ,_name)
     1)
    (`(load ,_name  ,_)
     1)
    (`(load ,_name  ,_  ,_)
     1)
    (`(load ,_name  ,_  ,_  ,_)
     1)
    (`(load ,_name  ,_  ,_  ,_ ,_)
     1)
    (`(provide _)
     1)
    (`(provide)
     1)
    (`(provide-theme _)
     1)
    (_ 2)))

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
  (prettier-elisp-current-defun))

;;;###autoload
(defun prettier-elisp-buffer ()
  "Format all buffer."
  (interactive)
  (dolist (fn '(prettier-elisp--ensure-top-level-newlines
                prettier-elisp-format-all-forms))
    (funcall fn)))

;;;###autoload
(define-minor-mode prettier-elisp-buffer-mode
  "Format whole buffer on file save when this mode is turned on."
  :lighter " Prettier"
  :global nil
  (prettier-elisp-mode -1)
  (if prettier-elisp-buffer-mode
      (add-hook 'before-save-hook #'prettier-elisp-buffer nil 'local)
    (remove-hook 'before-save-hook #'prettier-elisp-buffer 'local)))

;;;###autoload
(define-minor-mode prettier-elisp-mode
  "Format current top level form on file save when this mode is turned on."
  :lighter " Prettier"
  :global nil
  (if prettier-elisp-mode
      (add-hook 'before-save-hook #'prettier-elisp nil 'local)
    (remove-hook 'before-save-hook #'prettier-elisp 'local)))

(provide 'prettier-elisp)
;;; prettier-elisp.el ends here