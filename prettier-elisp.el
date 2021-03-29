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
  :link '(url-link :tag "Repository" "https://github.com/KarimAziev/prettier-elisp"))

(defcustom prettier-elisp-newline-symbols '("defcustom"
                                            "defgroup"
                                            "defvar"
                                            "defun"
                                            "cl-defun"
                                            "defmacro"
                                            "defconst"
                                            "defface"
                                            "provide"
                                            "define-minor-mode")
  "List of forms to divide with newline."
  :group 'prettier-elisp
  :type '(repeat (string  :tag "Name")))

(defvar prettier-elisp-newline-symbols-re nil)

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
               ;;(setq count (1+ count))
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

(defun prettier-elisp-ensure-parens-indent ()
  (save-restriction
    (widen)
    (save-excursion
      (goto-char (point-min))
      (save-match-data
        (while (prettier-elisp-re-search-forward "[a-zZ-A0-9]\\((\\)" nil t 1)
          (replace-match "\s(" nil nil nil 1))))))

;;;###autoload
(defun prettier-elisp-ensure-newlines ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (when (and (null prettier-elisp-newline-symbols-re)
               (<= 1 (length prettier-elisp-newline-symbols)))
      (setq prettier-elisp-newline-symbols-re
            (concat "\\(" "\\(^\\s-+\\)[(]" (regexp-opt
                                             prettier-elisp-newline-symbols
                                             t) "\\)")))
    (save-match-data
      (while (prettier-elisp-re-search-forward
              prettier-elisp-newline-symbols-re nil t 1)
        (replace-match "" nil nil nil 2)))
    (goto-char (point-min))
    (save-match-data
      (while (prettier-elisp-re-search-forward ")\n\n\\(\n+\\)" nil t 1)
        (replace-match "" nil nil nil 1)))))

;;;###autoload
(defun prettier-elisp-join-parens ()
  "Join newlines with parens in current function."
  (interactive)
  (when (which-function)
    (save-excursion
      (save-restriction
        (narrow-to-defun)
        (goto-char (point-min))
        (save-match-data
          (while (prettier-elisp-re-search-forward "^\\s-*[\n]+" nil t 1)
            (delete-region (match-beginning 0) (point))))
        (goto-char (point-min))
        (save-match-data
          (while (prettier-elisp-re-search-forward
                  "(\\([\n\t\s]+\\)[a-zZ-A0-9]" nil t 1)
            (replace-match "" nil nil nil 1)))
        (goto-char (point-min))
        (save-match-data
          (while (prettier-elisp-re-search-forward
                  "[a-zZ-A0-9)]\\([\n\t\s]+\\))" nil t 1)
            (delete-region (match-beginning 1) (match-end 1))))))))

;;;###autoload
(defun prettier-elisp ()
  "Format current defun at point and multy lines in buffer."
  (interactive)
  (save-excursion
    (save-restriction
      (save-match-data
        (when (which-function)
          (narrow-to-defun)
          (save-match-data
            (prettier-elisp-join-parens))
          (indent-buffer)
          (widen)
          (save-match-data
            (prettier-elisp-ensure-parens-indent))
          (save-match-data (prettier-elisp-ensure-newlines)))))))

;;;###autoload
(defun prettier-elisp-format-buffer ()
  "Format current defun at point."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (save-match-data (prettier-elisp-join-parens))
      (save-match-data (prettier-elisp-ensure-parens-indent))
      (save-match-data (prettier-elisp-ensure-newlines)))))

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