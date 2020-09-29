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

(defun prettier-elisp-join-parens ()
  "Join newlines with parens in current buffer."
  (interactive)
  (with-current-buffer (buffer-name)
    (save-restriction
      (save-excursion
        (goto-char (point-min))
        (while
            (or (looking-at ";;+.+\n+")
                (looking-back ";;+.+\n+"))
          (forward-line))
        (beginning-of-defun)
        (narrow-to-defun)
        (while (re-search-forward "^\\s-*[)\n]" nil t 1)
          (if (or (nth 3 (syntax-ppss))
                  (nth 4 (syntax-ppss)))
              (forward-line)
            (join-line)))))))

(defun prettier-elisp-join-empty-lines ()
  "Join multy whitespaces between parens."
  (interactive)
  (save-restriction
    (save-excursion
      (goto-char (point-min))
      (when (looking-at ";;+")
        (forward-list 1)
        (backward-char 1))
      (while (or (re-search-forward ")\n+\n\n+" nil t 1))
        (while (not (looking-back ")\n\n"))
          (join-line))
        (forward-list)))))

(defun prettier-elisp-ensure-parens-indent ()
  (when-let ((func (which-function)))
    (beginning-of-defun)
    (re-search-forward func nil t 1)
    (when (looking-at-p "(")
      (insert "\s"))))

(defun prettier-elisp ()
  "Format current defun at point."
  (interactive)
  (save-excursion
    (save-restriction
      (narrow-to-defun)
      (prettier-elisp-join-parens)
      (indent-buffer)
      (prettier-elisp-ensure-parens-indent))))

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
