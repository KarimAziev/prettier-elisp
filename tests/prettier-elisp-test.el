;;; prettier-elisp-test.el --- Configure elisp test -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>

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

;; A suite of tests

;;; Code:

(require 'ert)
(require 'prettier-elisp)

(defun prettier-elisp-generate-test-cases (&optional pre-transform-fn)
  "Generate test cases for Emacs Lisp formatting.

Optional argument PRE-TRANSFORM-FN is a function that is called with each test
case string before formatting."
  (let ((buff-str (buffer-substring-no-properties (point-min)
                                                  (point-max)))
        (test-cases))
    (prettier-elisp-with-temp-buffer
      buff-str
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-max))
          (while (and (not (bobp))
                      (condition-case nil
                          (progn
                            (backward-list 1)
                            t)
                        (error nil))
                      (looking-at "[(]"))
            (when-let* ((beg (point))
                        (end (ignore-errors
                               (save-excursion
                                 (forward-sexp 1)
                                 (point))))
                        (str (if pre-transform-fn
                                 (funcall pre-transform-fn
                                          (buffer-substring-no-properties beg
                                                                          end))
                               (buffer-substring-no-properties beg
                                                               end)))
                        (result (prettier-elisp-string str)))
              (push `(should (equal (prettier-elisp-string ,str) ,result)) test-cases))))))
    test-cases))

(ert-deftest prettier-elisp-test-string ()
  (let ((fill-column 80))
    (let ((uggly "(  defun uggly-fn ()
  (   while (
             re-search-forward
          \"\\\\(\\\\]\\\\|)\\\\)[\\n][\\s]*\\\\(\\\\]\\\\|)\\\\)\" nil t
          1)
    (forward-char -1)
    (let (a 1))
    (join-line)))"))
      (should (equal (prettier-elisp-string uggly)
                     "(defun uggly-fn ()
  (while (re-search-forward
          \"\\\\(\\\\]\\\\|)\\\\)[\\n][\\s]*\\\\(\\\\]\\\\|)\\\\)\" nil t
          1)
    (forward-char -1)
    (let (a 1))
    (join-line)))
")))))

(ert-deftest prettier-elisp-test-string-with-no-lines ()
  (let ((fill-column 80))
    (should
     (equal
      (prettier-elisp-string "(defun km-today () (decode-time (current-time)))")
      "(defun km-today ()\n  (decode-time (current-time)))\n"))
    (should
     (equal
      (prettier-elisp-string
       "(defun km-tomorrow () (decode-time (time-add 86400 (current-time))))")
      "(defun km-tomorrow ()\n  (decode-time (time-add 86400 (current-time))))\n"))
    (should
     (equal
      (prettier-elisp-string
       "(defun km-shift-to-tomorrow (time-date) (let ((td time-date)) (setf (nthcdr 3 td) nil) (append td (km-date-part (km-tomorrow)))))")
      "(defun km-shift-to-tomorrow (time-date)\n  (let ((td time-date))\n    (setf (nthcdr 3 td) nil)\n    (append td (km-date-part (km-tomorrow)))))\n"))
    (should
     (equal
      (prettier-elisp-string
       "(defun km-shift-to-tomorrow-maybe (time-date) (let ((td (encode-time time-date))) (if (time-less-p td nil) (km-shift-to-tomorrow time-date) time-date)))")
      "(defun km-shift-to-tomorrow-maybe (time-date)\n  (let ((td (encode-time time-date)))\n    (if (time-less-p td nil)\n        (km-shift-to-tomorrow time-date) time-date)))\n"))
    (should
     (equal
      (prettier-elisp-string
       "(let ((targs (append (list (km-time-to-tomorrow-encoded time) nil fn) args))) (apply 'run-at-time targs))")
      "(let ((targs (append (list (km-time-to-tomorrow-encoded time) nil fn) args)))\n  (apply 'run-at-time targs))\n"))))

(ert-deftest prettier-elisp-test--with-no-spaces ()
  (let ((fill-column 80))
    (should
     (equal
      (prettier-elisp-string
       "(defun km-today () (decode-time (current-time)))")
      "(defun km-today ()\n  (decode-time (current-time)))\n"))
    (should
     (equal
      (prettier-elisp-string
       "(defun km-tomorrow () (decode-time (time-add 86400 (current-time))))")
      "(defun km-tomorrow ()\n  (decode-time (time-add 86400 (current-time))))\n"))
    (should
     (equal
      (prettier-elisp-string
       "(defun km-shift-to-tomorrow (time-date) (let ((td time-date)) (setf (nthcdr 3 td) nil) (append td (km-date-part (km-tomorrow)))))")
      "(defun km-shift-to-tomorrow (time-date)\n  (let ((td time-date))\n    (setf (nthcdr 3 td) nil)\n    (append td (km-date-part (km-tomorrow)))))\n"))
    (should
     (equal
      (prettier-elisp-string
       "(defun km-shift-to-tomorrow-maybe (time-date) (let ((td (encode-time time-date))) (if (time-less-p td nil) (km-shift-to-tomorrow time-date) time-date)))")
      "(defun km-shift-to-tomorrow-maybe (time-date)\n  (let ((td (encode-time time-date)))\n    (if (time-less-p td nil)\n        (km-shift-to-tomorrow time-date) time-date)))\n"))
    (should
     (equal
      (prettier-elisp-string
       "(let((targs (append (list (km-time-to-tomorrow-encoded time) nil fn) args))) (apply'run-at-time targs))")
      "(let ((targs (append (list (km-time-to-tomorrow-encoded time) nil fn) args)))\n  (apply'run-at-time targs))\n"))))

(ert-deftest prettier-elisp-test-string-test-with-comments ()
  (let ((fill-column 80))
    (should
   (equal (prettier-elisp-string
           "(let ((prefix-object  )
      (  prefix-class)) (setq prefix-object (plist-get (symbol-plist 'magit-log) 'transient--prefix))
  ;; get the class of the object


  (setq prefix-class (eieio-object-class prefix-object))
  ;; get the slots for that class, returns a list of structs
  (eieio-class-slots   prefix-class)
  ;; print some basic information about slots & methods
  )")
          "(let ((prefix-object)
      (prefix-class))
  (setq prefix-object (plist-get (symbol-plist 'magit-log) 'transient--prefix))
  ;; get the class of the object
  (setq prefix-class (eieio-object-class prefix-object))
  ;; get the slots for that class, returns a list of structs
  (eieio-class-slots   prefix-class)
  ;; print some basic information about slots & methods
  )
"))
    ))

(provide 'prettier-elisp-test)
;;; prettier-elisp-test.el ends here
;; Local Variables:
;; fill-column: 80
;; End:
