;;; wikidoc.el --- use elisp doc strings to make other documentation

;; Copyright (C) 2010, 2011, 2012  Nic Ferrier

;; Author: Nic Ferrier <nic@ferrier.me.uk>
;; Maintainer: Nic Ferrier <nic@ferrier.me.uk>
;; Created: 5th October 2010
;; Version: 0.8.1
;; Keywords: lisp

;; This file is NOT part of GNU Emacs.

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

;;; Commentary:
;;
;; This is for making documentation from Elisp doc strings. It is
;; intended as an alternative to using info.
;;
;; Right now, the only output format is creole-wiki. But that could
;; change.

;;; Source code
;;
;; https://github.com/nicferrier/elwikidoc

;;; Style note
;;
;; This codes uses the emacs style of:
;;
;;    wikidoc---private-function
;;
;; for private functions.
;;
;;; Examples
;;
;; This is the main paragraph converter:
;;
;; (wikidoc--convert "this is a function.
;;  this is an example
;;  this should 'not' be changed to a link
;; This is the main text. It can include references to other 'functions'.
;; When there are 'two' references 'on the same line' it should
;; still work.")

;;; Code:

(eval-when-compile (require 'cl))

;;;###autoload
(defun wikidoc-grab-list (prefix &optional no-private)
  "Grab a list or functions matching PREFIX possibly with NO-PRIVATE"
  (let ((prefix-sym (symbol-name prefix))
        (res '()))
    (mapatoms
     (lambda (atom)
       (let ((sn (symbol-name atom)))
         (and (fboundp atom)
              (if (string-prefix-p prefix-sym sn)
                  (if no-private
                      (if (not (string-match "[^-]+--.*" sn))
                          (setq res (cons atom res)))
                    (setq res (cons atom res)))))))
     obarray)
    res))

(defun wikidoc--convert-line (line &optional arguments-to-mangle)
  "Converts a single LINE of function documentation.

This deals with things like quoted LISP forms which can be turned
into links.

ARGUMENTS-TO-MANGLE can be a list of arguments which are to be
detected and replaced like:

ARGUMENT -> //argument//

The list should be of symbols, not strings."
  (let* ((case-fold-search nil)
         (arglstre
          (when arguments-to-mangle
            (regexp-opt
             (loop for arg in
                   (cond
                    ;; CL style arguments are a list of lists
                    ((and (listp arguments-to-mangle)
                          (listp (car arguments-to-mangle)))
                     (car arguments-to-mangle))
                    ;; Normal function args are just a list
                    (t
                     arguments-to-mangle))
                   collect
                  (cond
                   ((listp arg)
                    (upcase (symbol-name (car arg))))
                   ((symbolp arg)
                    (upcase (symbol-name arg)))))))))
    (save-match-data
      (while (string-match ".*\\(`[^']+'\\).*" line)
        (setq line
              (replace-match
               (format "[[%s]]"
                       (let ((name (match-string 1 line)))
                         (save-match-data
                           (string-match "`\\([^']+\\)'" name)
                           (match-string 1 name))))
               nil nil line 1)))
      (if arglstre
          (replace-regexp-in-string
           arglstre
           (lambda (matched) (format "//%s//" (downcase matched)))
           line t nil)
        line))))

(defun wikidoc--convert (str &optional arguments-to-mangle)
  "Convert function documentation type doc STR to creole.

ARGUMENTS-TO-MANGLE can be a list of arguments which are to be
detected and replaced like:

ARGUMENT -> //argument//

The list should be of symbols, not strings."
  (let (in-pre)
    (concat
     (mapconcat
      (lambda (line)
        (cond
         ((string-match "^ " line)
          (if in-pre
              line
            (progn
              (setq in-pre 't)
              (concat
               "{{{\n"
               (wikidoc--convert-line line arguments-to-mangle)))))
         ((and in-pre (not (string-match "^ " line)))
          (setq in-pre nil)
          (concat "}}}\n" (wikidoc--convert-line line arguments-to-mangle)))
         ('t
          (wikidoc--convert-line line arguments-to-mangle))))
      (split-string str "\n")
      "\n")
     ;; end any pre that we started
     (if in-pre "\n}}}\n"))))

(defun wikidoc--convert-fn (fn)
  "Converter function for documentation of FN to WikiCreole.

The documentation is written, in WikiCreole form, into the
current buffer."
  (let* (arglist
         (fundoc-list
          (or
           (help-split-fundoc
            (documentation fn) nil)
           (cons "()" (documentation fn))))
         (args
          (or
           (cdar
            (read-from-string
             (downcase
              (car fundoc-list))))
           (help-function-arglist fn t)))
         (docbody (cdr fundoc-list))
         (fmted
          (format
           "=== %s %s ===\n\n%s\n\n\n"
           (symbol-name fn) ;; the func name
           (mapconcat
            (lambda (arg)
              (cond
               ((or
                 (equal '&optional arg)
                 (equal '&key arg)
                 (equal '&rest arg))
                (format "%s" arg))
               ((listp arg)
                (add-to-list 'arglist (car arg))
                (format "%s (%S)"
                        (car arg)
                        (cadr arg)))
               ('t
                (add-to-list 'arglist arg)
                (format "%s" arg))))
            (if (and (listp args)
                     (listp (car args)))
                (car args)
              args)
            " ")
           (when docbody
             (wikidoc--convert docbody arglist)))))
    (when (and docbody fmted)
      (insert fmted))))

;;;###autoload
(defun wikidoc-insert (elisp-prefix buffer)
  "Make creole doc for functions beginning with ELISP-PREFIX in BUFFER.

When called interactively with a PREFIX argument this function
will use the current buffer for BUFFER.

Otherwise the BUFFER will be created named like:

 *wikidoc-ELISP-PREFIX*

If Transient Mark mode is set in the specified buffer the active
region is killed before the new wiki text is inserted.
"
  (interactive
   (let ((elisp-prefix
          (completing-read "elisp prefix: "
                           obarray nil nil nil nil)))
     (list (intern elisp-prefix)
           (if current-prefix-arg
               (current-buffer)
             nil))))
  (let* ((lst (sort
               (wikidoc-grab-list elisp-prefix 't)
               'string-lessp)))
    (if (not (bufferp buffer))
        (progn
          (setq buffer (get-buffer-create (format "*wikidoc-%s*" elisp-prefix)))
          (with-current-buffer buffer
            (mapc 'wikidoc--convert-fn lst))
          (switch-to-buffer buffer))
      (progn
        (with-current-buffer buffer
          (if (use-region-p)
              (delete-region (region-beginning) (region-end)))
          (mapc 'wikidoc--convert-fn  lst))))))

(provide 'wikidoc)

;;; wikidoc.el ends here
