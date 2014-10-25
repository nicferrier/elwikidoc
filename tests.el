;;; tests for wikidoc  -*- lexical-binding: t -*-

(require 'ert)
(require 'wikidoc)

(ert-deftest wikidoc-test-convert-line ()
  "Can we convert lines with lisp refs and arguments?"
  (let ((l '("This is a line of documentation with `lisp-references'" .
             "This is a line of documentation with [[lisp-references]]")))
    (should
     (equal
      (cdr l)
      (wikidoc--convert-line (car l)))))
  (let ((l
         '("A line of doc with 2 args of car and cdr: CAR and CDR" .
           "A line of doc with 2 args of car and cdr: //car// and //cdr//")))
    (should
     (equal
      (cdr l)
      (wikidoc--convert-line (car l) '(car cdr))))))


;;; tests.el ends here
