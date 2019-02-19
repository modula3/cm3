;;
;; $Id$
;;

(require-modules "hashtable")
(load "../../scheme-lib/src/hashtable.scm")

(define t (make-string-hash-table 10))

(t 'add-entry! "a" 0)
(t 'add-entry! "b" 1)
(t 'add-entry! "c" 2)
(t 'add-entry! "d" 3)
(t 'add-entry! "e" 4)
(t 'add-entry! "f" 5)

(t 'keys)
