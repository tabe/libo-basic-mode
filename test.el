(require 'cl)

(assert (eq nil (append-map #'(lambda (x) x) nil)))
(assert (equal '(a a b c b c d d) (append-map #'(lambda (x) (append x x))  '((a) (b c) (d)))))

(assert (eq nil (filter-map #'(lambda (x) x) nil))) 
(assert (equal '(a b c) (filter-map #'(lambda (x) (and (stringp x) (intern x))) '("a" 1 nil "b" "c"))))

(assert (initial-string-p "" ""))
(assert (initial-string-p "" "a"))
(assert (not (initial-string-p "a" "")))
(assert (not (initial-string-p "a" "b")))
(assert (initial-string-p "ab " "ab cd"))

(assert (eq nil (with-index nil)))
(assert (equal '((a 0) (b 1) (c 2)) (with-index '(a b c))))

(assert (eq nil (ooo-basic-uno-name-to-list "")))
(assert (equal '(com sun star) (ooo-basic-uno-name-to-list "com.sun.star")))

(assert (not (ooo-basic-uno-module-name-p "")))
(assert (ooo-basic-uno-module-name-p "com.sun.star"))
(assert (ooo-basic-uno-module-name-p "com.sun.star.packages.zip"))

(assert (string= "http://api.openoffice.org/docs/common/ref/com/sun/star/module-ix.html"
                 (ooo-basic-idl-reference-url "com.sun.star")))
