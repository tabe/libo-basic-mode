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

(assert (not (ooo-basic-uno-constant-group-name-p "")))
(assert (not (ooo-basic-uno-constant-group-name-p "com.sun.star")))
(assert (ooo-basic-uno-constant-group-name-p "com.sun.star.accessibility.AccessibleEventId"))
(assert (not (ooo-basic-uno-constant-group-name-p "com.sun.star.accessibility.AccessibleEventId.NAME_CHANGED")))
(assert (ooo-basic-uno-constant-group-name-p "com.sun.star.awt.Command"))
(assert (not (ooo-basic-uno-constant-group-name-p "com.sun.star.xml.sax")))
(assert (ooo-basic-uno-constant-group-name-p "com.sun.star.xml.sax.FastToken"))

(assert (not (ooo-basic-uno-constant-name-p "")))
(assert (not (ooo-basic-uno-constant-name-p "com.sun.star")))
(assert (not (ooo-basic-uno-constant-name-p "com.sun.star.accessibility.AccessibleEventId")))
(assert (ooo-basic-uno-constant-name-p "com.sun.star.accessibility.AccessibleEventId.NAME_CHANGED"))
(assert (ooo-basic-uno-constant-name-p "com.sun.star.xsd.WhiteSpaceTreatment.Collapse"))

(assert (not (ooo-basic-uno-name-p "")))
(assert (ooo-basic-uno-name-p "com.sun.star"))
(assert (ooo-basic-uno-name-p "com.sun.star.packages.zip"))
(assert (ooo-basic-uno-name-p "com.sun.star.accessibility.AccessibleEventId"))
(assert (ooo-basic-uno-name-p "com.sun.star.awt.Command"))
(assert (ooo-basic-uno-name-p "com.sun.star.xml.sax.FastToken"))
(assert (ooo-basic-uno-name-p "com.sun.star.accessibility.AccessibleEventId.NAME_CHANGED"))
(assert (ooo-basic-uno-name-p "com.sun.star.xsd.WhiteSpaceTreatment.Collapse"))

(assert (string= "http://api.openoffice.org/docs/common/ref/com/sun/star/module-ix.html"
                 (ooo-basic-idl-reference-url "com.sun.star")))
(assert (string= "http://api.openoffice.org/docs/common/ref/com/sun/star/accessibility/AccessibleEventId.html"
                 (ooo-basic-idl-reference-url "com.sun.star.accessibility.AccessibleEventId")))
(assert (string= "http://api.openoffice.org/docs/common/ref/com/sun/star/script/browse/BrowseNodeFactoryViewTypes.html#MACROSELECTOR"
                 (ooo-basic-idl-reference-url "com.sun.star.script.browse.BrowseNodeFactoryViewTypes.MACROSELECTOR")))
