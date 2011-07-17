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

(assert (eq nil (libo-basic-uno-name-to-list "")))
(assert (equal '(com sun star) (libo-basic-uno-name-to-list "com.sun.star")))

;;; Regexps

(defun assert-string-match (re str &optional just)
  "Assert that the regexp matches the given string."
  (let ((re (if just (concat "^" re "$") re)))
    (unless (string-match re str)
      (error "assert-string-match failed: %s" (prin1-to-string (list re str))))))

(defun assert-string-not-match (re str)
  "Assert that the regexp does not match the given string."
  (let ((p (string-match re str)))
    (when p
      (error "assert-string-not-match failed: %s => %d" (prin1-to-string (list re str)) p))))

(assert (string-match libo-basic-blank-re ""))
(assert (string-match libo-basic-blank-re "  \t"))
(assert (not (string-match libo-basic-blank-re "a")))

(assert (string-match libo-basic-comment-re "' comment"))
(assert (string-match libo-basic-comment-re "  Rem comment"))
(assert (string-match libo-basic-comment-re "\t '"))
(assert (not (string-match libo-basic-comment-re "Remember me")))
(assert (not (string-match libo-basic-comment-re "    ")))

(assert (string-match libo-basic-continuation-re " foobar _ "))
(assert (not (string-match libo-basic-continuation-re " _ \\")))

(assert (string-match libo-basic-definition-start-re "Public Sub main()"))
(assert (string-match libo-basic-definition-start-re "\tFunction foo()"))
(assert (string-match libo-basic-definition-start-re "\t Private \tFunction bar"))
(assert (not (string-match libo-basic-definition-start-re "\t privatesub foo()")))
(assert (not (string-match libo-basic-definition-start-re "\t public bar()")))

(assert (not (string-match libo-basic-definition-end-re "End")))
(assert (string-match libo-basic-definition-end-re "End Function"))
(assert (string-match libo-basic-definition-end-re "\tEnd  \t Sub"))

(assert (string-match libo-basic-label-re "foo:"))
(assert (string-match libo-basic-label-re "\tf_o:  "))
(assert (string-match libo-basic-label-re "\t100:  "))
(assert (not (string-match libo-basic-label-re "\t10 ")))
(assert (not (string-match libo-basic-label-re "keyword:=")))

(assert (string-match libo-basic-if-re "If"))
(assert (string-match libo-basic-if-re " \tif "))
(assert (string-match libo-basic-if-re "#if True then"))
(assert (not (string-match libo-basic-if-re " iff")))

(assert (string-match libo-basic-else-re " else foo"))
(assert (string-match libo-basic-else-re " elseif bar"))
(assert (string-match libo-basic-else-re "\t#elseif bar then"))
(assert (not (string-match libo-basic-else-re "elsef")))

(assert (string-match libo-basic-endif-re "EndIf"))
(assert (string-match libo-basic-endif-re "\tend if"))
(assert (string-match libo-basic-endif-re " #end if"))
(assert (not (string-match libo-basic-endif-re "if end")))

(assert (string-match libo-basic-case-re "Case"))
(assert (string-match libo-basic-case-re "\t Case "))
(assert (string-match libo-basic-case-re " #Case "))
(assert (not (string-match libo-basic-case-re "Select Case")))

(assert (string-match libo-basic-select-re "Select Case"))
(assert (string-match libo-basic-select-re "\t Select\tCase "))
(assert (string-match libo-basic-select-re "#Select\tCase "))
(assert (not (string-match libo-basic-select-re "select")))
(assert (not (string-match libo-basic-select-re "selectcase")))

(assert (string-match libo-basic-end-select-re "End Select"))
(assert (string-match libo-basic-end-select-re "\t End\tSelect "))
(assert (string-match libo-basic-end-select-re "#End  Select "))
(assert (not (string-match libo-basic-end-select-re "endselect")))

(assert (string-match libo-basic-do-re "do"))
(assert (string-match libo-basic-do-re "\tDo While"))
(assert (string-match libo-basic-do-re "#Do While"))
(assert (not (string-match libo-basic-do-re "don't")))

(assert (string-match libo-basic-loop-re "loop"))
(assert (string-match libo-basic-loop-re "\tLoop Until"))
(assert (string-match libo-basic-loop-re "#Loop "))
(assert (not (string-match libo-basic-loop-re "loopy")))

(assert (string-match libo-basic-for-re "For\t"))
(assert (string-match libo-basic-for-re "\t For"))
(assert (string-match libo-basic-for-re "#For"))
(assert (not (string-match libo-basic-for-re "ForEach ")))

(assert (string-match libo-basic-next-re "Next"))
(assert (string-match libo-basic-next-re " \tNext x"))
(assert (string-match libo-basic-next-re "#Next"))
(assert (not (string-match libo-basic-next-re " \tnextone")))

(assert (string-match libo-basic-while-re "While\t"))
(assert (string-match libo-basic-while-re "\t While x = 0"))
(assert (string-match libo-basic-while-re "#While True"))
(assert (not (string-match libo-basic-while-re "Whilex = 0")))

(assert (string-match libo-basic-wend-re "Wend"))
(assert (string-match libo-basic-wend-re " \tWend "))
(assert (string-match libo-basic-wend-re "#Wend"))
(assert (not (string-match libo-basic-wend-re " \tw end")))

(assert (string-match libo-basic-with-re "With"))
(assert (string-match libo-basic-with-re " \tWith "))
(assert (string-match libo-basic-with-re " #with"))
(assert (not (string-match libo-basic-with-re " \twithin")))

(assert (string-match libo-basic-end-with-re "End With"))
(assert (string-match libo-basic-end-with-re " \tEnd With "))
(assert (string-match libo-basic-end-with-re " #End\tWith "))
(assert (not (string-match libo-basic-end-with-re "endwith")))
(assert (not (string-match libo-basic-end-with-re " \tend if")))

(assert (string-match libo-basic-redim-re "ReDim abc(x)"))
(assert (string-match libo-basic-redim-re "redim preserve x86(a + b)"))
(assert (not (string-match libo-basic-redim-re "dim x as integer")))

(assert-string-match libo-basic-variable-spec-re "x" t)
(assert-string-match libo-basic-variable-spec-re "C99" t)
(assert-string-match libo-basic-variable-spec-re "x As Integer" t)
(assert-string-match libo-basic-variable-spec-re "x() As Integer" t)
(assert-string-match libo-basic-variable-spec-re "x(1 to 8) As Integer" t)
(assert-string-not-match libo-basic-variable-spec-re "0xCC")

;;; Predicates

(assert (not (libo-basic-uno-module-name-p "")))
(assert (libo-basic-uno-module-name-p "com.sun.star"))
(assert (libo-basic-uno-module-name-p "com.sun.star.packages.zip"))

(assert (not (libo-basic-uno-constant-group-name-p "")))
(assert (not (libo-basic-uno-constant-group-name-p "com.sun.star")))
(assert (libo-basic-uno-constant-group-name-p "com.sun.star.accessibility.AccessibleEventId"))
(assert (not (libo-basic-uno-constant-group-name-p "com.sun.star.accessibility.AccessibleEventId.NAME_CHANGED")))
(assert (libo-basic-uno-constant-group-name-p "com.sun.star.awt.Command"))
(assert (not (libo-basic-uno-constant-group-name-p "com.sun.star.xml.sax")))
(assert (libo-basic-uno-constant-group-name-p "com.sun.star.xml.sax.FastToken"))

(assert (not (libo-basic-uno-constant-name-p "")))
(assert (not (libo-basic-uno-constant-name-p "com.sun.star")))
(assert (not (libo-basic-uno-constant-name-p "com.sun.star.accessibility.AccessibleEventId")))
(assert (libo-basic-uno-constant-name-p "com.sun.star.accessibility.AccessibleEventId.NAME_CHANGED"))
(assert (libo-basic-uno-constant-name-p "com.sun.star.xsd.WhiteSpaceTreatment.Collapse"))

(assert (not (libo-basic-uno-name-p "")))
(assert (libo-basic-uno-name-p "com.sun.star"))
(assert (libo-basic-uno-name-p "com.sun.star.packages.zip"))
(assert (libo-basic-uno-name-p "com.sun.star.accessibility.AccessibleEventId"))
(assert (libo-basic-uno-name-p "com.sun.star.awt.Command"))
(assert (libo-basic-uno-name-p "com.sun.star.xml.sax.FastToken"))
(assert (libo-basic-uno-name-p "com.sun.star.accessibility.AccessibleEventId.NAME_CHANGED"))
(assert (libo-basic-uno-name-p "com.sun.star.xsd.WhiteSpaceTreatment.Collapse"))

(assert (string= "http://api.openoffice.org/docs/common/ref/com/sun/star/module-ix.html"
                 (libo-basic-idl-reference-url "com.sun.star")))
(assert (string= "http://api.openoffice.org/docs/common/ref/com/sun/star/accessibility/AccessibleEventId.html"
                 (libo-basic-idl-reference-url "com.sun.star.accessibility.AccessibleEventId")))
(assert (string= "http://api.openoffice.org/docs/common/ref/com/sun/star/script/browse/BrowseNodeFactoryViewTypes.html#MACROSELECTOR"
                 (libo-basic-idl-reference-url "com.sun.star.script.browse.BrowseNodeFactoryViewTypes.MACROSELECTOR")))
