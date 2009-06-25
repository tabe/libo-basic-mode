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

;;; Regexps

(assert (string-match ooo-basic-blank-re ""))
(assert (string-match ooo-basic-blank-re "  \t"))
(assert (not (string-match ooo-basic-blank-re "a")))

(assert (string-match ooo-basic-comment-re "' comment"))
(assert (string-match ooo-basic-comment-re "  Rem comment"))
(assert (string-match ooo-basic-comment-re "\t '"))
(assert (not (string-match ooo-basic-comment-re "Remember me")))
(assert (not (string-match ooo-basic-comment-re "    ")))

(assert (string-match ooo-basic-continuation-re " foobar _ "))
(assert (not (string-match ooo-basic-continuation-re " _ \\")))

(assert (string-match ooo-basic-definition-start-re "Public Sub main()"))
(assert (string-match ooo-basic-definition-start-re "\tFunction foo()"))
(assert (string-match ooo-basic-definition-start-re "\t Private \tFunction bar"))
(assert (not (string-match ooo-basic-definition-start-re "\t privatesub foo()")))
(assert (not (string-match ooo-basic-definition-start-re "\t public bar()")))

(assert (not (string-match ooo-basic-definition-end-re "End")))
(assert (string-match ooo-basic-definition-end-re "End Function"))
(assert (string-match ooo-basic-definition-end-re "\tEnd  \t Sub"))

(assert (string-match ooo-basic-label-re "foo:"))
(assert (string-match ooo-basic-label-re "\tf_o:  "))
(assert (string-match ooo-basic-label-re "\t100:  "))
(assert (not (string-match ooo-basic-label-re "\t10 ")))
(assert (not (string-match ooo-basic-label-re "keyword:=")))

(assert (string-match ooo-basic-if-re "If"))
(assert (string-match ooo-basic-if-re " \tif "))
(assert (string-match ooo-basic-if-re "#if True then"))
(assert (not (string-match ooo-basic-if-re " iff")))

(assert (string-match ooo-basic-else-re " else foo"))
(assert (string-match ooo-basic-else-re " elseif bar"))
(assert (string-match ooo-basic-else-re "\t#elseif bar then"))
(assert (not (string-match ooo-basic-else-re "elsef")))

(assert (string-match ooo-basic-endif-re "EndIf"))
(assert (string-match ooo-basic-endif-re "\tend if"))
(assert (string-match ooo-basic-endif-re " #end if"))
(assert (not (string-match ooo-basic-endif-re "if end")))

(assert (string-match ooo-basic-case-re "Case"))
(assert (string-match ooo-basic-case-re "\t Case "))
(assert (string-match ooo-basic-case-re " #Case "))
(assert (not (string-match ooo-basic-case-re "Select Case")))

(assert (string-match ooo-basic-select-re "Select Case"))
(assert (string-match ooo-basic-select-re "\t Select\tCase "))
(assert (string-match ooo-basic-select-re "#Select\tCase "))
(assert (not (string-match ooo-basic-select-re "select")))
(assert (not (string-match ooo-basic-select-re "selectcase")))

(assert (string-match ooo-basic-end-select-re "End Select"))
(assert (string-match ooo-basic-end-select-re "\t End\tSelect "))
(assert (string-match ooo-basic-end-select-re "#End  Select "))
(assert (not (string-match ooo-basic-end-select-re "endselect")))

(assert (string-match ooo-basic-do-re "do"))
(assert (string-match ooo-basic-do-re "\tDo While"))
(assert (string-match ooo-basic-do-re "#Do While"))
(assert (not (string-match ooo-basic-do-re "don't")))

(assert (string-match ooo-basic-loop-re "loop"))
(assert (string-match ooo-basic-loop-re "\tLoop Until"))
(assert (string-match ooo-basic-loop-re "#Loop "))
(assert (not (string-match ooo-basic-loop-re "loopy")))

(assert (string-match ooo-basic-for-re "For\t"))
(assert (string-match ooo-basic-for-re "\t For"))
(assert (string-match ooo-basic-for-re "#For"))
(assert (not (string-match ooo-basic-for-re "ForEach ")))

(assert (string-match ooo-basic-next-re "Next"))
(assert (string-match ooo-basic-next-re " \tNext x"))
(assert (string-match ooo-basic-next-re "#Next"))
(assert (not (string-match ooo-basic-next-re " \tnextone")))

(assert (string-match ooo-basic-while-re "While\t"))
(assert (string-match ooo-basic-while-re "\t While x = 0"))
(assert (string-match ooo-basic-while-re "#While True"))
(assert (not (string-match ooo-basic-while-re "Whilex = 0")))

(assert (string-match ooo-basic-wend-re "Wend"))
(assert (string-match ooo-basic-wend-re " \tWend "))
(assert (string-match ooo-basic-wend-re "#Wend"))
(assert (not (string-match ooo-basic-wend-re " \tw end")))

(assert (string-match ooo-basic-with-re "With"))
(assert (string-match ooo-basic-with-re " \tWith "))
(assert (string-match ooo-basic-with-re " #with"))
(assert (not (string-match ooo-basic-with-re " \twithin")))

(assert (string-match ooo-basic-end-with-re "End With"))
(assert (string-match ooo-basic-end-with-re " \tEnd With "))
(assert (string-match ooo-basic-end-with-re " #End\tWith "))
(assert (not (string-match ooo-basic-end-with-re "endwith")))
(assert (not (string-match ooo-basic-end-with-re " \tend if")))

(assert (string-match ooo-basic-redim-re "ReDim abc(x)"))
(assert (string-match ooo-basic-redim-re "redim preserve x86(a + b)"))
(assert (not (string-match ooo-basic-redim-re "dim x as integer")))

;;; Predicates

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
