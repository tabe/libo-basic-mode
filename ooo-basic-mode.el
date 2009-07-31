;;; ooo-basic-mode.el -- A major mode for editing OpenOffice.org Basic programs

;; Copyright (C) 2009 Takeshi Abe <tabe@fixedpoint.jp>
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Author: Takeshi Abe <tabe@fixedpoint.jp>
;; Version: 0.0.5_alpha
;; Keywords: languages, basic, not so evil

;; Installation:
;; Put ooo-basic-mode.el somewhere in your path, compile it, and
;; add the following to your .emacs:
;; (autoload 'ooo-basic-mode "ooo-basic-mode" "A major mode for OpenOffice.org Basic." t)
;; (push '("\\.bas\\'" . ooo-basic-mode) auto-mode-alist)
;; (setq ooo-basic-ooo-program-directory "/var/lib/openoffice.org3/program")

;; Code:

(require 'font-lock)

(defun append-map (f ls)
  "Like mapcar, expect that the results are appended into one."
  (apply 'append (mapcar f ls)))

(defun filter-map (f ls)
  "Like mapcar, expect that only non-nil are saved."
  (let ((r nil))
    (dolist (e ls (reverse r))
      (let ((x (funcall f e)))
        (when x
          (setq r (cons x r)))))))

(defun initial-string-p (s str)
  "Return t if the first string is a initial string of the second one,
nil otherwise."
  (let ((len (length s)))
    (or (= 0 len)
        (and (<= len (length str))
             (string= s (substring str 0 len))))))

(defun with-index (ls)
  "Return the list ((e_0 0) (e_1 1) ... (e_n n)) of pairs, where the given one is (e_0 e_1 ... e_n)."
  (let ((r nil)
        (i 0))
    (dolist (e ls (reverse r))
      (setq r (cons (list e i) r))
      (setq i (+ i 1)))))

(defconst ooo-basic-mode-version "0.0.5_alpha"
  "Version string for ooo-basic-mode.")

(defgroup ooo-basic nil
  "Customization variables for `ooo-basic-mode'."
  :tag "OpenOffice.org Basic"
  :group 'languages)

(defcustom ooo-basic-indent-level 4
  "Number of spaces for each indentation step."
  :type 'integer
  :group 'ooo-basic)

(defcustom ooo-basic-absolute-indent-level-for-label 1
  "Absolute number of spaces for the indentation of a label."
  :type 'integer
  :group 'ooo-basic)

(defvar ooo-basic-mode-map nil
  "Keymap used in ooo-basic-mode.")

(unless ooo-basic-mode-map
  (setq ooo-basic-mode-map (make-sparse-keymap)))

(defvar ooo-basic-keywords
  '("As"
    "ByRef"
    "ByVal"
    "Call"
    "Case"
    "Const"
    "Do"
    "Dim"
    "Else"
    "Elseif"
    "End"
    "EndIf"
    "Error"
    "Explicit"
    "For"
    "Function"
    "Global"
    "GoSub"
    "GoTo"
    "If"
    "Input"
    "Let"
    "Line"
    "Loop"
    "New"
    "Next"
    "On"
    "Option"
    "Optional"
    "Private"
    "Public"
    "ReDim"
    "Resume"
    "Return"
    "Select"
    "Set"
    "Static"
    "Sub"
    "Then"
    "To"
    "Type"
    "Wend"
    "When"
    "While"
    "With"
    )
  "Reserved keywords for OpenOffice.org Basic.")

(defvar ooo-basic-types
  '("Boolean"
    "Currency"
    "Date"
    "Double"
    "Integer"
    "Long"
    "Object"
    "Single"
    "String"
    "Variant"
    )
  "Basic types used in OpenOffice.org Basic.")

(defvar ooo-basic-global-variables
  '("Erl" "Err" "Error$"
    "StarDesktop"
    "ThisComponent"
    )
  "Global Variables provided in OpenOffice.org Basic.")

(defvar ooo-basic-builtin-constants
  '("True" "False" "PI")
  "Builtin constants available in OpenOffice.org Basic.")

(defvar ooo-basic-builtin-operators
  '("=" "<" ">" "<=" ">=" "<>" "Is"
    "AND" "EQV" "IMP" "NOT" "OR" "XOR"
    "MOD"
    )
  "Builtin operators available in OpenOffice.org Basic.")

(defvar ooo-basic-builtin-properties
  '("BasicLibraries" "DialogLibraries" "GlobalScope")
  "Builtin properties available in OpenOffice.org Basic.")

(defvar ooo-basic-builtin-functions
  '("CBool" "CDbl" "CInt" "CLng" "CSng" "CStr"
    "CDate" "CDateFromIso" "CDateToIso"
    "DefBool" "DefDate" "DefDbl" "DefInt" "DefLng" "DefObj" "DefVar"
    "Array" "DimArray"
    "Choose" "IIF"
    "IsArray" "IsDate" "IsEmpty" "IsNull" "IsNumeric" "IsObject" "IsMissing"
    "IsUnoStruct"
    "FindObject" "FindPropertyObject"
    "Asc" "Chr" "Str" "Val" "Space" "String"
    "Left" "Right" "Mid" "Len" "InStr" "LTrim" "RTrim" "Trim" "StrComp"
    "Format" "LCase" "UCase" "StrConv"
    "DateSerial" "TimeSerial" "DateValue" "TimeValue"
    "Day" "Month" "Year" "WeekDay" "Hour" "Minute" "Second"
    "Date" "Time" "Now" "Timer"
    "Dir" "MkDir" "RmDir" "ChDir" "ChDrive" "CurDir" "CurDrive"
    "FileAttr" "FileCopy" "Kill" "FileExists" "GetAttr" "SetAttr" "Name"
    "FileDateTime" "FileLen" "FreeFile"
    "Open" "Close" "Print" "Reset" "Get" "Put" "Seek" "Write" "Eof" "Loc" "Lof"
    "MsgBox" "InputBox"
    "Switch"
    "Beep" "Shell" "Wait" "GetSystemTicks" "Environ"
    "LBound" "UBound"
    "LSet" "RSet"
    "TypeName" "VarType"
    "Blue" "Green" "Red" "QBColor" "RGB"
    "Atn" "Cos" "Sin" "Tan" "Exp" "Log"
    "Randomize" "Rnd" "Sqr" "Fix" "Int" "Abs" "Sgn" "Hex" "Oct"
    "GetSolarVersion"
    "TwipsPerPixelX" "TwipsPerPixelY"
    "CreateUnoDialog" "CreateUnoListener" "CreateUnoStruct"
    "CreateUnoService" "CreateUnoServiceWithArguments"
    "EqualUnoObjects" "HasUnoInterfaces"
    "GetProcessServiceManager"
    "ConvertFromUrl" "ConvertToUrl"
    "CompatibilityMode"
    "FreeLibrary"
    )
  "Builtin functions available in OpenOffice.org Basic.")

(defvar ooo-basic-blank-re
  "^\\s-*$"
  "Regexp to detect a blank line.")

(defvar ooo-basic-comment-re
  "^\\s-*\\(?:'\\|Rem\\>\\)"
  "Regexp to detect a line for comment only.")

(defvar ooo-basic-continuation-re
  "^.*_\\s-*$"
  "Regexp to detect a line continuing its next one.")

(defvar ooo-basic-definition-start-re
  "^\\s-*\\(?:P\\(?:ublic\\|rivate\\)\\s-+\\)?\\(Sub\\|Function\\|Type\\)\\>"
  "Regexp to detect the start of a definition.")

(defvar ooo-basic-definition-end-re
  "^\\s-*End\\s-+\\(Sub\\|Function\\|Type\\)\\>"
  "Regexp to detect the end of a definition.")

(defvar ooo-basic-label-re
  "^\\s-*\\([a-zA-Z0-9_]+\\):\\(?:$\\|[^=]\\)"
  "Regexp to detect a label.")

(defvar ooo-basic-if-re
  "^\\s-*#?If\\>"
  "Regexp to detect an if line")

(defvar ooo-basic-else-re
  "^\\s-*#?Else\\(?:If\\)?\\>"
  "Regexp to detect an else line.")

(defvar ooo-basic-endif-re
  "^\\s-*#?End\\s-*If\\>"
  "Regexp to detect an endif line.")

(defvar ooo-basic-case-re
  "^\\s-*#?Case\\>"
  "Regexp to detect a case clause.")

(defvar ooo-basic-select-re
  "^\\s-*#?Select\\s-+Case\\>"
  "Regexp to detect an select line.")

(defvar ooo-basic-end-select-re
  "^\\s-*#?End\\s-+Select\\>"
  "Regexp to detect the end of select statement.")

(defvar ooo-basic-for-re
  "^\\s-*#?For\\>"
  "Regexp to detect a for clause.")

(defvar ooo-basic-next-re
  "^\\s-*#?Next\\>"
  "Regexp to detect a next line.")

(defvar ooo-basic-do-re
  "^\\s-*#?Do\\>"
  "Regexp to detect a do line.")

(defvar ooo-basic-loop-re
  "^\\s-*#?Loop\\>"
  "Regexp to detect a loop line.")

(defvar ooo-basic-while-re
  "^\\s-*#?While\\>"
  "Regexp to detect a while clause.")

(defvar ooo-basic-wend-re
  "^\\s-*#?Wend\\>"
  "Regexp to detect a wend line.")

(defvar ooo-basic-with-re
  "^\\s-*#?With\\>"
  "Regexp to detect a with clause.")

(defvar ooo-basic-end-with-re
  "^\\s-*#?End\\s-+With\\>"
  "Regexp to detect the end of a with statement.")

(defvar ooo-basic-redim-re
  "\\<ReDim\\(?:\\s-+\\(Preserve\\)\\)?\\s-+\\([A-z_][A-z_0-9]*\\)"
  "Regexp to detect a ReDim statement.")

(defvar ooo-basic-variable-spec-re
  "\\<\\([A-z_][A-z_0-9]*\\)\\(?:\\s-*(.*)\\)?\\(?:\\s-+As\\s-+[A-z_][A-z_0-9]*\\>\\)?"
  "Regexp to detect a variable occurence in declaration.")

(defvar ooo-basic-font-lock-keywords-1
  `(("\\<\\([A-z_][A-z_0-9]*\\)\\([!@#$%&]\\)" ; auto type modifier
     (1 font-lock-variable-name-face)
     (2 font-lock-type-face)
     )
    ("\\<By\\(?:Ref\\|Val\\)\\s-+\\([A-z_][A-z_0-9]*\\)\\(?:\\s-*([^)]*)\\|\\>\\)"
     (1 font-lock-variable-name-face)
     )
    (,(concat "\\<\\(?:Dim\\|Static\\|Private\\|Public\\)\\s-+" ooo-basic-variable-spec-re)
     (1 font-lock-variable-name-face)
     (,ooo-basic-variable-spec-re
      (let* ((s (save-excursion (search-forward "," nil t)))
             (eol (save-excursion (re-search-forward "\\('\\|\\<REM\\>\\|$\\)" nil t))))
        (cond ((and s (<= s eol))
               (goto-char s)
               eol)
              (t
               (re-search-forward "$" nil t)
               nil)))
      nil
      (1 font-lock-variable-name-face))
     )
    ("\\<Declare\\>"
     (0 font-lock-keyword-face)
     ("\\<\\(?:Function\\|Sub\\)\\>" nil nil (0 font-lock-keyword-face))
     ("\\<\\([A-z_][A-z_0-9]*\\)\\s-+\\(Lib\\)\\>" nil nil
      (1 font-lock-function-name-face)
      (2 font-lock-keyword-face))
     ("\\<Alias\\>" nil nil (0 font-lock-keyword-face))
     )
    ("^\\s-*\\(?:P\\(?:rivate\\|ublic\\)\\s-+\\)?\\(?:Sub\\|Function\\)\\s-+\\([A-z_][A-z_0-9]*\\)\\>"
     (1 font-lock-function-name-face)
     (,(concat "[(,]\\s-*\\(?:Optional\\s-+\\)?\\(?:By\\(?:Ref\\|Val\\)\\s-+\\)?" ooo-basic-variable-spec-re) nil nil
      (1 font-lock-variable-name-face)
      )
     )
    ("^\\s-*\\(?:P\\(?:rivate\\|ublic\\)\\s-+\\)?Type\\s-+\\([A-z_][A-z_0-9]*\\)\\>"
     (1 font-lock-function-name-face)
     )
    ,ooo-basic-definition-end-re
    (,ooo-basic-label-re
     (1 font-lock-constant-face)
     )
    ("\\<GoTo\\s-+\\([A-z_0-9]+\\)"
     (1 font-lock-constant-face)
     )
    ("\\<Exit\\>"
     (0 font-lock-keyword-face)
     ("\\<\\(Sub\\|Function\\)\\>" nil nil (1 font-lock-keyword-face))
     )
    ("\\<\\(Do\\|Loop\\)\\s-+\\(Until\\)\\>"
     (1 font-lock-keyword-face)
     (2 font-lock-keyword-face)
     )
    ("\\<For\\s-+\\([A-z_][A-z_0-9]*\\)\\s-+=.+\\<To\\>"
     (1 font-lock-variable-name-face)
     ("\\<\\(Step\\)\\>" nil nil (0 font-lock-keyword-face))
     )
    ("\\<For\\s-+\\(Each\\)\\>"
     (1 font-lock-keyword-face)
     ("\\<\\([A-z_][A-z_0-9]*\\)\\s-+\\(In\\)\\>" nil nil
      (1 font-lock-variable-name-face)
      (2 font-lock-keyword-face))
     )
    ("\\<Next\\s-+\\([A-z_][A-z_0-9]*\\)\\>"
     (1 font-lock-variable-name-face)
     )
    (,ooo-basic-redim-re
     (1 font-lock-keyword-face)
     (2 font-lock-variable-name-face)
     )
    ("\\<REM\\>"
     (0 ,(if (boundp 'font-lock-comment-delimiter-face) 'font-lock-comment-delimiter-face 'font-lock-comment-face))
     (".*$" nil nil (0 font-lock-comment-face))
     )
    ("\\<Const\\s-*\\([A-z_][A-z_0-9]*\\)\\>"
     (1 font-lock-constant-face)
     )
    ("^\\s-*Option\\>"
     ("\\<\\(Base\\|VBASupport\\)\\s-+\\([01]\\)\\>" nil nil
      (1 font-lock-keyword-face)
      (2 font-lock-constant-face))
     )
    )
  "Level 1.")

(defvar ooo-basic-font-lock-keywords-2
  (append ooo-basic-font-lock-keywords-1
          `(,(regexp-opt ooo-basic-keywords 'words)
            (,(regexp-opt ooo-basic-types 'words) 0 font-lock-type-face)
            (,(regexp-opt (append ooo-basic-builtin-constants
                                  ooo-basic-global-variables
                                  ooo-basic-builtin-properties)
                          'words)
             0 font-lock-constant-face)
            ))
  "Level 2.")

(defvar ooo-basic-font-lock-keywords-3
  (let ((builtins (append ooo-basic-builtin-operators
                          ooo-basic-builtin-functions)))
    (append ooo-basic-font-lock-keywords-2
            `((,(regexp-opt builtins 'words) 0 font-lock-builtin-face)
              )))
  "Level 3.")

(defvar ooo-basic-mode-syntax-table
  (let ((table (make-syntax-table)))
    (with-syntax-table table
      (modify-syntax-entry ?_ "w")
      (modify-syntax-entry ?\$ "w")
      (modify-syntax-entry ?\\ "w") ; backslash is *not* an escape character
      (modify-syntax-entry ?\" "\"\\")
      (modify-syntax-entry ?\' "<")
      (modify-syntax-entry ?\n ">")
      (modify-syntax-entry ?\= ".")
      (modify-syntax-entry ?\< ".")
      (modify-syntax-entry ?\> ".")
      (modify-syntax-entry ?# "'")
      table))
  "Syntax table used in ooo-basic-mode.")

(defvar ooo-basic-uno-modules
  '((com
     (sun
      (star
       (accessibility)
       (animations)
       (auth)
       (awt
        (tree)
        )
       (beans)
       (bridge
        (oleautomation)
        )
       (chart)
       (chart2
        (data)
        )
       (configuration
        (backend
         (xml)
         )
        (bootstrap)
        )
       (connection)
       (container)
       (corba)
       (datatransfer
        (clipboard)
        (dnd)
        )
       (deployment
        (ui)
        )
       (document)
       (drawing
        (framework)
        )
       (embed)
       (form
        (binding)
        (component)
        (control)
        (inspection)
        (runtime)
        (submission)
        (validation)
        )
       (formula)
       (frame
        (status)
        )
       (gallery)
       (geometry)
       (graphic)
       (i18n)
       (image)
       (inspection)
       (installation)
       (io)
       (java)
       (lang)
       (ldap)
       (linguistic2)
       (loader)
       (logging)
       (mail)
       (media)
       (mozilla)
       (oooimprovement)
       (packages
        (manifest)
        (zip)
        )
       (plugin)
       (presentation)
       (rdf)
       (reflection)
       (registry)
       (rendering)
       (report
        (inspection)
        )
       (resource)
       (scanner)
       (script
        (browse)
        (provider)
        )
       (sdb
        (application)
        (tools)
        )
       (sdbc)
       (sdbcx)
       (security)
       (setup)
       (sheet)
       (smarttags)
       (style)
       (svg)
       (system)
       (table)
       (task)
       (test
        (bridge)
        (performance)
        )
       (text
        (FieldMaster)
        (textfield
         (docinfo)
         )
        )
       (ucb)
       (ui
        (dialogs)
        )
       (uno)
       (uri)
       (util)
       (view)
       (xforms)
       (xml
        (crypt
         (sax)
         )
        (csax)
        (dom
         (events)
         (views)
         )
        (input)
        (sax)
        (wrapper)
        (xpath)
        )
       (xsd)
       ))))
  "Modules in UNO."
  )

(defvar ooo-basic-uno-constants
  '((com
     (sun
      (star
       (accessibility
        (AccessibleEventId
         (NAME_CHANGED)                        ; 1
         (DESCRIPTION_CHANGED)                 ; 2
         (ACTION_CHANGED)                      ; 3
         (STATE_CHANGED)                       ; 4
         (ACTIVE_DESCENDANT_CHANGED)           ; 5
         (BOUNDRECT_CHANGED)                   ; 6
         (CHILD)                               ; 7
         (INVALIDATE_ALL_CHILDREN)             ; 8
         (SELECTION_CHANGED)                   ; 9
         (VISIBLE_DATA_CHANGED)                ; 10
         (VALUE_CHANGED)                       ; 11
         (CONTENT_FLOWS_FROM_RELATION_CHANGED) ; 12
         (CONTENT_FLOWS_TO_RELATION_CHANGED)   ; 13
         (CONTROLLED_BY_RELATION_CHANGED)      ; 14
         (CONTROLLER_FOR_RELATION_CHANGED)     ; 15
         (LABEL_FOR_RELATION_CHANGED)          ; 16
         (LABELED_BY_RELATION_CHANGED)         ; 17
         (MEMBER_OF_RELATION_CHANGED)          ; 18
         (SUB_WINDOW_OF_RELATION_CHANGED)      ; 19
         (CARET_CHANGED)                       ; 20
         (TEXT_SELECTION_CHANGED)              ; 21
         (TEXT_CHANGED)                        ; 22
         (TEXT_ATTRIBUTE_CHANGED)              ; 23
         (HYPERTEXT_CHANGED)                   ; 24
         (TABLE_CAPTION_CHANGED)               ; 25
         (TABLE_COLUMN_DESCRIPTION_CHANGED)    ; 26
         (TABLE_COLUMN_HEADER_CHANGED)         ; 27
         (TABLE_MODEL_CHANGED)                 ; 28
         (TABLE_ROW_DESCRIPTION_CHANGED)       ; 29
         (TABLE_ROW_HEADER_CHANGED)            ; 30
         (TABLE_SUMMARY_CHANGED)               ; 31
         )
        (AccessibleRelationType
         (INVALID)                      ; 0
         (CONTENT_FLOWS_FROM)           ; 1
         (CONTENT_FLOWS_TO)             ; 2
         (CONTROLLED_BY)                ; 3
         (CONTROLLER_FOR)               ; 4
         (LABEL_FOR)                    ; 5
         (LABELED_BY)                   ; 6
         (MEMBER_OF)                    ; 7
         (SUB_WINDOW_OF)                ; 8
         (NODE_CHILD_OF)                ; 9
         )
        (AccessibleRole
         (UNKNOWN)                      ; 0
         (ALERT)                        ; 1
         (COLUMN_HEADER)                ; 2
         (CANVAS)                       ; 3
         (CHECK_BOX)                    ; 4
         (CHECK_MENU_ITEM)              ; 5
         (COLOR_CHOOSER)                ; 6
         (COMBO_BOX)                    ; 7
         (DATE_EDITOR)                  ; 8
         (DESKTOP_ICON)                 ; 9
         (DESKTOP_PANE)                 ; 10
         (DIRECTORY_PANE)               ; 11
         (DIALOG)                       ; 12
         (DOCUMENT)                     ; 13
         (EMBEDDED_OBJECT)              ; 14
         (END_NOTE)                     ; 15
         (FILE_CHOOSER)                 ; 16
         (FILLER)                       ; 17
         (FONT_CHOOSER)                 ; 18
         (FOOTER)                       ; 19
         (FOOTNOTE)                     ; 20
         (FRAME)                        ; 21
         (GLASS_PANE)                   ; 22
         (GRAPHIC)                      ; 23
         (GROUP_BOX)                    ; 24
         (HEADER)                       ; 25
         (HEADING)                      ; 26
         (HYPER_LINK)                   ; 27
         (ICON)                         ; 28
         (INTERNAL_FRAME)               ; 29
         (LABEL)                        ; 30
         (LAYERED_PANE)                 ; 31
         (LIST)                         ; 32
         (LIST_ITEM)                    ; 33
         (MENU)                         ; 34
         (MENU_BAR)                     ; 35
         (MENU_ITEM)                    ; 36
         (OPTION_PANE)                  ; 37
         (PAGE_TAB)                     ; 38
         (PAGE_TAB_LIST)                ; 39
         (PANEL)                        ; 40
         (PARAGRAPH)                    ; 41
         (PASSWORD_TEXT)                ; 42
         (POPUP_MENU)                   ; 43
         (PUSH_BUTTON)                  ; 44
         (PROGRESS_BAR)                 ; 45
         (RADIO_BUTTON)                 ; 46
         (RADIO_MENU_ITEM)              ; 47
         (ROW_HEADER)                   ; 48
         (ROOT_PANE)                    ; 49
         (SCROLL_BAR)                   ; 50
         (SCROLL_PANE)                  ; 51
         (SHAPE)                        ; 52
         (SEPARATOR)                    ; 53
         (SLIDER)                       ; 54
         (SPIN_BOX)                     ; 55
         (SPLIT_PANE)                   ; 56
         (STATUS_BAR)                   ; 57
         (TABLE)                        ; 58
         (TABLE_CELL)                   ; 59
         (TEXT)                         ; 60
         (TEXT_FRAME)                   ; 61
         (TOGGLE_BUTTON)                ; 62
         (TOOL_BAR)                     ; 63
         (TOOL_TIP)                     ; 64
         (TREE)                         ; 65
         (VIEW_PORT)                    ; 66
         (WINDOW)                       ; 67
         (BUTTON_DROPDOWN)              ; 68
         (BUTTON_MENU)                  ; 69
         (CAPTION)                      ; 70
         (CHART)                        ; 71
         (EDIT_BAR)                     ; 72
         (FORM)                         ; 73
         (IMAGE_MAP)                    ; 74
         (NOTE)                         ; 75
         (PAGE)                         ; 76
         (RULER)                        ; 77
         (SECTION)                      ; 78
         (TREE_ITEM)                    ; 79
         (TREE_TABLE)                   ; 80
         )
        (AccessibleStateType
         (INVALID)                      ; 0
         (ACTIVE)                       ; 1
         (ARMED)                        ; 2
         (BUSY)                         ; 3
         (CHECKED)                      ; 4
         (DEFUNC)                       ; 5
         (EDITABLE)                     ; 6
         (ENABLED)                      ; 7
         (EXPANDABLE)                   ; 8
         (EXPANDED)                     ; 9
         (FOCUSABLE)                    ; 10
         (FOCUSED)                      ; 11
         (HORIZONTAL)                   ; 12
         (ICONIFIED)                    ; 13
         (INDETERMINATE)                ; 14
         (MANAGES_DESCENDANTS)          ; 15
         (MODAL)                        ; 16
         (MULTI_LINE)                   ; 17
         (MULTI_SELECTABLE)             ; 18
         (OPAQUE)                       ; 19
         (PRESSED)                      ; 20
         (RESIZABLE)                    ; 21
         (SELECTABLE)                   ; 22
         (SELECTED)                     ; 23
         (SENSITIVE)                    ; 24
         (SHOWING)                      ; 25
         (SINGLE_LINE)                  ; 26
         (STALE)                        ; 27
         (TRANSIENT)                    ; 28
         (VERTICAL)                     ; 29
         (VISIBLE)                      ; 30
         )
        (AccessibleTableModelChangeType
         (INSERT)                       ; 1
         (DELETE)                       ; 2
         (UPDATE)                       ; 3
         )
        (AccessibleTextType
         (CHARACTER)                    ; 1
         (WORD)                         ; 2
         (SENTENCE)                     ; 3
         (PARAGRAPH)                    ; 4
         (LINE)                         ; 5
         (GLYPH)                        ; 6
         (ATTRIBUTE_RUN)                ; 7
         )
        )
       (animations
        (AnimationAdditiveMode
         (BASE)                         ; 0
         (SUM)                          ; 1
         (REPLACE)                      ; 2
         (MULTIPLY)                     ; 3
         (NONE)                         ; 4
         )
        (AnimationCalcMode
         (DISCRETE)                     ; 0
         (LINEAR)                       ; 1
         (PACED)                        ; 2
         (SPLINE)                       ; 3
         )
        (AnimationColorSpace
         (RGB)                          ; 0
         (HSL)                          ; 1
         )
        (AnimationEndSync
         (FIRST)                        ; 0
         (LAST)                         ; 1
         (ALL)                          ; 2
         (MEDIA)                        ; 3
         )
        (AnimationFill
         (DEFAULT)                      ; 0
         (INHERIT)                      ; 0
         (REMOVE)                       ; 1
         (FREEZE)                       ; 2
         (HOLD)                         ; 3
         (TRANSITION)                   ; 4
         (AUTO)                         ; 5
         )
        (AnimationNodeType
         (CUSTOM)                       ; 0
         (PAR)                          ; 1
         (SEQ)                          ; 2
         (ITERATE)                      ; 3
         (ANIMATE)                      ; 4
         (SET)                          ; 5
         (ANIMATEMOTION)                ; 6
         (ANIMATECOLOR)                 ; 7
         (ANIMATETRANSFORM)             ; 8
         (TRANSITIONFILTER)             ; 9
         (AUDIO)                        ; 10
         (COMMAND)                      ; 11
         )
        (AnimationRestart
         (DEFAULT)                      ; 0
         (INHERIT)                      ; 0
         (ALWAYS)                       ; 1
         (WHEN_NOT_ACTIVE)              ; 2
         (NEVER)                        ; 3
         )
        (AnimationTransformType
         (TRANSLATE)                    ; 0
         (SCALE)                        ; 1
         (ROTATE)                       ; 2
         (SKEWX)                        ; 3
         (SKEWY)                        ; 4
         )
        (AnimationValueType
         (STRING)                       ; 0
         (NUMBER)                       ; 1
         (COLOR)                        ; 2
         )
        (EventTrigger
         (NONE)                         ; 0
         (ON_BEGIN)                     ; 1
         (ON_END)                       ; 2
         (BEGIN_EVENT)                  ; 3
         (END_EVENT)                    ; 4
         (ON_CLICK)                     ; 5
         (ON_DBL_CLICK)                 ; 6
         (ON_MOUSE_ENTER)               ; 7
         (ON_MOUSE_LEAVE)               ; 8
         (ON_NEXT)                      ; 9
         (ON_PREV)                      ; 10
         (ON_STOP_AUDIO)                ; 11
         (REPEAT)                       ; 12
         )
        (TransitionSubType
         (DEFAULT)                      ; 0
         (LEFTTORIGHT)                  ; 1
         (TOPTOBOTTOM)                  ; 2
         (TOPLEFT)                      ; 3
         (TOPRIGHT)                     ; 4
         (BOTTOMRIGHT)                  ; 5
         (BOTTOMLEFT)                   ; 6
         (TOPCENTER)                    ; 7
         (RIGHTCENTER)                  ; 8
         (BOTTOMCENTER)                 ; 9
         (LEFTCENTER)                   ; 10
         (CORNERSIN)                    ; 11
         (CORNERSOUT)                   ; 12
         (VERTICAL)                     ; 13
         (HORIZONTAL)                   ; 14
         (DIAGONALBOTTOMLEFT)           ; 15
         (DIAGONALTOPLEFT)              ; 16
         (DOUBLEBARNDOOR)               ; 17
         (DOUBLEDIAMOND)                ; 18
         (DOWN)                         ; 19
         (LEFT)                         ; 20
         (UP)                           ; 21
         (RIGHT)                        ; 22
         (RECTANGLE)                    ; 25
         (DIAMOND)                      ; 26
         (CIRCLE)                       ; 27
         (FOURPOINT)                    ; 28
         (FIVEPOINT)                    ; 29
         (SIXPOINT)                     ; 30
         (HEART)                        ; 31
         (KEYHOLE)                      ; 32
         (CLOCKWISETWELVE)              ; 33
         (CLOCKWISETHREE)               ; 34
         (CLOCKWISESIX)                 ; 35
         (CLOCKWISENINE)                ; 36
         (TWOBLADEVERTICAL)             ; 37
         (TWOBLADEHORIZONTAL)           ; 38
         (FOURBLADE)                    ; 39
         (CLOCKWISETOP)                 ; 40
         (CLOCKWISERIGHT)               ; 41
         (CLOCKWISEBOTTOM)              ; 42
         (CLOCKWISELEFT)                ; 43
         (CLOCKWISETOPLEFT)             ; 44
         (COUNTERCLOCKWISEBOTTOMLEFT)   ; 45
         (CLOCKWISEBOTTOMRIGHT)         ; 46
         (COUNTERCLOCKWISETOPRIGHT)     ; 47
         (CENTERTOP)                    ; 48
         (CENTERRIGHT)                  ; 49
         (TOP)                          ; 50
         (BOTTOM)                       ; 52
         (FANOUTVERTICAL)               ; 54
         (FANOUTHORIZONTAL)             ; 55
         (FANINVERTICAL)                ; 56
         (FANINHORIZONTAL)              ; 57
         (PARALLELVERTICAL)             ; 58
         (PARALLELDIAGONAL)             ; 59
         (OPPOSITEVERTICAL)             ; 60
         (OPPOSITEHORIZONTAL)           ; 61
         (PARALLELDIAGONALTOPLEFT)      ; 62
         (PARALLELDIAGONALBOTTOMLEFT)   ; 63
         (TOPLEFTHORIZONTAL)            ; 64
         (TOPLEFTDIAGONAL)              ; 65
         (TOPRIGHTDIAGONAL)             ; 66
         (BOTTOMRIGHTDIAGONAL)          ; 67
         (BOTTOMLEFTDIAGONAL)           ; 68
         (TOPLEFTCLOCKWISE)             ; 69
         (TOPRIGHTCLOCKWISE)            ; 70
         (BOTTOMRIGHTCLOCKWISE)         ; 71
         (BOTTOMLEFTCLOCKWISE)          ; 72
         (TOPLEFTCOUNTERCLOCKWISE)      ; 73
         (TOPRIGHTCOUNTERCLOCKWISE)     ; 74
         (BOTTOMRIGHTCOUNTERCLOCKWISE)  ; 75
         (BOTTOMLEFTCOUNTERCLOCKWISE)   ; 76
         (VERTICALTOPSAME)              ; 77
         (VERTICALBOTTOMSAME)           ; 78
         (VERTICALTOPLEFTOPPOSITE)      ; 79
         (VERTICALBOTTOMLEFTOPPOSITE)   ; 80
         (HORIZONTALLEFTSAME)           ; 81
         (HORIZONTALRIGHTSAME)          ; 82
         (HORIZONTALTOPLEFTOPPOSITE)    ; 83
         (HORIZONTALTOPRIGHTOPPOSITE)   ; 84
         (DIAGONALBOTTOMLEFTOPPOSITE)   ; 85
         (DIAGONALTOPLEFTOPPOSITE)      ; 86
         (TWOBOXTOP)                    ; 87
         (TWOBOXBOTTOM)                 ; 88
         (TWOBOXLEFT)                   ; 89
         (TWOBOXRIGHT)                  ; 90
         (FOURBOXVERTICAL)              ; 91
         (FOURBOXHORIZONTAL)            ; 92
         (VERTICALLEFT)                 ; 93
         (VERTICALRIGHT)                ; 94
         (HORIZONTALLEFT)               ; 95
         (HORIZONTALRIGHT)              ; 96
         (FROMLEFT)                     ; 97
         (FROMTOP)                      ; 98
         (FROMRIGHT)                    ; 99
         (FROMBOTTOM)                   ; 100
         (CROSSFADE)                    ; 101
         (FADETOCOLOR)                  ; 102
         (FADEFROMCOLOR)                ; 103
         (FADEOVERCOLOR)                ; 104
         (THREEBLADE)                   ; 105
         (EIGHTBLADE)                   ; 106
         (ONEBLADE)                     ; 107
         (ACROSS)                       ; 108
         (TOPLEFTVERTICAL)              ; 109
         (COMBHORIZONTAL)               ; 110
         (COMBVERTICAL)                 ; 111
         (IN)                           ; 112
         (OUT)                          ; 113
         (ROTATEIN)                     ; 114
         (ROTATEOUT)                    ; 115
         (FROMTOPLEFT)                  ; 116
         (FROMTOPRIGHT)                 ; 117
         (FROMBOTTOMLEFT)               ; 118
         (FROMBOTTOMRIGHT)              ; 119
         )
        (TransitionType
         (BARWIPE)                      ; 1
         (BOXWIPE)                      ; 2
         (FOURBOXWIPE)                  ; 3
         (BARNDOORWIPE)                 ; 4
         (DIAGONALWIPE)                 ; 5
         (BOWTIEWIPE)                   ; 6
         (MISCDIAGONALWIPE)             ; 7
         (VEEWIPE)                      ; 8
         (BARNVEEWIPE)                  ; 9
         (ZIGZAGWIPE)                   ; 10
         (BARNZIGZAGWIPE)               ; 11
         (IRISWIPE)                     ; 12
         (TRIANGLEWIPE)                 ; 13
         (ARROWHEADWIPE)                ; 14
         (PENTAGONWIPE)                 ; 15
         (HEXAGONWIPE)                  ; 16
         (ELLIPSEWIPE)                  ; 17
         (EYEWIPE)                      ; 18
         (ROUNDRECTWIPE)                ; 19
         (STARWIPE)                     ; 20
         (MISCSHAPEWIPE)                ; 21
         (CLOCKWIPE)                    ; 22
         (PINWHEELWIPE)                 ; 23
         (SINGLESWEEPWIPE)              ; 24
         (FANWIPE)                      ; 25
         (DOUBLEFANWIPE)                ; 26
         (DOUBLESWEEPWIPE)              ; 27
         (SALOONDOORWIPE)               ; 28
         (WINDSHIELDWIPE)               ; 29
         (SNAKEWIPE)                    ; 30
         (SPIRALWIPE)                   ; 31
         (PARALLELSNAKESWIPE)           ; 32
         (BOXSNAKESWIPE)                ; 33
         (WATERFALLWIPE)                ; 34
         (PUSHWIPE)                     ; 35
         (SLIDEWIPE)                    ; 36
         (FADE)                         ; 37
         (RANDOMBARWIPE)                ; 38
         (CHECKERBOARDWIPE)             ; 39
         (DISSOLVE)                     ; 40
         (BLINDSWIPE)                   ; 41
         (RANDOM)                       ; 42
         (ZOOM)                         ; 43
         )
        )
       (awt
        (Command
         (CONTEXTMENU)                  ; 1
         (STARTDRAG)                    ; 2
         (WHEEL)                        ; 3
         (STARTAUTOSCROLL)              ; 4
         (AUTOSCROLL)                   ; 5
         (VOICE)                        ; 6
         (STARTEXTTEXTINPUT)            ; 7
         (EXTTEXTINPUT)                 ; 8
         (ENDEXTTEXTINPUT)              ; 9
         (INPUTCONTEXTCHANGE)           ; 10
         (CURSORPOS)                    ; 11
         (PASTESELECTION)               ; 12
         (MODKEYCHANGE)                 ; 13
         (HANGUL_HANJA_CONVERSION)      ; 14
         (USER)                         ; 4096
         )
        (DeviceCapability
         (RASTEROPERATIONS)             ; 1
         (GETBITS)                      ; 2
         )
        (FieldUnit
         (FUNIT_NONE)                   ; 0
         (FUNIT_MM)                     ; 1
         (FUNIT_CM)                     ; 2
         (FUNIT_M)                      ; 3
         (FUNIT_KM)                     ; 4
         (FUNIT_TWIP)                   ; 5
         (FUNIT_POINT)                  ; 6
         (FUNIT_PICA)                   ; 7
         (FUNIT_INCH)                   ; 8
         (FUNIT_FOOT)                   ; 9
         (FUNIT_MILE)                   ; 10
         (FUNIT_CUSTOM)                 ; 11
         (FUNIT_PERCENT)                ; 12
         (FUNIT_100TH_MM)               ; 13
         )
        (FocusChangeReason
         (TAB)                          ; 1
         (CURSOR)                       ; 2
         (MNEMONIC)                     ; 4
         (FORWARD)                      ; 16
         (BACKWARD)                     ; 32
         (AROUND)                       ; 64
         (UNIQUEMNEMONIC)               ; 256
         )
        (FontEmphasisMark
         (NONE)                         ; 0x0000
         (DOT)                          ; 0x0001
         (CIRCLE)                       ; 0x0002
         (DISC)                         ; 0x0003
         (ACCENT)                       ; 0x0004
         (ABOVE)                        ; 0x1000
         (BELOW)                        ; 0x2000
         )
        (FontFamily
         (DONTKNOW)                     ; 0
         (DECORATIVE)                   ; 1
         (MODERN)                       ; 2
         (ROMAN)                        ; 3
         (SCRIPT)                       ; 4
         (SWISS)                        ; 5
         (SYSTEM)                       ; 6
         )
        (FontPitch
         (DONTKNOW)                     ; 0
         (FIXED)                        ; 1
         (VARIABLE)                     ; 2
         )
        (FontRelief
         (NONE)                         ; 0
         (EMBOSSED)                     ; 1
         (ENGRAVED)                     ; 2
         )
        (FontStrikeout
         (NONE)                         ; 0
         (SINGLE)                       ; 1
         (DOUBLE)                       ; 2
         (DONTKNOW)                     ; 3
         (BOLD)                         ; 4
         (SLASH)                        ; 5
         (X)                            ; 6
         )
        (FontType
         (DONTKNOW)                     ; 0
         (RASTER)                       ; 1
         (DEVICE)                       ; 2
         (SCALABLE)                     ; 4
         )
        (FontUnderline
         (NONE)                         ; 0
         (SINGLE)                       ; 1
         (DOUBLE)                       ; 2
         (DOTTED)                       ; 3
         (DONTKNOW)                     ; 4
         (DASH)                         ; 5
         (LONGDASH)                     ; 6
         (DASHDOT)                      ; 7
         (DASHDOTDOT)                   ; 8
         (SMALLWAVE)                    ; 9
         (WAVE)                         ; 10
         (DOUBLEWAVE)                   ; 11
         (BOLD)                         ; 12
         (BOLDDOTTED)                   ; 13
         (BOLDDASH)                     ; 14
         (BOLDLONGDASH)                 ; 15
         (BOLDDASHDOT)                  ; 16
         (BOLDDASHDOTDOT)               ; 17
         (BOLDWAVE)                     ; 18
         )
        (FontWeight
         (DONTKNOW)                     ; 0.000000
         (THIN)                         ; 50.000000
         (ULTRALIGHT)                   ; 60.000000
         (LIGHT)                        ; 75.000000
         (SEMILIGHT)                    ; 90.000000
         (NORMAL)                       ; 100.000000
         (SEMIBOLD)                     ; 110.000000
         (BOLD)                         ; 150.000000
         (ULTRABOLD)                    ; 175.000000
         (BLACK)                        ; 200.000000
         )
        (FontWidth
         (DONTKNOW)                     ; 0.000000
         (ULTRACONDENSED)               ; 50.000000
         (EXTRACONDENSED)               ; 60.000000
         (CONDENSED)                    ; 75.000000
         (SEMICONDENSED)                ; 90.000000
         (NORMAL)                       ; 100.000000
         (SEMIEXPANDED)                 ; 110.000000
         (EXPANDED)                     ; 150.000000
         (EXTRAEXPANDED)                ; 175.000000
         (ULTRAEXPANDED)                ; 200.000000
         )
        (ImageAlign
         (LEFT)                         ; 0
         (TOP)                          ; 1
         (RIGHT)                        ; 2
         (BOTTOM)                       ; 3
         )
        (ImagePosition
         (LeftTop)                      ; 0
         (LeftCenter)                   ; 1
         (LeftBottom)                   ; 2
         (RightTop)                     ; 3
         (RightCenter)                  ; 4
         (RightBottom)                  ; 5
         (AboveLeft)                    ; 6
         (AboveCenter)                  ; 7
         (AboveRight)                   ; 8
         (BelowLeft)                    ; 9
         (BelowCenter)                  ; 10
         (BelowRight)                   ; 11
         (Centered)                     ; 12
         )
        (ImageStatus
         (IMAGESTATUS_ERROR)            ; 1
         (IMAGESTATUS_SINGLEFRAMEDONE)  ; 2
         (IMAGESTATUS_STATICIMAGEDONE)  ; 3
         (IMAGESTATUS_ABORTED)          ; 4
         )
        (InvalidateStyle
         (CHILDREN)                     ; 1
         (NOCHILDREN)                   ; 2
         (NOERASE)                      ; 4
         (UPDATE)                       ; 8
         (TRANSPARENT)                  ; 16
         (NOTRANSPARENT)                ; 32
         (NOCLIPCHILDREN)               ; 16384
         )
        (Key
         (NUM0)                         ; 256
         (NUM1)                         ; 257
         (NUM2)                         ; 258
         (NUM3)                         ; 259
         (NUM4)                         ; 260
         (NUM5)                         ; 261
         (NUM6)                         ; 262
         (NUM7)                         ; 263
         (NUM8)                         ; 264
         (NUM9)                         ; 265
         (A)                            ; 512
         (B)                            ; 513
         (C)                            ; 514
         (D)                            ; 515
         (E)                            ; 516
         (F)                            ; 517
         (G)                            ; 518
         (H)                            ; 519
         (I)                            ; 520
         (J)                            ; 521
         (K)                            ; 522
         (L)                            ; 523
         (M)                            ; 524
         (N)                            ; 525
         (O)                            ; 526
         (P)                            ; 527
         (Q)                            ; 528
         (R)                            ; 529
         (S)                            ; 530
         (T)                            ; 531
         (U)                            ; 532
         (V)                            ; 533
         (W)                            ; 534
         (X)                            ; 535
         (Y)                            ; 536
         (Z)                            ; 537
         (F1)                           ; 768
         (F2)                           ; 769
         (F3)                           ; 770
         (F4)                           ; 771
         (F5)                           ; 772
         (F6)                           ; 773
         (F7)                           ; 774
         (F8)                           ; 775
         (F9)                           ; 776
         (F10)                          ; 777
         (F11)                          ; 778
         (F12)                          ; 779
         (F13)                          ; 780
         (F14)                          ; 781
         (F15)                          ; 782
         (F16)                          ; 783
         (F17)                          ; 784
         (F18)                          ; 785
         (F19)                          ; 786
         (F20)                          ; 787
         (F21)                          ; 788
         (F22)                          ; 789
         (F23)                          ; 790
         (F24)                          ; 791
         (F25)                          ; 792
         (F26)                          ; 793
         (DOWN)                         ; 1024
         (UP)                           ; 1025
         (LEFT)                         ; 1026
         (RIGHT)                        ; 1027
         (HOME)                         ; 1028
         (END)                          ; 1029
         (PAGEUP)                       ; 1030
         (PAGEDOWN)                     ; 1031
         (RETURN)                       ; 1280
         (ESCAPE)                       ; 1281
         (TAB)                          ; 1282
         (BACKSPACE)                    ; 1283
         (SPACE)                        ; 1284
         (INSERT)                       ; 1285
         (DELETE)                       ; 1286
         (ADD)                          ; 1287
         (SUBTRACT)                     ; 1288
         (MULTIPLY)                     ; 1289
         (DIVIDE)                       ; 1290
         (POINT)                        ; 1291
         (COMMA)                        ; 1292
         (LESS)                         ; 1293
         (GREATER)                      ; 1294
         (EQUAL)                        ; 1295
         (OPEN)                         ; 1296
         (CUT)                          ; 1297
         (COPY)                         ; 1298
         (PASTE)                        ; 1299
         (UNDO)                         ; 1300
         (REPEAT)                       ; 1301
         (FIND)                         ; 1302
         (PROPERTIES)                   ; 1303
         (FRONT)                        ; 1304
         (CONTEXTMENU)                  ; 1305
         (HELP)                         ; 1306
         (MENU)                         ; 1307
         (HANGUL_HANJA)                 ; 1308
         (DECIMAL)                      ; 1309
         (TILDE)                        ; 1310
         (QUOTELEFT)                    ; 1311
         (DELETE_TO_BEGIN_OF_LINE)      ; 1536
         (DELETE_TO_END_OF_LINE)        ; 1537
         (DELETE_TO_BEGIN_OF_PARAGRAPH) ; 1538
         (DELETE_TO_END_OF_PARAGRAPH)   ; 1539
         (DELETE_WORD_BACKWARD)         ; 1540
         (DELETE_WORD_FORWARD)          ; 1541
         (INSERT_LINEBREAK)             ; 1542
         (INSERT_PARAGRAPH)             ; 1543
         (MOVE_WORD_BACKWARD)           ; 1544
         (MOVE_WORD_FORWARD)            ; 1545
         (MOVE_TO_BEGIN_OF_LINE)        ; 1546
         (MOVE_TO_END_OF_LINE)          ; 1547
         (MOVE_TO_BEGIN_OF_PARAGRAPH)   ; 1548
         (MOVE_TO_END_OF_PARAGRAPH)     ; 1549
         (SELECT_BACKWARD)              ; 1550
         (SELECT_FORWARD)               ; 1551
         (SELECT_WORD_BACKWARD)         ; 1552
         (SELECT_WORD_FORWARD)          ; 1553
         (SELECT_WORD)                  ; 1554
         (SELECT_LINE)                  ; 1555
         (SELECT_PARAGRAPH)             ; 1556
         (SELECT_ALL)                   ; 1557
         )
        (KeyFunction
         (DONTKNOW)                     ; 0
         (NEW)                          ; 1
         (OPEN)                         ; 2
         (SAVE)                         ; 3
         (SAVEAS)                       ; 4
         (PRINT)                        ; 5
         (CLOSE)                        ; 6
         (QUIT)                         ; 7
         (CUT)                          ; 8
         (COPY)                         ; 9
         (PASTE)                        ; 10
         (UNDO)                         ; 11
         (REDO)                         ; 12
         (DELETE)                       ; 13
         (REPEAT)                       ; 14
         (FIND)                         ; 15
         (FINDBACKWARD)                 ; 16
         (PROPERTIES)                   ; 17
         (FRONT)                        ; 18
         )
        (KeyModifier
         (SHIFT)                        ; 1
         (MOD1)                         ; 2
         (MOD2)                         ; 4
         )
        (LineEndFormat
         (CARRIAGE_RETURN)              ; 0
         (LINE_FEED)                    ; 1
         (CARRIAGE_RETURN_LINE_FEED)    ; 2
         )
        (MenuItemStyle
         (CHECKABLE)                    ; 1
         (RADIOCHECK)                   ; 2
         (AUTOCHECK)                    ; 4
         )
        (MessageBoxButtons
         (BUTTONS_OK)                   ; 1
         (BUTTONS_OK_CANCEL)            ; 2
         (BUTTONS_YES_NO)               ; 3
         (BUTTONS_YES_NO_CANCEL)        ; 4
         (BUTTONS_RETRY_CANCEL)         ; 5
         (BUTTONS_ABORT_IGNORE_RETRY)   ; 6
         (DEFAULT_BUTTON_OK)            ; 0x10000
         (DEFAULT_BUTTON_CANCEL)        ; 0x20000
         (DEFAULT_BUTTON_RETRY)         ; 0x30000
         (DEFAULT_BUTTON_YES)           ; 0x40000
         (DEFAULT_BUTTON_NO)            ; 0x50000
         (DEFAULT_BUTTON_IGNORE)        ; 0x60000
         )
        (MouseButton
         (LEFT)                         ; 1
         (RIGHT)                        ; 2
         (MIDDLE)                       ; 4
         )
        (PopupMenuDirection
         (DEFAULT)                      ; 0
         (DOWN)                         ; 1
         (EXECUTE_DEFAULT)              ; 0
         (EXECUTE_DOWN)                 ; 1
         (EXECUTE_UP)                   ; 2
         (EXECUTE_LEFT)                 ; 4
         (EXECUTE_RIGHT)                ; 8
         )
        (PosSize
         (X)                            ; 1
         (Y)                            ; 2
         (WIDTH)                        ; 4
         (HEIGHT)                       ; 8
         (POS)                          ; 3
         (SIZE)                         ; 12
         (POSSIZE)                      ; 15
         )
        (ScrollBarOrientation
         (HORIZONTAL)                   ; 0
         (VERTICAL)                     ; 1
         )
        (Style
         (FRAME)                        ; 0
         (DIALOG)                       ; 1
         )
        (SystemPointer
         (ARROW)                        ; 0
         (INVISIBLE)                    ; 1
         (WAIT)                         ; 2
         (TEXT)                         ; 3
         (HELP)                         ; 4
         (CROSS)                        ; 5
         (MOVE)                         ; 6
         (NSIZE)                        ; 7
         (SSIZE)                        ; 8
         (WSIZE)                        ; 9
         (ESIZE)                        ; 10
         (NWSIZE)                       ; 11
         (NESIZE)                       ; 12
         (SWSIZE)                       ; 13
         (SESIZE)                       ; 14
         (WINDOW_NSIZE)                 ; 15
         (WINDOW_SSIZE)                 ; 16
         (WINDOW_WSIZE)                 ; 17
         (WINDOW_ESIZE)                 ; 18
         (WINDOW_NWSIZE)                ; 19
         (WINDOW_NESIZE)                ; 20
         (WINDOW_SWSIZE)                ; 21
         (WINDOW_SESIZE)                ; 22
         (HSPLIT)                       ; 23
         (VSPLIT)                       ; 24
         (HSIZEBAR)                     ; 25
         (VSIZEBAR)                     ; 26
         (HAND)                         ; 27
         (REFHAND)                      ; 28
         (PEN)                          ; 29
         (MAGNIFY)                      ; 30
         (FILL)                         ; 31
         (ROTATE)                       ; 32
         (HSHEAR)                       ; 33
         (VSHEAR)                       ; 34
         (MIRROR)                       ; 35
         (CROOK)                        ; 36
         (CROP)                         ; 37
         (MOVEPOINT)                    ; 38
         (MOVEBEZIERWEIGHT)             ; 39
         (MOVEDATA)                     ; 40
         (COPYDATA)                     ; 41
         (LINKDATA)                     ; 42
         (MOVEDATALINK)                 ; 43
         (COPYDATALINK)                 ; 44
         (MOVEFILE)                     ; 45
         (COPYFILE)                     ; 46
         (LINKFILE)                     ; 47
         (MOVEFILELINK)                 ; 48
         (COPYFILELINK)                 ; 49
         (MOVEFILES)                    ; 50
         (COPYFILES)                    ; 51
         (NOTALLOWED)                   ; 52
         (DRAW_LINE)                    ; 53
         (DRAW_RECT)                    ; 54
         (DRAW_POLYGON)                 ; 55
         (DRAW_BEZIER)                  ; 56
         (DRAW_ARC)                     ; 57
         (DRAW_PIE)                     ; 58
         (DRAW_CIRCLECUT)               ; 59
         (DRAW_ELLIPSE)                 ; 60
         (DRAW_FREEHAND)                ; 61
         (DRAW_CONNECT)                 ; 62
         (DRAW_TEXT)                    ; 63
         (DRAW_CAPTION)                 ; 64
         (CHART)                        ; 65
         (DETECTIVE)                    ; 66
         (PIVOT_COL)                    ; 67
         (PIVOT_ROW)                    ; 68
         (PIVOT_FIELD)                  ; 69
         (CHAIN)                        ; 70
         (CHAIN_NOTALLOWED)             ; 71
         )
        (TextAlign
         (LEFT)                         ; 0
         (CENTER)                       ; 1
         (RIGHT)                        ; 2
         )
        (VisualEffect
         (NONE)                         ; 0
         (LOOK3D)                       ; 1
         (FLAT)                         ; 2
         )
        (WindowAttribute
         (SHOW)                         ; 1
         (FULLSIZE)                     ; 2
         (OPTIMUMSIZE)                  ; 4
         (MINSIZE)                      ; 8
         (BORDER)                       ; 16
         (SIZEABLE)                     ; 32
         (MOVEABLE)                     ; 64
         (CLOSEABLE)                    ; 128
         (SYSTEMDEPENDENT)              ; 256
         (NODECORATION)                 ; 512
         )
        )
       (beans
        (MethodConcept
         (ALL)                          ; -1
         (DANGEROUS)                    ; 1
         (PROPERTY)                     ; 2
         (LISTENER)                     ; 4
         (ENUMERATION)                  ; 8
         (NAMECONTAINER)                ; 16
         (INDEXCONTAINER)               ; 32
         )
        (PropertyAttribute
         (MAYBEVOID)                    ; 1
         (BOUND)                        ; 2
         (CONSTRAINED)                  ; 4
         (TRANSIENT)                    ; 8
         (READONLY)                     ; 16
         (MAYBEAMBIGUOUS)               ; 32
         (MAYBEDEFAULT)                 ; 64
         (REMOVEABLE)                   ; 128
         (REMOVABLE)                    ; 128
         (OPTIONAL)                     ; 256
         )
        (PropertyConcept
         (ALL)                          ; -1
         (DANGEROUS)                    ; 1
         (PROPERTYSET)                  ; 2
         (ATTRIBUTES)                   ; 4
         (METHODS)                      ; 8
         )
        (PropertySetInfoChange
         (PROPERTY_INSERTED)            ; 0
         (PROPERTY_REMOVED)             ; 1
         )
        (TolerantPropertySetResultType
         (SUCCESS)                      ; 0
         (UNKNOWN_PROPERTY)             ; 1
         (ILLEGAL_ARGUMENT)             ; 2
         (PROPERTY_VETO)                ; 3
         (WRAPPED_TARGET)               ; 4
         (UNKNOWN_FAILURE)              ; 5
         )
        )
       (bridge
        (ModelDependent
         (UNO)                          ; 1
         (OLE)                          ; 2
         (JAVA)                         ; 3
         (CORBA)                        ; 4
         )
        )
       (chart
        (ChartAxisAssign
         (PRIMARY_Y)                    ; 2
         (SECONDARY_Y)                  ; 4
         )
        (ChartAxisMarks
         (NONE)                         ; 0
         (INNER)                        ; 1
         (OUTER)                        ; 2
         )
        (ChartDataCaption
         (NONE)                         ; 0
         (VALUE)                        ; 1
         (PERCENT)                      ; 2
         (TEXT)                         ; 4
         (FORMAT)                       ; 8
         (SYMBOL)                       ; 16
         )
        (ChartSolidType
         (RECTANGULAR_SOLID)            ; 0
         (CYLINDER)                     ; 1
         (CONE)                         ; 2
         (PYRAMID)                      ; 3
         )
        (ChartSymbolType
         (NONE)                         ; -3
         (AUTO)                         ; -2
         (BITMAPURL)                    ; -1
         (SYMBOL0)                      ; 0
         (SYMBOL1)                      ; 1
         (SYMBOL2)                      ; 2
         (SYMBOL3)                      ; 3
         (SYMBOL4)                      ; 4
         (SYMBOL5)                      ; 5
         (SYMBOL6)                      ; 6
         (SYMBOL7)                      ; 7
         )
        (DataLabelPlacement
         (AVOID_OVERLAP)                ; 0
         (CENTER)                       ; 1
         (TOP)                          ; 2
         (TOP_LEFT)                     ; 3
         (LEFT)                         ; 4
         (BOTTOM_LEFT)                  ; 5
         (BOTTOM)                       ; 6
         (BOTTOM_RIGHT)                 ; 7
         (RIGHT)                        ; 8
         (TOP_RIGHT)                    ; 9
         (INSIDE)                       ; 10
         (OUTSIDE)                      ; 11
         (NEAR_ORIGIN)                  ; 12
         )
        (ErrorBarStyle
         (NONE)                         ; 0
         (VARIANCE)                     ; 1
         (STANDARD_DEVIATION)           ; 2
         (ABSOLUTE)                     ; 3
         (RELATIVE)                     ; 4
         (ERROR_MARGIN)                 ; 5
         (STANDARD_ERROR)               ; 6
         (FROM_DATA)                    ; 7
         )
        )
       (chart2
        (AxisPosition
         (MAIN)                         ; 0
         (SECONDARY)                    ; 1
         )
        (AxisType
         (REALNUMBER)                   ; 0
         (PERCENT)                      ; 1
         (CATEGORY)                     ; 2
         (SERIES)                       ; 3
         )
        (DataPointGeometry3D
         (CUBOID)                       ; 0
         (CYLINDER)                     ; 1
         (CONE)                         ; 2
         (PYRAMID)                      ; 3
         )
        (TickmarkStyle
         (NONE)                         ; 0
         (INNER)                        ; 1
         (OUTER)                        ; 2
         )
        )
       (configuration
        (backend
         (NodeAttribute
          (FINALIZED) ; 256
          (MANDATORY) ; 512
          (READONLY)  ; 1024
          (FUSE)      ; 2048
          (MASK)      ; 32512; // 0xFF00, changed to 0x7F00 because only 3 bits)
          )
         (SchemaAttribute
          (REQUIRED)                    ; 1
          (LOCALIZED)                   ; 2
          (EXTENSIBLE)                  ; 4
          (MASK)                        ; 255; // 0x00FF
          )
         )
        )
       (datatransfer
        (clipboard
         (RenderingCapabilities
          (Delayed)                     ; 1
          (Persistant)                  ; 2
          )
         )
        (dnd
         (DNDConstants
          (ACTION_NONE)                 ; 0x00
          (ACTION_COPY)                 ; 0x01
          (ACTION_MOVE)                 ; 0x02
          (ACTION_COPY_OR_MOVE)         ; 0x03
          (ACTION_LINK)                 ; 0x04
          (ACTION_REFERENCE)            ; 0x04
          (ACTION_DEFAULT)              ; 0x80
          )
         )
        )
       (document
        (LinkUpdateModes
         (NEVER)                        ; 0
         (MANUAL)                       ; 1
         (AUTO)                         ; 2
         (GLOBAL_SETTING)               ; 3
         )
        (MacroExecMode
         (NEVER_EXECUTE)                   ; 0
         (FROM_LIST)                       ; 1
         (ALWAYS_EXECUTE)                  ; 2
         (USE_CONFIG)                      ; 3
         (ALWAYS_EXECUTE_NO_WARN)          ; 4
         (USE_CONFIG_REJECT_CONFIRMATION)  ; 5
         (USE_CONFIG_APPROVE_CONFIRMATION) ; 6
         (FROM_LIST_NO_WARN)               ; 7
         (FROM_LIST_AND_SIGNED_WARN)       ; 8
         (FROM_LIST_AND_SIGNED_NO_WARN)    ; 9
         )
        (PrinterIndependentLayout
         (DISABLED)                     ; 1
         (LOW_RESOLUTION)               ; 2
         (ENABLED)                      ; LOW_RESOLUTION
         (HIGH_RESOLUTION)              ; 3
         (LIKE_PARENT)                  ; 4
         )
        (RedlineDisplayType
         (NONE)                         ; 0
         (INSERTED)                     ; 1
         (INSERTED_AND_REMOVED)         ; 2
         (REMOVED)                      ; 3
         )
        (UpdateDocMode
         (NO_UPDATE)                    ; 0
         (QUIET_UPDATE)                 ; 1
         (ACCORDING_TO_CONFIG)          ; 2
         (FULL_UPDATE)                  ; 3
         )
        )
       (drawing
        (CanvasFeature
         (None)                         ; 0
         (SpriteCanvas)                 ; 1
         )
        (CaptionEscapeDirection
         (horizontal)                   ; 0
         (vertical)                     ; 1
         (auto)                         ; 2
         )
        (CaptionType
         (straight)                     ; 0
         (angled)                       ; 1
         (connector)                    ; 2
         )
        (EnhancedCustomShapeGluePointType
         (NONE)                         ; 0
         (SEGMENTS)                     ; 1
         (CUSTOM)                       ; 2
         (RECT)                         ; 3
         )
        (EnhancedCustomShapeParameterType
         (NORMAL)                       ; 0
         (EQUATION)                     ; 1
         (ADJUSTMENT)                   ; 2
         (LEFT)                         ; 3
         (TOP)                          ; 4
         (RIGHT)                        ; 5
         (BOTTOM)                       ; 6
         (XSTRETCH)                     ; 7
         (YSTRETCH)                     ; 8
         (HASSTROKE)                    ; 9
         (HASFILL)                      ; 10
         (WIDTH)                        ; 11
         (HEIGHT)                       ; 12
         (LOGWIDTH)                     ; 13
         (LOGHEIGHT)                    ; 14
         )
        (EnhancedCustomShapeSegmentCommand
         (UNKNOWN)                      ; 0
         (MOVETO)                       ; 1
         (LINETO)                       ; 2
         (CURVETO)                      ; 3
         (CLOSESUBPATH)                 ; 4
         (ENDSUBPATH)                   ; 5
         (NOFILL)                       ; 6
         (NOSTROKE)                     ; 7
         (ANGLEELLIPSETO)               ; 8
         (ANGLEELLIPSE)                 ; 9
         (ARCTO)                        ; 10
         (ARC)                          ; 11
         (CLOCKWISEARCTO)               ; 12
         (CLOCKWISEARC)                 ; 13
         (ELLIPTICALQUADRANTX)          ; 14
         (ELLIPTICALQUADRANTY)          ; 15
         (QUADRATICCURVETO)             ; 16
         )
        )
       (embed
        (Actions
         (PREVENT_CLOSE)                ; 1
         (PREVENT_TERMINATION)          ; 2
         )
        (Aspects
         (MSOLE_CONTENT)                ; 1
         (MSOLE_THUMBNAIL)              ; 2
         (MSOLE_ICON)                   ; 4
         (MSOLE_DOCPRINT)               ; 8
         )
        (ElementModes
         (READ)                         ; 1
         (SEEKABLE)                     ; 2
         (SEEKABLEREAD)                 ; 3
         (WRITE)                        ; 4
         (READWRITE)                    ; 7
         (TRUNCATE)                     ; 8
         (NOCREATE)                     ; 16
         )
        (EmbedMapUnits
         (ONE_100TH_MM)                 ; 0
         (ONE_10TH_MM)                  ; 1
         (ONE_MM)                       ; 2
         (ONE_CM)                       ; 3
         (ONE_1000TH_INCH)              ; 4
         (ONE_100TH_INCH)               ; 5
         (ONE_10TH_INCH)                ; 6
         (ONE_INCH)                     ; 7
         (POINT)                        ; 8
         (TWIP)                         ; 9
         (PIXEL)                        ; 10
         )
        (EmbedMisc
         (MS_EMBED_RECOMPOSEONRESIZE)            ; 1
         (MS_EMBED_ONLYICONIC)                   ; 2
         (MS_EMBED_INSERTNOTREPLACE)             ; 4
         (MS_EMBED_STATIC)                       ; 8
         (MS_EMBED_CANTLINKINSIDE)               ; 16
         (MS_EMBED_CANLINKBYOLE1)                ; 32
         (MS_EMBED_ISLINKOBJECT)                 ; 64
         (MS_EMBED_INSIDEOUT)                    ; 128
         (MS_EMBED_ACTIVATEWHENVISIBLE)          ; 256
         (MS_EMBED_RENDERINGISDEVICEINDEPENDENT) ; 512
         (MS_EMBED_INVISIBLEATRUNTIME)           ; 1024
         (MS_EMBED_ALWAYSRUN)                    ; 2048
         (MS_EMBED_ACTSLIKEBUTTON)               ; 4096
         (MS_EMBED_ACTSLIKELABEL)                ; 8192
         (MS_EMBED_NOUIACTIVATE)                 ; 16384
         (MS_EMBED_ALIGNABLE)                    ; 32768
         (MS_EMBED_SIMPLEFRAME)                  ; 65536
         (MS_EMBED_SETCLIENTSITEFIRST)           ; 131072
         (MS_EMBED_IMEMODE)                      ; 262144
         (MS_EMBED_IGNOREACTIVATEWHENVISIBLE)    ; 524288
         (MS_EMBED_WANTSTOMENUMERGE)             ; 1048576
         (MS_EMBED_SUPPORTSMULTILEVELUNDO)       ; 2097152
         (EMBED_ACTIVATEIMMEDIATELY)             ; 0x100000000
         (EMBED_NEVERRESIZE)                     ; 0x200000000
         (EMBED_NEEDSSIZEONLOAD)                 ; 0x400000000
         )
        (EmbedStates
         (LOADED)                       ; 0
         (RUNNING)                      ; 1
         (ACTIVE)                       ; 2
         (INPLACE_ACTIVE)               ; 3
         (UI_ACTIVE)                    ; 4
         )
        (EmbedUpdateModes
         (ALWAYS_UPDATE)                ; 0
         (EXPLICIT_UPDATE)              ; 1
         )
        (EmbedVerbs
         (MS_OLEVERB_PRIMARY)           ; 0
         (MS_OLEVERB_SHOW)              ; -1
         (MS_OLEVERB_OPEN)              ; -2
         (MS_OLEVERB_HIDE)              ; -3
         (MS_OLEVERB_UIACTIVATE)        ; -4
         (MS_OLEVERB_IPACTIVATE)        ; -5
         (MS_OLEVERB_DISCARDUNDOSTATE)  ; -6
         )
        (EntryInitModes
         (DEFAULT_INIT)                 ; 0
         (TRUNCATE_INIT)                ; 1
         (NO_INIT)                      ; 2
         (MEDIA_DESCRIPTOR_INIT)        ; 3
         (URL_LINK_INIT)                ; 4
         )
        (VerbAttributes
         (MS_VERBATTR_NEVERDIRTIES)     ; 1
         (MS_VERBATTR_ONCONTAINERMENU)  ; 2
         )
        )
       (form
        (runtime
         (FormFeature
          (MoveAbsolute)                ; 1
          (TotalRecords)                ; 2
          (MoveToFirst)                 ; 3
          (MoveToPrevious)              ; 4
          (MoveToNext)                  ; 5
          (MoveToLast)                  ; 6
          (MoveToInsertRow)             ; 7
          (SaveRecordChanges)           ; 8
          (UndoRecordChanges)           ; 9
          (DeleteRecord)                ; 10
          (ReloadForm)                  ; 11
          (SortAscending)               ; 12
          (SortDescending)              ; 13
          (InteractiveSort)             ; 14
          (AutoFilter)                  ; 15
          (InteractiveFilter)           ; 16
          (ToggleApplyFilter)           ; 17
          (RemoveFilterAndSort)         ; 18
          (RefreshCurrentControl)       ; 19
          )
         )
        (FormComponentType
         (CONTROL)                      ; 1
         (COMMANDBUTTON)                ; 2
         (RADIOBUTTON)                  ; 3
         (IMAGEBUTTON)                  ; 4
         (CHECKBOX)                     ; 5
         (LISTBOX)                      ; 6
         (COMBOBOX)                     ; 7
         (GROUPBOX)                     ; 8
         (TEXTFIELD)                    ; 9
         (FIXEDTEXT)                    ; 10
         (GRIDCONTROL)                  ; 11
         (FILECONTROL)                  ; 12
         (HIDDENCONTROL)                ; 13
         (IMAGECONTROL)                 ; 14
         (DATEFIELD)                    ; 15
         (TIMEFIELD)                    ; 16
         (NUMERICFIELD)                 ; 17
         (CURRENCYFIELD)                ; 18
         (PATTERNFIELD)                 ; 19
         (SCROLLBAR)                    ; 20
         (SPINBUTTON)                   ; 21
         (NAVIGATIONBAR)                ; 22
         )
        )
       (frame
        (status
         (ItemState
          (unknown)                     ; 0
          (disabled)                    ; 1
          (read_only)                   ; 2
          (dont_care)                   ; 16
          (default_value)               ; 32
          (set)                         ; 64
          )
         )
        (CommandGroup
         (INTERNAL)                     ; 0
         (APPLICATION)                  ; 1
         (VIEW)                         ; 2
         (DOCUMENT)                     ; 3
         (EDIT)                         ; 4
         (MACRO)                        ; 5
         (OPTIONS)                      ; 6
         (MATH)                         ; 7
         (NAVIGATOR)                    ; 8
         (INSERT)                       ; 9
         (FORMAT)                       ; 10
         (TEMPLATE)                     ; 11
         (TEXT)                         ; 12
         (FRAME)                        ; 13
         (GRAPHIC)                      ; 14
         (TABLE)                        ; 15
         (ENUMERATION)                  ; 16
         (DATA)                         ; 17
         (SPECIAL)                      ; 18
         (IMAGE)                        ; 19
         (CHART)                        ; 20
         (EXPLORER)                     ; 21
         (CONNECTOR)                    ; 22
         (MODIFY)                       ; 23
         (DRAWING)                      ; 24
         (CONTROLS)                     ; 25
         )
        (DispatchResultState
         (FAILURE)                      ; 0
         (SUCCESS)                      ; 1
         (DONTKNOW)                     ; 2
         )
        (FrameSearchFlag
         (AUTO)                         ; 0
         (PARENT)                       ; 1
         (SELF)                         ; 2
         (CHILDREN)                     ; 4
         (CREATE)                       ; 8
         (SIBLINGS)                     ; 16
         (TASKS)                        ; 32
         (ALL)                          ; 23
         (GLOBAL)                       ; 55
         )
        (LayoutManagerEvents
         (LOCK)                         ; 0
         (UNLOCK)                       ; 1
         (LAYOUT)                       ; 2
         (VISIBLE)                      ; 3
         (INVISIBLE)                    ; 4
         (MERGEDMENUBAR)                ; 5
         (UIELEMENT_VISIBLE)            ; 6
         (UIELEMENT_INVISIBLE)          ; 7
         )
        (UntitledNumbersConst
         (INVALID_NUMBER)               ; 0
         )
        (WindowArrange
         (TILE)                         ; 1
         (VERTICAL)                     ; 2
         (HORIZONTAL)                   ; 3
         (CASCADE)                      ; 4
         (MAXIMIZE)                     ; 5
         (MINIMIZE)                     ; 6
         )
        )
       (gallery
        (GalleryItemType
         (EMPTY)                        ; 0
         (GRAPHIC)                      ; 1
         (MEDIA)                        ; 2
         (DRAWING)                      ; 3
         )
        )
       (graphic
        (GraphicColorMode
         (NORMAL)                       ; 0
         (HIGH_CONTRAST)                ; 1
         )
        (GraphicType
         (EMPTY)                        ; 0
         (PIXEL)                        ; 1
         (VECTOR)                       ; 2
         )
        )
       (i18n
        (AmPmValue
         (AM)                           ; 0
         (PM)                           ; 1
         )
        (BreakType
         (WORDBOUNDARY)                 ; 1
         (HYPHENATION)                  ; 2
         (HANGINGPUNCTUATION)           ; 3
         )
        (CTLScriptType
         (CTL_UNKNOWN)                  ; 0
         (CTL_HEBREW)                   ; 1
         (CTL_ARABIC)                   ; 2
         (CTL_THAI)                     ; 3
         (CTL_INDIC)                    ; 4
         )
        (CalendarDisplayCode
         (SHORT_DAY)                    ; 1
         (LONG_DAY)                     ; 2
         (SHORT_DAY_NAME)               ; 3
         (LONG_DAY_NAME)                ; 4
         (SHORT_MONTH)                  ; 5
         (LONG_MONTH)                   ; 6
         (SHORT_MONTH_NAME)             ; 7
         (LONG_MONTH_NAME)              ; 8
         (SHORT_YEAR)                   ; 9
         (LONG_YEAR)                    ; 10
         (SHORT_ERA)                    ; 11
         (LONG_ERA)                     ; 12
         (SHORT_YEAR_AND_ERA)           ; 13
         (LONG_YEAR_AND_ERA)            ; 14
         (SHORT_QUARTER)                ; 15
         (LONG_QUARTER)                 ; 16
         )
        (CalendarDisplayIndex
         (AM_PM)                        ; 0
         (DAY)                          ; 1
         (MONTH)                        ; 2
         (YEAR)                         ; 3
         (ERA)                          ; 4
         )
        (CalendarFieldIndex
         (AM_PM)                        ; 0
         (DAY_OF_MONTH)                 ; 1
         (DAY_OF_WEEK)                  ; 2
         (DAY_OF_YEAR)                  ; 3
         (DST_OFFSET)                   ; 4
         (HOUR)                         ; 5
         (MINUTE)                       ; 6
         (SECOND)                       ; 7
         (MILLISECOND)                  ; 8
         (WEEK_OF_MONTH)                ; 9
         (WEEK_OF_YEAR)                 ; 10
         (YEAR)                         ; 11
         (MONTH)                        ; 12
         (ERA)                          ; 13
         (ZONE_OFFSET)                  ; 14
         (FIELD_COUNT)                  ; 15
         )
        (CharType
         (ANY_CHAR)                     ; 0
         (UPPERCASE_LETTER)             ; 1
         (LOWERCASE_LETTER)             ; 2
         (TITLECASE_LETTER)             ; 3
         (MODIFIER_LETTER)              ; 4
         (OTHER_LETTER)                 ; 5
         (NON_SPACING_MARK)             ; 6
         (ENCLOSING_MARK)               ; 7
         (COMBINING_SPACING_MARK)       ; 8
         (DECIMAL_DIGIT_NUMBER)         ; 9
         (LETTER_NUMBER)                ; 10
         (OTHER_NUMBER)                 ; 11
         (SPACE_SEPARATOR)              ; 12
         (LINE_SEPARATOR)               ; 13
         (PARAGRAPH_SEPARATOR)          ; 14
         (CONTROL)                      ; 15
         (FORMAT)                       ; 16
         (PRIVATE_USE)                  ; 17
         (SURROGATE)                    ; 18
         (DASH_PUNCTUATION)             ; 19
         (START_PUNCTUATION)            ; 20
         (END_PUNCTUATION)              ; 21
         (CONNECTOR_PUNCTUATION)        ; 22
         (OTHER_PUNCTUATION)            ; 23
         (MATH_SYMBOL)                  ; 24
         (CURRENCY_SYMBOL)              ; 25
         (MODIFIER_SYMBOL)              ; 26
         (OTHER_SYMBOL)                 ; 27
         (INITIAL_PUNCTUATION)          ; 28
         (FINAL_PUNCTUATION)            ; 29
         (GENERAL_TYPES_COUNT)          ; 30
         )
        (CharacterIteratorMode
         (SKIPCHARACTER)                ; 0
         (SKIPCELL)                     ; 1
         (SKIPCONTROLCHARACTER)         ; 2
         )
        (CollatorOptions
         (CollatorOptions_IGNORE_CASE)        ; 1
         (CollatorOptions_IGNORE_KANA)        ; 2
         (CollatorOptions_IGNORE_WIDTH)       ; 4
         (CollatorOptions_IGNORE_CASE_ACCENT) ; 8
         )
        (InputSequenceCheckMode
         (PASSTHROUGH)                  ; 0
         (BASIC)                        ; 1
         (STRICT)                       ; 2
         )
        (KCharacterType
         (DIGIT)                        ; 0x00000001
         (UPPER)                        ; 0x00000002
         (LOWER)                        ; 0x00000004
         (TITLE_CASE)                   ; 0x00000008
         (ALPHA)                        ; 0x0000000E
         (CONTROL)                      ; 0x00000010
         (PRINTABLE)                    ; 0x00000020
         (BASE_FORM)                    ; 0x00000040
         (LETTER)                       ; 0x00000080
         )
        (KNumberFormatType
         (SHORT)                        ; 1
         (MEDIUM)                       ; 2
         (LONG)                         ; 3
         )
        (KNumberFormatUsage
         (DATE)                         ; 1
         (TIME)                         ; 2
         (DATE_TIME)                    ; 3
         (FIXED_NUMBER)                 ; 4
         (FRACTION_NUMBER)              ; 5
         (PERCENT_NUMBER)               ; 6
         (SCIENTIFIC_NUMBER)            ; 7
         (CURRENCY)                     ; 8
         )
        (KParseTokens
         (ASC_UPALPHA)                    ; 0x00000001
         (ASC_LOALPHA)                    ; 0x00000002
         (ASC_DIGIT)                      ; 0x00000004
         (ASC_UNDERSCORE)                 ; 0x00000008
         (ASC_DOLLAR)                     ; 0x00000010
         (ASC_DOT)                        ; 0x00000020
         (ASC_COLON)                      ; 0x00000040
         (ASC_CONTROL)                    ; 0x00000200
         (ASC_ANY_BUT_CONTROL)            ; 0x00000400
         (ASC_OTHER)                      ; 0x00000800
         (UNI_UPALPHA)                    ; 0x00001000
         (UNI_LOALPHA)                    ; 0x00002000
         (UNI_DIGIT)                      ; 0x00004000
         (UNI_TITLE_ALPHA)                ; 0x00008000
         (UNI_MODIFIER_LETTER)            ; 0x00010000
         (UNI_OTHER_LETTER)               ; 0x00020000
         (UNI_LETTER_NUMBER)              ; 0x00040000
         (UNI_OTHER_NUMBER)               ; 0x00080000
         (TWO_DOUBLE_QUOTES_BREAK_STRING) ; 0x10000000
         (UNI_OTHER)                      ; 0x20000000
         (IGNORE_LEADING_WS)              ; 0x40000000
         (ASC_ALPHA)                      ; ASC_UPALPHA | ASC_LOALPHA
         (ASC_ALNUM)                      ; ASC_ALPHA | ASC_DIGIT
         (UNI_ALPHA)               ; UNI_UPALPHA | UNI_LOALPHA | UNI_TITLE_ALPHA
         (UNI_ALNUM)               ; UNI_ALPHA | UNI_DIGIT
         (UNI_LETTER)              ; UNI_ALPHA | UNI_MODIFIER_LETTER |
         (UNI_NUMBER)              ; UNI_DIGIT | UNI_LETTER_NUMBER |
         (ANY_ALPHA)               ; ASC_ALPHA | UNI_ALPHA
         (ANY_DIGIT)               ; ASC_DIGIT | UNI_DIGIT
         (ANY_ALNUM)               ; ASC_ALNUM | UNI_ALNUM
         (ANY_LETTER)              ; ASC_ALPHA | UNI_LETTER
         (ANY_NUMBER)              ; ASC_DIGIT | UNI_NUMBER
         (ANY_LETTER_OR_NUMBER)    ; ANY_LETTER | ANY_NUMBER
         )
        (KParseType
         (ONE_SINGLE_CHAR)              ; 0x00000001
         (BOOLEAN)                      ; 0x00000002
         (IDENTNAME)                    ; 0x00000004
         (SINGLE_QUOTE_NAME)            ; 0x00000008
         (DOUBLE_QUOTE_STRING)          ; 0x00000010
         (ASC_NUMBER)                   ; 0x00000020
         (UNI_NUMBER)                   ; 0x00000040
         (MISSING_QUOTE)                ; 0x40000000
         (ANY_NUMBER)                   ; ASC_NUMBER | UNI_NUMBER
         )
        (LocaleItem
         (DATE_SEPARATOR)                  ; 0
         (THOUSAND_SEPARATOR)              ; 1
         (DECIMAL_SEPARATOR)               ; 2
         (TIME_SEPARATOR)                  ; 3
         (TIME_100SEC_SEPARATOR)           ; 4
         (LIST_SEPARATOR)                  ; 5
         (SINGLE_QUOTATION_START)          ; 6
         (SINGLE_QUOTATION_END)            ; 7
         (DOUBLE_QUOTATION_START)          ; 8
         (DOUBLE_QUOTATION_END)            ; 9
         (MEASUREMENT_SYSTEM)              ; 10
         (TIME_AM)                         ; 11
         (TIME_PM)                         ; 12
         (LONG_DATE_DAY_OF_WEEK_SEPARATOR) ; 13
         (LONG_DATE_DAY_SEPARATOR)         ; 14
         (LONG_DATE_MONTH_SEPARATOR)       ; 15
         (LONG_DATE_YEAR_SEPARATOR)        ; 16
         (COUNT)                           ; 17
         )
        (Months
         (JANUARY)                      ; 0
         (FEBURARY)                     ; 1
         (MARCH)                        ; 2
         (APRIL)                        ; 3
         (MAY)                          ; 4
         (JUNE)                         ; 5
         (JULY)                         ; 6
         (AUGUST)                       ; 7
         (SEPTEMBER)                    ; 8
         (OCTOBER)                      ; 9
         (NOVEMBER)                     ; 10
         (DECEMBER)                     ; 11
         )
        (NativeNumberMode
         (NATNUM0)                      ; 0
         (NATNUM1)                      ; 1
         (NATNUM2)                      ; 2
         (NATNUM3)                      ; 3
         (NATNUM4)                      ; 4
         (NATNUM5)                      ; 5
         (NATNUM6)                      ; 6
         (NATNUM7)                      ; 7
         (NATNUM8)                      ; 8
         (NATNUM9)                      ; 9
         (NATNUM10)                     ; 10
         (NATNUM11)                     ; 11
         )
        (NumberFormatIndex
         (NUMBER_START)                 ; 0
         (NUMBER_STANDARD)              ; NUMBER_START
         (NUMBER_INT)                   ; NUMBER_START+1
         (NUMBER_DEC2)                  ; NUMBER_START+2
         (NUMBER_1000INT)               ; NUMBER_START+3
         (NUMBER_1000DEC2)              ; NUMBER_START+4
         (NUMBER_SYSTEM)                ; NUMBER_START+5
         (NUMBER_END)                   ; NUMBER_SYSTEM
         (SCIENTIFIC_START)             ; NUMBER_END+1
         (SCIENTIFIC_000E000)           ; SCIENTIFIC_START
         (SCIENTIFIC_000E00)            ; SCIENTIFIC_START+1
         (SCIENTIFIC_END)               ; SCIENTIFIC_000E00
         (PERCENT_START)                ; SCIENTIFIC_END+1
         (PERCENT_INT)                  ; PERCENT_START
         (PERCENT_DEC2)                 ; PERCENT_START+1
         (PERCENT_END)                  ; PERCENT_DEC2
         (FRACTION_START)               ; PERCENT_END+1
         (FRACTION_1)                   ; FRACTION_START
         (FRACTION_2)                   ; FRACTION_START+1
         (FRACTION_END)                 ; FRACTION_2
         (CURRENCY_START)               ; FRACTION_END+1
         (CURRENCY_1000INT)             ; CURRENCY_START
         (CURRENCY_1000DEC2)            ; CURRENCY_START+1
         (CURRENCY_1000INT_RED)         ; CURRENCY_START+2
         (CURRENCY_1000DEC2_RED)        ; CURRENCY_START+3
         (CURRENCY_1000DEC2_CCC)        ; CURRENCY_START+4
         (CURRENCY_1000DEC2_DASHED)     ; CURRENCY_START+5
         (CURRENCY_END)                 ; CURRENCY_1000DEC2_DASHED
         (DATE_START)                   ; CURRENCY_END+1
         (DATE_SYSTEM_SHORT)            ; DATE_START
         (DATE_SYSTEM_LONG)             ; DATE_START+1
         (DATE_SYS_DDMMYY)              ; DATE_START+2
         (DATE_SYS_DDMMYYYY)            ; DATE_START+3
         (DATE_SYS_DMMMYY)              ; DATE_START+4
         (DATE_SYS_DMMMYYYY)            ; DATE_START+5
         (DATE_DIN_DMMMYYYY)            ; DATE_START+6
         (DATE_SYS_DMMMMYYYY)           ; DATE_START+7
         (DATE_DIN_DMMMMYYYY)           ; DATE_START+8
         (DATE_SYS_NNDMMMYY)            ; DATE_START+9
         (DATE_DEF_NNDDMMMYY)           ; DATE_START+10
         (DATE_SYS_NNDMMMMYYYY)         ; DATE_START+11
         (DATE_SYS_NNNNDMMMMYYYY)       ; DATE_START+12
         (DATE_DIN_MMDD)                ; DATE_START+13
         (DATE_DIN_YYMMDD)              ; DATE_START+14
         (DATE_DIN_YYYYMMDD)            ; DATE_START+15
         (DATE_SYS_MMYY)                ; DATE_START+16
         (DATE_SYS_DDMMM)               ; DATE_START+17
         (DATE_MMMM)                    ; DATE_START+18
         (DATE_QQJJ)                    ; DATE_START+19
         (DATE_WW)                      ; DATE_START+20
         (DATE_END)                     ; DATE_WW
         (TIME_START)                   ; DATE_END+1
         (TIME_HHMM)                    ; TIME_START
         (TIME_HHMMSS)                  ; TIME_START+1
         (TIME_HHMMAMPM)                ; TIME_START+2
         (TIME_HHMMSSAMPM)              ; TIME_START+3
         (TIME_HH_MMSS)                 ; TIME_START+4
         (TIME_MMSS00)                  ; TIME_START+5
         (TIME_HH_MMSS00)               ; TIME_START+6
         (TIME_END)                     ; TIME_HH_MMSS00
         (DATETIME_START)               ; TIME_END + 1
         (DATETIME_SYSTEM_SHORT_HHMM)   ; DATETIME_START
         (DATETIME_SYS_DDMMYYYY_HHMMSS) ; DATETIME_START+1
         (DATETIME_END)                 ; DATETIME_SYS_DDMMYYYY_HHMMSS
         (BOOLEAN)                      ; DATETIME_END+1
         (TEXT)                         ; BOOLEAN+1
         (INDEX_TABLE_ENTRIES)          ; TEXT+1
         )
        (ScriptDirection
         (NEUTRAL)                      ; 0
         (LEFT_TO_RIGHT)                ; 1
         (RIGHT_TO_LEFT)                ; 2
         )
        (ScriptType
         (LATIN)                        ; 1
         (ASIAN)                        ; 2
         (COMPLEX)                      ; 3
         (WEAK)                         ; 4
         )
        (TextConversionOption
         (NONE)                         ; 0
         (CHARACTER_BY_CHARACTER)       ; 1; (1 << 0)
         (IGNORE_POST_POSITIONAL_WORD)  ; 2; (1 << 1)
         (USE_CHARACTER_VARIANTS)       ; 2; (1 << 1)
         )
        (TextConversionType
         (TO_HANGUL)                    ; 1
         (TO_HANJA)                     ; 2
         (TO_SCHINESE)                  ; 3
         (TO_TCHINESE)                  ; 4
         )
        (TransliterationType
         (NONE)                         ; 0
         (ONE_TO_ONE)                   ; 1
         (NUMERIC)                      ; 2
         (ONE_TO_ONE_NUMERIC)           ; 3
         (IGNORE)                       ; 4
         (CASCADE)                      ; 8
         )
        (UnicodeType
         (UNASSIGNED)                   ; 0
         (UPPERCASE_LETTER)             ; 1
         (LOWERCASE_LETTER)             ; 2
         (TITLECASE_LETTER)             ; 3
         (MODIFIER_LETTER)              ; 4
         (OTHER_LETTER)                 ; 5
         (NON_SPACING_MARK)             ; 6
         (ENCLOSING_MARK)               ; 7
         (COMBINING_SPACING_MARK)       ; 8
         (DECIMAL_DIGIT_NUMBER)         ; 9
         (LETTER_NUMBER)                ; 10
         (OTHER_NUMBER)                 ; 11
         (SPACE_SEPARATOR)              ; 12
         (LINE_SEPARATOR)               ; 13
         (PARAGRAPH_SEPARATOR)          ; 14
         (CONTROL)                      ; 15
         (FORMAT)                       ; 16
         (PRIVATE_USE)                  ; 17
         (SURROGATE)                    ; 18
         (DASH_PUNCTUATION)             ; 19
         (INITIAL_PUNCTUATION)          ; 20
         (FINAL_PUNCTUATION)            ; 21
         (CONNECTOR_PUNCTUATION)        ; 22
         (OTHER_PUNCTUATION)            ; 23
         (MATH_SYMBOL)                  ; 24
         (CURRENCY_SYMBOL)              ; 25
         (MODIFIER_SYMBOL)              ; 26
         (OTHER_SYMBOL)                 ; 27
         (START_PUNCTUATION)            ; 28
         (END_PUNCTUATION)              ; 29
         (GENERAL_TYPES_COUNT)          ; 30
         )
        (Weekdays
         (SUNDAY)                       ; 0
         (MONDAY)                       ; 1
         (TUESDAY)                      ; 2
         (WEDNESDAY)                    ; 3
         (THURSDAY)                     ; 4
         (FRIDAY)                       ; 5
         (SATURDAY)                     ; 6
         )
        (WordType
         (ANY_WORD)                     ; 0
         (ANYWORD_IGNOREWHITESPACES)    ; 1
         (DICTIONARY_WORD)              ; 2
         (WORD_COUNT)                   ; 3
         )
        (reservedWords
         (TRUE_WORD)                    ; 0
         (FALSE_WORD)                   ; 1
         (QUARTER1_WORD)                ; 2
         (QUARTER2_WORD)                ; 3
         (QUARTER3_WORD)                ; 4
         (QUARTER4_WORD)                ; 5
         (ABOVE_WORD)                   ; 6
         (BELOW_WORD)                   ; 7
         (QUARTER1_ABBREVIATION)        ; 8
         (QUARTER2_ABBREVIATION)        ; 9
         (QUARTER3_ABBREVIATION)        ; 10
         (QUARTER4_ABBREVIATION)        ; 11
         (COUNT)                        ; 12
         )
        )
       (inspection
        (PropertyControlType
         (ListBox)                      ; 1
         (ComboBox)                     ; 2
         (TextField)                    ; 3
         (MultiLineTextField)           ; 4
         (CharacterField)               ; 5
         (StringListField)              ; 6
         (ColorListBox)                 ; 7
         (NumericField)                 ; 8
         (DateField)                    ; 9
         (TimeField)                    ; 10
         (DateTimeField)                ; 11
         (HyperlinkField)               ; 12
         (Unknown)                      ; 13
         )
        (PropertyLineElement
         (InputControl)                 ; 0x01
         (PrimaryButton)                ; 0x02
         (SecondaryButton)              ; 0x04
         (All)                          ; 0xFF
         )
        )
       (linguistic2
        (ConversionDictionaryType
         (HANGUL_HANJA)                 ; 1
         (SCHINESE_TCHINESE)            ; 2
         )
        (ConversionPropertyType
         (NOT_DEFINED)                  ; 0
         (OTHER)                        ; 1
         (FOREIGN)                      ; 2
         (FIRST_NAME)                   ; 3
         (LAST_NAME)                    ; 4
         (TITLE)                        ; 5
         (STATUS)                       ; 6
         (PLACE_NAME)                   ; 7
         (BUSINESS)                     ; 8
         (ADJECTIVE)                    ; 9
         (IDIOM)                        ; 10
         (ABBREVIATION)                 ; 11
         (NUMERICAL)                    ; 12
         (NOUN)                         ; 13
         (VERB)                         ; 14
         (BRAND_NAME)                   ; 15
         )
        (DictionaryEventFlags
         (ADD_ENTRY)                    ; 1
         (DEL_ENTRY)                    ; 2
         (CHG_NAME)                     ; 4
         (CHG_LANGUAGE)                 ; 8
         (ENTRIES_CLEARED)              ; 16
         (ACTIVATE_DIC)                 ; 32
         (DEACTIVATE_DIC)               ; 64
         )
        (DictionaryListEventFlags
         (ADD_POS_ENTRY)                ; 1
         (DEL_POS_ENTRY)                ; 2
         (ADD_NEG_ENTRY)                ; 4
         (DEL_NEG_ENTRY)                ; 8
         (ACTIVATE_POS_DIC)             ; 16
         (DEACTIVATE_POS_DIC)           ; 32
         (ACTIVATE_NEG_DIC)             ; 64
         (DEACTIVATE_NEG_DIC)           ; 128
         )
        (LinguServiceEventFlags
         (SPELL_CORRECT_WORDS_AGAIN)    ; 1
         (SPELL_WRONG_WORDS_AGAIN)      ; 2
         (HYPHENATE_AGAIN)              ; 4
         (GRAMMAR_CHECK_AGAIN)          ; 5
         )
        (SpellFailure
         (IS_NEGATIVE_WORD)             ; 2
         (CAPTION_ERROR)                ; 3
         (SPELLING_ERROR)               ; 4
         )
        )
       (logging
        (LogLevel
         (OFF)                          ; 0x7FFFFFFF
         (SEVERE)                       ; 1000
         (WARNING)                      ; 900
         (INFO)                         ; 800
         (CONFIG)                       ; 700
         (FINE)                         ; 500
         (FINER)                        ; 400
         (FINEST)                       ; 300
         (ALL)                          ; -0x80000000
         )
        )
       (packages
        (zip
         (ZipConstants
          (DEFLATED)                    ; 8
          (NO_COMPRESSION)              ; 0
          (BEST_SPEED)                  ; 1
          (BEST_COMPRESSION)            ; 9
          (DEFAULT_COMPRESSION)         ; -1
          (FILTERED)                    ; 1
          (HUFFMAN_ONLY)                ; 2
          (DEFAULT_STRATEGY)            ; 0
          (STORED)                      ; 0
          (DEF_MEM_LEVEL)               ; 8
          (LOCSIG)                      ; 0x04034b50
          (EXTSIG)                      ; 0x08074b50
          (CENSIG)                      ; 0x02014b50
          (ENDSIG)                      ; 0x06054b50
          (SPANSIG)                     ; 0x08074b50
          (LOCHDR)                      ; 30
          (EXTHDR)                      ; 16
          (CENHDR)                      ; 46
          (ENDHDR)                      ; 22
          (LOCVER)                      ; 4
          (LOCFLG)                      ; 6
          (LOCHOW)                      ; 8
          (LOCTIM)                      ; 10
          (LOCCRC)                      ; 14
          (LOCSIZ)                      ; 18
          (LOCLEN)                      ; 22
          (LOCNAM)                      ; 26
          (LOCEXT)                      ; 28
          (EXTCRC)                      ; 4
          (EXTSIZ)                      ; 8
          (EXTLEN)                      ; 12
          (CENVEM)                      ; 4
          (CENVER)                      ; 6
          (CENFLG)                      ; 8
          (CENHOW)                      ; 10
          (CENTIM)                      ; 12
          (CENDAT)                      ; 14
          (CENCRC)                      ; 16
          (CENSIZ)                      ; 20
          (CENLEN)                      ; 24
          (CENNAM)                      ; 28
          (CENEXT)                      ; 30
          (CENCOM)                      ; 32
          (CENDSK)                      ; 34
          (CENATT)                      ; 36
          (CENATX)                      ; 38
          (CENOFF)                      ; 42
          (ENDSUB)                      ; 8
          (ENDTOT)                      ; 10
          (ENDSIZ)                      ; 12
          (ENDOFF)                      ; 16
          (ENDCOM)                      ; 20
          )
         )
        )
       (plugin
        (PluginMode
         (EMBED)                        ; 1
         (FULL)                         ; 2
         )
        )
       (presentation
        (EffectCommands
         (CUSTOM)                       ; 0
         (VERB)                         ; 1
         (PLAY)                         ; 2
         (TOGGLEPAUSE)                  ; 3
         (STOP)                         ; 4
         (STOPAUDIO)                    ; 5
         )
        (EffectNodeType
         (DEFAULT)                      ; 0
         (ON_CLICK)                     ; 1
         (WITH_PREVIOUS)                ; 2
         (AFTER_PREVIOUS)               ; 3
         (MAIN_SEQUENCE)                ; 4
         (TIMING_ROOT)                  ; 5
         (INTERACTIVE_SEQUENCE)         ; 6
         )
        (EffectPresetClass
         (CUSTOM)                       ; 0
         (ENTRANCE)                     ; 1
         (EXIT)                         ; 2
         (EMPHASIS)                     ; 3
         (MOTIONPATH)                   ; 4
         (OLEACTION)                    ; 5
         (MEDIACALL)                    ; 6
         )
        (ShapeAnimationSubType
         (AS_WHOLE)                     ; 0
         (ONLY_BACKGROUND)              ; 1
         (ONLY_TEXT)                    ; 2
         )
        (TextAnimationType
         (BY_PARAGRAPH)                 ; 0
         (BY_WORD)                      ; 1
         (BY_LETTER)                    ; 2
         )
        )
       (rdf
        (FileFormat
         (RDF_XML)         ; 0;    // "application/rdf+xml"
         (N3)              ; 1;    // "text/rdf+n3"
         (NTRIPLES)        ; 2;    // "text/plain"
         (TRIG)            ; 3;    // "application/x-trig"
         (TRIX)            ; 4;    // "if only the damn server were up i'd know"
         (TURTLE)          ; 5;    // "application/turtle"
         )
        (URIs
         (XSD_NCNAME)                   ; 1
         (XSD_STRING)                   ; 2
         (RDF_TYPE)                     ; 1000
         (RDF_SUBJECT)                  ; 1001
         (RDF_PREDICATE)                ; 1002
         (RDF_OBJECT)                   ; 1003
         (RDF_PROPERTY)                 ; 1004
         (RDF_STATEMENT)                ; 1005
         (RDFS_COMMENT)                 ; 1100
         (RDFS_LABEL)                   ; 1101
         (RDFS_DOMAIN)                  ; 1102
         (RDFS_RANGE)                   ; 1103
         (RDFS_SUBCLASSOF)              ; 1104
         (RDFS_LITERAL)                 ; 1105
         (OWL_CLASS)                    ; 1200
         (OWL_OBJECTPROPERTY)           ; 1201
         (OWL_DATATYPEPROPERTY)         ; 1202
         (OWL_FUNCTIONALPROPERTY)       ; 1203
         (PKG_HASPART)                  ; 2000
         (PKG_IDREF)                    ; 2001
         (PKG_PATH)                     ; 2002
         (PKG_MIMETYPE)                 ; 2003
         (PKG_PACKAGE)                  ; 2004
         (PKG_ELEMENT)                  ; 2005
         (PKG_FILE)                     ; 2006
         (ODF_PREFIX)                   ; 2100
         (ODF_SUFFIX)                   ; 2101
         (ODF_ELEMENT)                  ; 2102
         (ODF_CONTENTFILE)              ; 2103
         (ODF_STYLESFILE)               ; 2104
         (ODF_METADATAFILE)             ; 2105
         ;;(TEXT_META_FIELD) ; 3000
         )
        )
       (rendering
        (AnimationRepeat
         (ONE_SHOT)                     ; 0
         (ONE_SHOT_PINGPONG)            ; 1
         (PINGPONG)                     ; 2
         (REPEAT)                       ; 3
         )
        (BlendMode
         (NORMAL)                       ; 0
         (MULTIPLY)                     ; 1
         (SCREEN)                       ; 2
         (OVERLAY)                      ; 3
         (DARKEN)                       ; 4
         (LIGHTEN)                      ; 5
         (COLOR_DODGE)                  ; 6
         (COLOR_BURN)                   ; 7
         (HARD_LIGHT)                   ; 8
         (SOFT_LIGHT)                   ; 9
         (DIFFERENCE)                   ; 10
         (EXCLUSION)                    ; 11
         (HUE)                          ; 12
         (SATURATION)                   ; 13
         (COLOR)                        ; 14
         (LUMINOSITY)                   ; 15
         )
        (ColorComponentTag
         (DEVICE)                       ; 0
         (RGB_RED)                      ; 1
         (RGB_GREEN)                    ; 2
         (RGB_BLUE)                     ; 3
         (CMYK_CYAN)                    ; 4
         (CMYK_MAGENTA)                 ; 5
         (CMYK_YELLOW)                  ; 6
         (CMYK_BLACK)                   ; 7
         (CMYKOG_ORANGE)                ; 8
         (CMYKOG_GREEN)                 ; 9
         (SPOT)                         ; 10
         (INDEX)                        ; 11
         (ALPHA)                        ; 12
         (GREY)                         ; 13
         (CIEXYZ_X)                     ; 14
         (CIEXYZ_Y)                     ; 15
         (CIEXYZ_Z)                     ; 16
         (CIELAB_L)                     ; 17
         (CIELAB_A)                     ; 18
         (CIELAB_B)                     ; 19
         (HSV_H)                        ; 20
         (HSV_S)                        ; 21
         (HSV_V)                        ; 22
         (HSL_H)                        ; 23
         (HSL_S)                        ; 24
         (HSL_L)                        ; 25
         (YCBCR_Y)                      ; 26
         (YCBCR_CB)                     ; 27
         (YCBCR_CR)                     ; 28
         )
        (ColorSpaceType
         (DEVICE_COLOR)                 ; 0
         (GREY)                         ; 1
         (RGB)                          ; 2
         (CMYK)                         ; 3
         (CMYKOG)                       ; 4
         (CIEXYZ)                       ; 5
         (CIELAB)                       ; 6
         (SRGB)                         ; 7
         (HSV)                          ; 8
         (HSL)                          ; 9
         (YCBCR)                        ; 10
         (INDEXED)                      ; 11
         )
        (CompositeOperation
         (CLEAR)                        ; 0
         (SOURCE)                       ; 1
         (DESTINATION)                  ; 2
         (OVER)                         ; 3
         (UNDER)                        ; 4
         (INSIDE)                       ; 5
         (INSIDE_REVERSE)               ; 6
         (OUTSIDE)                      ; 7
         (OUTSIDE_REVERSE)              ; 8
         (ATOP)                         ; 9
         (ATOP_REVERSE)                 ; 10
         (XOR)                          ; 11
         (ADD)                          ; 12
         (SATURATE)                     ; 13
         )
        (EmphasisMark
         (NONE)                         ; 0
         (DOT_ABOVE)                    ; 1
         (DOT_BELOW)                    ; 2
         (CIRCLE_ABOVE)                 ; 3
         (CIRCLE_BELOW)                 ; 4
         (DISC_ABOVE)                   ; 5
         (DISC_BELOW)                   ; 6
         (ACCENT_ABOVE)                 ; 7
         (ACCENT_BELOW)                 ; 8
         )
        (FloatingPointBitmapFormat
         (HALFFLOAT)                    ; 0
         (FLOAT)                        ; 1
         (DOUBLE)                       ; 2
         )
        (InterpolationMode
         (NEAREST_NEIGHBOR)             ; 1
         (LINEAR)                       ; 2
         (CUBIC)                        ; 3
         (BEZIERSPLINE3)                ; 4
         (BEZIERSPLINE4)                ; 5
         )
        (PanoseArmStyle
         (ANYTHING)                     ; 0
         (NO_FIT)                       ; 1
         (STRAIGHT_HORIZONTAL)          ; 2
         (STRAIGHT_WEDGE)               ; 3
         (STRAIGHT_VERTICAL)            ; 4
         (STRAIGHT_SINGLE_SERIF)        ; 5
         (STRAIGHT_DOUBLE_SERIF)        ; 6
         (BENT_HORIZONTAL)              ; 7
         (BENT_WEDGE)                   ; 8
         (BENT_VERTICAL)                ; 9
         (BENT_SINGLE_SERIF)            ; 10
         (BENT_DOUBLE_SERIF)            ; 11
         )
        (PanoseContrast
         (ANYTHING)                     ; 0
         (NO_FIT)                       ; 1
         (NONE)                         ; 2
         (VERY_LOW)                     ; 3
         (LOW)                          ; 4
         (MEDIUM_LOW)                   ; 5
         (MEDIUM)                       ; 6
         (MEDIUM_HIGH)                  ; 7
         (HIGH)                         ; 8
         (VERY_HIGH)                    ; 9
         )
        (PanoseFamilyTypes
         (ANYTHING)                     ; 0
         (NO_FIT)                       ; 1
         (TEXT_DISPLAY)                 ; 2
         (SCRIPT)                       ; 3
         (DECORATIVE)                   ; 4
         (PICTORIAL)                    ; 5
         )
        (PanoseLetterForm
         (ANYTHING)                     ; 0
         (NO_FIT)                       ; 1
         (NORMAL_CONTACT)               ; 2
         (NORMAL_WEIGHTED)              ; 3
         (NORMAL_BOXED)                 ; 4
         (NORMAL_FLATTENED)             ; 5
         (NORMAL_ROUNDED)               ; 6
         (NORMAL_OFF_CENTER)            ; 7
         (NORMAL_SQUARE)                ; 8
         (OBLIQUE_CONTACT)              ; 9
         (OBLIQUE_WEIGHTED)             ; 10
         (OBLIQUE_BOXED)                ; 11
         (OBLIQUE_FLATTENED)            ; 12
         (OBLIQUE_ROUNDED)              ; 13
         (OBLIQUE_OFF_CENTER)           ; 14
         (OBLIQUE_SQUARE)               ; 15
         )
        (PanoseMidline
         (ANYTHING)                     ; 0
         (NO_FIT)                       ; 1
         (STANDARD_TRIMMED)             ; 2
         (STANDARD_POINTED)             ; 3
         (STANDARD_SERIFED)             ; 4
         (HIGH_TRIMMER)                 ; 5
         (HIGH_POINTED)                 ; 6
         (HIGH_SERIFED)                 ; 7
         (CONSTANT_TRIMMED)             ; 8
         (CONSTANT_POINTED)             ; 9
         (CONSTANT_SERIFED)             ; 10
         (LOW_TRIMMED)                  ; 11
         (LOW_POINTED)                  ; 12
         (LOW_SERIFED)                  ; 13
         )
        (PanoseProportion
         (ANYTHING)                     ; 0
         (NO_FIT)                       ; 1
         (OLD_SKOOL)                    ; 2
         (MODERN)                       ; 3
         (EVEN_WIDTH)                   ; 4
         (EXPANDED)                     ; 5
         (CONDENSED)                    ; 6
         (VERY_EXPANDED)                ; 7
         (VERY_CONDENSED)               ; 8
         (MONO_SPACED)                  ; 9
         )
        (PanoseSerifStyle
         (ANYTHING)                     ; 0
         (NO_FIT)                       ; 1
         (COVE)                         ; 2
         (OBTUSE_COVE)                  ; 3
         (SQUARE_COVE)                  ; 4
         (OBTUSE_SQUARE_COVE)           ; 5
         (SQUARE)                       ; 6
         (THIN)                         ; 7
         (BONE)                         ; 8
         (EXAGGERATED)                  ; 9
         (TRIANGLE)                     ; 10
         (NORMAL_SANS)                  ; 11
         (OBTUSE_SANS)                  ; 12
         (PERP_SANS)                    ; 13
         (FLARED)                       ; 14
         (ROUNDED)                      ; 15
         )
        (PanoseStrokeVariation
         (ANYTHING)                     ; 0
         (NO_FIT)                       ; 1
         (GRADUAL_DIAGONAL)             ; 2
         (GRADUAL_TRANSITIONAL)         ; 3
         (GRADUAL_VERTICAL)             ; 4
         (GRADUAL_HORIZONTAL)           ; 5
         (RAPID_VERTICAL)               ; 6
         (RAPID_HORIZONTAL)             ; 7
         (INSTANT_VERTICAL)             ; 8
         )
        (PanoseWeight
         (ANYTHING)                     ; 0
         (NO_FIT)                       ; 1
         (VERY_LIGHT)                   ; 2
         (LIGHT)                        ; 3
         (THIN)                         ; 4
         (BOOK)                         ; 5
         (MEDIUM)                       ; 6
         (DEMI_BOLD)                    ; 7
         (BOLD)                         ; 8
         (HEAVY)                        ; 9
         (BLACK)                        ; 10
         (NORD)                         ; 11
         )
        (PanoseXHeight
         (ANYTHING)                     ; 0
         (NO_FIT)                       ; 1
         (CONSTANT_SMALL)               ; 2
         (CONSTANT_STANDARD)            ; 3
         (CONSTANT_LARGE)               ; 4
         (DUCKING_SMALL)                ; 5
         (DUCKING_STANDARD)             ; 6
         (DUCKING_LARGE)                ; 7
         )
        (PathCapType
         (BUTT)                         ; 0
         (ROUND)                        ; 1
         (SQUARE)                       ; 2
         )
        (PathJoinType
         (NONE)                         ; 0
         (MITER)                        ; 1
         (ROUND)                        ; 2
         (BEVEL)                        ; 3
         )
        (RenderingIntent
         (PERCEPTUAL)                   ; 0
         (SATURATION)                   ; 1
         (RELATIVE_COLORIMETRIC)        ; 2
         (ABSOLUTE_COLORIMETRIC)        ; 3
         )
        (RepaintResult
         (REDRAWN)                      ; 1
         (DRAFTED)                      ; 2
         (FAILED)                       ; 3
         )
        (TextDirection
         (WEAK_LEFT_TO_RIGHT)           ; 0
         (STRONG_LEFT_TO_RIGHT)         ; 2
         (WEAK_RIGHT_TO_LEFT)           ; 1
         (STRONG_RIGHT_TO_LEFT)         ; 3
         )
        (TexturingMode
         (CLAMP)                        ; 0
         (REPEAT)                       ; 1
         )
        )
       (report
        (Calculation
         (NONE)                         ; 0
         (AVERAGE)                      ; 1
         (CORRELATION)                  ; 2
         (COUNT)                        ; 3
         (COVARIANCE)                   ; 4
         (DISTINCTCOUNT)                ; 5
         (MAXIMUM)                      ; 6
         (MEDIAN)                       ; 7
         (MINIMUM)                      ; 8
         (MODE)                         ; 9
         (NTHLARGEST)                   ; 10
         (NTHMOSTFREQUENT)              ; 11
         (NTHSMALLEST)                  ; 12
         (PERCENTAGE)                   ; 13
         (PERCENTILE)                   ; 14
         (POPSTANDARDDEVIATION)         ; 15
         (POPVARIANCE)                  ; 16
         (SAMPLESTANDARDDEVIATION)      ; 17
         (SAMPLEVARIANCE)               ; 18
         (SUM)                          ; 19
         (WEIGHTEDAVG)                  ; 20
         )
        (ForceNewPage
         (NONE)                         ; 0
         (BEFORE_SECTION)               ; 1
         (AFTER_SECTION)                ; 2
         (BEFORE_AFTER_SECTION)         ; 3
         )
        (GroupKeepTogether
         (PER_PAGE)                     ; 0
         (PER_COLUMN)                   ; 1
         )
        (GroupOn
         (DEFAULT)                      ; 0
         (PREFIX_CHARACTERS)            ; 1
         (YEAR)                         ; 2
         (QUARTAL)                      ; 3
         (MONTH)                        ; 4
         (WEEK)                         ; 5
         (DAY)                          ; 6
         (HOUR)                         ; 7
         (MINUTE)                       ; 8
         (INTERVAL)                     ; 9
         )
        (KeepTogether
         (NO)                           ; 0
         (WHOLE_GROUP)                  ; 1
         (WITH_FIRST_DETAIL)            ; 2
         )
        (ReportPrintOption
         (ALL_PAGES)                     ; 0
         (NOT_WITH_REPORT_HEADER)        ; 1
         (NOT_WITH_REPORT_FOOTER)        ; 2
         (NOT_WITH_REPORT_HEADER_FOOTER) ; 3
         )
        (SectionPageBreak
         (NONE)                         ; 0
         (SECTION)                      ; 1
         (AUTO)                         ; 2
         )
        )
       (script
        (browse
         (BrowseNodeFactoryViewTypes
          (MACROSELECTOR)               ; 0
          (MACROORGANIZER)              ; 1
          )
         (BrowseNodeTypes
          (SCRIPT)                      ; 0
          (CONTAINER)                   ; 1
          (ROOT)                        ; 2
          )
         )
        (provider
         (ScriptFrameworkErrorType
          (UNKNOWN)                     ; 0
          (NOTSUPPORTED)                ; 1
          )
         )
        )
       (sdb
        (application
         (CopyTableContinuation
          (Proceed)                     ; 0
          (CallNextHandler)             ; 1
          (Cancel)                      ; 2
          (AskUser)                     ; 3
          )
         (CopyTableOperation
          (CopyDefinitionAndData)       ; 0
          (CopyDefinitionOnly)          ; 1
          (CreateAsView)                ; 2
          (AppendData)                  ; 3
          )
         (DatabaseObject
          (TABLE)                      ; com::sun::star::sdb::CommandType::TABLE
          (QUERY)                      ; com::sun::star::sdb::CommandType::QUERY
          (FORM)                       ; 2
          (REPORT)                     ; 3
          )
         (DatabaseObjectContainer
          (TABLES)                      ; 1000
          (QUERIES)                     ; 1001
          (FORMS)                       ; 1002
          (REPORTS)                     ; 1003
          (DATA_SOURCE)                 ; 1004
          (CATALOG)                     ; 1005
          (SCHEMA)                      ; 1006
          (FORMS_FOLDER)                ; 1007
          (REPORTS_FOLDER)              ; 1008
          )
         )
        (tools
         (CompositionType
          (ForTableDefinitions)         ; 0
          (ForIndexDefinitions)         ; 1
          (ForDataManipulation)         ; 2
          (ForProcedureCalls)           ; 3
          (ForPrivilegeDefinitions)     ; 4
          (Complete)                    ; 5
          )
         )
        (BooleanComparisonMode
         (EQUAL_INTEGER)                ; 0
         (IS_LITERAL)                   ; 1
         (EQUAL_LITERAL)                ; 2
         (ACCESS_COMPAT)                ; 3
         )
        (CommandType
         (TABLE)                        ; 0
         (QUERY)                        ; 1
         (COMMAND)                      ; 2
         )
        (ErrorCondition
         (ROW_SET_OPERATION_VETOED)     ; 100
         (PARSER_CYCLIC_SUB_QUERIES)    ; 200
         (DB_OBJECT_NAME_WITH_SLASHES)  ; 300
         (DB_INVALID_SQL_NAME)          ; 301
         (DB_QUERY_NAME_WITH_QUOTES)    ; 302
         (DB_OBJECT_NAME_IS_USED)       ; 303
         (DB_NOT_CONNECTED)             ; 304
         )
        (RowChangeAction
         (INSERT)                       ; 1
         (UPDATE)                       ; 2
         (DELETE)                       ; 3
         )
        (SQLFilterOperator
         (EQUAL)                        ; 1
         (NOT_EQUAL)                    ; 2
         (LESS)                         ; 3
         (GREATER)                      ; 4
         (LESS_EQUAL)                   ; 5
         (GREATER_EQUAL)                ; 6
         (LIKE)                         ; 7
         (NOT_LIKE)                     ; 8
         (SQLNULL)                      ; 9
         (NOT_SQLNULL)                  ; 10
         )
        )
       (sdbc
        (BestRowScope
         (TEMPORARY)                    ; 0
         (TRANSACTION)                  ; 1
         (SESSION)                      ; 2
         )
        (BestRowType
         (UNKNOWN)                      ; 0
         (NOT_PSEUDO)                   ; 1
         (PSEUDO)                       ; 2
         )
        (ColumnSearch
         (NONE)                         ; 0
         (CHAR)                         ; 1
         (BASIC)                        ; 2
         (FULL)                         ; 3
         )
        (ColumnType
         (UNKNOWN)                      ; 0
         (NOT_PSEUDO)                   ; 1
         (PSEUDO)                       ; 2
         )
        (ColumnValue
         (NO_NULLS)                     ; 0
         (NULLABLE)                     ; 1
         (NULLABLE_UNKNOWN)             ; 2
         )
        (DataType
         (BIT)                          ; -7
         (TINYINT)                      ; -6
         (SMALLINT)                     ; 5
         (INTEGER)                      ; 4
         (BIGINT)                       ; -5
         (FLOAT)                        ; 6
         (REAL)                         ; 7
         (DOUBLE)                       ; 8
         (NUMERIC)                      ; 2
         (DECIMAL)                      ; 3
         (CHAR)                         ; 1
         (VARCHAR)                      ; 12
         (LONGVARCHAR)                  ; -1
         (DATE)                         ; 91
         (TIME)                         ; 92
         (TIMESTAMP)                    ; 93
         (BINARY)                       ; -2
         (VARBINARY)                    ; -3
         (LONGVARBINARY)                ; -4
         (SQLNULL)                      ; 0
         (OTHER)                        ; 1111
         (OBJECT)                       ; 2000
         (DISTINCT)                     ; 2001
         (STRUCT)                       ; 2002
         (ARRAY)                        ; 2003
         (BLOB)                         ; 2004
         (CLOB)                         ; 2005
         (REF)                          ; 2006
         (BOOLEAN)                      ; 16
         )
        (Deferrability
          (INITIALLY_DEFERRED)          ; 5
          (INITIALLY_IMMEDIATE)         ; 6
          (NONE)                        ; 7
          )
        (FetchDirection
         (FORWARD)                      ; 1000
         (REVERSE)                      ; 1001
         (UNKNOWN)                      ; 1002
         )
        (IndexType
         (STATISTIC)                    ; 0
         (CLUSTERED)                    ; 1
         (HASHED)                       ; 2
         (OTHER)                        ; 3
         )
        (KeyRule
         (CASCADE)                      ; 0
         (RESTRICT)                     ; 1
         (SET_NULL)                     ; 2
         (NO_ACTION)                    ; 3
         (SET_DEFAULT)                  ; 4
         )
        (ProcedureColumn
         (UNKNOWN)                      ; 0
         (IN)                           ; 1
         (INOUT)                        ; 2
         (RESULT)                       ; 3
         (OUT)                          ; 4
         (RETURN)                       ; 5
         )
        (ProcedureResult
         (UNKNOWN)                      ; 0
         (NONE)                         ; 1
         (RETURN)                       ; 2
         )
        (ResultSetConcurrency
         (READ_ONLY)                    ; 1007
         (UPDATABLE)                    ; 1008
         )
        (ResultSetType
         (FORWARD_ONLY)                 ; 1003
         (SCROLL_INSENSITIVE)           ; 1004
         (SCROLL_SENSITIVE)             ; 1005
         )
        (TransactionIsolation
         (NONE)                         ; 0
         (READ_UNCOMMITTED)             ; 1
         (READ_COMMITTED)               ; 2
         (REPEATABLE_READ)              ; 4
         (SERIALIZABLE)                 ; 8
         )
        )
       (sdbcx
        (CheckOption
         (NONE)                         ; 0
         (CASCADE)                      ; 2
         (LOCAL)                        ; 3
         )
        (CompareBookmark
         (LESS)                         ; -1
         (EQUAL)                        ; 0
         (GREATER)                      ; 1
         (NOT_EQUAL)                    ; 2
         (NOT_COMPARABLE)               ; 3
         )
        (KeyType
         (PRIMARY)                      ; 1
         (UNIQUE)                       ; 2
         (FOREIGN)                      ; 3
         )
        (Privilege
         (SELECT)                       ; 0x00000001
         (INSERT)                       ; 0x00000002
         (UPDATE)                       ; 0x00000004
         (DELETE)                       ; 0x00000008
         (READ)                         ; 0x00000010
         (CREATE)                       ; 0x00000020
         (ALTER)                        ; 0x00000040
         (REFERENCE)                    ; 0x00000080
         (DROP)                         ; 0x00000100
         )
        (PrivilegeObject
         (TABLE)                        ; 0
         (VIEW)                         ; 1
         (COLUMN)                       ; 2
         )
        )
       (security
        (CertificateCharacters
         (SELF_SIGNED)                  ; 0x00000001
         (HAS_PRIVATE_KEY)              ; 0x00000004
         )
        (CertificateValidity
         (VALID)                        ; 0x00000000
         (INVALID)                      ; 0x00000001
         (UNTRUSTED)                    ; 0x00000002
         (TIME_INVALID)                 ; 0x00000004
         (NOT_TIME_NESTED)              ; 0x00000008
         (REVOKED)                      ; 0x00000010
         (UNKNOWN_REVOKATION)           ; 0x00000020
         (SIGNATURE_INVALID)            ; 0x00000040
         (EXTENSION_INVALID)            ; 0x00000080
         (EXTENSION_UNKNOWN)            ; 0x00000100
         (ISSUER_UNKNOWN)               ; 0x00000200
         (ISSUER_UNTRUSTED)             ; 0x00000400
         (ISSUER_INVALID)               ; 0x00001000
         (ROOT_UNKNOWN)                 ; 0x00002000
         (ROOT_UNTRUSTED)               ; 0x00004000
         (ROOT_INVALID)                 ; 0x00010000
         (CHAIN_INCOMPLETE)             ; 0x00020000
         )
        (KeyUsage
         (DIGITAL_SIGNATURE)            ; 0x80
         (NON_REPUDIATION)              ; 0x40
         (KEY_ENCIPHERMENT)             ; 0x20
         (DATA_ENCIPHERMENT)            ; 0x10
         (KEY_AGREEMENT)                ; 0x08
         (KEY_CERT_SIGN)                ; 0x04
         (CRL_SIGN)                     ; 0x02
         )
        )
       (sheet
        (AddressConvention
         (UNSPECIFIED)                  ; -1
         (OOO)                          ; 0
         (XL_A1)                        ; 1
         (XL_R1C1)                      ; 2
         (XL_OOX)                       ; 3
         (LOTUS_A1)                     ; 4
         )
        (CellFlags
         (VALUE)                        ; 1
         (DATETIME)                     ; 2
         (STRING)                       ; 4
         (ANNOTATION)                   ; 8
         (FORMULA)                      ; 16
         (HARDATTR)                     ; 32
         (STYLES)                       ; 64
         (OBJECTS)                      ; 128
         (EDITATTR)                     ; 256
         (FORMATTED)                    ; 512
         )
        (DataPilotFieldGroupBy
         (SECONDS)                      ; 1
         (MINUTES)                      ; 2
         (HOURS)                        ; 4
         (DAYS)                         ; 8
         (MONTHS)                       ; 16
         (QUARTERS)                     ; 32
         (YEARS)                        ; 64
         )
        (DataPilotFieldLayoutMode
         (TABULAR_LAYOUT)               ; 0
         (OUTLINE_SUBTOTALS_TOP)        ; 1
         (OUTLINE_SUBTOTALS_BOTTOM)     ; 2
         )
        (DataPilotFieldReferenceItemType
         (NAMED)                        ; 0
         (PREVIOUS)                     ; 1
         (NEXT)                         ; 2
         )
        (DataPilotFieldReferenceType
         (NONE)                         ; 0
         (ITEM_DIFFERENCE)              ; 1
         (ITEM_PERCENTAGE)              ; 2
         (ITEM_PERCENTAGE_DIFFERENCE)   ; 3
         (RUNNING_TOTAL)                ; 4
         (ROW_PERCENTAGE)               ; 5
         (COLUMN_PERCENTAGE)            ; 6
         (TOTAL_PERCENTAGE)             ; 7
         (INDEX)                        ; 8
         )
        (DataPilotFieldShowItemsMode
         (FROM_TOP)                     ; 0
         (FROM_BOTTOM)                  ; 1
         )
        (DataPilotFieldSortMode
         (NONE)                         ; 0
         (MANUAL)                       ; 1
         (NAME)                         ; 2
         (DATA)                         ; 3
         )
        (DataPilotOutputRangeType
         (WHOLE)                        ; 0
         (TABLE)                        ; 1
         (RESULT)                       ; 2
         )
        (DataPilotTablePositionType
         (NOT_IN_TABLE)                 ; 0
         (RESULT)                       ; 1
         (ROW_HEADER)                   ; 2
         (COLUMN_HEADER)                ; 3
         (OTHER)                        ; 4
         )
        (DataResultFlags
         (HASDATA)                      ; 1
         (SUBTOTAL)                     ; 2
         (ERROR)                        ; 4
         )
        (FormulaLanguage
         (ODFF)                         ; 0
         (ODF_11)                       ; 1
         (ENGLISH)                      ; 2
         (NATIVE)                       ; 3
         )
        (FormulaMapGroup
         (SPECIAL)                      ; 0
         (SEPARATORS)                   ; 0x00000001
         (ARRAY_SEPARATORS)             ; 0x00000002
         (UNARY_OPERATORS)              ; 0x00000004
         (BINARY_OPERATORS)             ; 0x00000008
         (FUNCTIONS)                    ; 0x00000010
         (ALL_EXCEPT_SPECIAL)           ; 0x7fffffff
         )
        (FormulaMapGroupSpecialOffset
         (PUSH)                         ; 0
         (CALL)                         ; 1
         (STOP)                         ; 2
         (EXTERNAL)                     ; 3
         (NAME)                         ; 4
         (NO_NAME)                      ; 5
         (MISSING)                      ; 6
         (BAD)                          ; 7
         (SPACES)                       ; 8
         (MAT_REF)                      ; 9
         (DB_AREA)                      ; 10
         (MACRO)                        ; 11
         (COL_ROW_NAME)                 ; 12
         (COL_ROW_NAME_AUTO)            ; 13
         )
        (FormulaResult
         (VALUE)                        ; 1
         (STRING)                       ; 2
         (ERROR)                        ; 4
         )
        (FunctionCategory
         (DATABASE)                     ; 1
         (DATETIME)                     ; 2
         (FINANCIAL)                    ; 3
         (INFORMATION)                  ; 4
         (LOGICAL)                      ; 5
         (MATHEMATICAL)                 ; 6
         (MATRIX)                       ; 7
         (STATISTICAL)                  ; 8
         (SPREADSHEET)                  ; 9
         (TEXT)                         ; 10
         (ADDIN)                        ; 11
         )
        (MemberResultFlags
         (HASMEMBER)                    ; 1
         (SUBTOTAL)                     ; 2
         (CONTINUE)                     ; 4
         )
        (MoveDirection
         (DOWN)                         ; 0
         (RIGHT)                        ; 1
         (UP)                           ; 2
         (LEFT)                         ; 3
         )
        (NamedRangeFlag
         (FILTER_CRITERIA)              ; 1
         (PRINT_AREA)                   ; 2
         (COLUMN_HEADER)                ; 4
         (ROW_HEADER)                   ; 8
         )
        (ReferenceFlags
         (COLUMN_RELATIVE)              ; 1
         (COLUMN_DELETED)               ; 2
         (ROW_RELATIVE)                 ; 4
         (ROW_DELETED)                  ; 8
         (SHEET_RELATIVE)               ; 16
         (SHEET_DELETED)                ; 32
         (SHEET_3D)                     ; 64
         (RELATIVE_NAME)                ; 128
         )
        (StatusBarFunction
         (NONE)                         ; 0
         (AVERAGE)                      ; 1
         (COUNTNUMS)                    ; 2
         (COUNT)                        ; 3
         (MAX)                          ; 4
         (MIN)                          ; 5
         (SUM)                          ; 9
         )
        (TableValidationVisibility
         (INVISIBLE)                    ; 0
         (UNSORTED)                     ; 1
         (SORTEDASCENDING)              ; 2
         )
        )
       (style
        (CaseMap
         (NONE)                         ; 0
         (UPPERCASE)                    ; 1
         (LOWERCASE)                    ; 2
         (TITLE)                        ; 3
         (SMALLCAPS)                    ; 4
         )
        (LineNumberPosition
         (LEFT)                         ; 0
         (RIGHT)                        ; 1
         (INSIDE)                       ; 2
         (OUTSIDE)                      ; 3
         )
        (LineSpacingMode
         (PROP)                         ; 0
         (MINIMUM)                      ; 1
         (LEADING)                      ; 2
         (FIX)                          ; 3
         )
        (NumberingType
         (CHARS_UPPER_LETTER)               ; 0
         (CHARS_LOWER_LETTER)               ; 1
         (ROMAN_UPPER)                      ; 2
         (ROMAN_LOWER)                      ; 3
         (ARABIC)                           ; 4
         (NUMBER_NONE)                      ; 5
         (CHAR_SPECIAL)                     ; 6
         (PAGE_DESCRIPTOR)                  ; 7
         (BITMAP)                           ; 8
         (CHARS_UPPER_LETTER_N)             ; 9
         (CHARS_LOWER_LETTER_N)             ; 10
         (TRANSLITERATION)                  ; 11
         (NATIVE_NUMBERING)                 ; 12
         (FULLWIDTH_ARABIC)                 ; 13
         (CIRCLE_NUMBER)                    ; 14
         (NUMBER_LOWER_ZH)                  ; 15
         (NUMBER_UPPER_ZH)                  ; 16
         (NUMBER_UPPER_ZH_TW)               ; 17
         (TIAN_GAN_ZH)                      ; 18
         (DI_ZI_ZH)                         ; 19
         (NUMBER_TRADITIONAL_JA)            ; 20
         (AIU_FULLWIDTH_JA)                 ; 21
         (AIU_HALFWIDTH_JA)                 ; 22
         (IROHA_FULLWIDTH_JA)               ; 23
         (IROHA_HALFWIDTH_JA)               ; 24
         (NUMBER_UPPER_KO)                  ; 25
         (NUMBER_HANGUL_KO)                 ; 26
         (HANGUL_JAMO_KO)                   ; 27
         (HANGUL_SYLLABLE_KO)               ; 28
         (HANGUL_CIRCLED_JAMO_KO)           ; 29
         (HANGUL_CIRCLED_SYLLABLE_KO)       ; 30
         (CHARS_ARABIC)                     ; 31
         (CHARS_THAI)                       ; 32
         (CHARS_HEBREW)                     ; 33
         (CHARS_NEPALI)                     ; 34
         (CHARS_KHMER)                      ; 35
         (CHARS_LAO)                        ; 36
         (CHARS_TIBETAN)                    ; 37
         (CHARS_CYRILLIC_UPPER_LETTER_BG)   ; 38
         (CHARS_CYRILLIC_LOWER_LETTER_BG)   ; 39
         (CHARS_CYRILLIC_UPPER_LETTER_N_BG) ; 40
         (CHARS_CYRILLIC_LOWER_LETTER_N_BG) ; 41
         (CHARS_CYRILLIC_UPPER_LETTER_RU)   ; 42
         (CHARS_CYRILLIC_LOWER_LETTER_RU)   ; 43
         (CHARS_CYRILLIC_UPPER_LETTER_N_RU) ; 44
         (CHARS_CYRILLIC_LOWER_LETTER_N_RU) ; 45
         (CHARS_PERSIAN)                    ; 46
         )
        (ParagraphStyleCategory
         (TEXT)                         ; 0
         (CHAPTER)                      ; 1
         (LIST)                         ; 2
         (INDEX)                        ; 3
         (EXTRA)                        ; 4
         (HTML)                         ; 5
         )
        )
       (system
        (SimpleMailClientFlags
         (DEFAULTS)                     ; 0
         (NO_USER_INTERFACE)            ; 1
         (NO_LOGON_DIALOG)              ; 2
         )
        (SystemShellExecuteFlags
         (DEFAULTS)                     ; 0
         (NO_SYSTEM_ERROR_MESSAGE)      ; 1
         )
        )
       (text
        (AuthorDisplayFormat
         (FULL)                         ; 0
         (LAST_NAME)                    ; 1
         (FIRST_NAME)                   ; 2
         (INITIALS)                     ; 3
         )
        (BibliographyDataField
         (IDENTIFIER)                   ; 0
         (BIBILIOGRAPHIC_TYPE)          ; 1
         (ADDRESS)                      ; 2
         (ANNOTE)                       ; 3
         (AUTHOR)                       ; 4
         (BOOKTITLE)                    ; 5
         (CHAPTER)                      ; 6
         (EDITION)                      ; 7
         (EDITOR)                       ; 8
         (HOWPUBLISHED)                 ; 9
         (INSTITUTION)                  ; 10
         (JOURNAL)                      ; 11
         (MONTH)                        ; 12
         (NOTE)                         ; 13
         (NUMBER)                       ; 14
         (ORGANIZATIONS)                ; 15
         (PAGES)                        ; 16
         (PUBLISHER)                    ; 17
         (SCHOOL)                       ; 18
         (SERIES)                       ; 19
         (TITLE)                        ; 20
         (REPORT_TYPE)                  ; 21
         (VOLUME)                       ; 22
         (YEAR)                         ; 23
         (URL)                          ; 24
         (CUSTOM1)                      ; 25
         (CUSTOM2)                      ; 26
         (CUSTOM3)                      ; 27
         (CUSTOM4)                      ; 28
         (CUSTOM5)                      ; 29
         (ISBN)                         ; 30
         )
        (BibliographyDataType
         (ARTICLE)                      ; 0
         (BOOK)                         ; 1
         (BOOKLET)                      ; 2
         (CONFERENCE)                   ; 3
         (INBOOK)                       ; 4
         (INCOLLECTION)                 ; 5
         (INPROCEEDINGS)                ; 6
         (JOURNAL)                      ; 7
         (MANUAL)                       ; 8
         (MASTERSTHESIS)                ; 9
         (MISC)                         ; 10
         (PHDTHESIS)                    ; 11
         (PROCEEDINGS)                  ; 12
         (TECHREPORT)                   ; 13
         (UNPUBLISHED)                  ; 14
         (EMAIL)                        ; 15
         (WWW)                          ; 16
         (CUSTOM1)                      ; 17
         (CUSTOM2)                      ; 18
         (CUSTOM3)                      ; 19
         (CUSTOM4)                      ; 20
         (CUSTOM5)                      ; 21
         )
        (ChapterFormat
         (NAME)                         ; 0
         (NUMBER)                       ; 1
         (NAME_NUMBER)                  ; 2
         (NO_PREFIX_SUFFIX)             ; 3
         (DIGIT)                        ; 4
         )
        (CharacterCompressionType
         (NONE)                         ; 0
         (PUNCTUATION_ONLY)             ; 1
         (PUNCTUATION_AND_KANA)         ; 2
         )
        (ControlCharacter
         (PARAGRAPH_BREAK)              ; 0
         (LINE_BREAK)                   ; 1
         (HARD_HYPHEN)                  ; 2
         (SOFT_HYPHEN)                  ; 3
         (HARD_SPACE)                   ; 4
         (APPEND_PARAGRAPH)             ; 5
         )
        (DocumentStatistic
         (PAGES)                        ; 0
         (PARAS)                        ; 1
         (WORDS)                        ; 2
         (CHARS)                        ; 3
         )
        (FilenameDisplayFormat
         (FULL)                         ; 0
         (PATH)                         ; 1
         (NAME)                         ; 2
         (NAME_AND_EXT)                 ; 3
         )
        (FontEmphasis
         (NONE)                         ; 0
         (DOT_ABOVE)                    ; 1
         (CIRCLE_ABOVE)                 ; 2
         (DISK_ABOVE)                   ; 3
         (ACCENT_ABOVE)                 ; 4
         (DOT_BELOW)                    ; 11
         (CIRCLE_BELOW)                 ; 12
         (DISK_BELOW)                   ; 13
         (ACCENT_BELOW)                 ; 14
         )
        (FontRelief
         (NONE)                         ; 0
         (EMBOSSED)                     ; 1
         (ENGRAVED)                     ; 2
         )
        (FootnoteNumbering
         (PER_PAGE)                     ; 0
         (PER_CHAPTER)                  ; 1
         (PER_DOCUMENT)                 ; 2
         )
        (HoriOrientation
         (NONE)                         ; 0
         (RIGHT)                        ; 1
         (CENTER)                       ; 2
         (LEFT)                         ; 3
         (INSIDE)                       ; 4
         (OUTSIDE)                      ; 5
         (FULL)                         ; 6
         (LEFT_AND_WIDTH)               ; 7
         )
        (LabelFollow
         (LISTTAB)                      ; 0
         (SPACE)                        ; 1
         (NOTHING)                      ; 2
         )
        (MailMergeType
         (PRINTER)                      ; 1
         (FILE)                         ; 2
         (MAIL)                         ; 3
         )
        (ParagraphVertAlign
         (AUTOMATIC)                    ; 0
         (BASELINE)                     ; 1
         (TOP)                          ; 2
         (CENTER)                       ; 3
         (BOTTOM)                       ; 4
         )
        (PlaceholderType
         (TEXT)                         ; 0
         (TABLE)                        ; 1
         (TEXTFRAME)                    ; 2
         (GRAPHIC)                      ; 3
         (OBJECT)                       ; 4
         )
        (PositionAndSpaceMode
         (LABEL_WIDTH_AND_POSITION)     ; 0
         (LABEL_ALIGNMENT)              ; 1
         )
        (PositionLayoutDir
         (PositionInHoriL2R)            ; 1
         (PositionInLayoutDirOfAnchor)  ; 2
         )
        (ReferenceFieldPart
         (PAGE)                         ; 0
         (CHAPTER)                      ; 1
         (TEXT)                         ; 2
         (UP_DOWN)                      ; 3
         (PAGE_DESC)                    ; 4
         (CATEGORY_AND_NUMBER)          ; 5
         (ONLY_CAPTION)                 ; 6
         (ONLY_SEQUENCE_NUMBER)         ; 7
         (NUMBER)                       ; 8
         (NUMBER_NO_CONTEXT)            ; 9
         (NUMBER_FULL_CONTEXT)          ; 10
         )
        (ReferenceFieldSource
         (REFERENCE_MARK)               ; 0
         (SEQUENCE_FIELD)               ; 1
         (BOOKMARK)                     ; 2
         (FOOTNOTE)                     ; 3
         (ENDNOTE)                      ; 4
         )
        (RelOrientation
         (FRAME)                        ; 0
         (PRINT_AREA)                   ; 1
         (CHAR)                         ; 2
         (PAGE_LEFT)                    ; 3
         (PAGE_RIGHT)                   ; 4
         (FRAME_LEFT)                   ; 5
         (FRAME_RIGHT)                  ; 6
         (PAGE_FRAME)                   ; 7
         (PAGE_PRINT_AREA)              ; 8
         (TEXT_LINE)                    ; 9
         )
        (SetVariableType
         (VAR)                          ; 0
         (SEQUENCE)                     ; 1
         (FORMULA)                      ; 2
         (STRING)                       ; 3
         )
        (SizeType
         (VARIABLE)                     ; 0
         (FIX)                          ; 1
         (MIN)                          ; 2
         )
        (TemplateDisplayFormat
         (FULL)                         ; 0
         (PATH)                         ; 1
         (NAME)                         ; 2
         (NAME_AND_EXT)                 ; 3
         (AREA)                         ; 4
         (TITLE)                        ; 5
         )
        (TextGridMode
         (NONE)                         ; 0
         (LINES)                        ; 1
         (LINES_AND_CHARS)              ; 2
         )
        (TextMarkupType
         (SPELLCHECK)                   ; 1
         (GRAMMAR)                      ; 2
         (SMARTTAG)                     ; 3
         (SENTENCE)                     ; 4
         )
        (UserDataPart
         (COMPANY)                      ; 0
         (FIRSTNAME)                    ; 1
         (NAME)                         ; 2
         (SHORTCUT)                     ; 3
         (STREET)                       ; 4
         (COUNTRY)                      ; 5
         (ZIP)                          ; 6
         (CITY)                         ; 7
         (TITLE)                        ; 8
         (POSITION)                     ; 9
         (PHONE_PRIVATE)                ; 10
         (PHONE_COMPANY)                ; 11
         (FAX)                          ; 12
         (EMAIL)                        ; 13
         (STATE)                        ; 14
         )
        (UserFieldFormat
         (SYSTEM)                       ; 0
         (TEXT)                         ; 1
         (NUM)                          ; 2
         )
        (VertOrientation
         (NONE)                         ; 0
         (TOP)                          ; 1
         (CENTER)                       ; 2
         (BOTTOM)                       ; 3
         (CHAR_TOP)                     ; 4
         (CHAR_CENTER)                  ; 5
         (CHAR_BOTTOM)                  ; 6
         (LINE_TOP)                     ; 7
         (LINE_CENTER)                  ; 8
         (LINE_BOTTOM)                  ; 9
         )
        (WrapInfluenceOnPosition
         (ONCE_SUCCESSIVE)              ; 1
         (ONCE_CONCURRENT)              ; 2
         (ITERATIVE)                    ; 3
         )
        (WritingMode2
         (LR_TB)                        ; 0
         (RL_TB)                        ; 1
         (TB_RL)                        ; 2
         (TB_LR)                        ; 3
         (PAGE)                         ; 4
         )
        )
       (ucb
        (CommandInfoChange
         (COMMAND_INSERTED)             ; 0
         (COMMAND_REMOVED)              ; 1
         )
        (ConnectionMode
         (ONLINE)                       ; 0
         (OFFLINE)                      ; 1
         )
        (ContentAction
         (INSERTED)                     ; 0
         (REMOVED)                      ; 1
         (DELETED)                      ; 2
         (EXCHANGED)                    ; 4
         (SEARCH_MATCHED)               ; 128
         )
        (ContentInfoAttribute
         (NONE)                         ; 0
         (INSERT_WITH_INPUTSTREAM)      ; 1
         (KIND_DOCUMENT)                ; 2
         (KIND_FOLDER)                  ; 4
         (KIND_LINK)                    ; 8
         )
        (ContentResultSetCapability
         (SORTED)                       ; 1
         )
        (FetchError
         (SUCCESS)                      ; 0
         (ENDOFDATA)                    ; 1
         (EXCEPTION)                    ; 2
         )
        (FileSystemNotation
         (UNKNOWN_NOTATION)             ; 0
         (UNIX_NOTATION)                ; 1
         (DOS_NOTATION)                 ; 2
         (MAC_NOTATION)                 ; 3
         )
        (ListActionType
         (WELCOME)                      ; 20
         (INSERTED)                     ; 21
         (REMOVED)                      ; 22
         (CLEARED)                      ; 23
         (MOVED)                        ; 24
         (PROPERTIES_CHANGED)           ; 25
         (EXCHANGED)                    ; 26
         (COMPLETED)                    ; 27
         )
        (NameClash
         (ERROR)                        ; 0
         (OVERWRITE)                    ; 1
         (RENAME)                       ; 2
         (KEEP)                         ; 3
         (ASK)                          ; 4
         )
        (OpenMode
         (ALL)                          ; 0
         (FOLDERS)                      ; 1
         (DOCUMENTS)                    ; 3
         (DOCUMENT)                     ; 2
         (DOCUMENT_SHARE_DENY_NONE)     ; 4
         (DOCUMENT_SHARE_DENY_WRITE)    ; 5
         ;; (DOCUMENT_SHARE_DENY_READ)  ; 6
         )
        (RuleAction
         (NONE)                         ; 0
         (SHOW)                         ; 1
         (HIDE)                         ; 2
         (MARK)                         ; 3
         (UNMARK)                       ; 4
         (MARKREAD)                     ; 5
         (MARKUNREAD)                   ; 6
         (MOVE)                         ; 7
         (COPY)                         ; 8
         (DELETE)                       ; 9
         (LINK)                         ; 10
         (FORWARD)                      ; 11
         )
        (RuleOperator
         (CONTAINS)                     ; 1
         (CONTAINSNOT)                  ; 2
         (GREATEREQUAL)                 ; 3
         (LESSEQUAL)                    ; 4
         (EQUAL)                        ; 5
         (NOTEQUAL)                     ; 6
         (VALUE_TRUE)                   ; 7
         (VALUE_FALSE)                  ; 8
         )
        )
       (ui
        (dialogs
         (CommonFilePickerElementIds
          (PUSHBUTTON_OK)               ; 1
          (PUSHBUTTON_CANCEL)           ; 2
          (LISTBOX_FILTER)              ; 3
          (CONTROL_FILEVIEW)            ; 4
          (EDIT_FILEURL)                ; 5
          (LISTBOX_FILTER_LABEL)        ; 6
          (EDIT_FILEURL_LABEL)          ; 7
          )
         (ControlActions
          (ADD_ITEM)                    ; 1
          (ADD_ITEMS)                   ; 2
          (DELETE_ITEM)                 ; 3
          (DELETE_ITEMS)                ; 4
          (SET_SELECT_ITEM)             ; 5
          (GET_ITEMS)                   ; 6
          (GET_SELECTED_ITEM)           ; 7
          (GET_SELECTED_ITEM_INDEX)     ; 8
          (SET_HELP_URL)                ; 100
          (GET_HELP_URL)                ; 101
          )
         (ExecutableDialogResults
          (CANCEL)                      ; 0
          (OK)                          ; 1
          )
         (ExtendedFilePickerElementIds
          (CHECKBOX_AUTOEXTENSION)       ; 100
          (CHECKBOX_PASSWORD)            ; 101
          (CHECKBOX_FILTEROPTIONS)       ; 102
          (CHECKBOX_READONLY)            ; 103
          (CHECKBOX_LINK)                ; 104
          (CHECKBOX_PREVIEW)             ; 105
          (PUSHBUTTON_PLAY)              ; 106
          (LISTBOX_VERSION)              ; 107
          (LISTBOX_TEMPLATE)             ; 108
          (LISTBOX_IMAGE_TEMPLATE)       ; 109
          (CHECKBOX_SELECTION)           ; 110
          (LISTBOX_VERSION_LABEL)        ; 207
          (LISTBOX_TEMPLATE_LABEL)       ; 208
          (LISTBOX_IMAGE_TEMPLATE_LABEL) ; 209
          (LISTBOX_FILTER_SELECTOR)      ; 210
          )
         (FilePreviewImageFormats
          (BITMAP)                      ; 1
          )
         (ListboxControlActions
          (ADD_ITEM)                    ; 1
          (ADD_ITEMS)                   ; 2
          (DELETE_ITEM)                 ; 3
          (DELETE_ITEMS)                ; 4
          (SET_SELECT_ITEM)             ; 5
          (GET_ITEMS)                   ; 6
          (GET_SELECTED_ITEM)           ; 7
          )
         (TemplateDescription
          (FILEOPEN_SIMPLE)                               ; 0
          (FILESAVE_SIMPLE)                               ; 1
          (FILESAVE_AUTOEXTENSION_PASSWORD)               ; 2
          (FILESAVE_AUTOEXTENSION_PASSWORD_FILTEROPTIONS) ; 3
          (FILESAVE_AUTOEXTENSION_SELECTION)              ; 4
          (FILESAVE_AUTOEXTENSION_TEMPLATE)               ; 5
          (FILEOPEN_LINK_PREVIEW_IMAGE_TEMPLATE)          ; 6
          (FILEOPEN_PLAY)                                 ; 7
          (FILEOPEN_READONLY_VERSION)                     ; 8
          (FILEOPEN_LINK_PREVIEW)                         ; 9
          (FILESAVE_AUTOEXTENSION)                        ; 10
          )
         )
        (ActionTriggerSeparatorType
         (LINE)                         ; 0
         (SPACE)                        ; 1
         (LINEBREAK)                    ; 2
         )
        (ImageType
         (SIZE_DEFAULT)                 ; 0
         (SIZE_LARGE)                   ; 1
         (COLOR_NORMAL)                 ; 0
         (COLOR_HIGHCONTRAST)           ; 4
         )
        (ItemStyle
         (ALIGN_LEFT)                   ; 1
         (ALIGN_CENTER)                 ; 2
         (ALIGN_RIGHT)                  ; 3
         (DRAW_OUT3D)                   ; 4
         (DRAW_IN3D)                    ; 8
         (DRAW_FLAT)                    ; 12
         (OWNER_DRAW)                   ; 16
         (AUTO_SIZE)                    ; 32
         (RADIO_CHECK)                  ; 64
         (ICON)                         ; 128
         (DROP_DOWN)                    ; 256
         (REPEAT)                       ; 512
         (DROPDOWN_ONLY)                ; 1024
         )
        (ItemType
         (DEFAULT)                      ; 0
         (SEPARATOR_LINE)               ; 1
         (SEPARATOR_SPACE)              ; 2
         (SEPARATOR_LINEBREAK)          ; 3
         )
        (UIElementType
         (UNKNOWN)                      ; 0
         (MENUBAR)                      ; 1
         (POPUPMENU)                    ; 2
         (TOOLBAR)                      ; 3
         (STATUSBAR)                    ; 4
         (FLOATINGWINDOW)               ; 5
         (PROGRESSBAR)                  ; 6
         (COUNT)                        ; 7
         )
        )
       (util
        (Endianness
         (LITTLE)                       ; 0
         (BIG)                          ; 1
         )
        (MeasureUnit
         (MM_100TH)                     ; 0
         (MM_10TH)                      ; 1
         (MM)                           ; 2
         (CM)                           ; 3
         (INCH_1000TH)                  ; 4
         (INCH_100TH)                   ; 5
         (INCH_10TH)                    ; 6
         (INCH)                         ; 7
         (POINT)                        ; 8
         (TWIP)                         ; 9
         (M)                            ; 10
         (KM)                           ; 11
         (PICA)                         ; 12
         (FOOT)                         ; 13
         (MILE)                         ; 14
         (PERCENT)                      ; 15
         (PIXEL)                        ; 16
         )
        (NumberFormat
         (ALL)                          ; 0
         (DEFINED)                      ; 1
         (DATE)                         ; 2
         (TIME)                         ; 4
         (CURRENCY)                     ; 8
         (NUMBER)                       ; 16
         (SCIENTIFIC)                   ; 32
         (FRACTION)                     ; 64
         (PERCENT)                      ; 128
         (TEXT)                         ; 256
         (DATETIME)                     ; 6
         (LOGICAL)                      ; 1024
         (UNDEFINED)                    ; 2048
         )
        (SearchFlags
         (ALL_IGNORE_CASE)              ; 0x00000001
         (NORM_WORD_ONLY)               ; 0x00000010
         (REG_EXTENDED)                 ; 0x00000100
         (REG_NOSUB)                    ; 0x00000200
         (REG_NEWLINE)                  ; 0x00000400
         (REG_NOT_BEGINOFLINE)          ; 0x00000800
         (REG_NOT_ENDOFLINE)            ; 0x00001000
         (LEV_RELAXED)                  ; 0x00010000
         )
        )
       (view
        (DocumentZoomType
         (OPTIMAL)                      ; 0
         (PAGE_WIDTH)                   ; 1
         (ENTIRE_PAGE)                  ; 2
         (BY_VALUE)                     ; 3
         (PAGE_WIDTH_EXACT)             ; 4
         )
        )
       (xml
        (sax
         (FastToken
          (DONTKNOW)                    ; -1
          (NAMESPACE)                   ; 0x00010000
          )
         )
        )
       (xsd
        (DataTypeClass
         (STRING)                       ; 1
         (BOOLEAN)                      ; 2
         (DECIMAL)                      ; 3
         (FLOAT)                        ; 4
         (DOUBLE)                       ; 5
         (DURATION)                     ; 6
         (DATETIME)                     ; 7
         (TIME)                         ; 8
         (DATE)                         ; 9
         (gYearMonth)                   ; 10
         (gYear)                        ; 11
         (gMonthDay)                    ; 12
         (gDay)                         ; 13
         (gMonth)                       ; 14
         (hexBinary)                    ; 15
         (base64Binary)                 ; 16
         (anyURI)                       ; 17
         (QName)                        ; 18
         (NOTATION)                     ; 19
         )
        (WhiteSpaceTreatment
         (Preserve)                     ; 0
         (Replace)                      ; 1
         (Collapse)                     ; 2
         )
        )
       )
      )
     ))
  "Constants in UNO.")

(defun ooo-basic-uno-constants-to-constant-groups (constants)
  "Return the constant groups in UNO from constants."
  (filter-map
   #'(lambda (y)
       (and
        (or (cdr y)
            (not (symbolp (car y))))
        (cons (car y)
              (ooo-basic-uno-constants-to-constant-groups (cdr y)))))
   constants))

(defvar ooo-basic-uno-constant-groups
  (ooo-basic-uno-constants-to-constant-groups
   ooo-basic-uno-constants)
  "Constant Groups in UNO.")

(defvar ooo-basic-idl-reference-url-base
  "http://api.openoffice.org/docs/common/ref/"
  "The base URL for the IDL reference.")

(defun ooo-basic-uno-name-to-list (name)
  "Return the list of symbols obtained by splitting the argument at '.'."
  (and (< 0 (length name))
       (mapcar 'intern (split-string name "\\."))))

(defun ooo-basic-possible-sequences (forest)
  (append-map
   #'(lambda (tree)
       (let ((head (car tree))
             (tail (cdr tree)))
         (if tail
             (mapcar
              #'(lambda (rest)
                  (concat (symbol-name head) "." rest))
              (ooo-basic-possible-sequences (cdr tree)))
           (list (symbol-name head)))))
   forest))

(defun ooo-basic-traverse (seq forest)
  (and forest
       (let ((head (car seq))
             (tail (cdr seq)))
         (if tail
             (ooo-basic-traverse tail (assoc-default head forest))
           (let ((s (symbol-name head)))
             (append-map
              #'(lambda (tree)
                  (let ((h (car tree)))
                    (if h
                        (and (initial-string-p s (symbol-name h))
                             (ooo-basic-possible-sequences (list tree)))
                      (filter-map
                       #'(lambda (leaf)
                           (let ((str (symbol-name leaf)))
                             (and (initial-string-p s str) str)))
                       (cdr tree)))))
              forest))))))

(defun ooo-basic-uno-completion-function (str forest)
  (let* ((seq (ooo-basic-uno-name-to-list str))
         (seb (mapconcat 'symbol-name (butlast seq) ".")))
    (with-index
     (mapcar
      #'(lambda (x) (if (= 0 (length seb)) x (concat seb "." x)))
      (ooo-basic-traverse seq forest)))))

(defun ooo-basic-uno-name-completion (str)
  (ooo-basic-uno-completion-function
   str
   (append ooo-basic-uno-modules ooo-basic-uno-constants)))

(defun ooo-basic-uno-constant-name-completion (str)
  (ooo-basic-uno-completion-function
   str
   ooo-basic-uno-constants))

(defun ooo-basic-insert-uno-constant ()
  "Insert a constant in UNO."
  (interactive)
  (insert
   (completing-read
    "Constant: "
    (dynamic-completion-table ooo-basic-uno-constant-name-completion))))

(defun ooo-basic-uno-module-name-p (name)
  "Return non-nil if there exists a UNO module which has the given name,
nil otherwise."
  (let ((seq (ooo-basic-uno-name-to-list name)))
    (and seq
         (ooo-basic-traverse seq ooo-basic-uno-modules))))

(defun ooo-basic-uno-name-of-path (name forest)
  (let ((seq (ooo-basic-uno-name-to-list name)))
    (and seq
         (let ((s (ooo-basic-traverse seq forest)))
           (and s
                (null (cdr s))
                (not (string-match "\\." (car s))))))))

(defun ooo-basic-uno-constant-group-name-p (name)
  "Return non-nil if there exists a UNO constant group which has the given name,
nil otherwise."
  (ooo-basic-uno-name-of-path name ooo-basic-uno-constant-groups))

(defun ooo-basic-uno-constant-name-p (name)
  "Return non-nil if there exists a UNO constant which has the given name,
nil otherwise."
  (ooo-basic-uno-name-of-path name ooo-basic-uno-constants))

(defun ooo-basic-uno-name-p (name)
  "Return non-nil if there exists a UNO module, constant group, or constant
which has the given name, nil otherwise."
  (or (ooo-basic-uno-module-name-p name)
      (ooo-basic-uno-constant-group-name-p name)
      (ooo-basic-uno-constant-name-p name)))

(defun ooo-basic-idl-reference-url (name)
  "Return the URL of the IDL reference of a given name."
  (let ((slashed (replace-regexp-in-string "\\." "/" name)))
    (cond ((ooo-basic-uno-module-name-p name)
           (concat ooo-basic-idl-reference-url-base slashed "/module-ix.html"))
          ((ooo-basic-uno-constant-group-name-p name)
           (concat ooo-basic-idl-reference-url-base slashed ".html"))
          ((ooo-basic-uno-constant-name-p name)
           (let ((seq (split-string name "\\.")))
             (apply 'concat
                    ooo-basic-idl-reference-url-base
                    (mapconcat 'identity (butlast seq) "/")
                    ".html#"
                    (last seq)))))))

(defun ooo-basic-browse-idl-reference ()
  "Browse the IDL reference on a given topic."
  (interactive)
  (let* ((word (current-word))
         (default (if (ooo-basic-uno-name-p word) word nil))
         (name (completing-read
                (if default
                    (concat "Name (default " default "): ")
                  "Name: ")
                (dynamic-completion-table ooo-basic-uno-name-completion)
                nil
                t
                nil
                nil
                default))
         (url (ooo-basic-idl-reference-url name)))
    (if url
        (browse-url url)
      (message "name '%s' is not a UNO one." name))))

(defun ooo-basic-beginning-of-defun ()
  "Go back to the beginning of the definition in question."
  (interactive)
  (re-search-backward ooo-basic-definition-start-re nil t))

(defun ooo-basic-end-of-defun ()
  "Go forth to the end of the definition in question."
  (interactive)
  (re-search-forward ooo-basic-definition-end-re nil t))

(defun ooo-basic-previous-line-of-code ()
  "Move backward to reach a code line."
  (forward-line -1)
  (while (and (not (bobp))
              (or (looking-at ooo-basic-blank-re)
                  (looking-at ooo-basic-comment-re)))
    (forward-line -1)))

(defun ooo-basic-find-original-statement ()
  "Find an original line which starts as a logical one."
  (let ((here (point)))
    (ooo-basic-previous-line-of-code)
    (while (and (not (bobp))
                (looking-at ooo-basic-continuation-re))
      (setq here (point))
      (ooo-basic-previous-line-of-code))
    (goto-char here)))

(defun %ooo-basic-find-matching-statement (open-p close-p)
  (let ((level 0))
    (while (and (>= level 0) (not (bobp)))
      (ooo-basic-previous-line-of-code)
      (ooo-basic-find-original-statement)
      (cond ((funcall close-p)
             (setq level (+ level 1)))
            ((funcall open-p)
             (setq level (- level 1)))))))

(defun ooo-basic-find-matching-statement (open-re close-re)
  (%ooo-basic-find-matching-statement
   #'(lambda () (looking-at open-re))
   #'(lambda () (looking-at close-re))))

(defun ooo-basic-start-and-end-p (open-re close-function)
  (and (looking-at open-re)
       (save-excursion
         (beginning-of-line)
         ;; 1st reconstruct complete line
         (let* (complete-line
                (start-point (point))
                (line-beg start-point)
                line-end)
           (while (not line-end)
             (end-of-line)
             (setq line-end (point))
             (if (search-backward "_" line-beg t)
                 (if (looking-at  "_\\s-*$")
                     ;; folded line
                     (progn
                       (push (buffer-substring-no-properties line-beg (1- (point))) complete-line)
                       (setq line-end nil)
                       (beginning-of-line 2)
                       (setq line-beg (point)))
                   ;; _ found, but not a folded line (this is a syntax error)
                   (push (buffer-substring-no-properties line-beg line-end) complete-line))
               ;; not a folded line
               (push (buffer-substring-no-properties line-beg line-end) complete-line)))
           (setq complete-line (mapconcat 'identity (nreverse complete-line) " "))
           ;; now complete line has been reconstructed, drop confusing elements
           (let (p1
                 p2)
             ;; remove any string literal from complete line,
             ;; as strings may disrupt : and ' detection
             (while (and (setq p1 (string-match "\"" complete-line))
                         (setq p2 (string-match "\"" complete-line (1+ p1))))
               (setq complete-line (concat (substring complete-line 0 p1)
                                           (substring complete-line (1+ p2)))))
             ;; now drop tailing comment if any
             (when (setq p1 (string-match "'" complete-line))
               (setq complete-line (substring complete-line 0 p1))))
           (funcall close-function complete-line)))))

(defun ooo-basic-multiline-if-p ()
  "Decide whether an if statement begins but not single line."
  (ooo-basic-start-and-end-p
   ooo-basic-if-re
   #'(lambda (complete-line)
       ;; now drop 1st concatenated instruction is any
       (let ((p1 (string-match ":" complete-line)))
         (when p1
           (setq complete-line (substring complete-line 0 p1))))
       (string-match "Then\\s-*$" complete-line))))

(defun ooo-basic-multiline-do-p ()
  "Decide whether an do statement begins but not single line."
  (ooo-basic-start-and-end-p
   ooo-basic-do-re
   #'(lambda (complete-line)
       (not (string-match "\\<Loop\\>" complete-line)))))

(defun ooo-basic-find-matching-if ()
  "Move backward to find the matching if."
  (%ooo-basic-find-matching-statement
   'ooo-basic-multiline-if-p
   #'(lambda () (looking-at ooo-basic-endif-re))))

(defun ooo-basic-find-matching-select ()
  "Move backward to find the matching select."
  (ooo-basic-find-matching-statement ooo-basic-select-re ooo-basic-end-select-re))

(defun ooo-basic-find-matching-do ()
  "Move backward to find the mathing do."
  (%ooo-basic-find-matching-statement
   'ooo-basic-multiline-do-p
   #'(lambda () (looking-at ooo-basic-loop-re))))

(defun ooo-basic-find-matching-for ()
  "Move backward to find the matching for."
  (ooo-basic-find-matching-statement ooo-basic-for-re ooo-basic-next-re))

(defun ooo-basic-find-matching-while ()
  "Move backward to find the matching while."
  (ooo-basic-find-matching-statement ooo-basic-while-re ooo-basic-wend-re))

(defun ooo-basic-find-matching-with ()
  "Move backward to find the matching with."
  (ooo-basic-find-matching-statement ooo-basic-with-re ooo-basic-end-with-re))

(defun ooo-basic-indentation (current-point parse-status)
  "Return the proper indentation for the current line."
  (save-excursion
    (beginning-of-line)
    (cond ((looking-at ooo-basic-definition-start-re) 0)
          ((looking-at ooo-basic-definition-end-re) 0)
          ((looking-at ooo-basic-label-re)
           ooo-basic-absolute-indent-level-for-label)
          ((or (looking-at ooo-basic-else-re)
               (looking-at ooo-basic-endif-re))
           (ooo-basic-find-matching-if)
           (current-indentation))
          ((looking-at ooo-basic-end-select-re)
           (ooo-basic-find-matching-select)
           (current-indentation))
          ((looking-at ooo-basic-case-re)
           (ooo-basic-find-matching-select)
           (+ (current-indentation) ooo-basic-indent-level))
          ((looking-at ooo-basic-loop-re)
           (ooo-basic-find-matching-do)
           (current-indentation))
          ((looking-at ooo-basic-next-re)
           (ooo-basic-find-matching-for)
           (current-indentation))
          ((looking-at ooo-basic-wend-re)
           (ooo-basic-find-matching-while)
           (current-indentation))
          ((looking-at ooo-basic-end-with-re)
           (ooo-basic-find-matching-with)
           (current-indentation))
          (t
           (ooo-basic-previous-line-of-code)
           (while (looking-at ooo-basic-label-re)
             (ooo-basic-previous-line-of-code))
           (cond ((looking-at ooo-basic-continuation-re)
                  (ooo-basic-find-original-statement)
                  ;; Indent continuation line under matching open paren,
                  ;; or else one word in.
                  (let* ((orig (point))
                         (matching-open-paren
                          (condition-case ()
                              (save-excursion
                                (goto-char current-point)
                                (beginning-of-line)
                                (backward-up-list 1)
                                ;; Only if point is now w/in cont. block.
                                (and (<= orig (point))
                                     (current-column)))
                            (error nil))))
                    (if matching-open-paren
                        (1+ matching-open-paren)
                      ;; Else, after first word on original line.
                      (back-to-indentation)
                      (forward-word)
                      (while (looking-at "\\s-")
                        (forward-char))
                      (current-column))))
                 (t
                  (ooo-basic-find-original-statement)
                  (let* ((cur (current-indentation))
                         (tmp (+ cur ooo-basic-indent-level)))
                    (cond ((looking-at ooo-basic-definition-start-re) tmp)
                          ((ooo-basic-multiline-if-p) tmp)
                          ((looking-at ooo-basic-else-re) tmp)
                          ((looking-at ooo-basic-select-re) tmp)
                          ((looking-at ooo-basic-case-re) tmp)
                          ((ooo-basic-multiline-do-p) tmp)
                          ((looking-at ooo-basic-for-re) tmp)
                          ((looking-at ooo-basic-while-re) tmp)
                          ((looking-at ooo-basic-with-re) tmp)
                          (t cur)))))))))

(defun ooo-basic-indent-line ()
  "Indent the current line as OpenOffice.org Basic source text."
  (interactive)
  (let ((status (save-excursion
                  (if (functionp 'syntax-ppss)
                      (syntax-ppss (point-at-bol))
                    (parse-partial-sexp (point-min) (point-at-bol)))))
        (offset (- (current-column) (current-indentation))))
    (unless (nth 8 status)
      (indent-line-to (ooo-basic-indentation (point) status))
      (when (< 0 offset) (forward-char offset)))))

(defvar ooo-basic-definition-templates
  '("Sub ()\nEnd Sub\n\n"
    "Function () As Variant\nEnd Function\n\n"
    "Type \nEnd Type\n\n")
  "List of definition templates though which ooo-basic-new-definition cycles.")

(defun ooo-basic-new-definition ()
  "Insert template for a new definition. Repeat to cycle through alternatives."
  (interactive)
  (beginning-of-line)
  (let ((templates ooo-basic-definition-templates)
        (temp ooo-basic-blank-re)
        (bound (point)))
    (while temp
      (cond ((looking-at temp)
             (replace-match (or (car templates) ""))
             (setq temp nil))
            (t
             (setq temp (car templates)
                   templates (cdr templates)))))
    (goto-char bound)
    (when templates (search-forward " " nil t))))

(defconst ooo-basic-ooo-process-name
  "OpenOffice.org"
  "Name of OpenOffice.org process.")

(defconst ooo-basic-ooo-buffer-name
  "*OpenOffice.org*"
  "Name of buffer associated with OpenOffice.org process.")

(defvar ooo-basic-ooo-program-directory
  "/opt/openoffice.org3/program"
  "Directory of OpenOffice.org program.")

(defun ooo-basic-ooo-soffice ()
  "Path of the program of OpenOffice.org installation."
  (concat ooo-basic-ooo-program-directory "/soffice"))

(defun ooo-basic-start-ooo ()
  "Start OpenOffice.org."
  (interactive)
  (start-process ooo-basic-ooo-process-name
                 ooo-basic-ooo-buffer-name
                 (ooo-basic-ooo-soffice))
  (display-buffer ooo-basic-ooo-buffer-name))

(defun ooo-basic-quit-ooo ()
  "Quit OpenOffice.org."
  (interactive)
  (quit-process ooo-basic-ooo-process-name)
  (delete-windows-on ooo-basic-ooo-buffer-name))

(defun ooo-basic-call-macro-by-name (name)
  "Call a macro of OpenOffice.org by its name."
  (interactive "sMacro (e.g., 'Standard.Module1.Main'): ")
  (start-process ooo-basic-ooo-process-name
                 ooo-basic-ooo-buffer-name
                 (ooo-basic-ooo-soffice)
                 (concat "macro:///" name))
  (display-buffer ooo-basic-ooo-buffer-name))

(defun ooo-basic-mode-version ()
  "Echo the current version of ooo-basic-mode in the minibuffer."
  (interactive)
  (message "ooo-basic-mode %s" ooo-basic-mode-version))

(defun ooo-basic-mode ()
  "A major mode for editing OpenOffice.org Basic programs.
Turning on ooo-basic-mode runs the hook `ooo-basic-mode-hook'.
To see what version of ooo-basic-mode you are running, enter `\\[ooo-basic-mode-version]'.

Key bindings:
\\{ooo-basic-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map ooo-basic-mode-map)
  (define-key ooo-basic-mode-map "\C-c\C-ic" 'ooo-basic-insert-uno-constant)
  (define-key ooo-basic-mode-map "\C-c\C-b" 'ooo-basic-browse-idl-reference)
  (define-key ooo-basic-mode-map "\C-c\C-n" 'ooo-basic-new-definition)
  (define-key ooo-basic-mode-map "\C-c\C-s" 'ooo-basic-start-ooo)
  (define-key ooo-basic-mode-map "\C-c\C-q" 'ooo-basic-quit-ooo)
  (define-key ooo-basic-mode-map "\C-c\C-cm" 'ooo-basic-call-macro-by-name)
  (set-syntax-table ooo-basic-mode-syntax-table)
  (set (make-local-variable 'comment-start) "'")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'indent-line-function) 'ooo-basic-indent-line)
  (set (make-local-variable 'beginning-of-defun-function)
       'ooo-basic-beginning-of-defun)
  (set (make-local-variable 'end-of-defun-function)
       'ooo-basic-end-of-defun)
  (set (make-local-variable 'parse-sexp-ignore-comments) t)
  (set (make-local-variable 'font-lock-defaults)
       '((ooo-basic-font-lock-keywords-1
          ooo-basic-font-lock-keywords-2
          ooo-basic-font-lock-keywords-3)
         nil t nil))
  (setq major-mode 'ooo-basic-mode)
  (setq mode-name "OOo-Basic")
  (if (functionp 'run-mode-hooks)
      (run-mode-hooks 'ooo-basic-mode-hook)
    (run-hooks 'ooo-basic-mode-hook)
    (when (boundp 'after-change-major-mode-hook)
      (run-hooks 'after-change-major-mode-hook))))

(provide 'ooo-basic-mode)
;;; ooo-basic-mode.el ends here
