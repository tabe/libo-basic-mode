;;; ooo-basic-mode.el -- A major mode for editing OpenOffice.org Basic programs

;; Copyright (C) 2009 Takeshi Abe <tabe@fixedpoint.jp>

;; Author: Takeshi Abe <tabe@fixedpoint.jp>
;; Version: 0.0.1
;; Keywords: languages, basic, not so evil

;; Installation:
;; Put ooo-basic-mode.el somewhere in your path, compile it, and
;; add the following to your .emacs:
;; (autoload 'ooo-basic-mode "ooo-basic-mode" "A major mode for OpenOffice.org Basic." t)
;; (push '("\\.bas\\'" . ooo-basic-mode) auto-mode-alist)

;; Code:

(require 'font-lock)

(defconst ooo-basic-mode-version "0.0.1"
  "Version string for ooo-basic-mode.")

(defgroup ooo-basic nil
  "Customization variables for `ooo-basic-mode'."
  :tag "OpenOffice.org Basic"
  :group 'languages)

(defcustom ooo-basic-indent-level 4
  "Number of spaces for each indentation step."
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
    "Error"
    "Explicit"
    "False"
    "For"
    "Global"
    "GoTo"
    "If"
    "Input"
    "Let"
    "Line"
    "Loop"
    "Next"
    "On"
    "Option"
    "Optional"
    "Private"
    "Public"
    "ReDim"
    "Resume"
    "Select"
    "Set"
    "Static"
    "Then"
    "To"
    "True"
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

(defvar ooo-basic-builtin-operators
  '("AND" "EQV" "IMP" "NOT" "OR" "XOR"
    "MOD"
    )
  "Builtin operators available in OpenOffice.org Basic.")

(defvar ooo-basic-builtin-properties
  '("BasicLibraries" "DialogLibraries" "GlobalScope")
  "Builtin properties available in OpenOffice.org Basic.")

(defvar ooo-basic-builtin-functions
  '("CBool" "CDate" "CDbl" "CInt" "CLng" "CSng" "CStr" "Val"
    "IsArray" "IsDate" "IsEmpty" "IsNull" "IsNumeric" "IsObject" "IsMissing"
    "Asc" "Chr" "Str" "Val" "Space" "String"
    "Left" "Right" "Mid" "Len" "InStr" "LTrim" "RTrim" "Trim" "StrComp"
    "Format" "LCase" "UCase"
    "DateSerial" "TimeSerial" "DateValue" "TimeValue"
    "Day" "Month" "Year" "WeekDay" "Hour" "Minute" "Second"
    "Date" "Time" "Now" "Timer"
    "Dir" "MkDir" "RmDir" "ChDir" "ChDrive" "CurDir" "CurDrive"
    "FileAttr" "FileCopy" "Kill" "FileExists" "GetAttr" "SetAttr" "Name"
    "FileDateTime" "FileLen" "FreeFile"
    "Open" "Close" "Print" "Reset" "Get" "Put" "Write" "Eof" "Lof"
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
    "CreateUnoDialog" "CreateUnoListener" "CreateUnoService" "CreateUnoStruct"
    "GetProcessServiceManager"
    "ConvertFromUrl" "ConvertToUrl"
    "CompatibilityMode"
    )
  "Builtin functions available in OpenOffice.org Basic.")

(defvar ooo-basic-definition-start-re
  "^[ \t]*\\(?:Public\\|Private\\)?[ \t]*\\(Sub\\|Function\\|Type\\)\\>"
  "Regexp to detect the start of a definition.")

(defvar ooo-basic-definition-end-re
  "^[ \t]*End\\>"
  "Regexp to detect the end of a definition.")

(defvar ooo-basic-font-lock-keywords-1
  (eval-when-compile
    `((,ooo-basic-definition-start-re
       (1 font-lock-keyword-face)
       ("\\<\\([A-z_][A-z_0-9]*\\)\\>.*$" nil nil (1 font-lock-function-name-face))
       )
      (,ooo-basic-definition-end-re
       (0 font-lock-keyword-face)
       ("\\<\\(Sub\\|Function\\|Type\\)\\>" nil nil (1 font-lock-keyword-face))
       )
      ("\\<Exit\\>"
       (0 font-lock-keyword-face)
       ("\\<\\(Sub\\|Function\\)\\>" nil nil (1 font-lock-keyword-face))
       )
      ("\\<ReDim\\>"
       (0 font-lock-keyword-face)
       ("\\<Preserve\\>" nil nil (0 font-lock-keyword-face))
       )
      ("\\<REM\\>"
       (0 ,(if (boundp 'font-lock-comment-delimiter-face) 'font-lock-comment-delimiter-face 'font-lock-comment-face))
       (".*$" nil nil (0 font-lock-comment-face))
       )
      )
    )
  "Level 1.")

(defvar ooo-basic-font-lock-keywords-2
  (append ooo-basic-font-lock-keywords-1
          `(,(regexp-opt ooo-basic-keywords 'words)
            (,(regexp-opt ooo-basic-types 'words) 0 font-lock-type-face)
            (,(regexp-opt (append ooo-basic-global-variables ooo-basic-builtin-properties) 'words)
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

(defun ooo-basic-indentation (parse-status)
  "Return the proper indentation for the current line."
  (save-excursion
;    (back-to-indentation)
    (beginning-of-line)
    (cond ((looking-at "[ \t]*$") 0)
          ((looking-at ooo-basic-definition-start-re) 0)
          ((looking-at ooo-basic-definition-end-re) 0)
          (t ooo-basic-indent-level))))

(defun ooo-basic-indent-line ()
  "Indent the current line as OpenOffice.org Basic source text."
  (interactive)
  (let ((status (save-excursion
                  (if (functionp 'syntax-ppss)
                      (syntax-ppss (point-at-bol))
                    (parse-partial-sexp (point-min) (point-at-bol)))))
        (offset (- (current-column) (current-indentation))))
    (unless (nth 8 status)
      (indent-line-to (ooo-basic-indentation status))
      (when (< 0 offset) (forward-char offset)))))

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
  (set-syntax-table ooo-basic-mode-syntax-table)
  (set (make-local-variable 'comment-start) "'")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'indent-line-function) 'ooo-basic-indent-line)
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
