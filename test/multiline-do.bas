Sub Main()
    Dim x as int

    x = 0
    Do Until x = 100 ' multiline
        Print x
        x = x + 1
    Loop
    Do : x = x - 1 : Loop _
       Until x = 0 ' single line
    Print "done."
End Sub
