Sub Main
    Dim x As String
    x = "x"
    If x = "x" Then _
       MsgBox "single" : MsgBox "line" ' single line
    If x = "x" _
       Then ' multiline
        MsgBox x
    End If
End Sub
