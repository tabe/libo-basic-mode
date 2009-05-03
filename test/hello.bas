' hello.bas

Sub Main
    Dim h As String
    Dim w As String

    h = "Hello"
    w = "World"
    If h = w Then
        MsgBox "impossible"
    Else
        MsgBox h & ", " & _
              w & "."
    End If
End Sub
