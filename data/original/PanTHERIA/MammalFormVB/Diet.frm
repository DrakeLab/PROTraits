VERSION 5.00
Begin {C62A69F0-16DC-11CE-9E98-00AA00574A4F} Diet 
   Caption         =   "Diet Data Entry"
   ClientHeight    =   7000
   ClientLeft      =   0
   ClientTop       =   0
   ClientWidth     =   6405
   OleObjectBlob   =   "Diet.frx":0000
   StartUpPosition =   1  'CenterOwner
End
Attribute VB_Name = "Diet"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private Sub DIETAssess_Change()

Call SomeDietDataPresentCheck

End Sub

Private Sub DietCancel_Click()

Call UserForm_Terminate

End Sub

Private Sub DIETDur_Change()

Call SomeDietDataPresentCheck

End Sub



Private Sub DIETMeas_Change()

Select Case DIETMeas.ListIndex
    Case 6
        DIETInfo.Caption = "Qualitative:" & Chr(13) & "Enter a '1' for each category listed in the source."
    Case 5
        DIETInfo.Caption = "Ranked:" & Chr(13) & "Number all dietary elements reflecting the ranked importance of each category, 1 (most) to 8 (least). Several categories can have the same number if they contribute equally to the diet."
    Case 0 To 4
        DIETInfo.Caption = "Percentage:" & Chr(13) & "Enter the percentages of each dietary element. The sum of values must not exceed 100, but can be less than 100."
    Case Else
        DIETInfo.Caption = ""
End Select

Call SomeDietDataPresentCheck

End Sub

Private Sub UserForm_Initialize()
        
DIETMeas.List() = Array("% volume", "% mass", "% time spent", "% energy", "% unspecified", "Ranked", "Qualitative")
DIETDur.List() = Array("Unspecified", "Year round", "Part of year")
DIETAssess.List() = Array("Unspecified", "Feeding Observation", "Stomach Contents", "Scat Contents")

End Sub

Private Sub DIETWriteEntry_Click()

'currently no data checking involved but could be written in

Dim Output$
Output = DIETMeas.Value & Chr(9) & "NA" & Chr(9) & DIETDur.Value & Chr(9) & DIETAssess.Value & Chr(9) & Vert.Text & Chr(9) & Invert.Text & Chr(9) & _
    Fruit.Text & Chr(9) & Flowers.Text & Chr(9) & Seeds.Text & Chr(9) & _
    Grass.Text & Chr(9) & Tree.Text & Chr(9) & Roots.Text

Call ExportData(Output)

Call NonLocDataClear

Call UserForm_Terminate 'Unload Me ' this should mean it comes up blank next time as well.

End Sub

Private Sub SomeDietDataPresentCheck()

If (ContentCheck(Vert.Value) Or ContentCheck(Invert.Value) Or _
    ContentCheck(Fruit.Value) Or ContentCheck(Flowers.Value) Or _
    ContentCheck(Seeds.Value) Or ContentCheck(Grass.Value) Or _
    ContentCheck(Tree.Value) Or ContentCheck(Roots.Value)) And _
    DIETMeas.ListIndex <> -1 And DIETDur.ListIndex <> -1 And _
    DIETAssess.ListIndex <> -1 _
    Then
        DIETWriteEntry.Enabled = True
    Else
        DIETWriteEntry.Enabled = False
End If
    



End Sub

Private Sub UserForm_QueryClose(Cancel As Integer, CloseMode As Integer)
    'Prevent user from closing with the Close box in the title bar.
    Dim alertresponse As Integer
    
    If CloseMode <> 1 Then
            Cancel = 1
            alertresponse = MsgBox("Use the Cancel button", 16, "Alert")
    End If
    
End Sub


Private Sub UserForm_Terminate()

Unload Me

End Sub

Private Sub Vert_Change()

Call SomeDietDataPresentCheck

End Sub

Private Sub Invert_Change()

Call SomeDietDataPresentCheck

End Sub
Private Sub Fruit_Change()

Call SomeDietDataPresentCheck

End Sub
Private Sub Flowers_Change()

Call SomeDietDataPresentCheck

End Sub
Private Sub Seeds_Change()

Call SomeDietDataPresentCheck

End Sub
Private Sub Grass_Change()

Call SomeDietDataPresentCheck

End Sub
Private Sub Tree_Change()

Call SomeDietDataPresentCheck

End Sub
Private Sub Roots_Change()

Call SomeDietDataPresentCheck

End Sub

Private Sub Vert_KeyPress(ByVal KeyAscii As MSForms.ReturnInteger)

KeyAscii = FilterKeypress(KeyAscii, "0123456789.")

End Sub

Private Sub Invert_KeyPress(ByVal KeyAscii As MSForms.ReturnInteger)

KeyAscii = FilterKeypress(KeyAscii, "0123456789.")

End Sub
Private Sub Fruit_KeyPress(ByVal KeyAscii As MSForms.ReturnInteger)

KeyAscii = FilterKeypress(KeyAscii, "0123456789.")

End Sub
Private Sub Flowers_KeyPress(ByVal KeyAscii As MSForms.ReturnInteger)

KeyAscii = FilterKeypress(KeyAscii, "0123456789.")

End Sub
Private Sub Seeds_KeyPress(ByVal KeyAscii As MSForms.ReturnInteger)

KeyAscii = FilterKeypress(KeyAscii, "0123456789.")

End Sub
Private Sub Grass_KeyPress(ByVal KeyAscii As MSForms.ReturnInteger)

KeyAscii = FilterKeypress(KeyAscii, "0123456789.")

End Sub
Private Sub Tree_KeyPress(ByVal KeyAscii As MSForms.ReturnInteger)

KeyAscii = FilterKeypress(KeyAscii, "0123456789.")

End Sub
Private Sub Roots_KeyPress(ByVal KeyAscii As MSForms.ReturnInteger)

KeyAscii = FilterKeypress(KeyAscii, "0123456789.")

End Sub
