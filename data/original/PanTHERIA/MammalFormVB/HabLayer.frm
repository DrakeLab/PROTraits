VERSION 5.00
Begin {C62A69F0-16DC-11CE-9E98-00AA00574A4F} HabLayer 
   Caption         =   "Habitat Layer"
   ClientHeight    =   5000
   ClientLeft      =   0
   ClientTop       =   0
   ClientWidth     =   6405
   OleObjectBlob   =   "HabLayer.frx":0000
   StartUpPosition =   1  'CenterOwner
End
Attribute VB_Name = "HabLayer"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private Sub HLAqc_Change()

Call HLContentCheck

End Sub

Private Sub HLAqc_KeyPress(ByVal KeyAscii As MSForms.ReturnInteger)

KeyAscii = FilterKeypress(KeyAscii, "0123456789.")

End Sub

Private Sub HLArb_Change()

Call HLContentCheck

End Sub

Private Sub HLArb_KeyPress(ByVal KeyAscii As MSForms.ReturnInteger)

KeyAscii = FilterKeypress(KeyAscii, "0123456789.")

End Sub

Private Sub HLCancel_Click()

Unload HabLayer
Call NonLocDataClear

End Sub

Private Sub HLFoss_Change()

Call HLContentCheck

End Sub

Private Sub HLFoss_KeyPress(ByVal KeyAscii As MSForms.ReturnInteger)

KeyAscii = FilterKeypress(KeyAscii, "0123456789.")

End Sub

Private Sub HLGnd_Change()

Call HLContentCheck

End Sub

Private Sub HLGnd_KeyPress(ByVal KeyAscii As MSForms.ReturnInteger)

KeyAscii = FilterKeypress(KeyAscii, "0123456789.")

End Sub

Private Sub HLMeas_Change()

Select Case HLMeas.ListIndex
    Case 2
        HLInfo.Caption = "Qualitative:" & Chr(13) & "Enter a '1' for each habitat layer listed in the source."
    Case 1
        HLInfo.Caption = "Ranked:" & Chr(13) & "Number all habitat layers reflecting the ranked importance of each category, 1 (most) to 4 (least). Several categories can have the same number if the habitat layers are used equally."
    Case 0
        HLInfo.Caption = "Percentage:" & Chr(13) & "Enter the percentages usage of each habiat layer. The sum of values must not exceed 100, but can be less than 100."
    Case Else
        HLInfo.Caption = ""
End Select


Call HLContentCheck

End Sub


Private Sub UserForm_Initialize()

HLMeas.List() = Array("Percentage", "Ranked", "Qualitative")

End Sub

Private Sub HLWrite_Click()

'currently no data checking involved but could be written in

Dim Output$

Output = HLMeas.Value & Chr(9) & "NA" & Chr(9) & HLFoss.Text & Chr(9) & HLGnd.Text & Chr(9) & HLArb.Text & Chr(9) & HLAqc.Text & Chr(9) & Chr(9) & Chr(9) & Chr(9) & Chr(9) & Chr(9)

Call ExportData(Output)
Unload HabLayer
Call NonLocDataClear

End Sub


Private Sub HLContentCheck()

If (ContentCheck(HLFoss) Or ContentCheck(HLGnd) Or _
   ContentCheck(HLArb) Or ContentCheck(HLAqc)) And _
   HLMeas.ListIndex <> -1 Then
    HLWrite.Enabled = True
Else
    HLWrite.Enabled = False
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
