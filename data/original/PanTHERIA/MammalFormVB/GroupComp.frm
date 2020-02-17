VERSION 5.00
Begin {C62A69F0-16DC-11CE-9E98-00AA00574A4F} GroupComp 
   Caption         =   "Group Composition and Size"
   ClientHeight    =   7000
   ClientLeft      =   0
   ClientTop       =   0
   ClientWidth     =   6405
   OleObjectBlob   =   "GroupComp.frx":0000
   StartUpPosition =   1  'CenterOwner
End
Attribute VB_Name = "GroupComp"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Private Sub UserForm_QueryClose(Cancel As Integer, CloseMode As Integer)
    'Prevent user from closing with the Close box in the title bar.
    Dim alertresponse As Integer
    
    If CloseMode <> 1 Then
            Cancel = 1
            alertresponse = MsgBox("Use the Cancel button", 16, "Alert")
    End If
    
End Sub

Private Sub GPActiveFemale_KeyPress(ByVal KeyAscii As MSForms.ReturnInteger)

KeyAscii = FilterKeypress(KeyAscii, "0123456789")

End Sub
Private Sub GPActiveMale_KeyPress(ByVal KeyAscii As MSForms.ReturnInteger)

KeyAscii = FilterKeypress(KeyAscii, "0123456789")

End Sub

Private Sub GPAFemale_KeyPress(ByVal KeyAscii As MSForms.ReturnInteger)

KeyAscii = FilterKeypress(KeyAscii, "0123456789")

End Sub

Private Sub GPMale_KeyPress(ByVal KeyAscii As MSForms.ReturnInteger)

KeyAscii = FilterKeypress(KeyAscii, "0123456789")

End Sub

Private Sub GPCancel_Click()

Unload GroupComp
Call NonLocDataClear

End Sub

Private Sub GPDur_Change()

GPCheckAll

End Sub

Private Sub GPMGType_Change()

GPCheckAll

End Sub

Private Sub GPSizeData_Change()

GPCheckAll

End Sub

Private Sub GPSizeData_KeyPress(ByVal KeyAscii As MSForms.ReturnInteger)

KeyAscii = FilterKeypress(KeyAscii, "0123456789")

End Sub

Private Sub GPDur_KeyPress(ByVal KeyAscii As MSForms.ReturnInteger)

KeyAscii = FilterKeypress(KeyAscii, "0123456789.")

End Sub

Private Sub GPStartDate_KeyPress(ByVal KeyAscii As MSForms.ReturnInteger)

KeyAscii = FilterKeypress(KeyAscii, "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789/")

End Sub

Private Sub GPStartDate_Change()

GPCheckAll

End Sub

Private Sub GPType_Change()

GPCheckAll

End Sub

Private Sub GPUnits_Change()

GPCheckAll

End Sub

Private Sub GPWrite_Click()

'check to see if this is a 'just write mating group type situation'
'in which case substitute in Unspecified to the blank fields.
If ContentCheck(GPSizeData) = False Then GPSizeData.Value = "Unspecified"
If GPUnits.ListIndex = -1 Then GPUnits.ListIndex = 0

Dim Output$
Output = GPUnits.Value & Chr(9) & GPSizeData.Text & Chr(9) & _
    GPType.Value & Chr(9) & GPDur.Text & Chr(9) & _
    GPStartDate.Text & Chr(9) & GPMGType.Value & Chr(9) & _
    GPMale.Text & Chr(9) & GPFemale.Text & Chr(9) & _
    GPActiveMale & Chr(9) & GPActiveFemale & Chr(9) & Chr(9)

Call ExportData(Output)

Unload GroupComp

Call NonLocDataClear

End Sub



Private Sub UserForm_Initialize()

GPType.List() = Array("Population group", "Social group", "Feeding group", "Hunting group", "Parental care group", "Mating group")
GPUnits.List() = Array("Unspecified", "Total number of individuals", "Number of adults")
GPMGType.List() = Array("Polygynous", "Polyandrous", "Monogamous", "Promiscuous", "Unspecified")
Call FrameEnable(GPMGDetails, False)

GPDur.Text = "Unspecified"
GPStartDate.Text = "Unspecified"
GPFemale.Text = "Unspecified"
GPMale.Text = "Unspecified"
GPActiveMale.Text = "Unspecified"
GPActiveFemale.Text = "Unspecified"

End Sub



Private Sub GPCheckAll()

'This is a bit tricky because the combinations of required data
'aren't obvious.

'If size given then GP size cannot be blank, otherwise no restrictions
'at present.


If GPType.ListIndex = 5 Then
    Call FrameEnable(GPMGDetails, True)
    If GPMGType.ListIndex <> -1 Then GPWrite.Enabled = True Else GPWrite.Enabled = False
Else
    Call FrameEnable(GPMGDetails, False)
    If ContentCheck(GPSizeData) = True And GPUnits.ListIndex <> -1 Then GPWrite.Enabled = True Else GPWrite.Enabled = False
End If

End Sub
