VERSION 5.00
Begin {C62A69F0-16DC-11CE-9E98-00AA00574A4F} PopDens 
   Caption         =   "Population Density Data Entry"
   ClientHeight    =   6000
   ClientLeft      =   0
   ClientTop       =   0
   ClientWidth     =   5595
   OleObjectBlob   =   "PopDens.frx":0000
   StartUpPosition =   1  'CenterOwner
End
Attribute VB_Name = "PopDens"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False






Private Sub PDAreaUnits_Change()

'disable fuckwittery
If PDAreaUnits.Value = "---" Then
    PDAreaUnits.ListIndex = -1
    Exit Sub
End If


PDCheckAll

End Sub

Private Sub PDCancel_Click()

Unload PopDens

Call NonLocDataClear
End Sub

Private Sub PDArea_Change()

PDCheckAll

End Sub
Private Sub PDStartDate_Change()

PDCheckAll

End Sub

Private Sub PDAreaType_Change()

PDCheckAll

End Sub

Private Sub PDData_Change()

PDCheckAll

End Sub

Private Sub PDData_KeyPress(ByVal KeyAscii As MSForms.ReturnInteger)

KeyAscii = FilterKeypress(KeyAscii, "0123456789.")

End Sub

Private Sub PDArea_KeyPress(ByVal KeyAscii As MSForms.ReturnInteger)

KeyAscii = FilterKeypress(KeyAscii, "0123456789.")

End Sub
Private Sub PDDur_KeyPress(ByVal KeyAscii As MSForms.ReturnInteger)

KeyAscii = FilterKeypress(KeyAscii, "0123456789.")

End Sub
Private Sub PDStartDate_KeyPress(ByVal KeyAscii As MSForms.ReturnInteger)

KeyAscii = FilterKeypress(KeyAscii, "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789/")

End Sub
Private Sub PDMethod_Change()

PDCheckAll

End Sub

Private Sub PDUnits_Change()

'disable fuckwittery
If PDUnits.Value = "---" Then
    PDUnits.ListIndex = -1
    Exit Sub
End If
'Select correct default choice in area units.

PDAreaUnits.ListIndex = PDUnits.ListIndex Mod 10

PDCheckAll

End Sub

Private Sub PDDur_Change()

PDCheckAll

End Sub

Private Sub PDWrite_Click()

Dim Output$

Output = PDUnits.Value & Chr(9) & PDData.Value & Chr(9) & _
    PDMethod.Value & Chr(9) & PDArea.Text & Chr(9) & _
    PDAreaUnits.Value & Chr(9) & _
    PDAreaType.Value & Chr(9) & PDDur.Text & Chr(9) & _
    PDStartDate.Text & Chr(9) & PDPreyData.Value & Chr(9) & Chr(9) & Chr(9)
    
    

Call ExportData(Output)
Unload PopDens
Call NonLocDataClear

End Sub

Private Sub UserForm_Initialize()

PDUnits.List() = Array("Individuals/m2", "Individuals/ha", "Individuals/km2", "Individuals/acre", "Individuals/square mile", "---", "Individuals/m", "Individuals/km", "Individuals/mile", "---", "Groups/m2", "Groups/ha", "Groups/km2", "Groups/acre", "Groups/square mile", "---", "Groups/m", "Groups/km", "Groups/mile")
PDMethod.List() = Array("Unspecified", "Direct counts", "Indirect counts", "Calculation from direct counts", "Calculation from indirect counts", "Home Range extrapolation")
PDAreaType.List() = Array("Unspecified", "Human boundaries", "Ecological boundaries", "Transect boundaries")
PDAreaUnits.List = Array("m2", "hectares", "km2", "acres", "square miles", "---", "metres", "kilometres", "miles")
PDDur.Value = "Unspecified"
PDStartDate = "Unspecified"
PDArea = "Unspecified"

End Sub

Private Sub PDCheckAll()

If ContentCheck(PDData) And PDUnits.ListIndex <> -1 _
And PDMethod.ListIndex <> -1 And PDAreaType.ListIndex <> -1 _
And ContentCheck(PDDur) And ContentCheck(PDArea) And _
ContentCheck(PDStartDate) And PDAreaUnits.ListIndex <> -1 Then
    PDWrite.Enabled = True
Else
    PDWrite.Enabled = False
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
