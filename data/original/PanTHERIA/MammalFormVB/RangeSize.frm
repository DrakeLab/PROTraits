VERSION 5.00
Begin {C62A69F0-16DC-11CE-9E98-00AA00574A4F} RangeSize 
   Caption         =   "Ranging Behaviour Data Entry"
   ClientHeight    =   7005
   ClientLeft      =   0
   ClientTop       =   0
   ClientWidth     =   6405
   OleObjectBlob   =   "RangeSize.frx":0000
   StartUpPosition =   1  'CenterOwner
End
Attribute VB_Name = "RangeSize"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False







Private Sub RSCancel_Click()

Unload RangeSize

Call NonLocDataClear

End Sub



Private Sub RSData_Change()

RSCheckAll

End Sub

Private Sub RSData_KeyPress(ByVal KeyAscii As MSForms.ReturnInteger)

KeyAscii = FilterKeypress(KeyAscii, "0123456789.")

End Sub

Private Sub RSDur_Change()

RSCheckAll

End Sub

Private Sub RSDur_KeyPress(ByVal KeyAscii As MSForms.ReturnInteger)

KeyAscii = FilterKeypress(KeyAscii, "0123456789.")

End Sub


Private Sub RSGroupSize_Change()

RSCheckAll

End Sub
Private Sub RSGroupSize_KeyPress(ByVal KeyAscii As MSForms.ReturnInteger)

KeyAscii = FilterKeypress(KeyAscii, "0123456789")

End Sub

Private Sub RSGroupToggle_Change()

If RSGroupToggle Then
    With RSGroupType
        .Enabled = True
        .List() = Array("Population group", "Social group", "Feeding group", "Foraging group", "Parental care group", "Mating group")
        .ListIndex = -1
    End With
    With RSGroupSize
        .Enabled = True
        .Value = ""
    End With
End If

RSCheckAll

End Sub

Private Sub RSGroupType_Change()

RSCheckAll

End Sub

Private Sub RSIndividualToggle_Change()

If RSIndividualToggle Then
    With RSGroupType
        .Enabled = False
        .List() = Array("NA")
        .ListIndex = 0
    End With
    With RSGroupSize
        .Enabled = False
        .Value = "NA"
    End With
End If

RSCheckAll

End Sub

Private Sub RSMethod_Change()

RSCheckAll

End Sub

Private Sub RSType_Change()

RSCheckAll

End Sub

Private Sub RSUnits_Change()

'disable fuckwittery
If RSUnits.Value = "---" Then
    RSUnits.ListIndex = -1
    Exit Sub
End If

RSCheckAll

End Sub
Private Sub RSStartDate_Change()

RSCheckAll

End Sub
Private Sub RSStartDate_KeyPress(ByVal KeyAscii As MSForms.ReturnInteger)

KeyAscii = FilterKeypress(KeyAscii, "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789/")

End Sub


Private Sub RSWrite_Click()


Dim Output$
Output = RSUnits.Value & Chr(9) & RSData.Text & Chr(9) & _
    RSType.Value & Chr(9) & RSMethod.Value & Chr(9) & _
    RSDur.Text & Chr(9) & RSStartDate.Text & Chr(9) & _
    RSGroupType.Value & Chr(9) & RSGroupSize.Text & Chr(9) & Chr(9) & Chr(9) & Chr(9)
    
Call ExportData(Output)

Unload RangeSize

Call NonLocDataClear

End Sub

Private Sub UserForm_Initialize()

RSUnits.List() = Array("m2", "hectares", "km2", "acres", "square miles", "---", "metres", "kilometres", "miles")
RSType.List() = Array("Home Range", "Day Range", "Territory size")
RSMethod.List() = Array("Direct observation", "Radio telemetry", "Trapping", "Unspecified")
RSGroupType.List() = Array("Population group", "Social group", "Feeding group", "Hunting group", "Parental care group", "Mating group")
RSDur.Value = "Unspecified"
RSStartDate = "Unspecified"

End Sub



Private Sub RSCheckAll()

If ContentCheck(RSData.Value) And RSUnits.ListIndex <> -1 _
And RSType.ListIndex <> -1 And RSMethod.ListIndex <> -1 _
And ContentCheck(RSDur.Value) And RSGroupType.ListIndex <> -1 _
And ContentCheck(RSGroupSize) And ContentCheck(RSStartDate) Then
    RSWrite.Enabled = True
Else
    RSWrite.Enabled = False
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
