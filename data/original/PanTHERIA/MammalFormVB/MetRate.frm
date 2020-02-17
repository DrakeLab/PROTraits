VERSION 5.00
Begin {C62A69F0-16DC-11CE-9E98-00AA00574A4F} MetRate 
   Caption         =   "Metabolic Rate"
   ClientHeight    =   4800
   ClientLeft      =   0
   ClientTop       =   0
   ClientWidth     =   4995
   OleObjectBlob   =   "MetRate.frx":0000
   StartUpPosition =   1  'CenterOwner
End
Attribute VB_Name = "MetRate"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False

Private Sub MRCancel_Click()

Unload MetRate

Call NonLocDataClear

End Sub

Private Sub MRData_Change()

CheckAllMR

End Sub

Private Sub MRData_KeyPress(ByVal KeyAscii As MSForms.ReturnInteger)

KeyAscii = FilterKeypress(KeyAscii, "0123456789.")

End Sub

Private Sub MRDefn_Change()

CheckAllMR

End Sub

Private Sub MRSize_Change()

CheckAllMR

End Sub

Private Sub MRSize_KeyPress(ByVal KeyAscii As MSForms.ReturnInteger)

KeyAscii = FilterKeypress(KeyAscii, "0123456789.")

End Sub

Private Sub MRTemp_Change()

CheckAllMR

End Sub

Private Sub MRTemp_KeyPress(ByVal KeyAscii As MSForms.ReturnInteger)

KeyAscii = FilterKeypress(KeyAscii, "0123456789.,=ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")

End Sub
Private Sub MRUnits_Change()

CheckAllMR

End Sub

Private Sub MRWrite_Click()

Dim Output$
Output = MRUnits.Value & Chr(9) & MRData.Value & Chr(9) & _
    MRDefn.Value & Chr(9) & MRSize.Text & Chr(9) & MRTemp.Text & Chr(9) & Chr(9) & Chr(9) & Chr(9) & Chr(9) & Chr(9) & Chr(9)
    
Call ExportData(Output)

Unload MetRate

Call NonLocDataClear

End Sub

Private Sub UserForm_Initialize()

MRUnits.List() = Array("cm3.02/hr", "cm3.02/g/hr", "mL.02/hr", "mL.02/g/hr", "mm3.02/hr", "mm3.02/g/hr")
MRDefn.List() = Array("Unspecified", "Basal", "Field/Active")
MRTemp.Value = "Unspecified"
MRSize.Value = "Unspecified"

End Sub


Private Sub CheckAllMR()

With MetRate
    If .MRUnits.ListIndex <> -1 And .MRDefn.ListIndex <> -1 And _
    ContentCheck(.MRData.Text) And ContentCheck(.MRTemp.Text) And _
    ContentCheck(.MRSize.Text) Then
        MRWrite.Enabled = True
    Else
        MRWrite.Enabled = False
    End If
        
End With

End Sub
Private Sub UserForm_QueryClose(Cancel As Integer, CloseMode As Integer)
    'Prevent user from closing with the Close box in the title bar.
    Dim alertresponse As Integer
    
    If CloseMode <> 1 Then
            Cancel = 1
            alertresponse = MsgBox("Use the Cancel button", 16, "Alert")
    End If
    
End Sub
