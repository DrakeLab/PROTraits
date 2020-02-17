VERSION 5.00
Begin {C62A69F0-16DC-11CE-9E98-00AA00574A4F} MammalForm 
   Caption         =   "MammalForm v 2.1"
   ClientHeight    =   10000
   ClientLeft      =   0
   ClientTop       =   0
   ClientWidth     =   13995
   OleObjectBlob   =   "MammalForm.frx":0000
   StartUpPosition =   1  'CenterOwner
End
Attribute VB_Name = "MammalForm"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
'********************************************
'* MammalForm coding follows:                 *
'********************************************



Private Sub TaxonomyFrame_Click()

End Sub

Private Sub UserForm_Initialize()

Call MamFormInit

End Sub

Private Sub Finish_Click()


Unload MammalForm

End

End Sub

' # RESET BUTTON CODING

Private Sub SourceFrameReset_Click()

SourceID.Text = ""
GuttingStatus.ListIndex = 0

'now initiate the click event for the reset button on the level below
TaxonomyFrameReset.Value() = True
Call FrameEnable(MammalForm.TaxonomyFrame, False)


'SourceID.SetFocus '- ideally this should be as code not comment but for reasons I can't fathom
' it causes validatesource to fire and hence throw an error.

End Sub

Private Sub TaxonomyFrameReset_click()

'clear the data in the controls
Order.ListIndex = -1
Dim LowerCtrls

    For Each LowerCtrls In Array(Family, Genus, Species)
        With LowerCtrls
            .Clear
            .Text = ""
            .Enabled = False
        End With
    Next

SubSpecies.Value = "Unspecified"
Authority.Value = "Unspecified"
ValidDisplay.Value = ""


'now initiate the click event for the reset button on the level below
DataFrameReset.Value() = True
Call FrameEnable(MammalForm.DataFrame, False)

If Order.Enabled Then Order.SetFocus

End Sub

Private Sub DataFrameReset_click()

'clear the data in the controls and reset to initial state

DataType.ListIndex = -1
UnitCat.Enabled = False
DataValue.Clear

If DataType.Enabled Then DataType.SetFocus
Call FrameEnable(MammalForm.DataDetails, False) 'lock data details
Call LoadDataSettings("reset") ' restore the defaults in the main fields
WriteEntry.Enabled = False
WriteEntry.Caption = "Write Data"
'clear out the optionals
Location.Text = ""
Latitude.Text = ""
Longitude.Text = ""
DataRange.Text = ""
LatTog.Value = False
LongTog.Value = False
NoLatLong = True

End Sub

' ********************************
' * SourceFrame coding follows:  *
' ********************************
'
' Two text fields in source frame: Source ID and UserID
' BOTH must have content in order to allow the user to
' enter any further data.
' - handled by function SourceFrameCompleteCheck
'
' Also there is an option to flag a source as only partly dealt with
' Once this is checked, further details of missing info are required

Private Sub SourceID_BeforeUpdate(ByVal Cancel As MSForms.ReturnBoolean)

Call ValidateSource

Call SourceFrameChecking

End Sub

Private Sub UserID_BeforeUpdate(ByVal Cancel As MSForms.ReturnBoolean)

Call SourceFrameChecking

End Sub

Private Sub GuttingStatus_Click()

If GuttingStatus = "Gutting" Then
    GuttingNotes.Enabled = False
    GuttingNotes = "NA"
Else
    GuttingNotes.Enabled = True
    GuttingNotes = ""
End If

Call SourceFrameChecking

End Sub

Private Sub GuttingNotes_BeforeUpdate(ByVal Cancel As MSForms.ReturnBoolean)

Call SourceFrameChecking

End Sub

' **********************************
' * TaxonomyFrame coding follows:  *
' **********************************
'
' The frame contains 4 linked combo boxes which are used
' to display mammalian taxonomy as a series of hierarchical
' choices.
'
' The combo boxes use similar code and so call a public subroutine
' (TaxonChange) containing the common code along with some box
' specific differences. Contents monitoring as follows:
'
'   Simple choices from the drop down menu do not use the click event in order
'   to trap changes because typed entries have to be trapped on exit from the control.
'   Because VBA doesn't handle setfocus from within exit events properly, this requires
'   the use of tabsentry controls. On tab, control passes to the sentry, which then
'   proceeds to entry checking.

Private Sub Order_KeyPress(ByVal KeyAscii As MSForms.ReturnInteger)

        KeyAscii = FilterKeypress(KeyAscii, "alpha")

End Sub

Private Sub OrderTabSentry_Enter()

        Call TaxonChange(MammalForm.Order)

End Sub

Private Sub Family_KeyPress(ByVal KeyAscii As MSForms.ReturnInteger)

        KeyAscii = FilterKeypress(KeyAscii, "alpha")

End Sub

Private Sub FamilyTabSentry_Enter()

        Call TaxonChange(MammalForm.Family)

End Sub


Private Sub Genus_KeyPress(ByVal KeyAscii As MSForms.ReturnInteger)

        KeyAscii = FilterKeypress(KeyAscii, "alpha")

End Sub

Private Sub GenusTabSentry_Enter()

        Call TaxonChange(MammalForm.Genus)
        
End Sub

Private Sub SpeciesTabSentry_Enter()

        Call SpeciesChecker
        If SubSpecies.Enabled Then SubSpecies.SetFocus
        
End Sub

Private Sub Species_KeyPress(ByVal KeyAscii As MSForms.ReturnInteger)

    KeyAscii = FilterKeypress(KeyAscii, "lowercase")

End Sub


' ******************************
' * DataFrame coding follows:  *
' ******************************

Private Sub WriteEntry_Click()

' Now has two functions - either writes a data line or loads the relevant subform
Dim DataString$

If WriteEntry.Caption = "Write Data" Then
    DataString = UnitCat.Value & Chr(9) & DataValue.Value & Chr(9) & Chr(9) & Chr(9) & Chr(9) & Chr(9) & Chr(9) & Chr(9) & Chr(9) & Chr(9) & Chr(9)
    Call ExportData(DataString)
    Call NonLocDataClear
ElseIf WriteEntry.Caption = "Continue..." Then
    Debug.Print (SelectedTypeSettings.UnitCat.List(0))
    Load SelectedTypeSettings.SubFormName
    SelectedTypeSettings.SubFormName.Show
End If

'and there is also the possibility that we got here through
'an unmined reference entry, in which case we need a higher
'level reset
If GuttingStatus.ListIndex = 2 Then SourceFrameReset.Value = True
    
End Sub

Private Sub DataType_Click()

'restore to initial state - can't use clearnonlocdata because
' that resets the datatype itself to -1.

'disable selection of the "---" separators
If DataType.Value = "---" Then
    DataType.ListIndex = DataType.ListIndex + 1
End If


DataValue.Value = ""
DataValue.Enabled = False
DataRange.Value = ""
UnitCat.Clear
Call FrameEnable(MammalForm.DataDetails, False)
WriteEntry.Enabled = False
WriteEntry.Caption = "Write Data"

'Load settings for the DataDetails frame
If DataType.ListIndex <> -1 Then
    Call LoadDataSettings(DataType.Value)
    UnitCat.Enabled = True
End If

End Sub
Private Sub UnitCat_click()

'disable selection of the "---" separators
If UnitCat.Value = "---" Then
    UnitCat.ListIndex = UnitCat.ListIndex + 1
End If

'reset the DataValue and lock it down
DataValue.Value = ""
DataValue.Enabled = False
Call FrameEnable(MammalForm.DataDetails, False)

' unlock DataValue if both a valid unit has been chosen
' and the selection of DataType is not a subform type.
If UnitCat.ListIndex <> -1 Then DataValue.Enabled = True

End Sub


Private Sub DataValue_Change()

If ContentCheck(DataValue.Text) Then
    Call FrameEnable(MammalForm.DataDetails, True)
    Call AllDataDetails
Else
    Call FrameEnable(MammalForm.DataDetails, False)
    WriteEntry.Enabled = False
End If

End Sub

Private Sub DataValue_KeyPress(ByVal KeyAscii As MSForms.ReturnInteger)

KeyAscii = FilterKeypress(KeyAscii, "0123456789.yYeEsSnNoO")
' allows yes/no info  - for migratory behaviour - bit of a kludge
' it would be neater to specify a datasetting that gives the allowable
' characters for data value entry for each data type

End Sub

Private Sub DataRange_KeyPress(ByVal KeyAscii As MSForms.ReturnInteger)

KeyAscii = FilterKeypress(KeyAscii, "0123456789.,")

End Sub

' ********************************
' * DataDetails coding follows:  *
' ********************************

' These calls should check to see if all the details are provided
' before enabling the write/continue button.


Private Sub Details_1_BeforeUpdate(ByVal Cancel As MSForms.ReturnBoolean)

Call AllDataDetails

End Sub

Private Sub Details_2_BeforeUpdate(ByVal Cancel As MSForms.ReturnBoolean)

Call AllDataDetails

End Sub

Private Sub Details_3_BeforeUpdate(ByVal Cancel As MSForms.ReturnBoolean)

Call AllDataDetails

End Sub

Private Sub Details_3_KeyPress(ByVal KeyAscii As MSForms.ReturnInteger)

If Details_3_Label.Caption = "Sample Size" Then KeyAscii = FilterKeypress(KeyAscii, "numeric")

End Sub

Private Sub Details_4_BeforeUpdate(ByVal Cancel As MSForms.ReturnBoolean)

Call AllDataDetails

End Sub

Private Sub Details_5_BeforeUpdate(ByVal Cancel As MSForms.ReturnBoolean)

Call AllDataDetails

End Sub

Private Sub Details_6_BeforeUpdate(ByVal Cancel As MSForms.ReturnBoolean)

If Details_6_Label.Caption = "Life Stage" And Details_6.Value <> "Weanling" Then
    If Details_7_Label.Caption = "Weanling Definition" Then Details_7.ListIndex = 0
End If

Call AllDataDetails

End Sub

Private Sub Details_7_BeforeUpdate(ByVal Cancel As MSForms.ReturnBoolean)

'disable fuckwittery
If Details_7.Value = "---" Then
    Details_7.ListIndex = Details_7.ListIndex + 1
End If

Call AllDataDetails

End Sub

Private Sub LatTog_Click()

With LatTog
    If .Value Then
        .Caption = "S"
    Else
        .Caption = "N"
    End If
End With

End Sub

Private Sub LongTog_Click()
With LongTog
    If .Value Then
        .Caption = "W"
    Else
        .Caption = "E"
    End If
End With
End Sub

Private Sub NoLatLong_Click()

Call LatLongToggler
        
End Sub

Private Sub DecDeg_Click()

Call LatLongToggler
        
End Sub
Private Sub DMSDeg_Click()

Call LatLongToggler
        
End Sub

Private Sub Latitude_KeyPress(ByVal KeyAscii As MSForms.ReturnInteger)

KeyAscii = FilterKeypress(KeyAscii, "0123456789.:")

End Sub

Private Sub Longitude_KeyPress(ByVal KeyAscii As MSForms.ReturnInteger)

KeyAscii = FilterKeypress(KeyAscii, "0123456789.:")

End Sub
