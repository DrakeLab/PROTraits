Attribute VB_Name = "MammalFormFunctions"
Option Explicit

Public MamFormDir As String 'stores the working directory
Public SelectedTypeSettings As DataTypeSettings
Public UserRefID As Integer 'stores the EndNote reference number from pasted sources.

Type DataComboSet
    Name As String
    List As Variant
    Default As Integer
    NA As Boolean
End Type

Type DataTypeSettings
    'used to store the contents and setup of menus in
    'DataFrame and hence loaded into the form.
    UnitCat As DataComboSet
    SubForm As Boolean
    SubFormName As Object
    DataValue As DataComboSet
    DataValueAsText As Boolean 'toggles between list like and textbox like behaviour
    DataRangeValue As String
    Details_1 As DataComboSet
    Details_2 As DataComboSet
    Details_3 As DataComboSet
    Details_4 As DataComboSet
    Details_5 As DataComboSet
    Details_6 As DataComboSet
    Details_7 As DataComboSet
End Type
   
Type TaxonXref
    'Taxonomic data in the files is stored as records of
    'this type. The name is displayed in the form and the
    'start and stop numbers indicate the range of the relevant
    'records in the next rank down. For the Species data Startnum
    'stores the startBYTE and stopnum the LENGTH of the data field.
    'for recalling the valid species details from store in a binary
    'file - because startBYTE goes >32768 this uses a LONG in order
    'to use just one UDT for recall.
    Name As String * 30
    StartNum As Long
    StopNum As Integer
End Type

Public Sub Button1_Click()

'This is the launch button on the worksheet
'I can't work out how to rename it

Load MammalForm
MammalForm.Show

End Sub

Public Function ContentCheck(ByVal MyString As String) As Boolean


    ' call to check for at least some non-space content
    ' in a text string. Returns a boolean indicating whether
    ' content has been found.
    
    If Len(Trim(MyString)) = 0 Then
        ContentCheck = False
    Else
        ContentCheck = True
    End If

End Function

Public Sub ExportData(ByVal DataString As String)

    'This routine dumps the common information from MammalForm
    'into a tab delimited line along with the values passed
    'by the calling routine, which should be another tab delimited
    'text string containing varying other numbers of tab delited fields.
    
    
    
    With MammalForm
        'set up the file and open it for append
        Dim DataStamp$, DataFile$
        DataStamp = .UserID.Text & "_trdata_" & Format(Date, "ddmmyy")
        DataFile = MamFormDir & Application.PathSeparator & DataStamp & ".txt"
        
        'Add row headers as the first line if the file does not exist
        If Dir(DataFile) = "" Then
            Open DataFile For Append As #1
            Print #1, "1"; Chr(9); "2"; Chr(9); "3"; Chr(9); "4"; Chr(9); "5"; Chr(9); "6"; Chr(9); _
            "7"; Chr(9); "8"; Chr(9); "9"; Chr(9); "10"; Chr(9); "11"; Chr(9); "12"; Chr(9); _
            "13"; Chr(9); "14"; Chr(9); "15"; Chr(9); "16"; Chr(9); "17"; Chr(9); _
            "18"; Chr(9); "19"; Chr(9); "20"; Chr(9); "21"; Chr(9); "22"; Chr(9); _
            "23"; Chr(9); "24"; Chr(9); "25"; Chr(9); "26"; Chr(9); "27"; Chr(9); _
            "28"; Chr(9); "29"; Chr(9); "30"; Chr(9); "31"; Chr(9); "32"; Chr(9); _
            "33"; Chr(9); "34"; Chr(9); "35"; Chr(9); "36" ' oddly, ending the line with ; suppresses the CR
        Else
            Open DataFile For Append As #1
        End If
        
        'manipulate some of the control contents that need to be changed
        Dim UserUniqueRef$, LatLongSys$, SignedLat$, SignedLong$
        UserUniqueRef = .UserID.Text & "_tt_" & UserRefID
        If .DecDeg Then LatLongSys = "DEC"
        If .DMSDeg Then LatLongSys = "DMS"
        If .NoLatLong Then LatLongSys = "Unspecified"
        If .LatTog Then SignedLat = "-" & .Latitude.Text Else SignedLat = .Latitude.Text
        If .LongTog Then SignedLong = "-" & .Longitude.Text Else SignedLong = .Longitude.Text
        
        If .GuttingStatus.ListIndex = 2 Then
            Print #1, UserUniqueRef; Chr(9); .GuttingStatus.Value; Chr(9); _
                .GuttingNotes.Text; Chr(9); .SourceID.Text; "ID " & UserUniqueRef & "\r" & _
                "GT " & .GuttingStatus.Value & ": " & .GuttingNotes.Text & "\r"
        Else
            Print #1, UserUniqueRef; Chr(9); .GuttingStatus.Value; Chr(9); _
                .GuttingNotes.Text; Chr(9); .SourceID.Text; "ID " & UserUniqueRef & "\r" & _
                "GT " & .GuttingStatus.Value & ": " & .GuttingNotes.Text & "\r" & _
                "TX" & .Order.Value & ": " & .Family.Value & "\r"; _
                Chr(9); .Order.Value; Chr(9); .Family.Value; Chr(9); .Genus.Value; _
                Chr(9); .Species.Value; Chr(9); .SubSpecies.Text; Chr(9); _
                .Authority.Text; Chr(9); .Details_1.Value; Chr(9); .Details_2.Value; _
                Chr(9); .Details_3.Value; Chr(9); .Details_4.Value; _
                Chr(9); .Details_5.Value; Chr(9); .Details_6.Value; _
                Chr(9); .Details_7.Value; Chr(9); .Location.Text; _
                Chr(9); LatLongSys; Chr(9); SignedLat; Chr(9); SignedLong; Chr(9); _
                .DataRange.Text; Chr(9); .DataType.Value; Chr(9); DataString; Chr(9); DataStamp
        End If
        
        Close #1
    
    End With

End Sub

Public Sub FrameEnable(FrameObject As Object, toggle As Boolean)

    'Takes any given frame on a form and disables the frame
    'and its children controls.

    'first disable the frame itself
    
    With FrameObject
            If toggle = True Then
                .BorderColor = &H80000006
                .ForeColor = &H80000006
            ElseIf toggle = False Then
                .BorderColor = &H80000011
                .ForeColor = &H80000011
            End If
    End With
    
    'then its control collection
    Dim ChildControl As Control
    For Each ChildControl In FrameObject.Controls
        With ChildControl
                If toggle = True Then
                        ChildControl.Enabled = True
                ElseIf toggle = False Then
                        ChildControl.Enabled = False
                End If
        End With
    Next ChildControl


End Sub

Public Sub TaxonChange(HigherTaxon As Control)

    'Note different vartypes for start and stop...
    Dim LowerCtrls&, StartIndex&, StopIndex%, LowerTaxa As Variant, LowerTaxon As Control
    
    
    Select Case HigherTaxon.Name
        Case "Order"
            ReDim LowerTaxa(3)
            LowerTaxa = Array("Family", "Genus", "Species")
        Case "Family"
            ReDim LowerTaxa(2)
            LowerTaxa = Array("Genus", "Species")
        Case "Genus"
            ReDim LowerTaxa(1)
            LowerTaxa = Array("Species")
    End Select
    
    ' takes the name of one of the taxonomy combo boxes and processes
    ' the clearing and loading of taxonomy lists
    
    ' first clear and disable any downstream taxa based on the taxon changed
    ' need to both .clear and .text = ""
    
    For LowerCtrls = 0 To UBound(LowerTaxa)
        With MammalForm.Controls.Item(LowerTaxa(LowerCtrls))
            .Clear
            .Text = ""
            .Enabled = False
        End With
    Next
    
    MammalForm.SubSpecies.Text = "Unspecified"
    MammalForm.Authority.Text = "Unspecified"
    MammalForm.ValidDisplay.Text = ""
    
    ' lockout and reset DataFrame
    'Call FrameEnable(MammalForm.DataFrame, False)
    'MammalForm.DataFrameReset.Value() = True
    
       
    ' Three possible outcomes:
    ' i) a list matching entry,
    ' ii) a non-list matching entry,
    ' iii) an empty box
    ' but the previous code sets the default state for iii)
    
    Set LowerTaxon = MammalForm.Controls.Item(LowerTaxa(0))
    
    With MammalForm
        If HigherTaxon.ListIndex <> -1 Then
            ' i) we have a match
            ' load the LowerTaxon box listing and enable the control
            StartIndex = CInt(HigherTaxon.List(HigherTaxon.ListIndex, 1))
            StopIndex = CInt(HigherTaxon.List(HigherTaxon.ListIndex, 2))
            LowerTaxon.Enabled = True
            LowerTaxon.List() = LoadTaxonArray(LowerTaxon.Name, StartIndex, StopIndex)
            LowerTaxon.SetFocus
        ElseIf HigherTaxon.ListIndex = -1 And ContentCheck(HigherTaxon.Value) Then
            'ii) we have a non-matching entry
            LowerTaxon.Enabled = True
            LowerTaxon.SetFocus
        Else
            If MammalForm.SubSpecies.Enabled Then MammalForm.SubSpecies.SetFocus
        End If
    End With


End Sub

Public Sub AllDataDetails()

'checks for content and choices in all controls in DataDetails before
'enabling or disabling the write control

'This could definately be optimized

With MammalForm
    ' if all the DataDetails frames have content then...
    If .Details_1.ListIndex <> -1 And .Details_2.ListIndex <> -1 And _
    ContentCheck(.Details_3.Value) And .Details_4.ListIndex <> -1 And _
    .Details_5.ListIndex <> -1 And .Details_6.ListIndex <> -1 And _
    .Details_7.ListIndex <> -1 Then
        ' if the datatype is a subform, enable WriteEntry...
        If SelectedTypeSettings.SubForm Then
            .WriteEntry.Enabled = True
        ' or if there is a valid unit and DataValue, enable WriteEntry ...
        'DESIGN NOTE - should this line be necessary?
        ElseIf .UnitCat.ListIndex <> -1 And ContentCheck(.DataValue.Text) Then
            .WriteEntry.Enabled = True
        'otherwise, disable WriteEntry
        Else
            .WriteEntry.Enabled = False
        End If
    Else
        .WriteEntry.Enabled = False
    End If
End With

End Sub

Public Sub LoadDataSettings(WhichType As String)

    
    'for each datatype this code specifies the following for each control in data details:
    ' • the list of possible options
    ' • the default - note that if no default (listindex = -1) then
    '   the coding of AllDataDetails above requires a choice to be made before
    '   enabling the WriteEntry control
    ' • whether or not that control is locked. Note that unless a disabled control
    '   is given some kind of placeholder value ("NA") then WriteEntry _cannot_
    '   be enabled.
    ' • the caption on the control.
    
        
    Dim SelectedTypeSettings As DataTypeSettings, UnitType$
    Dim ListEntries As Integer, ListMaxChar As Integer

    SelectedTypeSettings = SettingsDefinition(WhichType) 'Get the correct setup details
    
    With MammalForm
        'skip this unless the frame is actually enabled. Gets rid of slight lag and flicker
        'also need to skip loading the states if gutting = "unmined"
        If .DataFrame.Enabled = False Or .GuttingStatus.ListIndex = 2 Then Exit Sub

        'loop through the comboboxes in data details implementing the setup
        Dim DetailCtrl As Control, DetailCtrlLabel As Control
        Dim SettingsChunk As DataComboSet, DataControls As Integer
        
        For DataControls = 1 To 9 ' hoik each set of datacomboset out of the type setttings in turn
        
        Select Case DataControls ' and normal settings
            Case 1 'SourceType
                Set DetailCtrl = .Details_1
                Set DetailCtrlLabel = .Details_1_Label
                SettingsChunk = SelectedTypeSettings.Details_1
            Case 2 'Metric
                Set DetailCtrl = .Details_2
                Set DetailCtrlLabel = .Details_2_Label
                SettingsChunk = SelectedTypeSettings.Details_2
            Case 3 'SampleSize
                Set DetailCtrl = .Details_3
                Set DetailCtrlLabel = .Details_3_Label
                SettingsChunk = SelectedTypeSettings.Details_3
            Case 4 'Captivity
                Set DetailCtrl = .Details_4
                Set DetailCtrlLabel = .Details_4_Label
                SettingsChunk = SelectedTypeSettings.Details_4
            Case 5 'Sex
                Set DetailCtrl = .Details_5
                Set DetailCtrlLabel = .Details_5_Label
                SettingsChunk = SelectedTypeSettings.Details_5
            Case 6 'Lifestage
                Set DetailCtrl = .Details_6
                Set DetailCtrlLabel = .Details_6_Label
                SettingsChunk = SelectedTypeSettings.Details_6
            Case 7 'Unused
                Set DetailCtrl = .Details_7
                Set DetailCtrlLabel = .Details_7_Label
                SettingsChunk = SelectedTypeSettings.Details_7
            Case 8 'Units
                Set DetailCtrl = .UnitCat
                Set DetailCtrlLabel = .UnitCatLabel
                SettingsChunk = SelectedTypeSettings.UnitCat
            Case 9 'DataValue
                Set DetailCtrl = .DataValue
                Set DetailCtrlLabel = .DataValueLabel
                SettingsChunk = SelectedTypeSettings.DataValue
        End Select
            
            With SettingsChunk
                DetailCtrl.List() = .List
                DetailCtrl.ListIndex = .Default
                If .NA Then
                    DetailCtrl.ForeColor = &H80000011
                    DetailCtrl.Locked = True
                    DetailCtrlLabel.ForeColor = &H80000011
                    ElseIf Not .NA Then
                    DetailCtrl.ForeColor = &H80000008
                    DetailCtrl.Locked = False
                    DetailCtrlLabel.ForeColor = &H80000008
                End If
                DetailCtrlLabel.Caption = .Name
                'this could be usefully placed in its own sub for use on any listwidth
                ListMaxChar = 0
                For ListEntries = 0 To UBound(.List)
                    If Len(.List(ListEntries)) > ListMaxChar Then ListMaxChar = Len(.List(ListEntries))
                Next ListEntries
                If 8 * ListMaxChar > 90 Then
                    DetailCtrl.ListWidth = 8 * ListMaxChar ' the 8 is guess work - seems to be about right with Geneva
                Else
                    DetailCtrl.ListWidth = 90
                End If
            End With
            
            Next DataControls
                
        'Set up fine tuning on datavalue
        'i.e. if it is locked down then enable the other controls
        If SelectedTypeSettings.DataValue.NA Then
            Call FrameEnable(MammalForm.DataDetails, True)
        End If
        
        'and whether it behaves as a list box or text box
        If SelectedTypeSettings.DataValueAsText Then
            .DataValue.ShowDropButtonWhen = 0
            .DataValue.List() = Array("")
            .DataValue.Style = 0
        ElseIf Not SelectedTypeSettings.DataValueAsText Then
            .DataValue.ShowDropButtonWhen = 2
            .DataValue.List() = SelectedTypeSettings.DataValue.List
            .DataValue.Style = 2
            .DataRange.Enabled = False
            .DataRange.Value = "NA"
        End If
        
        ' Set up the Range box
        If SelectedTypeSettings.DataRangeValue = "Unspecified" Then
            .DataRange.Enabled = True
            .DataRange.Value = SelectedTypeSettings.DataRangeValue
        ElseIf SelectedTypeSettings.DataRangeValue = "NA" Then
            .DataRange.Enabled = False
            .DataRange.Value = SelectedTypeSettings.DataRangeValue
        End If
        
        
        ' check for subform
        If SelectedTypeSettings.SubForm Then
                .UnitCat.AddItem ("SubForm...")
                .UnitCat.ListIndex = 0
                .UnitCat.Enabled = False
                .DataValue.Enabled = False
                .WriteEntry.Caption = "Continue..."
                .WriteEntry.Enabled = False
                Call FrameEnable(MammalForm.DataDetails, True)
                Call AllDataDetails
        End If

    End With

End Sub



Public Function LoadTaxonArray(Rank As String, ByVal StartRec As Integer, ByVal StopRec As Integer)

    'function loads records of type TaxonXref from files
    Dim CurrentTaxon As TaxonXref, XrefFile$, taxa%
    
    XrefFile = MamFormDir & Application.PathSeparator & "Mammal Form Data" & Application.PathSeparator
    Select Case Rank
        Case "Order"
            XrefFile = XrefFile & "ORDERS"
        Case "Family"
            XrefFile = XrefFile & "FAMILIES"
        Case "Genus"
            XrefFile = XrefFile & "GENERA"
        Case "Species"
            XrefFile = XrefFile & "SPECIES"
    End Select
    
    Open XrefFile For Random As #1 Len = Len(CurrentTaxon)
    'set up the array for the correct # of records
    ReDim TaxonArray((StopRec - StartRec), 3)
    
    'load the records from file into TaxonArray
    For taxa = 0 To StopRec - StartRec
    Get #1, StartRec + taxa, CurrentTaxon
        TaxonArray(taxa, 0) = Trim(CurrentTaxon.Name)
        TaxonArray(taxa, 1) = CurrentTaxon.StartNum
        TaxonArray(taxa, 2) = CurrentTaxon.StopNum
    Next taxa
    
    Close #1    ' Close file.
    
    LoadTaxonArray = TaxonArray 'return array

End Function


Public Function LoadValidDetails(StartByte&, NumBytes%) As String

    Dim DetailsString$, DetailsFile$
    DetailsString = String(NumBytes, " ") 'creates string the right length to hold the details - can't dim a fixed length using a variable
    DetailsFile = MamFormDir & Application.PathSeparator & "Mammal Form Data" & Application.PathSeparator & "VALID.DETAILS"
    
    
    Open DetailsFile For Binary As #1
    Get #1, StartByte, DetailsString
    Close #1

    LoadValidDetails = DetailsString
    
End Function

Public Function SourceFrameChecking()
    
'Assesses the state of the source frame
With MammalForm

If Not .SourceID = "" And Not .UserID = "" And Not .GuttingStatus = "" And Not .GuttingNotes = "" Then
    If .GuttingStatus = "Gutting" Or .GuttingStatus = "Partial" Then
        Call FrameEnable(MammalForm.TaxonomyFrame, True)
        .Species.Enabled = False
        .Genus.Enabled = False
        .Family.Enabled = False
    ElseIf .GuttingStatus = "Unmined" Then
        .WriteEntry.Enabled = True
    End If
Else
    Call FrameEnable(MammalForm.TaxonomyFrame, False)
    MammalForm.TaxonomyFrameReset = True
End If

End With


End Function
Public Function ValidateSource()

'checks for acceptable SourceID content
With MammalForm

    'only fires on alteration of source id content as a result of before update
    Dim RefNoLoc%
    On Error GoTo BadRefError
        RefNoLoc = InStr(.SourceID.Text, "$")
        UserRefID = CInt(Right(MammalForm.SourceID.Text, Len(MammalForm.SourceID.Text) - RefNoLoc))
        .SourceID.Text = Left(.SourceID.Text, RefNoLoc - 1)
    On Error GoTo 0
            
End With

Exit Function

BadRefError:

'error handler for bad pastes into the source field
Dim MsgBoxVar As Integer
Beep
MsgBoxVar = MsgBox("An invalid entry has been made in the Source field." & Chr(13) & Chr(13) & _
                   "Please paste (ctrl+v) a reference from EndNote," & Chr(13) & _
                   "that has been copied as a formatted reference" & Chr(13) & _
                   "(ctrl+k) using the 'MammalForm' Style." & Chr(13) & Chr(13) & _
                   "See the manual for details.", 16, "Source Misformat!")
MammalForm.SourceID.Text = ""
MammalForm.SourceID.SetFocus
UserRefID = 0

End Function

Public Sub MamFormInit()

'does preliminary actions when form is initialized
'DESIGN NOTE: could do LOF checks here on datafiles to look for fubars
'Order 936 bytes
'Family 6876 bytes
'Genus 91944 bytes
'Species 845028 bytes
'Valid.Details 575927 bytes

With MammalForm
    
    'record where the workbook is saved
    Workbooks("MammalForm v2.1.xls").Activate
    MamFormDir = ActiveWorkbook.Path
    
    'load the basic lists
    .DataType.List() = Array("Body Mass", "Adult Limb Length", "Head-Body Length", _
                "---", "Gestation Length", "Litter size", "Teat Number", _
                 "Litters Per Year", "Interbirth Interval", "---", _
                 "Age at Eye Opening", "Weaning Age", "Sexual Maturity Age", _
                 "Dispersal Age", "Age at First Birth", "Growth Data", _
                "Average Lifespan", "Maximum Longevity", "Mortality Data", _
                 "Activity Cycle", "Migratory Behaviour", "---", "Metabolic Rate", _
                 "Diet", "Ranging Behaviour", "Population Density", _
                 "Group Composition & Size", "Habitat Layer")
    .Order.List() = LoadTaxonArray("Order", 1, 26)
    .UserID.List() = Array("AndyP", "ChrisC", "DaveO", "GeoM", "GuyC", "JohnG", "JonB", "KateJ", "MarcelC", "MikeH", "NickI", "RichG", "SamP")
    .GuttingStatus.List() = Array("Gutting", "Partial", "Unmined")
    .GuttingStatus.ListIndex = 0
    
    'lock down the frames below source until data has been entered
    Call FrameEnable(MammalForm.TaxonomyFrame, False)
    Call FrameEnable(MammalForm.DataFrame, False)
    Call FrameEnable(MammalForm.DataDetails, False)
        
    'go to the start point
    .UserID.SetFocus
    
   
End With
End Sub

Public Function SettingsDefinition(SelectedType As String) As DataTypeSettings

    ' when a data type is selected, LoadDataSettings is called, which in turn
    ' calls this function to return the DataTypeSettings relevant to the choice
    ' of data type. Hence the big dull case select. If called with aan invalid
    ' string then this acts to load defaults. I've used "reset".
        
    With SelectedTypeSettings
    
    'Load the User Type with the defaults, which can then be overridden if needed by the case select
            .UnitCat.Name = "Units"
            .UnitCat.List = Array("") ' no default
            .UnitCat.NA = False
            .UnitCat.Default = -1
            .DataValue.Name = "Data Value"
            .DataValue.List = Array("")
            .DataValue.NA = False
            .DataValue.Default = -1
            .DataValueAsText = True
            .DataRangeValue = "Unspecified"
            .SubForm = False
            Set .SubFormName = Nothing
            .Details_1.Name = "Source Type"
            .Details_1.List = Array("Primary", "Secondary")
            .Details_1.Default = -1
            .Details_1.NA = False
            .Details_2.Name = "Measure"
            .Details_2.List = Array("Mean", "Median", "Mode", "Midrange", "Unspecified Value", "Maximum", "Minimum")
            .Details_2.Default = -1
            .Details_2.NA = False
            .Details_3.Name = "Sample Size"
            .Details_3.List = Array("Unspecified")
            .Details_3.Default = 0
            .Details_3.NA = False
            .Details_4.Name = "Captivity"
            .Details_4.List = Array("Captive", "Wild", "Provisioned", "Unspecified")
            .Details_4.Default = -1
            .Details_4.NA = False
            .Details_5.Name = "Sex"
            .Details_5.List = Array("Male", "Female", "Both", "Unspecified")
            .Details_5.Default = -1
            .Details_5.NA = False
            .Details_6.Name = "Life Stage"
            .Details_6.List = Array("Adult", "Weanling", "Neonate", "Unspecified")
            .Details_6.Default = -1
            .Details_6.NA = False
            .Details_7.Name = "Other variables"
            .Details_7.List = Array("NA")
            .Details_7.Default = 0
            .Details_7.NA = True
    
    
    
    Select Case SelectedType
        Case "Body Mass"
            .UnitCat.List = Array("grams", "kilograms")
            .Details_1.List = Array("Primary", "Secondary", "Extrapolated")
            .Details_7.Name = "Weanling Definition"
            .Details_7.List = Array("Not Applicable", "---", "End of Weaning", "Nutritionally Independent", "First Solid Food", "Last Observed Nursing", "Age at First Flight", "Age at Pouch Exit", "Length of Teat Attachment", "Unspecified")
            .Details_7.NA = False
            .Details_7.Default = -1
        Case "Head-Body Length"
            .UnitCat.List = Array("millimetres", "centimetres", "metres")
            .Details_1.List = Array("Primary", "Secondary", "Extrapolated")
            .Details_7.Name = "Weanling Definition"
            .Details_7.List = Array("Not Applicable", "---", "End of Weaning", "Nutritionally Independent", "First Solid Food", "Last Observed Nursing", "Age at First Flight", "Age at Pouch Exit", "Length of Teat Attachment", "Unspecified")
            .Details_7.NA = False
            .Details_7.Default = -1
        Case "Adult Limb Length"
            .UnitCat.List = Array("millimetres", "centimetres", "metres")
            .Details_1.List = Array("Primary", "Secondary", "Extrapolated")
            .Details_6.Default = 0 'Adult
            .Details_6.NA = True
            .Details_7.Name = "Limb Section"
            .Details_7.List = Array("foot", "shin", "femur", "foot + shin", "foot + shin + femur", "shin + femur", "---", "hand", "forearm", "humerus", "hand +forearm", "hand + forearm + humerus", "forearm + humerus")
            .Details_7.NA = False
            .Details_7.Default = -1
        Case "Gestation Length"
            .UnitCat.List = Array("days", "weeks", "months", "years")
            .Details_3.Name = "Date of Birth"
            .Details_3.Default = -1
            .Details_5.Name = "Start Point"
            .Details_5.List = Array("First observed copulation", "Fertilization", "Implantation", "Palpably pregnant", "Laying", "Unspecified")
            .Details_6.Name = "End Point"
            .Details_6.List = Array("Fertilization", "Implantation", "Palpably pregnant", "Birth", "End of obligate teat attachment ", "First pouch emergence ", "Permanent exit from pouch ", "Hatching ", "Unspecified")
            .Details_7.Name = "Growth type"
            .Details_7.List = Array("Active", "Inactive", "Uncertain", "Unspecified")
            .Details_7.NA = False
            .Details_7.Default = -1
        Case "Age at Eye Opening"
            .UnitCat.List = Array("hours", "days", "weeks", "months")
            .Details_6.Default = 2 'neonate
            .Details_6.NA = True
        Case "Litter size"
            .UnitCat.List = Array("NA")
            .UnitCat.NA = True
            .UnitCat.Default = 0
            .Details_5.Default = 1 'Female
            .Details_5.NA = True
            .Details_6.Default = 0 'Adult
            .Details_6.NA = True
            .Details_7.Name = "Count made:"
            .Details_7.List = Array("At birth", "After birth", "Before birth")
            .Details_7.NA = False
            .Details_7.Default = -1
        Case "Teat Number"
            .UnitCat.List = Array("NA")
            .UnitCat.NA = True
            .UnitCat.Default = 0
            .Details_5.Default = 1 'Female
            .Details_5.NA = True
            .Details_6.Default = 0 'Adult
            .Details_6.NA = True
            Dim TeatArray(25), MaxTeats As Integer
            TeatArray(0) = "Unspecified"
            For MaxTeats = 0 To 24
                TeatArray(MaxTeats + 1) = MaxTeats
            Next MaxTeats
            .Details_7.Name = "# Functional Teats"
            .Details_7.List = TeatArray
            .Details_7.Default = -1
            .Details_7.NA = False
         Case "Growth Data", "Mortality Data" '# BOOLEANs so uses
            .UnitCat.List = Array("NA")
            .UnitCat.NA = True
            .UnitCat.Default = 0
            .DataValue.List = Array("Yes")
            .DataValue.NA = True
            .DataValue.Default = 0
            .DataRangeValue = "NA"
            .Details_1.List = Array("NA")
            .Details_1.Default = 0
            .Details_1.NA = True
            .Details_2.List = Array("NA")
            .Details_2.Default = 0
            .Details_2.NA = True
            .Details_3.List = Array("NA")
            .Details_3.Default = 0
            .Details_3.NA = True
            .Details_4.List = Array("NA")
            .Details_4.Default = 0
            .Details_4.NA = True
            .Details_5.List = Array("NA")
            .Details_5.Default = 0
            .Details_5.NA = True
            .Details_6.List = Array("NA")
            .Details_6.Default = 0
            .Details_6.NA = True
        Case "Weaning Age"
            .UnitCat.List = Array("days", "weeks", "months", "years")
            .Details_6.Default = 1 'Weanling
            .Details_6.NA = True
            .Details_7.Name = "WA Defn."
            .Details_7.List = Array("Weaning/Lactation Length", "Nutritionally Independent", "First Solid Food", "Last Observed Nursing", "Age at First Flight", "Age at Pouch Exit", "Length of Teat Attachment", "Unspecified")
            .Details_7.NA = False
            .Details_7.Default = -1
        Case "Sexual Maturity Age"
            .UnitCat.List = Array("days", "weeks", "months", "years")
            .Details_6.Default = 0 'Adult
            .Details_6.NA = True
            .Details_7.Name = "SMA Defn."
            .Details_7.List = Array("Physically Sexually Mature", "Age at First Estrus", "Age at Spermatogenesis", "Age at First Mating", "Age at First Pregnancy", "Age at Testes Descent", "Unspecified")
            .Details_7.NA = False
            .Details_7.Default = -1
        Case "Age at First Birth"
            .UnitCat.List = Array("days", "weeks", "months", "years")
            .Details_5.Default = 1 'Female
            .Details_5.NA = True
            .Details_6.Default = 0 'Adult
            .Details_6.NA = True
        Case "Litters Per Year"
            .UnitCat.List = Array("NA")
            .UnitCat.Default = 0
            .UnitCat.NA = True
            .Details_5.Default = 1 'Female
            .Details_5.NA = True
            .Details_6.Default = 0 'Adult
            .Details_6.NA = True
        Case "Interbirth Interval"
            .UnitCat.List = Array("days", "weeks", "months", "years")
            .Details_5.Default = 1 'Female
            .Details_5.NA = True
            .Details_6.Default = 0 'Adult
            .Details_6.NA = True
            .Details_7.Name = "IBI Defn."
            .Details_7.List = Array("After successful litter", "After failed litter", "Unspecified")
            .Details_7.NA = False
            .Details_7.Default = -1
        Case "Average Lifespan"
            .UnitCat.List = Array("days", "weeks", "months", "years")
            .Details_6.List = Array("Embryo", "Neonate", "Weanling", "Juvenile", "Before Sexual Maturity", "After Sexual Maturity", "Before First Birth", "At First Birth", "After First Birth", "Adult", "Unspecified")
            .Details_7.Name = "Lifespan Estimation"
            .Details_7.List = Array("Observation", "Capture-Recapture", "Projected", "Unspecified")
            .Details_7.NA = False
            .Details_7.Default = -1
        Case "Metabolic Rate"
            .UnitCat.List = Array("Subform")
            .UnitCat.NA = True
            .UnitCat.Default = 0
            .SubForm = True
            Set .SubFormName = MetRate
            .Details_6.Default = 0 'Adult
            .Details_6.NA = True
        Case "Diet"
            .UnitCat.List = Array("Subform")
            .UnitCat.NA = True
            .UnitCat.Default = 0
            .DataRangeValue = "NA"
            .SubForm = True
            Set .SubFormName = New Diet
            .Details_2.List = Array("NA")
            .Details_2.Default = 0
            .Details_2.NA = True
            .Details_4.Default = 1 'wild
            .Details_4.NA = True
            .Details_6.Default = 0 'adult
            .Details_6.NA = True
        Case "Ranging Behaviour"
            .UnitCat.List = Array("Subform")
            .UnitCat.NA = True
            .UnitCat.Default = 0
            .SubForm = True
            Set .SubFormName = RangeSize
            .Details_4.List = Array("Unspecified", "Wild", "Provisioned")
            .Details_6.Default = 0 'adult
            .Details_6.NA = True
        Case "Population Density"
            .UnitCat.List = Array("Subform")
            .UnitCat.NA = True
            .UnitCat.Default = 0
            .SubForm = True
            Set .SubFormName = PopDens
            .Details_4.List = Array("Unspecified", "Wild", "Provisioned")
            .Details_6.Default = 0 'adult
            .Details_6.NA = True
        Case "Group Composition & Size"
            .UnitCat.List = Array("Subform")
            .UnitCat.NA = True
            .UnitCat.Default = 0
            .SubForm = True
            Set .SubFormName = GroupComp
            .Details_4.List = Array("Unspecified", "Wild", "Provisioned")
            .Details_6.Default = 0 'adult
            .Details_6.NA = True
        Case "Activity Cycle"
            .UnitCat.List = Array("NA")
            .UnitCat.NA = True
            .UnitCat.Default = 0
            .DataValue.List = Array("Diurnal", "Crepuscular", "Nocturnal", "Diurnal/Crepuscular", "Nocturnal/Crepuscular", "Cathemeral")
            .DataValueAsText = False
            .DataRangeValue = "NA"
            .Details_2.List = Array("NA")
            .Details_2.Default = 0
            .Details_2.NA = True
            .Details_3.List = Array("NA") 'sample size not relevant
            .Details_3.Default = 0
            .Details_3.NA = True
            .Details_4.Default = 1
            .Details_4.NA = True
            .Details_6.Default = 0 'adult
            .Details_6.NA = True
        Case "Habitat Layer"
            .UnitCat.List = Array("Subform")
            .UnitCat.NA = True
            .UnitCat.Default = 0
            .DataRangeValue = "NA"
            .SubForm = True
            Set .SubFormName = HabLayer
            .Details_2.List = Array("NA")
            .Details_2.Default = 0
            .Details_2.NA = True
            .Details_4.List = Array("Unspecified", "Wild", "Provisioned")
            .Details_6.Default = 0 'adult
            .Details_6.NA = True
        Case "Dispersal Age"
            .UnitCat.List = Array("days", "weeks", "months", "years")
            .Details_6.List = Array("NA")
            .Details_6.Default = 0
            .Details_6.NA = True
        Case "Migratory Behaviour"
            .UnitCat.List = Array("NA", "---", "metres", "kilometres", "miles", "---", "m2", "hectares", "km2", "acres", "square miles")
            .Details_6.Default = 0 'adult
            .Details_6.NA = True
        Case "Maximum Longevity"
            .UnitCat.List = Array("days", "weeks", "months", "years")
            .Details_2.Default = 5
            .Details_2.NA = True
            .Details_6.Default = 0 'adult
            .Details_6.NA = True
            .Details_7.Name = "Longevity Estimation"
            .Details_7.List = Array("Observation", "Capture-Recapture", "Projected", "Unspecified")
            .Details_7.NA = False
        End Select
    End With
    
    'return the settings
    SettingsDefinition = SelectedTypeSettings

End Function

Public Sub NonLocDataClear()
    
    'used to enable location data to persist after write entry.
    'Loc Data is wiped by the reset button for data frame.
    
    With MammalForm
        .DataType.ListIndex = -1
        .UnitCat.Enabled = False
        .DataValue.Clear
        If .DataType.Enabled Then .DataType.SetFocus
        Call FrameEnable(MammalForm.DataDetails, False) 'lock data details
        Call LoadDataSettings("reset") ' restore the defaults in the main fields
        .WriteEntry.Enabled = False
        .WriteEntry.Caption = "Write Data"
        .DataRange.Text = ""
    End With

End Sub



Public Function FilterKeypress(ByVal KeyAscii As Integer, _
    Optional AcceptCharacters As String, _
    Optional DenyCharacters As String) As Integer
    
    Dim DetectSets As String
    
    If Len(AcceptCharacters) Then DetectSets = AcceptCharacters Else DetectSets = DenyCharacters
    
    Select Case DetectSets
        Case "numeric"
            DetectSets = "0123456789"
        Case "lowercase"
            DetectSets = "abcdefghijklmnopqrstuvwxyz"
        Case "uppercase"
            DetectSets = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        Case "alpha"
            DetectSets = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
        Case "alphanumeric"
            DetectSets = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
    End Select
    
    If Len(AcceptCharacters) Then AcceptCharacters = DetectSets Else DenyCharacters = DetectSets
    
    
    ' Accept by default
    FilterKeypress = KeyAscii

    ' Always accept delete
    If KeyAscii = 8 Then Exit Function

    ' If list of characters to accept has been specified, use it
    If Len(AcceptCharacters) Then
        If InStr(1, AcceptCharacters, Chr$(KeyAscii)) = 0 Then
            FilterKeypress = 0
            Beep
        End If

    ' Otherwise use the list of characters to deny
    ElseIf Len(DenyCharacters) Then
        If InStr(1, DenyCharacters, Chr$(KeyAscii)) > 0 Then
            FilterKeypress = 0
            Beep
        End If
    End If
End Function


Public Sub SpeciesChecker()

With MammalForm

    ' The status of the Species combobox controls the state of the DataFrame:
    ' the default state of the DataFrame is locked down...
    Call FrameEnable(MammalForm.DataFrame, False)
    .DataFrameReset.Value() = True
    .ValidDisplay.Text = ""
    
    'But is unlocked under the following states:
    If .Species.ListIndex <> -1 Then
        'i.e. W&R recognized species name chosen from the drop down
        'retrieve W&R valid binomial and details and enable DataFrame
        Dim ValidSpeciesDetails As String
        .ValidDisplay.Text = LoadValidDetails(.Species.List(.Species.ListIndex, 1), .Species.List(.Species.ListIndex, 2))
        Call FrameEnable(MammalForm.DataFrame, True)
        Call FrameEnable(MammalForm.DataDetails, False)
        Call LatLongToggler
        .UnitCat.Enabled = False
        .DataValue.Enabled = False
        .WriteEntry.Enabled = False
    ElseIf .Species.ListIndex = -1 And ContentCheck(.Species.Value) Then
        'i.e. some other species name typed in
        'put alert in ValidDisplay and enable DataFrame
        .ValidDisplay.Text = "Not a recognized combination (Wilson & Reeder 1993)"
        Call FrameEnable(MammalForm.DataFrame, True)
        Call FrameEnable(MammalForm.DataDetails, False)
        Call LatLongToggler
        .UnitCat.Enabled = False
        .DataValue.Enabled = False
        .WriteEntry.Enabled = False
    End If
    
    .SubSpecies.Text = "Unspecified"
    .Authority.Text = "Unspecified"

End With


End Sub

Public Sub LatLongToggler()

With MammalForm
    If .NoLatLong.Value Then
            .LatLabel.ForeColor = &H80000011
            .LongLabel.ForeColor = &H80000011
            .Latitude.Enabled = False
            .Longitude.Enabled = False
            .LatTog.Enabled = False
            .LongTog.Enabled = False
            .Latitude.Text = "NA"
            .Longitude.Text = "NA"
            .LatTog = False
            .LongTog = False
    Else
            .LatLabel.ForeColor = &H80000012
            .LongLabel.ForeColor = &H80000012
            .Latitude.Enabled = True
            .Longitude.Enabled = True
            .LatTog.Enabled = True
            .LongTog.Enabled = True
            .Latitude.Text = "Unspecified"
            .Longitude.Text = "Unspecified"
    End If
End With

End Sub
