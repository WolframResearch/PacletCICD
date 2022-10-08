(* TODO
    Create an Initialization and Deinitialization step for workflow jobs that
    download/upload workflow values for cross-job persistence.
*)

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Header*)
BeginPackage[ "Wolfram`PacletCICD`" ];

ClearAll[
    $WorkflowValueScope,
    InitializeWorkflowValues,
    WorkflowValue
];

Begin[ "`Private`" ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Config*)
$wfvRoot := GeneralUtilities`EnsureDirectory @ {
    $UserBaseDirectory, "Wolfram", "PacletCICD", "WorkflowValues"
};

$wfRoot   := GeneralUtilities`EnsureDirectory @ { $wfvRoot, "Workflow" };
$jobRoot  := GeneralUtilities`EnsureDirectory @ { $wfvRoot, "Job"      };
$stepRoot := GeneralUtilities`EnsureDirectory @ { $wfvRoot, "Step"     };

$wfDownloadLocation := ExpandFileName[ ".paclet-workflow-values" ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Argument Patterns*)
$$wfScope    = "Workflow"|"Job"|"Step";
$$appendable = _List | _Association? AssociationQ;

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*InitializeWorkflowValues*)
InitializeWorkflowValues[ ] := (
    EchoEvaluation @ FileNames[ ".paclet-workflow-values", Infinity ];
    InitializeWorkflowValues @ $wfDownloadLocation
);

InitializeWorkflowValues[ src_? DirectoryQ ] :=
    Module[ { files },
        files = EchoEvaluation @ FileNames[ "*.wxf", src, Infinity ];
        importDownloadedWFV[ src, # ] & /@ files
    ];

InitializeWorkflowValues[ ___ ] := Missing[ "NotAvailable" ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*importDownloadedWFV*)
importDownloadedWFV[ src_, file_ ] :=
    CopyFile[
        file,
        ExpandFileName @ FileNameJoin @ { $wfRoot, relativePath[ src, file ] },
        OverwriteTarget -> True
    ];

importDownloadedWFV // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*$WorkflowValueScope*)
$WorkflowValueScope = "Job";

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*WorkflowValue*)
WorkflowValue::ValueName =
"Expected a string instead of `1` as the workflow value name.";

WorkflowValue::InvalidScope =
"`1` is not a valid workflow scope.";

WorkflowValue::InvalidAppendType =
"Cannot append `1` to `2`.";

WorkflowValue::CorruptFile =
"Internal Error: The workflow value stored in `1` is corrupted.";

WorkflowValue::InvalidFile =
"Internal Error: Expected a file to exist at `1`.";

WorkflowValue::WriteFailed =
"Internal Error: Failed to write WXF data to `1`.";

WorkflowValue::AppendFailed =
"Internal Error: Failed to append `2` to workflow value `1`.";

WorkflowValue::PayloadFailed =
"Internal Error: Failed to construct workflow payload: `1`.";

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Options*)
WorkflowValue // Options = { };

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Main Definition*)
WorkflowValue[ a___ ] := catchTop @ getWorkflowValue @ a;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*UpValues*)

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*Set*)
WorkflowValue /: HoldPattern @ Set[ WorkflowValue[ a___ ], v_ ] :=
    catchTop @ setWorkflowValue[ a, v ];

WorkflowValue /: HoldPattern @ Set[ WorkflowValue[ a___ ][ k_ ], v_ ] :=
    catchTop @ appendWorkflowValue[ a, k -> v ];

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*SetDelayed*)
WorkflowValue /: HoldPattern @ SetDelayed[ WorkflowValue[ a___ ], v_ ] :=
    catchTop @ setDelayedWorkflowValue[ a, v ];

WorkflowValue /: HoldPattern @ SetDelayed[ WorkflowValue[ a___ ][ k_ ], v_ ] :=
    catchTop @ appendWorkflowValue[ a, k :> v ];

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*AppendTo*)
WorkflowValue /: HoldPattern @ AppendTo[ WorkflowValue[ a___ ], v_ ] :=
    catchTop @ appendWorkflowValue[ a, v ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*$defaultScope*)
$defaultScope := $WorkflowValueScope;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*getWorkflowValue*)
getWorkflowValue[ name_ ] := getWorkflowValue[ name, $defaultScope ];

getWorkflowValue[ name_? StringQ, "Step"     ] := getStepValue @ name;
getWorkflowValue[ name_? StringQ, "Job"      ] := getJobValue @ name;
getWorkflowValue[ name_? StringQ, "Workflow" ] := getWFValue @ name;

getWorkflowValue[ name: Except[ _String? StringQ ], _ ] :=
    throwMessageFailure[ WorkflowValue::ValueName, name ];

getWorkflowValue[ name_, scope: Except[ $$wfScope ] ] :=
    throwMessageFailure[ WorkflowValue::InvalidScope, scope ];

getWorkflowValue // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*getStepValue*)
getStepValue[ name_String ] := getStepValue[ name, encodeWFVName @ name ];

getStepValue[ name_String, base_String ] :=
    With[ { file = FileNameJoin @ { $stepRoot, base } },
        If[ FileExistsQ @ file,
            readWFFile @ file,
            getJobValue[ name, base ]
        ]
    ];

getStepValue // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*getJobValue*)
getJobValue[ name_String ] := getJobValue[ name, encodeWFVName @ name ];

getJobValue[ name_String, base_String ] :=
    With[ { file = FileNameJoin @ { $jobRoot, base } },
        If[ FileExistsQ @ file,
            readWFFile @ file,
            getWFValue[ name, base ]
        ]
    ];

getJobValue // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*getWFValue*)
getWFValue[ name_String ] := getWFValue[ name, encodeWFVName @ name ];

getWFValue[ name_String, base_String ] :=
    With[ { file = FileNameJoin @ { $wfRoot, base } },
        If[ FileExistsQ @ file,
            readWFFile @ file,
            Missing[ "WorkflowValueNotFound", name ]
        ]
    ];

getWFValue // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*setWorkflowValue*)
setWorkflowValue[ name_, v_ ] := setWorkflowValue[ name, $defaultScope, v ];

setWorkflowValue[ name_? StringQ, "Step"    , v_ ] := setStepValue[ name, v ];
setWorkflowValue[ name_? StringQ, "Job"     , v_ ] := setJobValue[ name, v ];
setWorkflowValue[ name_? StringQ, "Workflow", v_ ] := setWFValue[ name, v ];

setWorkflowValue[ name: Except[ _String? StringQ ], _, _ ] :=
    throwMessageFailure[ WorkflowValue::ValueName, name ];

setWorkflowValue[ name_, scope: Except[ $$wfScope ], _ ] :=
    throwMessageFailure[ WorkflowValue::InvalidScope, scope ];

setWorkflowValue // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*setStepValue*)
setStepValue[ name_String, val_ ] :=
    With[ { file = FileNameJoin @ { $stepRoot, encodeWFVName @ name } },
        writeWFFile[ "Step", name, file, val ]
    ];

setStepValue // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*setJobValue*)
setJobValue[ name_String, val_ ] :=
    With[ { file = FileNameJoin @ { $jobRoot, encodeWFVName @ name } },
        writeWFFile[ "Job", name, file, val ]
    ];

setJobValue // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*setWFValue*)
setWFValue[ name_String, val_ ] :=
    With[ { file = FileNameJoin @ { $wfRoot, encodeWFVName @ name } },
        flagWFValueOutput @ $wfRoot;
        writeWFFile[ "Workflow", name, file, val ]
    ];

setWFValue // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*flagWFValueOutput*)
flagWFValueOutput[ root_ ] := flagWFValueOutput[ root ] =
    setOutput[ "PACLET_WORKFLOW_VALUES", root ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*setDelayedWorkflowValue*)
setDelayedWorkflowValue[ name_, scope_ ] := Failure[ "NotImplemented", <| |> ];
(* TODO (maybe) *)
setDelayedWorkflowValue // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*appendWorkflowValue*)
appendWorkflowValue[ name_, v_ ] :=
    appendWorkflowValue[ name, $defaultScope, v ];

appendWorkflowValue[ name_? StringQ, scope: $$wfScope, v_ ] := Enclose[
    Module[ { current, new },
        current = Confirm @ getInitialValueForAppend[ name, scope, v ];
        new     = ConfirmMatch[ Append[ current, v ], $$appendable ];
        Confirm @ setWorkflowValue[ name, scope, new ]
    ],
    throwMessageFailure[ WorkflowValue::AppendFailed, name, scope, v ] &
];

appendWorkflowValue // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*getInitialValueForAppend*)
getInitialValueForAppend[ name_, scope_, value_ ] :=
    Module[ { current, list },
        current = getWorkflowValue[ name, scope ];
        list    = Replace[ current,
                           Missing[ "WorkflowValueNotFound", name ] :>
                               initialWorkflowValue @ value
                  ];

        If[ MatchQ[ list, $$appendable ],
            list,
            throwMessageFailure[ WorkflowValue::InvalidAppendType, value, list ]
        ]
    ];

getInitialValueForAppend // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*initialWorkflowValue*)
initialWorkflowValue[ (Rule|RuleDelayed)[ _, _ ] ] := <| |>;
initialWorkflowValue[ _ ] := { };
initialWorkflowValue // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Common*)

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*encodeWFVName*)
encodeWFVName[ name_String ] := URLEncode @ name <> ".wxf";
encodeWFVName // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*readWFFile*)
readWFFile[ file_ ] :=
    readWFFile[ file, Quiet @ Developer`ReadWXFFile @ file ];

readWFFile[ file_, as_? wfPayloadQ ] := Lookup[ as, "Expression" ];

readWFFile[ file_? FileExistsQ, other_ ] :=
    throwMessageFailure[ WorkflowValue::CorruptFile, file, HoldForm @ other ];

readWFFile[ file_, other_ ] :=
    throwMessageFailure[ WorkflowValue::InvalidFile, file, HoldForm @ other ];

readWFFile // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*writeWFFile*)
writeWFFile[ scope_, name_, file_, expr_ ] :=
    Module[ { write, out },
        write = makeWFPayload[ scope, name, expr ];
        out   = Quiet @ Developer`WriteWXFFile[ file, write ];
        If[ FileExistsQ @ out,
            expr,
            writeWFFileError[ file, out ]
        ]
    ];

writeWFFile // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*makeWFPayload*)
makeWFPayload // Attributes = { HoldRest };
makeWFPayload[ scope_, name_, expr_ ] :=
    Module[ { as, pl },
        as = <| "Scope" -> scope, "Name" -> name, "Expression" :> expr |>;
        pl = Join[ as, $wfPayloadDefaults ];
        If[ wfPayloadQ @ pl,
            pl,
            throwMessageFailure[ WorkflowValue::PayloadFailed, pl ]
        ]
    ];

makeWFPayload // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*$wfPayloadDefaults*)
$wfPayloadDefaults := DeleteCases[
    Association[
        "Date"    -> DateObject[ TimeZone -> 0 ],
        "Version" -> 1,
        GetEnvironment @ { "GITHUB_WORKFLOW", "GITHUB_JOB" }
    ],
    None
];

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*writeWFFileError*)
writeWFFileError[ file_, other_ ] :=
    With[ { str = ExpandFileName @ file },
        throwMessageFailure[
            WorkflowValue::InvalidFile,
            file,
            HoldForm @ other
        ] /; ! StringQ @ str
    ];

writeWFFileError[ file_, other_Developer`WriteWXFFile ] :=
    throwMessageFailure[ WorkflowValue::WriteFailed, file, HoldForm @ other ];

writeWFFileError // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*wfPayloadQ*)
wfPayloadQ[ KeyValuePattern @ {
    "Expression" :> _,
    "Date"       -> _DateObject? DateObjectQ,
    "Version"    -> _Integer? IntegerQ,
    "Scope"      -> $$wfScope,
    "Name"       -> _String? StringQ
} ] := True;

wfPayloadQ[ ___ ] := False;

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Footer*)
End[ ];

EndPackage[ ];