(* TODO
    Create an Initialization and Deinitialization step for workflow jobs that
    download/upload workflow values for cross-job persistence.
*)

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Header*)
BeginPackage[ "Wolfram`PacletCICD`" ];

WorkflowValue // ClearAll;

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

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Argument Patterns*)
$$wfScope    = "Workflow"|"Job"|"Step";
$$appendable = _List | _Association? AssociationQ;

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

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Options*)
WorkflowValue // Options = { };

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Main Definition*)
WorkflowValue[ args___ ] := catchTop @ getWorkflowValue @ args;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*UpValues*)

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*Set*)
WorkflowValue /: HoldPattern @ Set[ WorkflowValue[ args___ ], value_ ] :=
    catchTop @ setWorkflowValue[ args, value ];

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*SetDelayed*)
WorkflowValue /: HoldPattern @ SetDelayed[ WorkflowValue[ args___ ], value_ ] :=
    catchTop @ setDelayedWorkflowValue[ args, value ];

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*AppendTo*)
WorkflowValue /: HoldPattern @ AppendTo[ WorkflowValue[ args___ ], value_ ] :=
    catchTop @ appendWorkflowValue[ args, value ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*$defaultScope*)
$defaultScope = "Job";

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
        writeWFFile[ file, val ]
    ];

setStepValue // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*setJobValue*)
setJobValue[ name_String, val_ ] :=
    With[ { file = FileNameJoin @ { $jobRoot, encodeWFVName @ name } },
        writeWFFile[ file, val ]
    ];

setJobValue // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*setWFValue*)
setWFValue[ name_String, val_ ] :=
    With[ { file = FileNameJoin @ { $wfRoot, encodeWFVName @ name } },
        flagWFValueOutput @ $wfRoot;
        writeWFFile[ file, val ]
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
        Confirm @ setWorkflowValue[ name, scope, v ]
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

readWFFile[ file_, $wfv[ expr___ ] ] :=
    expr;

readWFFile[ file_? FileExistsQ, other_ ] :=
    throwMessageFailure[ WorkflowValue::CorruptFile, file, HoldForm @ other ];

readWFFile[ file_, other_ ] :=
    throwMessageFailure[ WorkflowValue::InvalidFile, file, HoldForm @ other ];

readWFFile // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*writeWFFile*)
writeWFFile[ file_, expr_ ] :=
    Module[ { out },
        out = Quiet @ Developer`WriteWXFFile[ file, $wfv @ expr ];
        If[ FileExistsQ @ out,
            expr,
            writeWFFileError[ file, out ]
        ]
    ];

writeWFFile // catchUndefined;

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
(* ::Section::Closed:: *)
(*Package Footer*)
End[ ];

EndPackage[ ];