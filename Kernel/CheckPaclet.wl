(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Header*)
BeginPackage[ "Wolfram`PacletCICD`" ];

CheckPaclet // ClearAll;

Begin[ "`Private`" ];

$ContextAliases[ "dnc`"  ] = "DefinitionNotebookClient`";

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*CheckPaclet*)
CheckPaclet::invfile =
"`1` is not a valid definition notebook file or directory.";

CheckPaclet::invfmt =
"`1` is not a valid format specification.";

CheckPaclet::errors =
"Errors encountered while checking paclet.";

CheckPaclet::undefined =
"Unhandled arguments for `1` in `2`.";

CheckPaclet::unknown =
"An unexpected error occurred.";

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Options*)
CheckPaclet // Options = {
    "Target"           -> "Submit",
    "DisabledHints"    -> Automatic,
    "FailureCondition" -> "Error"
};

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Argument patterns*)
$$hintProp   = _String | All | Automatic | { ___String };
$$cpFMTName  = "JSON"|"Dataset"|Automatic|None;
$$cpFMT      = $$cpFMTName | { $$cpFMTName, $$hintProp };

$$cpOpts = OptionsPattern @ {
               CheckPaclet,
               dnc`CheckDefinitionNotebook
           };

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Main definition*)
CheckPaclet[ opts: $$cpOpts ] :=
    catchTop @ CheckPaclet[ File @ Directory[ ], opts ];

CheckPaclet[ dir_File? DirectoryQ, opts: $$cpOpts ] :=
    catchTop @ CheckPaclet[ findDefinitionNotebook @ dir, opts ];

CheckPaclet[ file_File, opts: $$cpOpts ] :=
    catchTop @ CheckPaclet[ file, Automatic, opts ];

CheckPaclet[ file_File? defNBQ, fmt: $$cpFMT, opts: $$cpOpts ] :=
    catchTop @ checkPaclet[
        file,
        "DisabledHints" -> toDisabledHints @ OptionValue[ "DisabledHints" ],
        takeCheckDefNBOpts @ opts,
        "ConsoleType"      -> Automatic,
        "ClickedButton"    -> OptionValue[ "Target" ],
        "Format"           -> toCheckFormat @ fmt,
        "FailureCondition" -> OptionValue[ "FailureCondition" ]
    ];

(* TODO: save as JSON to build dir so it gets included in build artifacts *)

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Error cases*)

(* Invalid file specification: *)
e: CheckPaclet[ file: Except[ _File? defNBQ ], ___ ] :=
    throwMessageFailure[ CheckPaclet::invfile, file, HoldForm @ e ];

(* Invalid format specification: *)
e: CheckPaclet[ file_File? defNBQ, fmt: Except[ $$cpFMT ], ___ ] :=
    throwMessageFailure[ CheckPaclet::invfmt, fmt, HoldForm @ e ];

(* Invalid options specification: *)
e: CheckPaclet[
    file_File? defNBQ,
    fmt: $$cpFMT,
    a: OptionsPattern[ ],
    inv: Except[ OptionsPattern[ ] ],
    ___
] :=
    throwMessageFailure[
        CheckPaclet::nonopt,
        HoldForm @ inv,
        2 + Length @ HoldComplete @ a,
        HoldForm @ e
    ];

(* Unexpected arguments: *)
e: CheckPaclet[ ___ ] :=
    throwMessageFailure[ CheckPaclet::undefined, CheckPaclet, HoldForm @ e ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Dependencies*)

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*checkPaclet*)
checkPaclet[ nb_, opts___ ] :=
    ccPromptFix @ Module[ { res, hints, data, dir, export, exported },
        needs[ "DefinitionNotebookClient`" -> None ];
        res    = dnc`CheckDefinitionNotebook[ nb, opts ];
        hints  = dnc`HintData[ "Paclet" ];
        data   = <| "Result" -> res, "HintData" -> hints, "File" -> nb |>;
        dir    = parentPacletDirectory @ nb;
        export = fileNameJoin @ { dir, "build", "check_results.wxf" };
        GeneralUtilities`EnsureDirectory @ DirectoryName @ export;
        ConsoleNotice[ "Exporting check results: " <> export ];
        exported = Export[ export,
                           data,
                           "WXF",
                           PerformanceGoal -> "Size"
                   ];
        setOutput[ "PACLET_CHECK_RESULTS", exported ];
        EchoEvaluation @ generateCheckReport @ data;
        checkExit @ res
    ];

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*generateCheckReport*)
generateCheckReport[ KeyValuePattern @ {
    "HintData" -> hints_,
    "File"     -> file_
} ] :=
    Enclose @ Module[ { job, index, url, row, head, title, grid, md },
        job   = ConfirmBy[ Environment[ "GITHUB_JOB" ], StringQ ];
        index = ConfirmBy[ notebookCellIDIndex @ file, AssociationQ ];
        url   = ghCommitFileURL[ file, Lookup[ index, #[ "CellID" ] ] ] &;
        row   = Append[ Lookup[ #1, { "CellID", "Level", "Tag" } ], url @ # ] &;
        head  = Style[ #, Bold ] & /@ { "CellID", "Level", "Tag", "Link" };
        title = Style[ "Check Results (" <> job <> ")", "Title" ];
        grid  = Grid @ Prepend[ row /@ hints, head ];
        md    = ConfirmBy[ ToMarkdownString @ { title, grid }, StringQ ];
        appendStepSummary @ md
    ];

generateCheckReport // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*checkExit*)
checkExit[ Failure[ "FailureCondition", as_Association ] ] :=
    exitFailure[
        "CheckPaclet::errors",
        Association[
            "MessageTemplate"   :> CheckPaclet::errors,
            "MessageParameters" :> { },
            KeyTake[ as, { "FailureCondition", "Result" } ]
        ],
        1
    ];

checkExit[ res_? FailureQ ] :=
    exitFailure[ CheckPaclet::unknown, 1, res ];

checkExit[ result_ ] := result;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*takeCheckDefNBOpts*)
takeCheckDefNBOpts[ opts: $$cpOpts ] := (
    needs[ "DefinitionNotebookClient`" -> None ];
    filterOptions[ dnc`CheckDefinitionNotebook, opts ]
);

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*toDisabledHints*)
toDisabledHints[ Automatic|Inherited ] := (
    needs[ "DefinitionNotebookClient`" -> None ];
    toDisabledHints @ Flatten @ {
        dnc`$DisabledHints,
        "PacletRequiresBuild",
        "PacletFileChanged",
        "PacletFilesChanged",
        $eventDisabledHints
    }
);

toDisabledHints[ tag_String ] :=
    Map[ <| "MessageTag" -> tag, "Level" -> #1, "ID" -> All |> &,
         { "Suggestion", "Warning", "Error" }
    ];

toDisabledHints[ as: KeyValuePattern[ "MessageTag" -> _ ] ] :=
    { as };

toDisabledHints[ as: KeyValuePattern[ "Tag" -> tag_ ] ] :=
    { Append[ as, "MessageTag" -> tag ] };

toDisabledHints[ hints_List ] :=
    DeleteDuplicates @ Flatten[ toDisabledHints /@ hints ];

toDisabledHints[ ___ ] := { };


$eventDisabledHints := eventDisabledHints @ Environment[ "GITHUB_EVENT_NAME" ];

eventDisabledHints[ "schedule"          ] := { "PacletVersionUnchanged" };
eventDisabledHints[ "workflow_dispatch" ] := { "PacletVersionUnchanged" };
eventDisabledHints[ ___                 ] := { };

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*toCheckFormat*)
toCheckFormat[ None             ] := None;
toCheckFormat[ fmt: $$cpFMTName ] := { fmt, $defaultHintProps };
toCheckFormat[ fmt_             ] := fmt;

$defaultHintProps = { "Level", "Message", "Tag", "CellID" };

(* ::**********************************************************************:: *)
(* ::Subsubsubsection::Closed:: *)
(*disableTag*)
disableTag[ tag_ ] :=
    Map[ <| "MessageTag" -> tag, "Level" -> #1, "ID" -> All |> &,
         { "Suggestion", "Warning", "Error" }
    ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Footer*)
End[ ];
EndPackage[ ];