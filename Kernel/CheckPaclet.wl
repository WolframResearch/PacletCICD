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
    (* TODO: MarkdownSummary option *)
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
    ccPromptFix @ Module[ { res, hints, data, exported },
        needs[ "DefinitionNotebookClient`" -> None ];
        hiddenDirectoryFix[ ];
        res   = dnc`CheckDefinitionNotebook[ nb, opts ];
        hints = $checkHintData;
        data  = <| "Result" -> res, "HintData" -> hints, "File" -> nb |>;
        exported = exportCheckResults[ nb, data ];
        generateCheckReport @ data;
        checkExit @ res
    ];

(* FIXME: temporary stuff: *)
<| a -> # + 1 & |>

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*$checkHintData*)
$checkHintData := withConsoleType[
    Automatic,
    DeleteMissing /@ dnc`HintData[
        "Paclet",
        { "Tag", "Level", "MessageText", "CellID", "SourcePosition" }
    ]
];

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*exportCheckResults*)
exportCheckResults[ _, KeyValuePattern[ "HintData" -> { } ] ] := Null;

exportCheckResults[ nb_, data_Association ] :=
    Module[ { dir, export, exported },
        dir = parentPacletDirectory @ nb;
        export = fileNameJoin @ { dir, "build", "check_results.wxf" };
        GeneralUtilities`EnsureDirectory @ DirectoryName @ export;
        exported = Export[ export, data, "WXF", PerformanceGoal -> "Size" ];
        setOutput[ "PACLET_CHECK_RESULTS", exported ];
        exported
    ];

exportCheckResults // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*generateCheckReport*)
generateCheckReport[ KeyValuePattern[ "HintData" -> { } ] ] :=
    appendStepSummary @ ToMarkdownString @ {
        Style[ "Check paclet results", "Section" ],
        ":white_check_mark: No issues found."
    };

generateCheckReport[ KeyValuePattern @ {
    "HintData" -> hints_,
    "File"     -> file_
} ] :=
    Enclose @ Module[ { job, index, head, title, grid, md },
        job   = ConfirmBy[ Environment[ "GITHUB_JOB" ], StringQ ];
        index = ConfirmBy[ notebookCellIDIndex @ file, AssociationQ ];
        head  = Style[ #, Bold ] & /@ { "Level", "Tag", "Message", "Link" };
        title = Style[ "Definition Notebook (" <> job <> ")", "Section" ];
        grid  = Grid @ Prepend[ reportHintRow[ file, index ] /@ hints, head ];
        md    = ConfirmBy[ ToMarkdownString @ { title, grid }, StringQ ];
        appendStepSummary @ md
    ];

generateCheckReport // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsubsubsection::Closed:: *)
(*reportHintRow*)
(* reportHintRow[ file_, index_ ][ hint_Association ] :=
    Enclose @ Module[ { lookup, level, tag, msg, id, pos, url, link },
        lookup = ConfirmBy[ Lookup[ hint, # ], StringQ ] &;
        level  = hintIcon @ lookup[ "Level" ];
        tag    = lookup[ "Tag" ];
        msg    = Style[ lookup[ "Message" ], "Text" ];
        id     = ConfirmBy[ Lookup[ hint, "CellID" ], IntegerQ ];
        pos    = Lookup[ index, id ];
        Print[ "hint: ", hint ];
        url    = ghCommitFileURL[ file, hint ];
        If[ url === None, url = ghCommitFileURL[ file, pos ] ];
        link   = Hyperlink[ ":link:", url ];
        { level, tag, msg, link }
    ]; *)

reportHintRow[ file_, index_ ][ hint_Association ] :=
    Enclose @ Module[ { lookup, level, tag, msg, url, link },
        lookup = ConfirmBy[ Lookup[ hint, # ], StringQ ] &;
        level  = hintIcon @ lookup[ "Level" ];
        tag    = lookup[ "Tag" ];
        msg    = Style[ lookup[ "Message" ], "Text" ];
        url    = sourceFileURL[ file, index, hint ];
        link   = Hyperlink[ ":link:", url ];
        { level, tag, msg, link }
    ];

(* ::**********************************************************************:: *)
(* ::Subsubsubsection::Closed:: *)
(*sourceFileURL*)
sourceFileURL[ nbFile_, cellIndex_, hint_ ] :=
    With[ { url = ghCommitFileURL @ hint },
        url /; StringQ @ url
    ];

sourceFileURL[ nbFile_, cellIndex_, hint_ ] :=
    Enclose @ Module[ { id, pos },
        id  = ConfirmBy[ Lookup[ hint, "CellID" ], IntegerQ ];
        pos = Lookup[ cellIndex, id ];
        ghCommitFileURL[ nbFile, pos ]
    ];

sourceFileURL // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsubsubsection::Closed:: *)
(*hintIcon*)
hintIcon[ "Error"      ] := ":x: Error";
hintIcon[ "Warning"    ] := ":warning: Warning";
hintIcon[ "Suggestion" ] := ":grey_question: Suggestion";
hintIcon[ other_       ] := other;
hintIcon // catchUndefined;

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