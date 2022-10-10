(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Header*)
BeginPackage[ "Wolfram`PacletCICD`" ];

BuildPaclet // ClearAll;

Begin[ "`Private`" ];

$ContextAliases[ "dnc`"  ] = "DefinitionNotebookClient`";
$ContextAliases[ "prdn`" ] = "PacletResource`DefinitionNotebook`";
$ContextAliases[ "pt`"   ] = "PacletTools`";

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*BuildPaclet*)
BuildPaclet::invfile =
"`1` is not a valid definition notebook file or directory.";

BuildPaclet::errors =
"Errors encountered while checking paclet.";

BuildPaclet::undefined =
"Unhandled arguments for `1` in `2`.";

BuildPaclet::archive =
"Could not find built paclet archive";

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Options*)
BuildPaclet // Options = {
    "Check"            -> False,
    "ConsoleType"      -> Automatic,
    "ExitOnFail"       -> Automatic,
    "SetWorkflowValue" -> True,
    "Target"           -> "Build"
};

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Argument patterns*)
$$bpOpts = OptionsPattern @ {
               BuildPaclet,
               CheckPaclet,
               dnc`CheckDefinitionNotebook,
               prdn`BuildPaclet
           };

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Main definition*)
BuildPaclet[ opts: $$bpOpts ] :=
    catchTop @ BuildPaclet[ File @ Directory[ ], opts ];

BuildPaclet[ dir_File? DirectoryQ, opts: $$bpOpts ] :=
    catchTop @ BuildPaclet[ findDefinitionNotebook @ dir, opts ];

BuildPaclet[ file_File? defNBQ, opts: $$bpOpts ] :=
    catchTop @ usingFrontEnd @ withDNCSettings[
        { OptionValue[ "ConsoleType" ], OptionValue[ "Target" ] },
        Module[ { checked, built },

            checked = If[ TrueQ @ OptionValue[ "Check" ],
                          checkForBuild[ file, opts ],
                          Missing[ "NotAvailable" ]
                      ];

            built = If[ FailureQ @ checked,
                        Missing[ "CheckFailed" ],
                        buildPaclet[ file, opts ]
                    ];

            combineBuildResult[ file, built, checked ]
        ]
    ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Error cases*)

(* Invalid file specification: *)
e: BuildPaclet[ file: Except[ _File? defNBQ ], ___ ] :=
    throwMessageFailure[ BuildPaclet::invfile, file, HoldForm @ e ];

(* Invalid options specification: *)
e: BuildPaclet[
    file_File? defNBQ,
    a: OptionsPattern[ ],
    inv: Except[ OptionsPattern[ ] ],
    ___
] :=
    throwMessageFailure[
        BuildPaclet::nonopt,
        HoldForm @ inv,
        1 + Length @ HoldComplete @ a,
        HoldForm @ e
    ];

(* Unexpected arguments: *)
e: BuildPaclet[ ___ ] :=
    throwMessageFailure[ BuildPaclet::undefined, BuildPaclet, HoldForm @ e ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Dependencies*)

$$cpOpts = OptionsPattern @ {
               CheckPaclet,
               dnc`CheckDefinitionNotebook
           };

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*checkForBuild*)
checkForBuild[ file_, opts___ ] :=
    catch @ CheckPaclet[ file, filterOptions[ $$cpOpts, opts ] ];

checkForBuild // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*buildPaclet*)
buildPaclet[ file_File, opts___ ] :=
    Module[ { nbo },
        nbo = First[ Notebooks @ ExpandFileName @ file, $Failed ];
        If[ MatchQ[ nbo, _NotebookObject ],
            buildPaclet[ nbo, opts ],
            openNotebookAndBuild[ file, opts ]
        ]
    ];

buildPaclet[ nbo_NotebookObject, opts___ ] :=
    Module[ { result, setWF },
        needs[ "PacletResource`DefinitionNotebook`" -> None ];

        result = pacletToolsMessageFix @ prdn`BuildPaclet[
            nbo,
            filterOptions[ Interactive -> False, opts ]
        ];

        setWF = OptionValue[ BuildPaclet, { opts }, "SetWorkflowValue" ];
        setGHBuildOutput[ result, setWF ]
    ];

buildPaclet // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*combineBuildResult*)
combineBuildResult[
    file_,
    b_,
    Failure[ "CheckPaclet::errors", as_Association ]
] :=
    Failure[
        "CheckPaclet::errors",
        Join[
            KeyDrop[ as, "Result" ],
            <|
                "Source"      -> file,
                "BuildResult" -> b,
                "CheckResult" -> Lookup[ as, "Result" ]
            |>
        ]
    ];

combineBuildResult[ file_, Success[ tag_, as_Association ], c_ ] :=
    Success[ tag, Join[ as, <| "Source" -> file, "CheckResult" -> c |> ] ];

combineBuildResult[ file_, built_? FailureQ, checked_ ] :=
    exitFailure[
        "BuildPacletFailure",
        <|
            "MessageTemplate" -> "Failed to build Paclet.",
            "Source"          -> file,
            "BuildResult"     -> built,
            "CheckResult"     -> checked
        |>
    ];

combineBuildResult[ file_, built_, checked_? FailureQ ] :=
    exitFailure[
        "CheckPacletFailure",
        <|
            "MessageTemplate" -> "Failed to check Paclet.",
            "Source"          -> file,
            "BuildResult"     -> built,
            "CheckResult"     -> checked
        |>
    ];

combineBuildResult // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsubsubsection::Closed:: *)
(*openNotebookAndBuild*)
openNotebookAndBuild[ file_, opts___ ] :=
    Module[ { nbo },
        needs[ "DefinitionNotebookClient`" -> None ];
        hiddenDirectoryFix[ ];
        WithCleanup[
            dnc`BeginConsoleGroup[ "BuildPaclet" ];
            nbo = dnc`OpenTemporaryNotebook @ file,
            buildPaclet[ nbo, opts ],
            dnc`CloseTemporaryNotebook @ nbo;
            dnc`EndConsoleGroup[ "BuildPaclet" ];
        ]
    ];

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*setGHBuildOutput*)
setGHBuildOutput[ res_, set_ ] := Enclose[
    If[ set, ghSetWFOutput[ "BuildPaclet", res ] ];
    Confirm @ setGHBuildOutput0 @ res
    ,
    exitFailure[ BuildPaclet::archive, 1, res ] &
];

setGHBuildOutput0[ KeyValuePattern[ "BuildResult"|"Result" -> res_ ] ] :=
    setGHBuildOutput0 @ res;

setGHBuildOutput0[
    result: Success[ _, KeyValuePattern[ "PacletArchive" -> pa_ ] ]
] :=
    Enclose @ Module[ { cStr, archive, file, pac, vers, full, buildDir, path },

        cStr     = ConfirmBy[ #, StringQ ] &;
        archive  = ConfirmBy[ ExpandFileName @ pa, FileExistsQ ];
        file     = cStr @ checkPacArchiveExtension @ archive;
        pac      = ConfirmBy[ PacletObject @ File @ file, PacletObjectQ ];
        vers     = cStr @ pac[ "Version" ];
        full     = cStr @ ExpandFileName @ file;
        buildDir = cStr @ ghRelativePath @ DirectoryName @ full;
        path     = cStr @ ghRelativePath @ full;

        setOutput[ "PACLET_BUILD_DIR"  , buildDir                   ];
        setOutput[ "PACLET_PATH"       , path                       ];
        setOutput[ "PACLET_FILE"       , cStr @ FileNameTake @ file ];
        setOutput[ "PACLET_RELEASE_TAG", "v" <> vers                ];

        result
    ];

setGHBuildOutput0[ ___ ] := $Failed;

(* ::**********************************************************************:: *)
(* ::Subsubsubsection::Closed:: *)
(*checkPacArchiveExtension*)
checkPacArchiveExtension[ archive_? FileExistsQ ] :=
    If[ ToLowerCase @ FileExtension @ archive === "paclet",
        archive,
        RenameFile[
            archive,
            ExpandFileName @ archive <> ".paclet",
            OverwriteTarget -> True
        ]
    ];

checkPacArchiveExtension[ ___ ] := $Failed;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*pacletToolsMessageFix*)
pacletToolsMessageFix // Attributes = { HoldFirst };

(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::SuspiciousSessionSymbol:: *)
pacletToolsMessageFix[ eval_ ] := (
    needs[ "PacletTools`" -> None ];
    needs[ "PacletResource`DefinitionNotebook`" -> None ];
    Internal`InheritedBlock[ { pt`PacletBuild, $Line },
        Unprotect @ pt`PacletBuild;
        pt`PacletBuild // Options = DeleteDuplicates @ Append[
            Options @ pt`PacletBuild,
            OverwriteTarget -> Automatic
        ];
        Quiet[
            eval,
            {
                FileHash::noopen,
                DocumentationBuild`DocumentationBuild::warning
            }
        ]
    ]
);
(* :!CodeAnalysis::EndBlock:: *)

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Footer*)
End[ ];
EndPackage[ ];
