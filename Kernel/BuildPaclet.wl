(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Header*)
BeginPackage[ "Wolfram`PacletCICD`" ];

BuildPaclet // ClearAll;

Begin[ "`Private`" ];

Needs[ "DefinitionNotebookClient`"          -> "dnc`"  ];
Needs[ "PacletResource`DefinitionNotebook`" -> "prdn`" ];

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
    "ConsoleType" -> Automatic,
    "Check"       -> False,
    "Target"      -> "Build",
    "ExitOnFail"  -> Automatic
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
    catchTop @ UsingFrontEnd @ withDNCSettings[
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

(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::SuspiciousSessionSymbol:: *)
buildPaclet[ nbo_NotebookObject, opts___ ] :=
    Module[ { result },

        result =
            Internal`InheritedBlock[ { $Line },
                prdn`BuildPaclet[
                    nbo,
                    filterOptions[ Interactive -> False, opts ]
                ]
            ];

        setGHBuildOutput @ result
    ];
(* :!CodeAnalysis::EndBlock:: *)

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
setGHBuildOutput[ res_ ] := Enclose[
    Confirm @ setGHBuildOutput0 @ res,
    exitFailure[ BuildPaclet::archive, 1, res ] &
];

setGHBuildOutput0[ KeyValuePattern[ "BuildResult"|"Result" -> res_ ] ] :=
    setGHBuildOutput0 @ res;

setGHBuildOutput0[
    result: Success[ _, KeyValuePattern[ "PacletArchive" -> pa_ ] ]
] :=
    Enclose @ Module[ { archive, file, pac, vers, full },

        archive = ConfirmBy[ ExpandFileName @ pa, FileExistsQ ];
        file    = ConfirmBy[ checkPacArchiveExtension @ archive, StringQ ];
        pac     = ConfirmBy[ PacletObject @ File @ file, PacletObjectQ ];
        vers    = ConfirmBy[ pac[ "Version" ], StringQ ];
        full    = ConfirmBy[ ExpandFileName @ file, StringQ ];

        setOutput[ "BUILD_DIR"  , ghRelativePath @ DirectoryName @ full ];
        setOutput[ "PACLET_PATH", ghRelativePath @ full ];
        setOutput[ "PACLET_FILE", FileNameTake @ file ];
        setOutput[ "RELEASE_TAG", "v" <> vers ];

        result
    ];

setGHBuildOutput0[ ___ ] := $Failed;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*ghRelativePath*)
ghRelativePath[ file_ ] := Enclose[
    Module[ { ws },
        ws = Environment[ "GITHUB_WORKSPACE" ];
        If[ StringQ @ ws,
            ConfirmBy[ relativePath[ ws, file ], StringQ ],
            ConfirmBy[ relativePath[ Directory[ ], file ], StringQ ]
        ]
    ],
    throwError[ "Could not determine relative path for file `1`", file ] &
];

ghRelativePath // catchUndefined;

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
(* ::Subsubsubsection::Closed:: *)
(*setOutput*)
setOutput[ name_, value_ ] /; dnc`$ConsoleType === "GitHub" :=
    setOutput[ $gitHubEnv, name, value ];

setOutput[ str_OutputStream, name_, value_ ] := (

    dnc`ConsolePrint @ StringJoin[
        "Setting GitHub environment variable ",
        ToString @ name,
        "=",
        ToString @ value
    ];

    WriteLine[ str, ToString @ name <> "=" <> ToString @ value ]
);

setOutput[ _, name_, value_ ] := (
    dnc`ConsolePrint @ StringJoin[
        "Setting GitHub environment variable using fallback ",
        ToString @ name,
        "=",
        ToString @ value
    ];

    dnc`ConsolePrint @ StringJoin[
        "::set-output name=",
        ToString @ name,
        "::",
        ToString @ value
    ]
);

(* ::**********************************************************************:: *)
(* ::Subsubsubsection::Closed:: *)
(*$gitHubEnv*)
$gitHubEnv := getGitHubEnv[ ];

getGitHubEnv[ ] := getGitHubEnv @ Environment[ "GITHUB_ENV" ];
getGitHubEnv[ e_String ] := getGitHubEnv @ First[ Streams @ e, OpenAppend @ e ];
getGitHubEnv[ s_OutputStream ] := $gitHubEnv = s;
getGitHubEnv[ ___ ] := $Failed;

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Footer*)
End[ ];
EndPackage[ ];
