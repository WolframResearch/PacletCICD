(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Header*)
BeginPackage[ "Wolfram`PacletCICD`" ];

BuildPaclet;

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
    "Preflight"   -> False,
    "Target"      -> "Build"
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
    catchTop @ withDNCSettings[
        { DefinitionNotebookClient`PackageScope`toConsoleType @ OptionValue[ "ConsoleType" ], OptionValue[ "Target" ] },
        Module[ { checked, built },

            checked = If[ TrueQ @ OptionValue[ "Preflight" ],
                          CheckPaclet[ file, filterOptions[ $$cpOpts, opts ] ],
                          Missing[ "NotAvailable" ]
                      ];

            built = UsingFrontEnd @ buildPaclet[ file, opts ];

            <| "BuildResult" -> built, "CheckResult" -> checked |>
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
    Module[ { result },
        result = prdn`BuildPaclet[
            nbo,
            filterOptions[ "Interactive" -> False, opts ]
        ];
        setGHBuildOutput @ result
    ];

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
(*withDNCSettings*)
withDNCSettings // Attributes = { HoldRest };

withDNCSettings[ { type_, tgt_ }, eval_ ] :=
    Internal`InheritedBlock[ { dnc`$ConsoleType, dnc`$ClickedButton },
        dnc`ConsolePrint[ dnc`$ConsoleType = type, "Level" -> "Error" ];
        dnc`$ClickedButton = tgt;
        eval
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

setGHBuildOutput0[ Success[ _, KeyValuePattern[ "PacletArchive" -> pa_ ] ] ] :=
    Enclose @ Module[ { archive, file, pac, vers },

        archive = ConfirmBy[ ExpandFileName @ pa, FileExistsQ ];
        file    = ConfirmBy[ checkPacArchiveExtension @ archive, StringQ ];
        pac     = ConfirmBy[ PacletObject @ File @ file, PacletObjectQ ];
        vers    = ConfirmBy[ pac[ "Version" ], StringQ ];

        setOutput[ "PACLET_PATH", ExpandFileName @ file ];
        setOutput[ "PACLET_FILE", FileNameTake @ file ];
        setOutput[ "RELEASE_TAG", "v" <> vers ];

        file
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