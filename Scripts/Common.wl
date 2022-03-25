(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::SuspiciousSessionSymbol:: *)

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Definitions*)

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*gitCommand*)
gitCommand[ { cmd__String }, dir_ ] :=
    Enclose @ Module[ { res },
        res = RunProcess[ { "git", cmd }, ProcessDirectory -> dir ];
        ConfirmAssert[ res[ "ExitCode" ] === 0 ];
        StringTrim @ ConfirmBy[ res[ "StandardOutput" ], StringQ ]
    ];

gitCommand[ cmd_String, dir_ ] := gitCommand[ { cmd }, dir ];

gitCommand[ cmd_ ] := gitCommand[ cmd, Directory[ ] ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*releaseID*)
releaseID[ dir_ ] :=
    With[ { sha = Environment[ "GITHUB_SHA" ] },
        If[ StringQ @ sha,
            sha,
            gitCommand[ { "rev-parse", "HEAD" }, dir ]
        ]
    ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*updatePacletInfo*)
updatePacletInfo[ dir_ ] := Enclose[
    Module[ { cs, file, string, id, date, new },
        cs     = ConfirmBy[ #, StringQ ] &;
        file   = cs @ FileNameJoin @ { dir, "PacletInfo.wl" };
        string = cs @ ReadString @ file;
        id     = cs @ releaseID @ dir;
        date   = cs @ DateString[ "ISODateTime", TimeZone -> 0 ];

        new = cs @ StringReplace[
            string,
            {
                "\r\n"           -> "\n",
                "$RELEASE_ID$"   -> id,
                "$RELEASE_DATE$" -> date <> "Z"
            }
        ];

        Print[ "Updating PacletInfo" ];
        Print[ "    ReleaseID: ", id ];
        Print[ "    ReleaseDate: ", date <> "Z" ];

        Confirm @ WithCleanup[ BinaryWrite[ file, new ],
                               Close @ file
                  ]
    ],
    Function[
        Print[ "::error::Failed to update PacletInfo template parameters." ];
        Exit[ 1 ]
    ]
];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*setResourceSystemBase*)
setResourceSystemBase[ ] := (
    Needs[ "ResourceSystemClient`" -> None ];
    $ResourceSystemBase =
        With[ { rsBase = Environment[ "RESOURCE_SYSTEM_BASE" ] },
            If[ StringQ @ rsBase, rsBase, $ResourceSystemBase ]
        ]
);

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*checkResult*)
checkResult // Attributes = { HoldFirst };
checkResult[ eval: (sym_Symbol)[ args___ ] ] :=
    Module[ { result, ctx, name, full },
        result = eval;
        If[ MatchQ[ Head @ result, HoldPattern @ sym ]
            ,
            ctx  = Context @ Unevaluated @ sym;
            name = SymbolName @ Unevaluated @ sym;
            full = ctx <> name;
            Print[ "::error::" <> full <> " not defined" ];
            Exit[ 1 ]
            ,
            Print @ result
        ]
    ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Setup*)
$pacDir = DirectoryName[ $InputFileName, 2 ];
Print[ "Paclet Directory: ", $pacDir ];

updatePacletInfo @ $pacDir;
PacletDirectoryLoad @ $pacDir;
Needs[ "Wolfram`PacletCICD`" -> None ];

setResourceSystemBase[ ];
Print[ "ResourceSystemBase: ", $ResourceSystemBase ];

$defNB = File @ FileNameJoin @ { $pacDir, "ResourceDefinition.nb" };
Print[ "Definition Notebook: ", $defNB ];

(* :!CodeAnalysis::EndBlock:: *)