#!/usr/bin/env wolframscript

(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::SuspiciousSessionSymbol:: *)

Print @ TableForm[ List @@@ GetEnvironment[ ] ];

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

        Confirm @ BinaryWrite[ file, new ]
    ],
    Print[ "::error::Failed to update PacletInfo template parameters." ];
    Exit[ 1 ]
];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Run*)
$pacDir = DirectoryName[ $InputFileName, 2 ];
updatePacletInfo @ $pacDir;
PacletDirectoryLoad @ $pacDir;

Needs[ "Wolfram`PacletCICD`" ];
Needs[ "ResourceSystemClient`" -> None ];
$ResourceSystemBase = Environment[ "RESOURCE_SYSTEM_BASE" ];

result = Wolfram`PacletCICD`SubmitPaclet[
    File @ FileNameJoin @ { $pacDir, "ResourceDefinition.nb" }
];

If[ MatchQ[ result, _Wolfram`PacletCICD`SubmitPaclet ],
    Print[ "::error::Wolfram`PacletCICD`SubmitPaclet not defined" ];
    Exit[ 1 ],
    Print @ result
];

(* :!CodeAnalysis::EndBlock:: *)