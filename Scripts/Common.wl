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

        Confirm @ BinaryWrite[ file, new ]
    ],
    Function[
        Print[ "::error::Failed to update PacletInfo template parameters." ];
        Exit[ 1 ]
    ]
];

(* :!CodeAnalysis::EndBlock:: *)