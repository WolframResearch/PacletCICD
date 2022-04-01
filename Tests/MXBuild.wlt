(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Initialization*)
VerificationTest[
    PacletObjectQ @ PacletObject @ File[
        $pacletDir =
            Module[ { root, mx },
                root = DirectoryName[ $TestFileName, 2 ];
                mx = FileNameJoin @ { root, "MXBuild" };
                If[ DirectoryQ @ mx, mx, root ]
            ]
    ],
    TestID -> "Initialize-PacletObject@@Tests/MXBuild.wlt:4,1-14,2"
]

(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::SuspiciousSessionSymbol:: *)
VerificationTest[
    Echo[ $TestFileName, "TestFileName" ];
    PacletDirectoryLoad @ Echo[ $pacletDir, "PacletDirectory" ],
    { ___, $pacletDir, ___ },
    SameTest -> MatchQ,
    TestID   -> "Initialize-PacletDirectoryLoad@@Tests/MXBuild.wlt:18,1-24,2"
]
(* :!CodeAnalysis::EndBlock:: *)

VerificationTest[
    Needs[ "Wolfram`PacletCICD`" ],
    Null,
    TestID -> "Initialize-Paclet-Needs@@Tests/MXBuild.wlt:27,1-31,2"
]

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Setup*)
VerificationTest[
    BooleanQ @ Wolfram`PacletCICD`Internal`$MX,
    True,
    TestID -> "MX-Boolean@@Tests/MXBuild.wlt:36,1-40,2"
]

VerificationTest[
    IfMX[ f_ ][ actual_, expected_ ] :=
        If[ TrueQ @ Wolfram`PacletCICD`Internal`$MX,
            f[ actual, expected ],
            True
        ],
    Null,
    TestID -> "Define-IfMX@@Tests/MXBuild.wlt:42,1-50,2"
]

VerificationTest[
    DirectoryQ @ Wolfram`PacletCICD`Private`$thisPacletDir,
    True,
    TestID -> "ThisPacletDirectory@@Tests/MXBuild.wlt:52,1-56,2"
]

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*LatestActionName*)
VerificationTest[
    ReplaceAll[
        HoldComplete[
            Wolfram`PacletCICD`Private`latestActionName[
                "rhennigan/build-paclet@latest",
                "rhennigan",
                "build-paclet"
            ],
            Wolfram`PacletCICD`Private`latestActionName[
                "rhennigan/check-paclet@latest",
                "rhennigan",
                "check-paclet"
            ],
            Wolfram`PacletCICD`Private`latestActionName[
                "rhennigan/submit-paclet@latest",
                "rhennigan",
                "submit-paclet"
            ],
            Wolfram`PacletCICD`Private`latestActionName[
                "rhennigan/test-paclet@latest",
                "rhennigan",
                "test-paclet"
            ]
        ],
        DownValues @ Wolfram`PacletCICD`Private`latestActionName
    ],
    HoldComplete[ _String, _String, _String, _String ],
    SameTest -> IfMX @ MatchQ,
    TestID   -> "LatestActionName@@Tests/MXBuild.wlt:61,1-90,2"
]

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*WXFResource*)
VerificationTest[
    ReplaceAll[
        HoldComplete[
            Wolfram`PacletCICD`Private`wxfResource[ "DefaultIcon"        ],
            Wolfram`PacletCICD`Private`wxfResource[ "GitHubIcon"         ],
            Wolfram`PacletCICD`Private`wxfResource[ "PublisherTokenIcon" ],
            Wolfram`PacletCICD`Private`wxfResource[ "TerminalIcon"       ]
        ],
        DownValues @ Wolfram`PacletCICD`Private`wxfResource
    ],
    HoldComplete[ _Graphics, _Graphics, _Graphics, _Graphics ],
    SameTest -> IfMX @ MatchQ,
    TestID   -> "WXFResource@@Tests/MXBuild.wlt:95,1-108,2"
]

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*WLResource*)
VerificationTest[
    ReplaceAll[
        HoldComplete @ Wolfram`PacletCICD`Private`wlResource[
            "GitHubEnvironmentData"
        ],
        DownValues @ Wolfram`PacletCICD`Private`wlResource
    ],
    HoldComplete @ { { _String, _String }.. },
    SameTest -> IfMX @ MatchQ,
    TestID   -> "WLResource@@Tests/MXBuild.wlt:113,1-123,2"
]

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*ActionIcon*)

(* TODO *)