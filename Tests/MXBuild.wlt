(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Initialization*)
VerificationTest[
    PacletObjectQ @ PacletObject @ File[
        $pacletDir = DirectoryName[ $TestFileName, 2 ]
    ],
    TestID -> "Initialize-Paclet-Directory"
]

VerificationTest[
    PacletDirectoryLoad @ $pacletDir,
    { ___, $pacletDir, ___ },
    SameTest -> MatchQ,
    TestID   -> "Initialize-Paclet-Load"
]

VerificationTest[
    Needs[ "Wolfram`PacletCICD`" ],
    Null,
    TestID -> "Initialize-Paclet-Needs"
]

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Setup*)
VerificationTest[
    BooleanQ @ Wolfram`PacletCICD`Internal`$MX,
    True,
    TestID -> "MX-Boolean"
]

VerificationTest[
    IfMX[ f_ ][ actual_, expected_ ] :=
        If[ TrueQ @ Wolfram`PacletCICD`Internal`$MX,
            f[ actual, expected ],
            True
        ],
    Null,
    TestID -> "Define-IfMX"
]

VerificationTest[
    DirectoryQ @ Wolfram`PacletCICD`Private`$thisPacletDir,
    True,
    TestID -> "ThisPacletDirectory"
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
    TestID   -> "LatestActionName"
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
    TestID   -> "WXFResource"
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
    TestID   -> "WLResource"
]

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*ActionIcon*)

(* TODO *)