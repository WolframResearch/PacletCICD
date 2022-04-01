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
    TestID -> "Initialize-PacletObject@@Tests/Utilities.wlt:4,1-14,2"
]

(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::SuspiciousSessionSymbol:: *)
VerificationTest[
    Echo[ $TestFileName, "TestFileName" ];
    PacletDirectoryLoad @ Echo[ $pacletDir, "PacletDirectory" ],
    { ___, $pacletDir, ___ },
    SameTest -> MatchQ,
    TestID   -> "Initialize-PacletDirectoryLoad@@Tests/Utilities.wlt:18,1-24,2"
]
(* :!CodeAnalysis::EndBlock:: *)

VerificationTest[
    Needs[ "Wolfram`PacletCICD`" ],
    Null,
    TestID -> "Initialize@@Tests/Utilities.wlt:27,1-31,2"
]

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*relativePath*)
VerificationTest[
    Wolfram`PacletCICD`Private`relativePath[
        "path/to/file",
        "path/another/file"
    ],
    "../../another/file",
    TestID -> "relativePath@@Tests/Utilities.wlt:36,1-43,2"
]

VerificationTest[
    Wolfram`PacletCICD`Private`relativePath[ "path/to/file", "path/to/file" ],
    ".",
    TestID -> "relativePath@@Tests/Utilities.wlt:45,1-49,2"
]

VerificationTest[
    Wolfram`PacletCICD`Private`relativePath[ "path/to/file" ][
        "path/another/file"
    ],
    "../../another/file",
    TestID -> "relativePath@@Tests/Utilities.wlt:51,1-57,2"
]
