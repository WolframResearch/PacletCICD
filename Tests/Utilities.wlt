(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Initialization*)
VerificationTest[
    PacletObjectQ @ PacletObject @ File[
        Wolfram`PacletCICD`Tests`$pacletDir = DirectoryName[
            System`$TestFileName,
            2
        ]
    ],
    TestID -> "Initialize@@Tests/Utilities.wlt:4,1-12,2"
]

VerificationTest[
    PacletDirectoryLoad @ Wolfram`PacletCICD`Tests`$pacletDir,
    { ___, Wolfram`PacletCICD`Tests`$pacletDir, ___ },
    SameTest -> MatchQ,
    TestID -> "Initialize@@Tests/Utilities.wlt:14,1-19,2"
]

VerificationTest[
    Block[ { $ContextPath }, Needs[ "Wolfram`PacletCICD`" ] ],
    Null,
    TestID -> "Initialize@@Tests/Utilities.wlt:21,1-25,2"
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
    TestID -> "relativePath@@Tests/Utilities.wlt:50,1-57,2"
]

VerificationTest[
    Wolfram`PacletCICD`Private`relativePath[ "path/to/file", "path/to/file" ],
    ".",
    TestID -> "relativePath@@Tests/Utilities.wlt:59,1-63,2"
]

VerificationTest[
    Wolfram`PacletCICD`Private`relativePath[ "path/to/file" ][ "path/another/file" ],
    "../../another/file",
    TestID -> "relativePath@@Tests/Utilities.wlt:65,1-69,2"
]
