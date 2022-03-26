(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Initialization*)
VerificationTest[
    PacletObjectQ @ PacletObject @ File[
        $pacletDir = DirectoryName[ $TestFileName, 2 ]
    ],
    TestID -> "Initialize@@Tests/Utilities.wlt:4,1-9,2"
]

VerificationTest[
    PacletDirectoryLoad @ $pacletDir,
    { ___, $pacletDir, ___ },
    SameTest -> MatchQ,
    TestID -> "Initialize@@Tests/Utilities.wlt:11,1-16,2"
]

VerificationTest[
    Needs[ "Wolfram`PacletCICD`" ],
    Null,
    TestID -> "Initialize@@Tests/Utilities.wlt:18,1-22,2"
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
    TestID -> "relativePath@@Tests/Utilities.wlt:27,1-34,2"
]

VerificationTest[
    Wolfram`PacletCICD`Private`relativePath[ "path/to/file", "path/to/file" ],
    ".",
    TestID -> "relativePath@@Tests/Utilities.wlt:36,1-40,2"
]

VerificationTest[
    Wolfram`PacletCICD`Private`relativePath[ "path/to/file" ][
        "path/another/file"
    ],
    "../../another/file",
    TestID -> "relativePath@@Tests/Utilities.wlt:42,1-48,2"
]

VerificationTest[
    1+1,
    3,
    TestID -> "FailureTest"
]