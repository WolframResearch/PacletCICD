(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Initialization*)
VerificationTest[
    PacletObjectQ @ PacletObject @ File[
        $pacletDir = DirectoryName[ $TestFileName, 2 ]
    ],
    TestID -> "Initialize@@Tests/ExampleDirectory.wlt:4,1-9,2"
]

VerificationTest[
    PacletDirectoryLoad @ $pacletDir,
    { ___, $pacletDir, ___ },
    SameTest -> MatchQ,
    TestID -> "Initialize@@Tests/ExampleDirectory.wlt:11,1-16,2"
]

VerificationTest[
    Needs[ "Wolfram`PacletCICD`" ],
    Null,
    TestID -> "Initialize@@Tests/ExampleDirectory.wlt:18,1-22,2"
]

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Context Check*)
VerificationTest[
    Context @ $ExamplesLocation,
    "Wolfram`PacletCICD`",
    TestID -> "$ExamplesLocation-Context@@Tests/ExampleDirectory.wlt:27,1-31,2"
]

VerificationTest[
    Context @ ExampleDirectory,
    "Wolfram`PacletCICD`",
    TestID -> "ExampleDirectory-Context@@Tests/ExampleDirectory.wlt:33,1-37,2"
]

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*$ExamplesLocation*)
VerificationTest[
    DeleteDirectory[
        $ExamplesLocation,
        DeleteContents -> True
    ],
    Null,
    TestID -> "$ExamplesLocation-Delete@@Tests/ExampleDirectory.wlt:42,1-49,2"
]

VerificationTest[
    DirectoryQ @ $ExamplesLocation,
    True,
    TestID -> "$ExamplesLocation-Create@@Tests/ExampleDirectory.wlt:51,1-55,2"
]

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*ExampleDirectory*)
VerificationTest[
    DeleteDirectory[ ExampleDirectory[ "FewIssues" ], DeleteContents -> True ],
    Null,
    TestID -> "ExampleDirectory-FewIssues-Delete@@Tests/ExampleDirectory.wlt:60,1-64,2"
]

VerificationTest[
    DeleteDirectory[ ExampleDirectory[ "MoreIssues" ], DeleteContents -> True ],
    Null,
    TestID -> "ExampleDirectory-MoreIssues-Delete@@Tests/ExampleDirectory.wlt:66,1-70,2"
]

VerificationTest[
    DirectoryQ @ ExampleDirectory[ "FewIssues" ],
    True,
    TestID -> "ExampleDirectory-FewIssues-Create@@Tests/ExampleDirectory.wlt:72,1-76,2"
]

VerificationTest[
    DirectoryQ @ ExampleDirectory[ "MoreIssues" ],
    True,
    TestID -> "ExampleDirectory-MoreIssues-Create@@Tests/ExampleDirectory.wlt:78,1-82,2"
]

VerificationTest[
    FileNameTake /@ FileNames[ All, ExampleDirectory[ "FewIssues" ] ],
    {
        "DefinitionNotebook.nb",
        "Documentation",
        ".github",
        "Kernel",
        "PacletInfo.wl"
    },
    TestID -> "ExampleDirectory-FewIssues-Files@@Tests/ExampleDirectory.wlt:84,1-94,2"
]

VerificationTest[
    FileNameTake /@ FileNames[ All, ExampleDirectory[ "MoreIssues" ] ],
    {
        "DefinitionNotebook.nb",
        "Documentation",
        ".github",
        "Kernel",
        "PacletInfo.wl"
    },
    TestID -> "ExampleDirectory-MoreIssues-Files@@Tests/ExampleDirectory.wlt:96,1-106,2"
]