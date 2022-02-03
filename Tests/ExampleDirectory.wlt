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
    TestID -> "Initialize@@Tests/ExampleDirectory.wlt:4,1-12,2"
]

VerificationTest[
    PacletDirectoryLoad @ Wolfram`PacletCICD`Tests`$pacletDir,
    { ___, Wolfram`PacletCICD`Tests`$pacletDir, ___ },
    SameTest -> MatchQ,
    TestID -> "Initialize@@Tests/ExampleDirectory.wlt:14,1-19,2"
]

VerificationTest[
    Block[ { $ContextPath }, Needs[ "Wolfram`PacletCICD`" ] ],
    Null,
    TestID -> "Initialize@@Tests/ExampleDirectory.wlt:21,1-25,2"
]

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*$ExamplesLocation*)
VerificationTest[
    DeleteDirectory[
        Wolfram`PacletCICD`$ExamplesLocation,
        DeleteContents -> True
    ],
    Null,
    TestID -> "$ExamplesLocation-Delete@@Tests/ExampleDirectory.wlt:30,1-37,2"
]

VerificationTest[
    DirectoryQ @ Wolfram`PacletCICD`$ExamplesLocation,
    True,
    TestID -> "$ExamplesLocation-Create@@Tests/ExampleDirectory.wlt:39,1-43,2"
]

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*ExampleDirectory*)
VerificationTest[
    DeleteDirectory[
        Wolfram`PacletCICD`ExampleDirectory[ "FewIssues" ],
        DeleteContents -> True
    ],
    Null,
    TestID -> "ExampleDirectory-FewIssues-Delete@@Tests/ExampleDirectory.wlt:48,1-55,2"
]

VerificationTest[
    DeleteDirectory[
        Wolfram`PacletCICD`ExampleDirectory[ "MoreIssues" ],
        DeleteContents -> True
    ],
    Null,
    TestID -> "ExampleDirectory-MoreIssues-Delete@@Tests/ExampleDirectory.wlt:57,1-64,2"
]

VerificationTest[
    DirectoryQ @ Wolfram`PacletCICD`ExampleDirectory[ "FewIssues" ],
    True,
    TestID -> "ExampleDirectory-FewIssues-Create@@Tests/ExampleDirectory.wlt:66,1-70,2"
]

VerificationTest[
    DirectoryQ @ Wolfram`PacletCICD`ExampleDirectory[ "MoreIssues" ],
    True,
    TestID -> "ExampleDirectory-MoreIssues-Create@@Tests/ExampleDirectory.wlt:72,1-76,2"
]

VerificationTest[
    Map[
        FileNameTake,
        FileNames[ All, Wolfram`PacletCICD`ExampleDirectory[ "FewIssues" ] ]
    ],
    {
        "DefinitionNotebook.nb",
        "Documentation",
        ".github",
        "Kernel",
        "PacletInfo.wl"
    },
    TestID -> "ExampleDirectory-FewIssues-Files@@Tests/ExampleDirectory.wlt:78,1-91,2"
]

VerificationTest[
    Map[
        FileNameTake,
        FileNames[ All, Wolfram`PacletCICD`ExampleDirectory[ "MoreIssues" ] ]
    ],
    {
        "DefinitionNotebook.nb",
        "Documentation",
        ".github",
        "Kernel",
        "PacletInfo.wl"
    },
    TestID -> "ExampleDirectory-MoreIssues-Files@@Tests/ExampleDirectory.wlt:93,1-106,2"
]