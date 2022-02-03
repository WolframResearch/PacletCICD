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
    TestID -> "Initialize"
]

VerificationTest[
    PacletDirectoryLoad @ Wolfram`PacletCICD`Tests`$pacletDir,
    { ___, Wolfram`PacletCICD`Tests`$pacletDir, ___ },
    SameTest -> MatchQ,
    TestID -> "Initialize"
]

VerificationTest[
    Block[ { $ContextPath }, Needs[ "Wolfram`PacletCICD`" ] ],
    Null,
    TestID -> "Initialize"
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
    TestID -> "$ExamplesLocation-Delete"
]

VerificationTest[
    DirectoryQ @ Wolfram`PacletCICD`$ExamplesLocation,
    True,
    TestID -> "$ExamplesLocation-Create"
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
    TestID -> "ExampleDirectory-FewIssues-Delete"
]

VerificationTest[
    DeleteDirectory[
        Wolfram`PacletCICD`ExampleDirectory[ "MoreIssues" ],
        DeleteContents -> True
    ],
    Null,
    TestID -> "ExampleDirectory-MoreIssues-Delete"
]

VerificationTest[
    DirectoryQ @ Wolfram`PacletCICD`ExampleDirectory[ "FewIssues" ],
    True,
    TestID -> "ExampleDirectory-FewIssues-Create"
]

VerificationTest[
    DirectoryQ @ Wolfram`PacletCICD`ExampleDirectory[ "MoreIssues" ],
    True,
    TestID -> "ExampleDirectory-MoreIssues-Create"
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
    TestID -> "ExampleDirectory-FewIssues-Files"
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
    TestID -> "ExampleDirectory-MoreIssues-Files"
]