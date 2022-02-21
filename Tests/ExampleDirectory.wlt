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
    DirectoryQ[ exDir = $ExamplesLocation ],
    True,
    TestID -> "$ExamplesLocation-Create@@Tests/ExampleDirectory.wlt:51,1-55,2"
]

VerificationTest[
    ResetExampleDirectory @ All,
    _Success,
    SameTest -> MatchQ,
    TestID -> "ResetExampleDirectory-All@@Tests/ExampleDirectory.wlt:57,1-62,2"
]

VerificationTest[
    DirectoryQ @ exDir,
    False,
    TestID -> "ResetExampleDirectory-Verified@@Tests/ExampleDirectory.wlt:64,1-68,2"
]

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*ExampleDirectory*)
VerificationTest[
    DeleteDirectory[ ExampleDirectory[ "FewIssues" ], DeleteContents -> True ],
    Null,
    TestID -> "ExampleDirectory-FewIssues-Delete@@Tests/ExampleDirectory.wlt:73,1-77,2"
]

VerificationTest[
    DeleteDirectory[ ExampleDirectory[ "MoreIssues" ], DeleteContents -> True ],
    Null,
    TestID -> "ExampleDirectory-MoreIssues-Delete@@Tests/ExampleDirectory.wlt:79,1-83,2"
]

VerificationTest[
    DirectoryQ @ ExampleDirectory[ "FewIssues" ],
    True,
    TestID -> "ExampleDirectory-FewIssues-Create@@Tests/ExampleDirectory.wlt:85,1-89,2"
]

VerificationTest[
    DirectoryQ @ ExampleDirectory[ "MoreIssues" ],
    True,
    TestID -> "ExampleDirectory-MoreIssues-Create@@Tests/ExampleDirectory.wlt:91,1-95,2"
]

VerificationTest[
    DirectoryQ @ ExampleDirectory[ "Sample" ],
    True,
    TestID -> "ExampleDirectory-Sample-Create@@Tests/ExampleDirectory.wlt:97,1-101,2"
]

VerificationTest[
    DirectoryQ @ ExampleDirectory[ "AdvancedSample" ],
    True,
    TestID -> "ExampleDirectory-AdvancedSample-Create@@Tests/ExampleDirectory.wlt:103,1-107,2"
]

VerificationTest[
    FileNameTake /@ FileNames[ All, ExampleDirectory[ "FewIssues" ] ],
    {
        "DefinitionNotebook.nb",
        "Documentation",
        "Kernel",
        "LICENSE",
        "PacletInfo.wl",
        "README.md"
    },
    TestID -> "ExampleDirectory-FewIssues-Files@@Tests/ExampleDirectory.wlt:109,1-120,2"
]

VerificationTest[
    FileNameTake /@ FileNames[ All, ExampleDirectory[ "MoreIssues" ] ],
    {
        "DefinitionNotebook.nb",
        "Documentation",
        "Kernel",
        "LICENSE",
        "PacletInfo.wl",
        "README.md"
    },
    TestID -> "ExampleDirectory-MoreIssues-Files@@Tests/ExampleDirectory.wlt:122,1-133,2"
]

VerificationTest[
    FileNameTake /@ FileNames[ All, ExampleDirectory[ "Sample" ] ],
    {
        "DefinitionNotebook.nb",
        "Documentation",
        "Kernel",
        "LICENSE",
        "PacletInfo.wl",
        "README.md",
        "Tests"
    },
    TestID -> "ExampleDirectory-Sample-Files@@Tests/ExampleDirectory.wlt:135,1-147,2"
]

VerificationTest[
    FileNameTake /@ FileNames[ All, ExampleDirectory[ "AdvancedSample" ] ],
    {
        "DefinitionNotebook.nb",
        "Documentation",
        "Kernel",
        "LICENSE",
        "PacletInfo.wl",
        "README.md",
        "Scripts",
        "Source",
        "Tests"
    },
    TestID -> "ExampleDirectory-AdvancedSample-Files@@Tests/ExampleDirectory.wlt:149,1-163,2"
]