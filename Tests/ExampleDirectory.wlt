(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Initialization*)
VerificationTest[
    PacletObjectQ @ PacletObject @ File[
        $pacletDir =
            Module[ { root, mx },
                root = DirectoryName[ $TestFileName, 2 ];
                mx = FileNameJoin @ { root, "MXBuild", "Wolfram__PacletCICD" };
                If[ DirectoryQ @ mx, mx, root ]
            ]
    ],
    TestID -> "Initialize-PacletObject@@Tests/ExampleDirectory.wlt:4,1-14,2"
]

(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::SuspiciousSessionSymbol:: *)
VerificationTest[
    Echo[ $TestFileName, "TestFileName" ];
    PacletDirectoryLoad @ Echo[ $pacletDir, "PacletDirectory" ],
    { ___, $pacletDir, ___ },
    SameTest -> MatchQ,
    TestID   -> "Initialize-PacletDirectoryLoad@@Tests/ExampleDirectory.wlt:18,1-24,2"
]
(* :!CodeAnalysis::EndBlock:: *)

VerificationTest[
    Needs[ "Wolfram`PacletCICD`" ],
    Null,
    TestID -> "Initialize@@Tests/ExampleDirectory.wlt:27,1-31,2"
]

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Context Check*)
VerificationTest[
    Context @ $ExamplesLocation,
    "Wolfram`PacletCICD`",
    TestID -> "$ExamplesLocation-Context@@Tests/ExampleDirectory.wlt:36,1-40,2"
]

VerificationTest[
    Context @ ExampleDirectory,
    "Wolfram`PacletCICD`",
    TestID -> "ExampleDirectory-Context@@Tests/ExampleDirectory.wlt:42,1-46,2"
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
    TestID -> "$ExamplesLocation-Delete@@Tests/ExampleDirectory.wlt:51,1-58,2"
]

VerificationTest[
    DirectoryQ[ exDir = ExampleDirectory[ "Sample" ] ],
    True,
    TestID -> "$ExamplesLocation-Create@@Tests/ExampleDirectory.wlt:60,1-64,2"
]

VerificationTest[
    ResetExampleDirectory @ All,
    { __Success },
    SameTest -> MatchQ,
    TestID -> "ResetExampleDirectory-All@@Tests/ExampleDirectory.wlt:66,1-71,2"
]

VerificationTest[
    DirectoryQ @ exDir,
    False,
    TestID -> "ResetExampleDirectory-Verified@@Tests/ExampleDirectory.wlt:73,1-77,2"
]

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*ExampleDirectory*)
VerificationTest[
    DeleteDirectory[ ExampleDirectory[ "FewIssues" ], DeleteContents -> True ],
    Null,
    TestID -> "ExampleDirectory-FewIssues-Delete@@Tests/ExampleDirectory.wlt:82,1-86,2"
]

VerificationTest[
    DeleteDirectory[ ExampleDirectory[ "MoreIssues" ], DeleteContents -> True ],
    Null,
    TestID -> "ExampleDirectory-MoreIssues-Delete@@Tests/ExampleDirectory.wlt:88,1-92,2"
]

VerificationTest[
    DirectoryQ @ ExampleDirectory[ "FewIssues" ],
    True,
    TestID -> "ExampleDirectory-FewIssues-Create@@Tests/ExampleDirectory.wlt:94,1-98,2"
]

VerificationTest[
    DirectoryQ @ ExampleDirectory[ "MoreIssues" ],
    True,
    TestID -> "ExampleDirectory-MoreIssues-Create@@Tests/ExampleDirectory.wlt:100,1-104,2"
]

VerificationTest[
    DirectoryQ @ ExampleDirectory[ "Sample" ],
    True,
    TestID -> "ExampleDirectory-Sample-Create@@Tests/ExampleDirectory.wlt:106,1-110,2"
]

VerificationTest[
    DirectoryQ @ ExampleDirectory[ "AdvancedSample" ],
    True,
    TestID -> "ExampleDirectory-AdvancedSample-Create@@Tests/ExampleDirectory.wlt:112,1-116,2"
]

VerificationTest[
    FileNameTake /@ FileNames[ All, ExampleDirectory[ "FewIssues" ] ],
    {
        "Documentation",
        "Kernel",
        "LICENSE",
        "PacletInfo.wl",
        "README.md",
        "ResourceDefinition.nb"
    },
    TestID -> "ExampleDirectory-FewIssues-Files@@Tests/ExampleDirectory.wlt:118,1-129,2"
]

VerificationTest[
    FileNameTake /@ FileNames[ All, ExampleDirectory[ "MoreIssues" ] ],
    {
        "Documentation",
        "Kernel",
        "LICENSE",
        "PacletInfo.wl",
        "README.md",
        "ResourceDefinition.nb"
    },
    TestID -> "ExampleDirectory-MoreIssues-Files@@Tests/ExampleDirectory.wlt:131,1-142,2"
]

VerificationTest[
    FileNameTake /@ FileNames[ All, ExampleDirectory[ "Sample" ] ],
    {
        "Documentation",
        "Kernel",
        "LICENSE",
        "PacletInfo.wl",
        "README.md",
        "ResourceDefinition.nb",
        "Tests"
    },
    TestID -> "ExampleDirectory-Sample-Files@@Tests/ExampleDirectory.wlt:144,1-156,2"
]

VerificationTest[
    FileNameTake /@ FileNames[ All, ExampleDirectory[ "AdvancedSample" ] ],
    {
        "Documentation",
        "Kernel",
        "LICENSE",
        "PacletInfo.wl",
        "README.md",
        "ResourceDefinition.nb",
        "Scripts",
        "Source",
        "Tests"
    },
    TestID -> "ExampleDirectory-AdvancedSample-Files@@Tests/ExampleDirectory.wlt:158,1-172,2"
]

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Regression Tests*)

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*Missing files*)
VerificationTest[
    dir = ExampleDirectory[ "Sample" ],
    _File? DirectoryQ,
    SameTest -> MatchQ,
    TestID   -> "MissingFile-Get-Sample@@Tests/ExampleDirectory.wlt:181,1-186,2"
]

VerificationTest[
    file = FileNameJoin @ { First @ dir, "PacletInfo.wl" },
    _? FileExistsQ,
    SameTest -> MatchQ,
    TestID   -> "MissingFile-Get-PacletInfo@@Tests/ExampleDirectory.wlt:188,1-193,2"
]

VerificationTest[
    hash = FileHash @ file,
    _? IntegerQ,
    SameTest -> MatchQ,
    TestID   -> "MissingFile-Get-FileHash@@Tests/ExampleDirectory.wlt:195,1-200,2"
]

VerificationTest[
    DeleteFile @ file; FileExistsQ @ file,
    False,
    TestID -> "MissingFile-DeleteFile@@Tests/ExampleDirectory.wlt:202,1-206,2"
]

VerificationTest[
    ExampleDirectory[ "Sample" ],
    _File? DirectoryQ,
    SameTest -> MatchQ,
    TestID   -> "MissingFile-Restore-Directory@@Tests/ExampleDirectory.wlt:208,1-213,2"
]

VerificationTest[
    FileExistsQ @ file && FileHash @ file === hash,
    True,
    TestID   -> "MissingFile-Restore-File@@Tests/ExampleDirectory.wlt:215,1-219,2"
]
