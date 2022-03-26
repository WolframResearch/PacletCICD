(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Initialization*)
VerificationTest[
    PacletObjectQ @ PacletObject @ File[
        $pacletDir = DirectoryName[ $TestFileName, 2 ]
    ],
    TestID -> "Initialize-PacletObject@@Tests/BuildPaclet.wlt:4,1-9,2"
]

VerificationTest[
    PacletDirectoryLoad @ $pacletDir,
    { ___, $pacletDir, ___ },
    SameTest -> MatchQ,
    TestID   -> "Initialize-PacletDirectoryLoad@@Tests/BuildPaclet.wlt:11,1-16,2"
]

VerificationTest[
    Needs[ "Wolfram`PacletCICD`" ],
    Null,
    TestID -> "Initialize-Needs-PacletCICD@@Tests/BuildPaclet.wlt:18,1-22,2"
]

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Context Check*)
VerificationTest[
    Context @ BuildPaclet,
    "Wolfram`PacletCICD`",
    TestID -> "Context-BuildPaclet@@Tests/BuildPaclet.wlt:27,1-31,2"
]

VerificationTest[
    Context @ ResetExampleDirectory,
    "Wolfram`PacletCICD`",
    TestID -> "Context-ResetExampleDirectory@@Tests/BuildPaclet.wlt:33,1-37,2"
]

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*ResetExampleDirectory*)
VerificationTest[
    ResetExampleDirectory @ All,
    { __Success },
    SameTest -> MatchQ,
    TestID   -> "ResetExampleDirectory-Initialization@@Tests/BuildPaclet.wlt:42,1-47,2"
]

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Definitions*)
VerificationTest[
    $PublisherID;
    $PublisherID = "MyPublisher",
    "MyPublisher",
    TestID -> "SetPublisherID@@Tests/BuildPaclet.wlt:52,1-57,2"
]

VerificationTest[
    DefinitionNotebookClient`ConsolePrint;
    suppressConsole // Attributes = { HoldFirst };
    suppressConsole[ eval_ ] :=
        Block[ { DefinitionNotebookClient`ConsolePrint },
            Wolfram`PacletCICD`Private`noExit @ eval
        ],
    Null,
    TestID -> "SuppressConsole-Definition@@Tests/BuildPaclet.wlt:59,1-68,2"
]

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*BuildPaclet*)
VerificationTest[
    FileNames[
        "*.paclet",
        FileNameJoin @ { First @ ExampleDirectory[ "FewIssues" ], "build" }
    ],
    { },
    SameTest -> MatchQ,
    TestID   -> "Empty-Build-Directory@@Tests/BuildPaclet.wlt:73,1-81,2"
]

VerificationTest[
    suppressConsole @ BuildPaclet @ ExampleDirectory[ "FewIssues" ],
    _Success,
    SameTest -> MatchQ,
    TestID   -> "BuildPaclet-FewIssues@@Tests/BuildPaclet.wlt:83,1-88,2"
]

VerificationTest[
    FileNames[
        "*.paclet",
        FileNameJoin @ { First @ ExampleDirectory[ "FewIssues" ], "build" }
    ],
    { _ },
    SameTest -> MatchQ,
    TestID   -> "Nonempty-Build-Directory@@Tests/BuildPaclet.wlt:90,1-98,2"
]

VerificationTest[
    suppressConsole @ BuildPaclet[
        ExampleDirectory[ "MoreIssues" ],
        "Check" -> True
    ],
    Failure[ "CheckPaclet::errors", _ ],
    { CheckPaclet::errors },
    SameTest -> MatchQ,
    TestID   -> "BuildPaclet-Check-Failure@@Tests/BuildPaclet.wlt:100,1-109,2"
]

VerificationTest[
    suppressConsole @ BuildPaclet[
        ExampleDirectory[ "MoreIssues" ],
        "Check" -> False
    ],
    Success[ "PacletBuild", _ ],
    SameTest -> MatchQ,
    TestID   -> "BuildPaclet-Check-Skipped@@Tests/BuildPaclet.wlt:111,1-119,2"
]

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Options*)

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*DisabledHints*)
VerificationTest[
    suppressConsole @ BuildPaclet[
        ExampleDirectory[ "MoreIssues" ],
        "Check"         -> True,
        "DisabledHints" -> {
            Inherited,
            "CodeInspectionIssues",
            "PublisherUpdateNotAllowed",
            "NotPublisherContext"
        }
    ],
    Success[ "PacletBuild", _ ],
    SameTest -> MatchQ,
    TestID   -> "BuildPaclet-DisabledHints@@Tests/BuildPaclet.wlt:128,1-142,2"
]

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*FailureCondition*)
VerificationTest[
    suppressConsole @ BuildPaclet[
        ExampleDirectory[ "MoreIssues" ],
        "Check"            -> True,
        "DisabledHints"    -> { Inherited, "CodeInspectionIssues" },
        "FailureCondition" -> { "Warning", "Error" }
    ],
    Failure[ "CheckPaclet::errors", _ ],
    { CheckPaclet::errors },
    SameTest -> MatchQ,
    TestID   -> "BuildPaclet-FailureCondition@@Tests/BuildPaclet.wlt:147,1-158,2"
]

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Cleanup*)
VerificationTest[
    ResetExampleDirectory @ All,
    { __Success },
    SameTest -> MatchQ,
    TestID   -> "ResetExampleDirectory-Cleanup@@Tests/BuildPaclet.wlt:163,1-168,2"
]

VerificationTest[
    $PublisherID = None,
    None,
    TestID -> "ClearPublisherID@@Tests/BuildPaclet.wlt:170,1-174,2"
]