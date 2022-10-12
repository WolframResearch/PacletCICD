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
    TestID -> "Initialize-PacletObject@@Tests/BuildPaclet.wlt:4,1-14,2"
]

(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::SuspiciousSessionSymbol:: *)
VerificationTest[
    Echo[ $TestFileName, "TestFileName" ];
    PacletDirectoryLoad @ Echo[ $pacletDir, "PacletDirectory" ],
    { ___, $pacletDir, ___ },
    SameTest -> MatchQ,
    TestID   -> "Initialize-PacletDirectoryLoad@@Tests/BuildPaclet.wlt:18,1-24,2"
]
(* :!CodeAnalysis::EndBlock:: *)

VerificationTest[
    Needs[ "Wolfram`PacletCICD`" ],
    Null,
    TestID -> "Initialize-Needs-PacletCICD@@Tests/BuildPaclet.wlt:27,1-31,2"
]

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Context Check*)
VerificationTest[
    Context @ BuildPaclet,
    "Wolfram`PacletCICD`",
    TestID -> "Context-BuildPaclet@@Tests/BuildPaclet.wlt:36,1-40,2"
]

VerificationTest[
    Context @ ResetExampleDirectory,
    "Wolfram`PacletCICD`",
    TestID -> "Context-ResetExampleDirectory@@Tests/BuildPaclet.wlt:42,1-46,2"
]

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*ResetExampleDirectory*)
VerificationTest[
    ResetExampleDirectory @ All,
    { __Success },
    SameTest -> MatchQ,
    TestID   -> "ResetExampleDirectory-Initialization@@Tests/BuildPaclet.wlt:51,1-56,2"
]

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Definitions*)
VerificationTest[
    $PublisherID;
    $PublisherID = "SamplePublisher",
    "SamplePublisher",
    TestID -> "SetPublisherID@@Tests/BuildPaclet.wlt:61,1-66,2"
]

(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::SuspiciousSessionSymbol:: *)
VerificationTest[
    DefinitionNotebookClient`ConsolePrint;
    suppressConsole // Attributes = { HoldFirst };
    suppressConsole[ eval_ ] :=
        Block[
            {
                DefinitionNotebookClient`ConsolePrint,
                DefinitionNotebookClient`BeginConsoleGroup,
                DefinitionNotebookClient`EndConsoleGroup,
                Wolfram`PacletCICD`Private`setOutput,
                Wolfram`PacletCICD`Private`appendStepSummary,
                Print = Null &
            },
            DefinitionNotebookClient`ConsolePrint // Options = {
                "Output" -> None
            };
            Wolfram`PacletCICD`Private`noExit @ eval
        ],
    Null,
    TestID -> "SuppressConsole-Definition@@Tests/BuildPaclet.wlt:70,1-90,2"
]
(* :!CodeAnalysis::EndBlock:: *)

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
    TestID   -> "Empty-Build-Directory@@Tests/BuildPaclet.wlt:96,1-104,2"
]

VerificationTest[
    suppressConsole @ BuildPaclet[
        ExampleDirectory[ "FewIssues" ],
        "SetWorkflowValue" -> False
    ],
    _Success,
    SameTest -> MatchQ,
    TestID   -> "BuildPaclet-FewIssues@@Tests/BuildPaclet.wlt:106,1-114,2"
]

VerificationTest[
    FileNames[
        "*.paclet",
        FileNameJoin @ { First @ ExampleDirectory[ "FewIssues" ], "build" }
    ],
    { _ },
    SameTest -> MatchQ,
    TestID   -> "Nonempty-Build-Directory@@Tests/BuildPaclet.wlt:116,1-124,2"
]

VerificationTest[
    suppressConsole @ BuildPaclet[
        ExampleDirectory[ "MoreIssues" ],
        "Check"            -> True,
        "SetWorkflowValue" -> False
    ],
    Failure[ "CheckPaclet::errors", _ ],
    { CheckPaclet::errors },
    SameTest -> MatchQ,
    TestID   -> "BuildPaclet-Check-Failure@@Tests/BuildPaclet.wlt:126,1-136,2"
]

VerificationTest[
    suppressConsole @ BuildPaclet[
        ExampleDirectory[ "MoreIssues" ],
        "Check"            -> False,
        "SetWorkflowValue" -> False
    ],
    Success[ "PacletBuild", _ ],
    SameTest -> MatchQ,
    TestID   -> "BuildPaclet-Check-Skipped@@Tests/BuildPaclet.wlt:138,1-147,2"
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
        "Check"            -> True,
        "SetWorkflowValue" -> False,
        "DisabledHints"    -> {
            Inherited,
            "CodeInspectionIssues",
            "CodeInspectionFileIssue/*",
            "PublisherUpdateNotAllowed",
            "NotPublisherContext"
        }
    ],
    Success[ "PacletBuild", _ ],
    SameTest -> MatchQ,
    TestID   -> "BuildPaclet-DisabledHints-1@@Tests/BuildPaclet.wlt:156,1-172,2"
]

VerificationTest[
    suppressConsole @ BuildPaclet[
        ExampleDirectory[ "MoreIssues" ],
        "Check"            -> True,
        "SetWorkflowValue" -> False,
        "DisabledHints"    -> {
            Inherited,
            "CodeInspectionIssues",
            "CodeInspectionFileIssue",
            "PublisherUpdateNotAllowed",
            "NotPublisherContext"
        }
    ],
    Success[ "PacletBuild", _ ],
    SameTest -> MatchQ,
    TestID   -> "BuildPaclet-DisabledHints-2@@Tests/BuildPaclet.wlt:174,1-190,2"
]

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*FailureCondition*)
VerificationTest[
    suppressConsole @ BuildPaclet[
        ExampleDirectory[ "MoreIssues" ],
        "Check"            -> True,
        "SetWorkflowValue" -> False,
        "DisabledHints"    -> {
            Inherited,
            "CodeInspectionIssues",
            "CodeInspectionFileIssue"
        },
        "FailureCondition" -> { "Warning", "Error" }
    ],
    Failure[ "CheckPaclet::errors", _ ],
    { CheckPaclet::errors },
    SameTest -> MatchQ,
    TestID   -> "BuildPaclet-FailureCondition@@Tests/BuildPaclet.wlt:195,1-211,2"
]

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Cleanup*)
VerificationTest[
    ResetExampleDirectory @ All,
    { __Success },
    SameTest -> MatchQ,
    TestID   -> "ResetExampleDirectory-Cleanup@@Tests/BuildPaclet.wlt:216,1-221,2"
]

VerificationTest[
    $PublisherID = None,
    None,
    TestID -> "ClearPublisherID@@Tests/BuildPaclet.wlt:223,1-227,2"
]