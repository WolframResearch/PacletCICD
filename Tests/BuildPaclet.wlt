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
    _Success,
    SameTest -> MatchQ,
    TestID   -> "ResetExampleDirectory-Initialization@@Tests/BuildPaclet.wlt:42,1-47,2"
]

VerificationTest[
    $PublisherID;
    $PublisherID = "MyPublisher",
    "MyPublisher",
    TestID -> "SetPublisherID@@Tests/BuildPaclet.wlt:49,1-54,2"
]

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*BuildPaclet*)
VerificationTest[
    BuildPaclet[ ExampleDirectory[ "FewIssues" ], "ConsoleType" -> None ],
    _Success,
    SameTest -> MatchQ,
    TestID   -> "BuildPaclet-FewIssues@@Tests/BuildPaclet.wlt:59,1-64,2"
]

VerificationTest[
    BuildPaclet[
        ExampleDirectory[ "MoreIssues" ],
        "Check" -> True,
        "ConsoleType" -> None
    ],
    Failure[ "CheckPaclet::errors", _ ],
    { CheckPaclet::errors },
    SameTest -> MatchQ,
    TestID   -> "BuildPaclet-Check-Failure@@Tests/BuildPaclet.wlt:66,1-76,2"
]

VerificationTest[
    BuildPaclet[
        ExampleDirectory[ "MoreIssues" ],
        "Check" -> False,
        "ConsoleType" -> None
    ],
    Success[ "PacletBuild", _ ],
    SameTest -> MatchQ,
    TestID   -> "BuildPaclet-Check-Skipped@@Tests/BuildPaclet.wlt:78,1-87,2"
]

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Options*)

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*DisabledHints*)
VerificationTest[
    BuildPaclet[
        ExampleDirectory[ "MoreIssues" ],
        "ConsoleType"   -> None,
        "Check"         -> True,
        "DisabledHints" -> { Inherited, "CodeInspectionIssues" }
    ],
    Success[ "PacletBuild", _ ],
    SameTest -> MatchQ,
    TestID   -> "BuildPaclet-DisabledHints@@Tests/BuildPaclet.wlt:96,1-106,2"
]

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*FailureCondition*)
VerificationTest[
    BuildPaclet[
        ExampleDirectory[ "MoreIssues" ],
        "ConsoleType"      -> None,
        "Check"            -> True,
        "DisabledHints"    -> { Inherited, "CodeInspectionIssues" },
        "FailureCondition" -> { "Warning", "Error" }
    ],
    Failure[ "CheckPaclet::errors", _ ],
    { CheckPaclet::errors },
    SameTest -> MatchQ,
    TestID   -> "BuildPaclet-FailureCondition@@Tests/BuildPaclet.wlt:111,1-123,2"
]

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Cleanup*)
VerificationTest[
    ResetExampleDirectory @ All,
    _Success,
    SameTest -> MatchQ,
    TestID -> "ResetExampleDirectory-Cleanup@@Tests/BuildPaclet.wlt:128,1-133,2"
]

VerificationTest[
    $PublisherID = None,
    None,
    TestID -> "ClearPublisherID@@Tests/BuildPaclet.wlt:135,1-139,2"
]