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
    TestID -> "Context-ResetExampleDirectory@@Tests/BuildPaclet.wlt:32,1-36,2"
]

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*ResetExampleDirectory*)
VerificationTest[
    ResetExampleDirectory @ All,
    _Success,
    SameTest -> MatchQ,
    TestID   -> "ResetExampleDirectory-Initialization@@Tests/BuildPaclet.wlt:41,1-46,2"
]

VerificationTest[
    $PublisherID = "MyPublisher",
    "MyPublisher",
    TestID -> "SetPublisherID@@Tests/BuildPaclet.wlt:48,1-52,2"
]

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*BuildPaclet*)
VerificationTest[
    BuildPaclet @ ExampleDirectory[ "FewIssues" ],
    _Success,
    SameTest -> MatchQ,
    TestID   -> "BuildPaclet-FewIssues@@Tests/BuildPaclet.wlt:57,1-62,2"
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
    TestID   -> "BuildPaclet-Check-Failure@@Tests/BuildPaclet.wlt:64,1-74,2"
]

VerificationTest[
    BuildPaclet[
        ExampleDirectory[ "MoreIssues" ],
        "Check" -> False,
        "ConsoleType" -> None
    ],
    Success[ "PacletBuild", _ ],
    SameTest -> MatchQ,
    TestID   -> "BuildPaclet-Check-Skipped@@Tests/BuildPaclet.wlt:76,1-85,2"
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
    TestID   -> "BuildPaclet-DisabledHints@@Tests/BuildPaclet.wlt:94,1-104,2"
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
    TestID   -> "BuildPaclet-FailureCondition@@Tests/BuildPaclet.wlt:109,1-121,2"
]

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Cleanup*)
VerificationTest[
    ResetExampleDirectory @ All,
    _Success,
    SameTest -> MatchQ,
    TestID -> "ResetExampleDirectory-Cleanup@@Tests/BuildPaclet.wlt:126,1-131,2"
]

VerificationTest[
    $PublisherID = None,
    None,
    TestID -> "ClearPublisherID@@Tests/BuildPaclet.wlt:133,1-137,2"
]