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
    TestID -> "Initialize-PacletObject@@Tests/TestPaclet.wlt:4,1-14,2"
]

(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::SuspiciousSessionSymbol:: *)
VerificationTest[
    Echo[ $TestFileName, "TestFileName" ];
    PacletDirectoryLoad @ Echo[ $pacletDir, "PacletDirectory" ],
    { ___, $pacletDir, ___ },
    SameTest -> MatchQ,
    TestID   -> "Initialize-PacletDirectoryLoad@@Tests/TestPaclet.wlt:18,1-24,2"
]
(* :!CodeAnalysis::EndBlock:: *)

VerificationTest[
    Needs[ "Wolfram`PacletCICD`" ],
    Null,
    TestID -> "Initialize-Paclet-Needs@@Tests/TestPaclet.wlt:27,1-31,2"
]

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Context Check*)
VerificationTest[
    Context @ TestPaclet,
    "Wolfram`PacletCICD`",
    TestID -> "TestPaclet-Context@@Tests/TestPaclet.wlt:36,1-40,2"
]

VerificationTest[
    Context @ ResetExampleDirectory,
    "Wolfram`PacletCICD`",
    TestID -> "Context-ResetExampleDirectory@@Tests/TestPaclet.wlt:42,1-46,2"
]

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*ResetExampleDirectory*)
VerificationTest[
    ResetExampleDirectory @ All,
    { __Success },
    SameTest -> MatchQ,
    TestID   -> "ResetExampleDirectory-Initialization@@Tests/TestPaclet.wlt:51,1-56,2"
]

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Definitions*)
VerificationTest[
    $PublisherID;
    $PublisherID = "SamplePublisher",
    "SamplePublisher",
    TestID -> "SetPublisherID@@Tests/TestPaclet.wlt:61,1-66,2"
]

VerificationTest[
    DefinitionNotebookClient`ConsolePrint;
    suppressConsole // Attributes = { HoldFirst };
    suppressConsole[ eval_ ] :=
        Block[
            {
                DefinitionNotebookClient`ConsolePrint,
                Wolfram`PacletCICD`Private`setOutput,
                Wolfram`PacletCICD`Private`appendStepSummary
            },
            DefinitionNotebookClient`ConsolePrint // Options = {
                "Output" -> None
            };
            Wolfram`PacletCICD`Private`noExit @ eval
        ],
    Null,
    TestID -> "SuppressConsole-Definition@@Tests/TestPaclet.wlt:68,1-85,2"
]

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*TestPaclet*)

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Sample*)
VerificationTest[
    FileNames[
        "*.paclet",
        FileNameJoin @ { First @ ExampleDirectory[ "Sample" ], "build" }
    ],
    { },
    SameTest -> MatchQ,
    TestID   -> "Empty-Build-Directory-Sample@@Tests/TestPaclet.wlt:94,1-102,2"
]

VerificationTest[
    result = suppressConsole @ TestPaclet[
        ExampleDirectory[ "Sample" ],
        "AnnotateTestIDs"  -> False,
        "SetWorkflowValue" -> False
    ],
    _Success,
    SameTest -> MatchQ,
    TestID   -> "TestPaclet-Sample@@Tests/TestPaclet.wlt:104,1-113,2"
]

VerificationTest[
    reports = result[ "Result" ],
    _Association? (AllTrue @ MatchQ[ _TestReportObject ]),
    SameTest -> MatchQ,
    TestID   -> "TestPaclet-Report-Sample@@Tests/TestPaclet.wlt:115,1-120,2"
]

VerificationTest[
    AllTrue[ reports, #[ "AllTestsSucceeded" ] & ],
    True,
    TestID -> "TestPaclet-Passed-Sample@@Tests/TestPaclet.wlt:122,1-126,2"
]

VerificationTest[
    Positive @ Total[ #[ "TestsSucceededCount" ] & /@ reports ],
    True,
    TestID -> "TestPaclet-Pass-Count-Sample@@Tests/TestPaclet.wlt:128,1-132,2"
]

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*FewIssues*)
VerificationTest[
    FileNames[
        "*.paclet",
        FileNameJoin @ { First @ ExampleDirectory[ "FewIssues" ], "build" }
    ],
    { },
    SameTest -> MatchQ,
    TestID   -> "Empty-Build-Directory-FewIssues@@Tests/TestPaclet.wlt:137,1-145,2"
]

VerificationTest[
    result = suppressConsole @ TestPaclet[
        ExampleDirectory[ "FewIssues" ],
        "AnnotateTestIDs"  -> False,
        "SetWorkflowValue" -> False
    ],
    _Failure,
    { TestPaclet::Failures },
    SameTest -> MatchQ,
    TestID   -> "TestPaclet-FewIssues@@Tests/TestPaclet.wlt:147,1-157,2"
]

VerificationTest[
    reports = result[ "Result" ],
    _Association? (AllTrue @ MatchQ[ _TestReportObject ]),
    SameTest -> MatchQ,
    TestID   -> "TestPaclet-Report-FewIssues@@Tests/TestPaclet.wlt:159,1-164,2"
]

VerificationTest[
    AllTrue[ reports, #[ "AllTestsSucceeded" ] & ],
    False,
    TestID -> "TestPaclet-Passed-FewIssues@@Tests/TestPaclet.wlt:166,1-170,2"
]

VerificationTest[
    Positive @ Total[ #[ "TestsSucceededCount" ] & /@ reports ],
    True,
    TestID -> "TestPaclet-Pass-Count-FewIssues@@Tests/TestPaclet.wlt:172,1-176,2"
]

VerificationTest[
    Positive @ Total[ #[ "TestsFailedCount" ] & /@ reports ],
    True,
    TestID -> "TestPaclet-Fail-Count-FewIssues@@Tests/TestPaclet.wlt:178,1-182,2"
]

VerificationTest[
    Union @ Cases[
        #[ "TestsFailed" ] & /@ reports,
        tro_TestResultObject :> tro[ "TestID" ],
        Infinity
    ],
    { "Addition", "AddOne-2" },
    TestID -> "TestPaclet-Failed-TestIDs-FewIssues@@Tests/TestPaclet.wlt:184,1-192,2"
]

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Cleanup*)
VerificationTest[
    ResetExampleDirectory @ All,
    { __Success },
    SameTest -> MatchQ,
    TestID   -> "ResetExampleDirectory-Cleanup@@Tests/TestPaclet.wlt:197,1-202,2"
]