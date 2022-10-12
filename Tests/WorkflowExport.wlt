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
    TestID -> "Initialize-PacletObject@@Tests/WorkflowExport.wlt:4,1-14,2"
]

(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::SuspiciousSessionSymbol:: *)
VerificationTest[
    Echo[ $TestFileName, "TestFileName" ];
    PacletDirectoryLoad @ Echo[ $pacletDir, "PacletDirectory" ],
    { ___, $pacletDir, ___ },
    SameTest -> MatchQ,
    TestID   -> "Initialize-PacletDirectoryLoad@@Tests/WorkflowExport.wlt:18,1-24,2"
]
(* :!CodeAnalysis::EndBlock:: *)

VerificationTest[
    Needs[ "Wolfram`PacletCICD`" ],
    Null,
    TestID -> "Initialize@@Tests/WorkflowExport.wlt:27,1-31,2"
]

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Context Check*)
VerificationTest[
    Context @ WorkflowExport,
    "Wolfram`PacletCICD`",
    TestID -> "WorkflowExport-Context@@Tests/WorkflowExport.wlt:36,1-40,2"
]

VerificationTest[
    Context @ ExampleDirectory,
    "Wolfram`PacletCICD`",
    TestID -> "ExampleDirectory-Context@@Tests/WorkflowExport.wlt:42,1-46,2"
]

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*WorkflowExport*)
VerificationTest[
    Quiet[
        check = WorkflowExport[ ExampleDirectory[ "FewIssues" ], "Check" ],
        WorkflowExport::entitlement
    ],
    _File? FileExistsQ,
    SameTest -> MatchQ,
    TestID -> "WorkflowExport-Check@@Tests/WorkflowExport.wlt:51,1-59,2"
]

VerificationTest[
    build = WorkflowExport[ ExampleDirectory[ "FewIssues" ], "Build" ],
    _File? FileExistsQ,
    SameTest -> MatchQ,
    TestID -> "WorkflowExport-Build@@Tests/WorkflowExport.wlt:61,1-66,2"
]

VerificationTest[
    release = WorkflowExport[ ExampleDirectory[ "FewIssues" ], "Release" ],
    _File? FileExistsQ,
    SameTest -> MatchQ,
    TestID -> "WorkflowExport-Release@@Tests/WorkflowExport.wlt:68,1-73,2"
]

VerificationTest[
    compile = WorkflowExport[ ExampleDirectory[ "FewIssues" ], "Compile" ],
    _File? FileExistsQ,
    SameTest -> MatchQ,
    TestID -> "WorkflowExport-Compile@@Tests/WorkflowExport.wlt:75,1-80,2"
]

VerificationTest[
    AllTrue[ Gather @ { check, build, release, compile }, Length[ # ] === 1 & ],
    True,
    TestID -> "WorkflowExport-Uniqueness@@Tests/WorkflowExport.wlt:82,1-86,2"
]

VerificationTest[
    StringReplace[
        ReadString @ WorkflowExport[ ExampleDirectory[ "Sample" ], "Submit" ],
        "\r\n" -> "\n"
    ],
    Workflow[ "Submit" ][ "YAML" ],
    TestID -> "WorkflowExport-YAML-Equivalence@@Tests/WorkflowExport.wlt:88,1-95,2"
]

VerificationTest[
    ResetExampleDirectory[ "Sample" ],
    _Success,
    SameTest -> MatchQ,
    TestID   -> "WorkflowExport-YAML-Equivalence-Cleanup@@Tests/WorkflowExport.wlt:97,1-102,2"
]