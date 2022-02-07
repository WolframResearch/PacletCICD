(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Initialization*)
VerificationTest[
    PacletObjectQ @ PacletObject @ File[
        $pacletDir = DirectoryName[ $TestFileName, 2 ]
    ],
    TestID -> "Initialize@@Tests/WorkflowExport.wlt:4,1-9,2"
]

VerificationTest[
    PacletDirectoryLoad @ $pacletDir,
    { ___, $pacletDir, ___ },
    SameTest -> MatchQ,
    TestID -> "Initialize@@Tests/WorkflowExport.wlt:11,1-16,2"
]

VerificationTest[
    Needs[ "Wolfram`PacletCICD`" ],
    Null,
    TestID -> "Initialize@@Tests/WorkflowExport.wlt:18,1-22,2"
]

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Context Check*)
VerificationTest[
    Context @ WorkflowExport,
    "Wolfram`PacletCICD`",
    TestID -> "WorkflowExport-Context@@Tests/WorkflowExport.wlt:27,1-31,2"
]

VerificationTest[
    Context @ ExampleDirectory,
    "Wolfram`PacletCICD`",
    TestID -> "ExampleDirectory-Context@@Tests/WorkflowExport.wlt:33,1-37,2"
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
    TestID -> "WorkflowExport-Check@@Tests/WorkflowExport.wlt:42,1-50,2"
]

VerificationTest[
    build = WorkflowExport[ ExampleDirectory[ "FewIssues" ], "Build" ],
    _File? FileExistsQ,
    SameTest -> MatchQ,
    TestID -> "WorkflowExport-Build@@Tests/WorkflowExport.wlt:52,1-57,2"
]

VerificationTest[
    release = WorkflowExport[ ExampleDirectory[ "FewIssues" ], "Release" ],
    _File? FileExistsQ,
    SameTest -> MatchQ,
    TestID -> "WorkflowExport-Release@@Tests/WorkflowExport.wlt:59,1-64,2"
]

VerificationTest[
    compile = WorkflowExport[ ExampleDirectory[ "FewIssues" ], "Compile" ],
    _File? FileExistsQ,
    SameTest -> MatchQ,
    TestID -> "WorkflowExport-Compile@@Tests/WorkflowExport.wlt:66,1-71,2"
]

VerificationTest[
    AllTrue[ Gather @ { check, build, release, compile }, Length[ # ] === 1 & ],
    True,
    TestID -> "WorkflowExport-Uniqueness@@Tests/WorkflowExport.wlt:73,1-77,2"
]