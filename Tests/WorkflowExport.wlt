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
    TestID -> "Initialize@@Tests/WorkflowExport.wlt:4,1-12,2"
]

VerificationTest[
    PacletDirectoryLoad @ Wolfram`PacletCICD`Tests`$pacletDir,
    { ___, Wolfram`PacletCICD`Tests`$pacletDir, ___ },
    SameTest -> MatchQ,
    TestID -> "Initialize@@Tests/WorkflowExport.wlt:14,1-19,2"
]

VerificationTest[
    Block[ { $ContextPath }, Needs[ "Wolfram`PacletCICD`" ] ],
    Null,
    TestID -> "Initialize@@Tests/WorkflowExport.wlt:21,1-25,2"
]

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*WorkflowExport*)
VerificationTest[
    Quiet[
        Wolfram`PacletCICD`Tests`check =
            Wolfram`PacletCICD`WorkflowExport[
                Wolfram`PacletCICD`ExampleDirectory[ "FewIssues" ],
                "Check"
            ],
        Wolfram`PacletCICD`WorkflowExport::entitlement
    ],
    _File? FileExistsQ,
    SameTest -> MatchQ,
    TestID -> "WorkflowExport-Check@@Tests/WorkflowExport.wlt:30,1-42,2"
]

VerificationTest[
    Wolfram`PacletCICD`Tests`build =
        Wolfram`PacletCICD`WorkflowExport[
            Wolfram`PacletCICD`ExampleDirectory[ "FewIssues" ],
            "Build"
        ],
    _File? FileExistsQ,
    SameTest -> MatchQ,
    TestID -> "WorkflowExport-Build@@Tests/WorkflowExport.wlt:44,1-53,2"
]

VerificationTest[
    Wolfram`PacletCICD`Tests`release =
        Wolfram`PacletCICD`WorkflowExport[
            Wolfram`PacletCICD`ExampleDirectory[ "FewIssues" ],
            "Release"
        ],
    _File? FileExistsQ,
    SameTest -> MatchQ,
    TestID -> "WorkflowExport-Release@@Tests/WorkflowExport.wlt:55,1-64,2"
]

VerificationTest[
    Wolfram`PacletCICD`Tests`compile =
        Wolfram`PacletCICD`WorkflowExport[
            Wolfram`PacletCICD`ExampleDirectory[ "FewIssues" ],
            "Compile"
        ],
    _File? FileExistsQ,
    SameTest -> MatchQ,
    TestID -> "WorkflowExport-Compile@@Tests/WorkflowExport.wlt:66,1-75,2"
]

VerificationTest[
    AllTrue[
        Gather @ {
            Wolfram`PacletCICD`Tests`check,
            Wolfram`PacletCICD`Tests`build,
            Wolfram`PacletCICD`Tests`release,
            Wolfram`PacletCICD`Tests`compile
        },
        Length[ #1 ] === 1 &
    ],
    True,
    TestID -> "WorkflowExport-Uniqueness@@Tests/WorkflowExport.wlt:77,1-89,2"
]