(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Initialization*)
VerificationTest[
    PacletObjectQ @ PacletObject @ File[
        $pacletDir = DirectoryName[ $TestFileName, 2 ]
    ],
    TestID -> "Initialize-PacletObject@@Tests/Workflows.wlt:4,1-9,2"
]

VerificationTest[
    PacletDirectoryLoad @ $pacletDir,
    { ___, $pacletDir, ___ },
    SameTest -> MatchQ,
    TestID -> "Initialize-PacletDirectoryLoad@@Tests/Workflows.wlt:11,1-16,2"
]

VerificationTest[
    Needs[ "Wolfram`PacletCICD`" ],
    Null,
    TestID -> "Initialize-Needs-PacletCICD@@Tests/Workflows.wlt:18,1-22,2"
]

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Context Check*)
VerificationTest[
    Context @ WorkflowStep,
    "Wolfram`PacletCICD`",
    TestID -> "Context-WorkflowStep@@Tests/Workflows.wlt:27,1-31,2"
]

VerificationTest[
    Context @ WorkflowJob,
    "Wolfram`PacletCICD`",
    TestID -> "Context-WorkflowJob@@Tests/Workflows.wlt:33,1-37,2"
]

VerificationTest[
    Context @ Workflow,
    "Wolfram`PacletCICD`",
    TestID -> "Context-Workflow@@Tests/Workflows.wlt:39,1-43,2"
]

VerificationTest[
    Context @ WorkflowStepQ,
    "Wolfram`PacletCICD`",
    TestID -> "Context-WorkflowStepQ@@Tests/Workflows.wlt:45,1-49,2"
]

VerificationTest[
    Context @ WorkflowJobQ,
    "Wolfram`PacletCICD`",
    TestID -> "Context-WorkflowJobQ@@Tests/Workflows.wlt:51,1-55,2"
]

VerificationTest[
    Context @ WorkflowQ,
    "Wolfram`PacletCICD`",
    TestID -> "Context-WorkflowQ@@Tests/Workflows.wlt:57,1-61,2"
]

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*WorkflowStep*)

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Named Steps*)
VerificationTest[
    WorkflowStepQ @ WorkflowStep[ "Checkout" ],
    True,
    TestID -> "WorkflowStep-Named-Checkout@@Tests/Workflows.wlt:70,1-74,2"
]

VerificationTest[
    WorkflowStepQ @ WorkflowStep[ "Check" ],
    True,
    TestID -> "WorkflowStep-Named-Check@@Tests/Workflows.wlt:76,1-80,2"
]

VerificationTest[
    WorkflowStepQ @ WorkflowStep[ "Build" ],
    True,
    TestID -> "WorkflowStep-Named-Build@@Tests/Workflows.wlt:82,1-86,2"
]

VerificationTest[
    WorkflowStepQ @ WorkflowStep[ "Test" ],
    True,
    TestID -> "WorkflowStep-Named-Test@@Tests/Workflows.wlt:88,1-92,2"
]

VerificationTest[
    WorkflowStepQ @ WorkflowStep[ "UploadBuildArtifacts" ],
    True,
    TestID -> "WorkflowStep-Named-UploadBuildArtifacts@@Tests/Workflows.wlt:94,1-98,2"
]

VerificationTest[
    WorkflowStepQ @ WorkflowStep[ "CreateRelease" ],
    True,
    TestID -> "WorkflowStep-Named-CreateRelease@@Tests/Workflows.wlt:100,1-104,2"
]

VerificationTest[
    WorkflowStepQ @ WorkflowStep[ "UploadRelease" ],
    True,
    TestID -> "WorkflowStep-Named-UploadRelease@@Tests/Workflows.wlt:106,1-110,2"
]

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*File*)
VerificationTest[
    WorkflowStep[ File[ "Scripts/MyWorkflowJob.wls" ] ][ "Data" ][ "run" ],
    "wolframscript Scripts/MyWorkflowJob.wls",
    TestID -> "WorkflowStep-File@@Tests/Workflows.wlt:115,1-119,2"
]

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Properties*)
VerificationTest[
    step = WorkflowStep[ "Checkout" ],
    WorkflowStep[
        "Checkout",
        <|
            "name" -> "Checkout",
            "id"   -> "checkout-code-step",
            "uses" -> "actions/checkout@v2"
        |>
    ],
    TestID -> "WorkflowStep-Checkout@@Tests/Workflows.wlt:124,1-135,2"
]

VerificationTest[
    step[ "Data" ],
    <|
        "name" -> "Checkout",
        "id"   -> "checkout-code-step",
        "uses" -> "actions/checkout@v2"
    |>,
    TestID -> "WorkflowStep-Checkout-Data@@Tests/Workflows.wlt:137,1-145,2"
]

VerificationTest[
    step[ "YAML" ],
    "name: Checkout\nid: checkout-code-step\nuses: actions/checkout@v2",
    TestID -> "WorkflowStep-Checkout-YAML@@Tests/Workflows.wlt:147,1-151,2"
]

VerificationTest[
    step =
        WorkflowStep @ <|
            "name" -> "Hello-World",
            "run" -> "wolframscript -code 'Print[hello]'",
            Environment -> <|
                "WOLFRAMSCRIPT_ENTITLEMENTID" ->
                    GitHubSecret[ "WOLFRAMSCRIPT_ENTITLEMENTID" ]
            |>
        |>,
    WorkflowStep[
        "Hello-World",
        <|
            "name" -> "Hello-World",
            "run" -> "wolframscript -code 'Print[hello]'",
            "env" -> <|
                "WOLFRAMSCRIPT_ENTITLEMENTID" -> "${{ secrets.WOLFRAMSCRIPT_ENTITLEMENTID }}"
            |>
        |>
    ],
    TestID -> "WorkflowStep-Custom-Code@@Tests/Workflows.wlt:153,1-174,2"
]

VerificationTest[
    step[ "YAML" ],
    "name: Hello-World\nrun: wolframscript -code 'Print[hello]'\nenv: \n  WOLFRAMSCRIPT_ENTITLEMENTID: ${{ secrets.WOLFRAMSCRIPT_ENTITLEMENTID }}",
    TestID -> "WorkflowStep-Custom-YAML@@Tests/Workflows.wlt:176,1-180,2"
]

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Options*)

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*TimeConstraint*)
VerificationTest[
    WorkflowStep[
        "Check",
        TimeConstraint -> 300
    ][ "Data" ][ "timeout-minutes" ],
    5,
    TestID -> "WorkflowStep-TimeConstraint-1@@Tests/Workflows.wlt:189,1-196,2"
]

VerificationTest[
    WorkflowStep[
        "Check",
        TimeConstraint -> Quantity[ 1/12, "Hours" ]
    ][ "Data" ][ "timeout-minutes" ],
    5,
    TestID -> "WorkflowStep-TimeConstraint-2@@Tests/Workflows.wlt:198,1-205,2"
]

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*ProcessEnvironment*)
VerificationTest[
    WorkflowStep[
        "Check",
        ProcessEnvironment -> <| "MY_VAR" -> "hello" |>
    ][ "Data" ][ "env" ],
    <| "MY_VAR" -> "hello" |>,
    TestID -> "WorkflowStep-ProcessEnvironment-1@@Tests/Workflows.wlt:210,1-217,2"
]

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Regression Tests*)
VerificationTest[
    StringMatchQ[
        WorkflowStep[ "Check" ][ "Data" ][ "uses" ],
        "rhennigan/check-paclet@v" ~~ DigitCharacter.. ~~ ___
    ],
    True,
    TestID -> "Normalize-Action-Version@@Tests/Workflows.wlt:222,1-229,2"
]

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*WorkflowJob*)

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Named Jobs*)
VerificationTest[
    WorkflowJobQ @ WorkflowJob[ "Check" ],
    True,
    TestID -> "WorkflowJob-Named-Check@@Tests/Workflows.wlt:238,1-242,2"
]

VerificationTest[
    WorkflowJobQ @ WorkflowJob[ "Build" ],
    True,
    TestID -> "WorkflowJob-Named-Build@@Tests/Workflows.wlt:244,1-248,2"
]

VerificationTest[
    WorkflowJobQ @ WorkflowJob[ "Release" ],
    True,
    TestID -> "WorkflowJob-Named-Release@@Tests/Workflows.wlt:250,1-254,2"
]

VerificationTest[
    WorkflowJobQ @ WorkflowJob[ "Test" ],
    True,
    TestID -> "WorkflowJob-Named-Test@@Tests/Workflows.wlt:256,1-260,2"
]

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*File*)
VerificationTest[
    WorkflowJob[ File[ "Scripts/MyWorkflowJob.wls" ] ][ "Data" ][ "steps" ],
    {
        <|
            "name" -> "Checkout",
            "id"   -> "checkout-code-step",
            "uses" -> "actions/checkout@v2"
        |>,
        <|
            "name" -> "MyWorkflowJob",
            "run"  -> "wolframscript Scripts/MyWorkflowJob.wls"
        |>
    },
    TestID -> "WorkflowJob-File@@Tests/Workflows.wlt:265,1-279,2"
]

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Workflow*)

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Named Workflows*)
VerificationTest[
    WorkflowQ @ Workflow[ "Release" ],
    True,
    TestID -> "Workflow-Named-Release@@Tests/Workflows.wlt:288,1-292,2"
]

VerificationTest[
    WorkflowQ @ Workflow[ "Build" ],
    True,
    TestID -> "Workflow-Named-Build@@Tests/Workflows.wlt:294,1-298,2"
]

VerificationTest[
    WorkflowQ @ Workflow[ "Check" ],
    True,
    TestID -> "Workflow-Named-Check@@Tests/Workflows.wlt:300,1-304,2"
]

VerificationTest[
    WorkflowQ @ Workflow[ "Test" ],
    True,
    TestID -> "Workflow-Named-Test@@Tests/Workflows.wlt:306,1-310,2"
]

VerificationTest[
    WorkflowQ @ Workflow[ "Compile" ],
    True,
    TestID -> "Workflow-Named-Compile@@Tests/Workflows.wlt:312,1-316,2"
]