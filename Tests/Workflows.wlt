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

VerificationTest[
    Context @ GitHubSecret,
    "Wolfram`PacletCICD`",
    TestID -> "Context-GitHubSecret@@Tests/Workflows.wlt:63,1-67,2"
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
    TestID -> "WorkflowStep-Named-Checkout@@Tests/Workflows.wlt:76,1-80,2"
]

VerificationTest[
    WorkflowStepQ @ WorkflowStep[ "Check" ],
    True,
    TestID -> "WorkflowStep-Named-Check@@Tests/Workflows.wlt:82,1-86,2"
]

VerificationTest[
    WorkflowStepQ @ WorkflowStep[ "Build" ],
    True,
    TestID -> "WorkflowStep-Named-Build@@Tests/Workflows.wlt:88,1-92,2"
]

VerificationTest[
    WorkflowStepQ @ WorkflowStep[ "Test" ],
    True,
    TestID -> "WorkflowStep-Named-Test@@Tests/Workflows.wlt:94,1-98,2"
]

VerificationTest[
    WorkflowStepQ @ WorkflowStep[ "UploadBuildArtifacts" ],
    True,
    TestID -> "WorkflowStep-Named-UploadBuildArtifacts@@Tests/Workflows.wlt:100,1-104,2"
]

VerificationTest[
    WorkflowStepQ @ WorkflowStep[ "CreateRelease" ],
    True,
    TestID -> "WorkflowStep-Named-CreateRelease@@Tests/Workflows.wlt:106,1-110,2"
]

VerificationTest[
    WorkflowStepQ @ WorkflowStep[ "UploadRelease" ],
    True,
    TestID -> "WorkflowStep-Named-UploadRelease@@Tests/Workflows.wlt:112,1-116,2"
]

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*File*)
VerificationTest[
    WorkflowStep[ File[ "Scripts/MyWorkflowJob.wls" ] ][ "Data" ][ "run" ],
    "wolframscript Scripts/MyWorkflowJob.wls",
    TestID -> "WorkflowStep-File@@Tests/Workflows.wlt:121,1-125,2"
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
    TestID -> "WorkflowStep-Checkout@@Tests/Workflows.wlt:130,1-141,2"
]

VerificationTest[
    step[ "Data" ],
    <|
        "name" -> "Checkout",
        "id"   -> "checkout-code-step",
        "uses" -> "actions/checkout@v2"
    |>,
    TestID -> "WorkflowStep-Checkout-Data@@Tests/Workflows.wlt:143,1-151,2"
]

VerificationTest[
    step[ "YAML" ],
    "name: Checkout\nid: checkout-code-step\nuses: actions/checkout@v2",
    TestID -> "WorkflowStep-Checkout-YAML@@Tests/Workflows.wlt:153,1-157,2"
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
    TestID -> "WorkflowStep-Custom-Code@@Tests/Workflows.wlt:159,1-180,2"
]

VerificationTest[
    step[ "YAML" ],
    "name: Hello-World\nrun: wolframscript -code 'Print[hello]'\nenv: \n  WOLFRAMSCRIPT_ENTITLEMENTID: ${{ secrets.WOLFRAMSCRIPT_ENTITLEMENTID }}",
    TestID -> "WorkflowStep-Custom-YAML@@Tests/Workflows.wlt:182,1-186,2"
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
    TestID -> "WorkflowStep-TimeConstraint-1@@Tests/Workflows.wlt:195,1-202,2"
]

VerificationTest[
    WorkflowStep[
        "Check",
        TimeConstraint -> Quantity[ 1/12, "Hours" ]
    ][ "Data" ][ "timeout-minutes" ],
    5,
    TestID -> "WorkflowStep-TimeConstraint-2@@Tests/Workflows.wlt:204,1-211,2"
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
    TestID -> "WorkflowStep-ProcessEnvironment-1@@Tests/Workflows.wlt:216,1-223,2"
]

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Regression Tests*)
VerificationTest[
    WithCleanup[
        On[ Wolfram`PacletCICD`Private`latestActionName::error ],
        WorkflowStep[ "Check" ][ "Data" ][ "uses" ],
        Off[ Wolfram`PacletCICD`Private`latestActionName::error ]
    ],
    "rhennigan/check-paclet@v" ~~ DigitCharacter.. ~~ ___,
    SameTest -> StringMatchQ,
    TestID   -> "Normalize-Action-Version@@Tests/Workflows.wlt:228,1-233,2"
]

VerificationTest[
    WorkflowStep[
        <|
            "Name" -> "Hello-World",
            "Run" -> "wolframscript -code 'Print[hello]'",
            "Environment" -> <|
                "WOLFRAMSCRIPT_ENTITLEMENTID" ->
                    GitHubSecret[ "WOLFRAMSCRIPT_ENTITLEMENTID" ]
            |>
        |>
    ][ "Data" ][ "env" ],
    <|
        "WOLFRAMSCRIPT_ENTITLEMENTID" -> "${{ secrets.WOLFRAMSCRIPT_ENTITLEMENTID }}"
    |>,
    TestID -> "WorkflowStep-Environment-Env@@Tests/Workflows.wlt:235,1-250,2"
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
    TestID -> "WorkflowJob-Named-Check@@Tests/Workflows.wlt:259,1-263,2"
]

VerificationTest[
    WorkflowJobQ @ WorkflowJob[ "Build" ],
    True,
    TestID -> "WorkflowJob-Named-Build@@Tests/Workflows.wlt:265,1-269,2"
]

VerificationTest[
    WorkflowJobQ @ WorkflowJob[ "Release" ],
    True,
    TestID -> "WorkflowJob-Named-Release@@Tests/Workflows.wlt:271,1-275,2"
]

VerificationTest[
    WorkflowJobQ @ WorkflowJob[ "Test" ],
    True,
    TestID -> "WorkflowJob-Named-Test@@Tests/Workflows.wlt:277,1-281,2"
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
    TestID -> "WorkflowJob-File@@Tests/Workflows.wlt:286,1-300,2"
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
    TestID -> "Workflow-Named-Release@@Tests/Workflows.wlt:309,1-313,2"
]

VerificationTest[
    WorkflowQ @ Workflow[ "Build" ],
    True,
    TestID -> "Workflow-Named-Build@@Tests/Workflows.wlt:315,1-319,2"
]

VerificationTest[
    WorkflowQ @ Workflow[ "Check" ],
    True,
    TestID -> "Workflow-Named-Check@@Tests/Workflows.wlt:321,1-325,2"
]

VerificationTest[
    WorkflowQ @ Workflow[ "Test" ],
    True,
    TestID -> "Workflow-Named-Test@@Tests/Workflows.wlt:327,1-331,2"
]

VerificationTest[
    WorkflowQ @ Workflow[ "Compile" ],
    True,
    TestID -> "Workflow-Named-Compile@@Tests/Workflows.wlt:333,1-337,2"
]

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Regression Tests*)
VerificationTest[
    Workflow[
        "Check",
        <|
            "On" -> <| "PullRequest" -> <| "Branches" -> "test" |> |>
        |>
    ][
        "Data"
    ][
        "on",
        "pull_request",
        "branches"
    ],
    { "test" },
    TestID -> "Workflow-PullRequest-Underscore@@Tests/Workflows.wlt:342,1-357,2"
]
