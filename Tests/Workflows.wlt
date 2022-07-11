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
    TestID -> "Initialize-PacletObject@@Tests/Workflows.wlt:4,1-14,2"
]

(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::SuspiciousSessionSymbol:: *)
VerificationTest[
    Echo[ $TestFileName, "TestFileName" ];
    PacletDirectoryLoad @ Echo[ $pacletDir, "PacletDirectory" ],
    { ___, $pacletDir, ___ },
    SameTest -> MatchQ,
    TestID   -> "Initialize-PacletDirectoryLoad@@Tests/Workflows.wlt:18,1-24,2"
]
(* :!CodeAnalysis::EndBlock:: *)

VerificationTest[
    Needs[ "Wolfram`PacletCICD`" ],
    Null,
    TestID -> "Initialize-Needs-PacletCICD@@Tests/Workflows.wlt:27,1-31,2"
]

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Context Check*)
VerificationTest[
    Context @ WorkflowStep,
    "Wolfram`PacletCICD`",
    TestID -> "Context-WorkflowStep@@Tests/Workflows.wlt:36,1-40,2"
]

VerificationTest[
    Context @ WorkflowJob,
    "Wolfram`PacletCICD`",
    TestID -> "Context-WorkflowJob@@Tests/Workflows.wlt:42,1-46,2"
]

VerificationTest[
    Context @ Workflow,
    "Wolfram`PacletCICD`",
    TestID -> "Context-Workflow@@Tests/Workflows.wlt:48,1-52,2"
]

VerificationTest[
    Context @ WorkflowStepQ,
    "Wolfram`PacletCICD`",
    TestID -> "Context-WorkflowStepQ@@Tests/Workflows.wlt:54,1-58,2"
]

VerificationTest[
    Context @ WorkflowJobQ,
    "Wolfram`PacletCICD`",
    TestID -> "Context-WorkflowJobQ@@Tests/Workflows.wlt:60,1-64,2"
]

VerificationTest[
    Context @ WorkflowQ,
    "Wolfram`PacletCICD`",
    TestID -> "Context-WorkflowQ@@Tests/Workflows.wlt:66,1-70,2"
]

VerificationTest[
    Context @ GitHubSecret,
    "Wolfram`PacletCICD`",
    TestID -> "Context-GitHubSecret@@Tests/Workflows.wlt:72,1-76,2"
]

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*WorkflowStep*)

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Named Steps*)
VerificationTest[
    WorkflowStepQ @ WorkflowStep[ "Build" ],
    True,
    TestID -> "WorkflowStep-Named-Build@@Tests/Workflows.wlt:85,1-89,2"
]

VerificationTest[
    WorkflowStepQ @ WorkflowStep[ "Check" ],
    True,
    TestID -> "WorkflowStep-Named-Check@@Tests/Workflows.wlt:91,1-95,2"
]

VerificationTest[
    WorkflowStepQ @ WorkflowStep[ "Checkout" ],
    True,
    TestID -> "WorkflowStep-Named-Checkout@@Tests/Workflows.wlt:97,1-101,2"
]

VerificationTest[
    WorkflowStepQ @ WorkflowStep[ "CreateRelease" ],
    True,
    TestID -> "WorkflowStep-Named-CreateRelease@@Tests/Workflows.wlt:103,1-107,2"
]

VerificationTest[
    WorkflowStepQ @ WorkflowStep[ "Download" ],
    True,
    TestID -> "WorkflowStep-Named-Download@@Tests/Workflows.wlt:109,1-113,2"
]

VerificationTest[
    WorkflowStepQ @ WorkflowStep[ "DownloadCompilationArtifacts" ],
    True,
    TestID -> "WorkflowStep-Named-DownloadCompilationArtifacts@@Tests/Workflows.wlt:115,1-119,2"
]

VerificationTest[
    WorkflowStepQ @ WorkflowStep[ "InstallWolframEngine" ],
    True,
    TestID -> "WorkflowStep-Named-InstallWolframEngine@@Tests/Workflows.wlt:121,1-125,2"
]

VerificationTest[
    WorkflowStepQ @ WorkflowStep[ "Publish" ],
    True,
    TestID -> "WorkflowStep-Named-Publish@@Tests/Workflows.wlt:127,1-131,2"
]

VerificationTest[
    WorkflowStepQ @ WorkflowStep[ "RestoreCachedWolframEngine" ],
    True,
    TestID -> "WorkflowStep-Named-RestoreCachedWolframEngine@@Tests/Workflows.wlt:133,1-137,2"
]

VerificationTest[
    WorkflowStepQ @ WorkflowStep[ "Submit" ],
    True,
    TestID -> "WorkflowStep-Named-Submit@@Tests/Workflows.wlt:139,1-143,2"
]

VerificationTest[
    WorkflowStepQ @ WorkflowStep[ "Test" ],
    True,
    TestID -> "WorkflowStep-Named-Test@@Tests/Workflows.wlt:145,1-149,2"
]

VerificationTest[
    WorkflowStepQ @ WorkflowStep[ "Upload" ],
    True,
    TestID -> "WorkflowStep-Named-Upload@@Tests/Workflows.wlt:151,1-155,2"
]

VerificationTest[
    WorkflowStepQ @ WorkflowStep[ "UploadBuildArtifacts" ],
    True,
    TestID -> "WorkflowStep-Named-UploadBuildArtifacts@@Tests/Workflows.wlt:157,1-161,2"
]

VerificationTest[
    WorkflowStepQ @ WorkflowStep[ "UploadRelease" ],
    True,
    TestID -> "WorkflowStep-Named-UploadRelease@@Tests/Workflows.wlt:163,1-167,2"
]

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*File*)
VerificationTest[
    WorkflowStep[ File[ "Scripts/MyWorkflowJob.wls" ] ][ "Data" ][ "run" ],
    "wolframscript Scripts/MyWorkflowJob.wls",
    TestID -> "WorkflowStep-File@@Tests/Workflows.wlt:172,1-176,2"
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
            "uses" -> "actions/checkout@v3"
        |>
    ],
    TestID -> "WorkflowStep-Checkout@@Tests/Workflows.wlt:181,1-192,2"
]

VerificationTest[
    step[ "Data" ],
    <|
        "name" -> "Checkout",
        "id"   -> "checkout-code-step",
        "uses" -> "actions/checkout@v3"
    |>,
    TestID -> "WorkflowStep-Checkout-Data@@Tests/Workflows.wlt:194,1-202,2"
]

VerificationTest[
    step[ "YAML" ],
    "name: Checkout\nid: checkout-code-step\nuses: actions/checkout@v3",
    TestID -> "WorkflowStep-Checkout-YAML@@Tests/Workflows.wlt:204,1-208,2"
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
    TestID -> "WorkflowStep-Custom-Code@@Tests/Workflows.wlt:210,1-231,2"
]

VerificationTest[
    step[ "YAML" ],
    "name: Hello-World\nrun: wolframscript -code 'Print[hello]'\nenv: \n  WOLFRAMSCRIPT_ENTITLEMENTID: ${{ secrets.WOLFRAMSCRIPT_ENTITLEMENTID }}",
    TestID -> "WorkflowStep-Custom-YAML@@Tests/Workflows.wlt:233,1-237,2"
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
    TestID -> "WorkflowStep-TimeConstraint-1@@Tests/Workflows.wlt:246,1-253,2"
]

VerificationTest[
    WorkflowStep[
        "Check",
        TimeConstraint -> Quantity[ 1/12, "Hours" ]
    ][ "Data" ][ "timeout-minutes" ],
    5,
    TestID -> "WorkflowStep-TimeConstraint-2@@Tests/Workflows.wlt:255,1-262,2"
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
    TestID -> "WorkflowStep-ProcessEnvironment-1@@Tests/Workflows.wlt:267,1-274,2"
]

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Regression Tests*)
VerificationTest[
    WorkflowStep[ "Check" ][ "Data" ][ "uses" ],
    "rhennigan/check-paclet@v" ~~ DigitCharacter.. ~~ ___,
    SameTest -> StringMatchQ,
    TestID   -> "Normalize-Action-Version@@Tests/Workflows.wlt:279,1-284,2"
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
    TestID -> "WorkflowStep-Environment-Env@@Tests/Workflows.wlt:286,1-301,2"
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
    TestID -> "WorkflowJob-Named-Check@@Tests/Workflows.wlt:310,1-314,2"
]

VerificationTest[
    WorkflowJobQ @ WorkflowJob[ "Build" ],
    True,
    TestID -> "WorkflowJob-Named-Build@@Tests/Workflows.wlt:316,1-320,2"
]

VerificationTest[
    WorkflowJobQ @ WorkflowJob[ "Publish" ],
    True,
    TestID -> "WorkflowJob-Named-Publish@@Tests/Workflows.wlt:322,1-326,2"
]

VerificationTest[
    WorkflowJobQ @ WorkflowJob[ "Release" ],
    True,
    TestID -> "WorkflowJob-Named-Release@@Tests/Workflows.wlt:328,1-332,2"
]

VerificationTest[
    WorkflowJobQ @ WorkflowJob[ "Test" ],
    True,
    TestID -> "WorkflowJob-Named-Test@@Tests/Workflows.wlt:334,1-338,2"
]

VerificationTest[
    WorkflowJobQ @ WorkflowJob[ "Submit" ],
    True,
    TestID -> "WorkflowJob-Named-Submit@@Tests/Workflows.wlt:340,1-344,2"
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
            "uses" -> "actions/checkout@v3"
        |>,
        <|
            "name" -> "MyWorkflowJob",
            "run"  -> "wolframscript Scripts/MyWorkflowJob.wls"
        |>
    },
    TestID -> "WorkflowJob-File@@Tests/Workflows.wlt:349,1-363,2"
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
    TestID -> "Workflow-Named-Release@@Tests/Workflows.wlt:372,1-376,2"
]

VerificationTest[
    WorkflowQ @ Workflow[ "Build" ],
    True,
    TestID -> "Workflow-Named-Build@@Tests/Workflows.wlt:378,1-382,2"
]

VerificationTest[
    WorkflowQ @ Workflow[ "Check" ],
    True,
    TestID -> "Workflow-Named-Check@@Tests/Workflows.wlt:384,1-388,2"
]

VerificationTest[
    WorkflowQ @ Workflow[ "Test" ],
    True,
    TestID -> "Workflow-Named-Test@@Tests/Workflows.wlt:390,1-394,2"
]

VerificationTest[
    WorkflowQ @ Workflow[ "Submit" ],
    True,
    TestID -> "Workflow-Named-Submit@@Tests/Workflows.wlt:396,1-400,2"
]

VerificationTest[
    WorkflowQ @ Workflow[ "Compile" ],
    True,
    TestID -> "Workflow-Named-Compile@@Tests/Workflows.wlt:402,1-406,2"
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
    TestID -> "Workflow-PullRequest-Underscore@@Tests/Workflows.wlt:411,1-426,2"
]

VerificationTest[
    StringContainsQ[ Workflow[ "Test" ][ "YAML" ], "env.PACLET_TEST_RESULTS" ],
    True,
    TestID -> "Workflow-Test-Results-File@@Tests/Workflows.wlt:428,1-432,2"
]