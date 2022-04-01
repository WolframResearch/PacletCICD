(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Header*)
BeginPackage[ "Wolfram`PacletCICD`" ];

ClearAll[
    GitHubSecret,
    Workflow,
    WorkflowExport,
    WorkflowJob,
    WorkflowJobQ,
    WorkflowQ,
    WorkflowEvaluate,
    WorkflowStep,
    WorkflowStepQ
];

Begin[ "`Private`" ];

$ContextAliases[ "sp`" ] = "System`Private`";

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Defaults*)
$buildAction     = "rhennigan/build-paclet@latest";
$checkAction     = "rhennigan/check-paclet@latest";
$testAction      = "rhennigan/test-paclet@latest";
$submitAction    = "rhennigan/submit-paclet@latest";
$defaultBranch   = "main";
$timeConstraint  = 10;
$actionTarget    = "Submit";
$defNotebookPath = "./ResourceDefinition.nb";
$defaultOS       = "Linux-x86-64";
$defaultRunner   = "ubuntu-latest";
$resSystemBase   = "https://www.wolframcloud.com/obj/resourcesystem/api/1.0";
$publisherToken  = GitHubSecret[ "RESOURCE_PUBLISHER_TOKEN" ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*WorkflowEvaluate*)
WorkflowEvaluate // Attributes = { HoldFirst };
WorkflowEvaluate[ code_ ] := WorkflowEvaluate[ code, makeEvalName @ code ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*makeEvalName*)
makeEvalName // Attributes = { HoldFirst };

makeEvalName[ File[ file_String ] ] := FileBaseName @ file;

makeEvalName[ command_String ] := "UntitledCommand";

makeEvalName[ (f_Symbol?symbolQ)[ (g_Symbol? symbolQ)[ ___ ], ___ ] ] :=
    StringJoin[
        Capitalize @ SymbolName @ Unevaluated @ f,
        Capitalize @ SymbolName @ Unevaluated @ g
    ];

makeEvalName[ (f_Symbol?symbolQ)[ ___ ] ] :=
    Capitalize @ SymbolName @ Unevaluated @ f;

makeEvalName[ ___ ] := "UntitledEvaluation";

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*WorkflowQ*)
WorkflowQ[ wf_ ] := workflowQ @ wf;
WorkflowQ[ ___ ] := False;

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*WorkflowJobQ*)
WorkflowJobQ[ wf_ ] := workflowJobQ @ wf;
WorkflowJobQ[ ___ ] := False;

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*WorkflowStepQ*)
WorkflowStepQ[ wf_ ] := workflowStepQ @ wf;
WorkflowStepQ[ ___ ] := False;

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Workflow*)
Workflow::invname =
"`1` is not a known workflow name.";

Workflow::invprop =
"`1` is not a valid Workflow property name.";

Workflow::TokenSecret =
"Publisher tokens should not be specified directly, and should instead be \
given as a GitHubSecret.";

Workflow::InvalidPublisherToken =
"Value of option PublisherToken -> `1` should be Automatic or None.";

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Options*)
Workflow // Options = {
    "BuildPacletAction"      -> $buildAction,
    "CheckPacletAction"      -> $checkAction,
    "SubmitPacletAction"     -> $submitAction,
    "TestPacletAction"       -> $testAction,
    "DefaultBranch"          -> $defaultBranch,
    "DefinitionNotebookPath" -> Automatic,
    OperatingSystem          -> Automatic,
    ProcessEnvironment       -> Automatic,
    "PublisherToken"         -> Automatic,
    ResourceSystemBase       -> Automatic,
    "Target"                 -> $actionTarget,
    TimeConstraint           -> Infinity
};

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Main definition*)
Workflow[
    name_String,
    as_Association,
    opts0: OptionsPattern[ ]
]? sp`HoldNotValidQ := withWorkflowOptions[ Workflow, opts0 ] @
    Module[ { opts, ts, new },
        opts = FilterRules[ { opts0 }, { ProcessEnvironment } ];

        new = withDefaultTimeConstraint[
            OptionValue[ TimeConstraint ],
            postProcessYAML @ makeWorkflowData[ name, <| as, opts |> ]
        ];

        (* TODO: always insert "name" property as first arg *)
        With[ { a = new },
            If[ FailureQ @ a,
                a,
                sp`HoldSetValid @ Workflow[ name, a ]
            ]
        ]
    ];

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*Named*)
Workflow[ name_String, opts: OptionsPattern[ ] ] :=
    catchTop @ If[ workflowNameQ @ name,
        Workflow[ name, <| |>, opts ],
        throwMessageFailure[ Workflow::invname, name ]
    ];

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*Custom*)
Workflow[ as_Association, opts: OptionsPattern[ ] ] :=
    catchTop @ Module[ { new, id },
        new = makeWorkflowData @ as;
        (* TODO: write a getWorkFlowID function *)
        id = First[ KeyTake[ new, { "id", "name" } ], CreateUUID[ ] ];
        Workflow[ id, new, opts ]
    ];

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*Merged*)
Workflow[
    wf_Workflow? workflowQ,
    as_Association,
    opts: OptionsPattern[ ]
] :=
    catchTop @ Module[ { name, as1, as2, merged, data },
        name   = wf[ "Name" ];
        as1    = wf[ "Data" ];
        as2    = makeWorkflowData[ name, as ];
        merged = merger @ { as1, as2 };
        data   = Join[ KeyTake[ merged, Keys @ as1 ], merged ];
        Workflow[ name, data, opts ]
    ];

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*File*)
Workflow[ File[ file_String ], opts: OptionsPattern[ ] ] :=
    Failure[ "NotImplemented", <| |> ]; (* TODO *)

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*Properties*)
(workflow_Workflow? workflowQ)[ prop_ ] :=
    catchTop @ workflowProperty[ workflow, prop ];

(* TODO: handle undefined and not validQ *)

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Formatting*)
Workflow /: MakeBoxes[ workflow_Workflow? workflowQ, fmt_ ] :=
    With[ { boxes = FormattingHelper[ workflow, fmt ] },
        boxes /; MatchQ[ boxes, _InterpretationBox ]
    ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*$workflowNames*)
$workflowNames := Keys @ $namedWorkflows;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*workflowNameQ*)
workflowNameQ[ name_ ] := MemberQ[ $workflowNames, toWorkflowName @ name ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*workflowQ*)
workflowQ // Attributes = { HoldFirst };

workflowQ[ workflow: Workflow[ name_? StringQ, as_? AssociationQ ] ] :=
    sp`HoldValidQ @ workflow;

workflowQ[ ___ ] := False;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*genericWFProperty*)
genericWFProperty[ _[ _, data_ ], "Data"    ] := data;
genericWFProperty[ _[ _, data_ ], "Dataset" ] := Dataset @ data;
genericWFProperty[ _[ name_, _ ], "Name"    ] := name;
genericWFProperty[ _[ _, data_ ], "YAML"    ] := toYAMLString @ data;
genericWFProperty // catchUndefined;

$$wfProp = Alternatives[ "Data", "Dataset", "Name", "YAML" ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*workflowProperty*)
workflowProperty[ wf_, prop: $$wfProp ] := genericWFProperty[ wf, prop ];
workflowProperty[ wf_, "Jobs" ] := getWorkflowJobs @ wf;
workflowProperty[ wf_, "JobGraph" ] := jobGraph @ wf;

workflowProperty[ _, prop_ ] :=
    throwMessageFailure[ Workflow::invprop, prop ];

workflowProperty // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*getWorkflowJobs*)
getWorkflowJobs[ wf_? workflowQ ] :=
    Enclose @ Module[ { data, jobs },
        data = ConfirmBy[ genericWFProperty[ wf, "Data" ], AssociationQ ];
        jobs = ConfirmBy[ Lookup[ data, "jobs" ], AssociationQ ];
        ConfirmBy[ WorkflowJob /@ jobs, AllTrue[ workflowJobQ ] ]
    ];

(* TODO: envInherit *)

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*jobGraph*)
jobGraph[ wf_? workflowQ ] :=
    Enclose @ Module[ { jobs, needs, vertices, edges },
        jobs = ConfirmBy[ getWorkflowJobs @ wf, AllTrue @ workflowJobQ ];
        needs = ConfirmBy[ (Slot[ 1 ][ "Needs" ] &) /@ jobs, AssociationQ ];
        vertices = Keys @ needs;
        edges = Flatten[ Thread /@ Normal @ DeleteCases[ needs, None ] ];
        Graph[
            vertices,
            Reverse[ edges, 2 ],
            VertexLabels -> Placed[ "Name", Center, graphLabel ]
        ]
    ];

(* ::**********************************************************************:: *)
(* ::Subsubsubsection::Closed:: *)
(*graphLabel*)
graphLabel[ lbl_ ] :=
    Framed[
        lbl,
        FrameMargins -> { { 1, 1 }, { 0, 0 } },
        RoundingRadius -> 3,
        FrameStyle -> GrayLevel[ 0.8 ],
        Background -> White
    ];

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*withDefaultTimeConstraint*)
withDefaultTimeConstraint // Attributes = { HoldRest };

withDefaultTimeConstraint[ ts0_, eval_ ] :=
    Module[ { rule, ts },
        rule = normalizeForYAML[ "jobs", Automatic, TimeConstraint -> ts0 ];
        ts = Last[ rule, $noValue ];
        Block[ { $timeConstraint = ts }, eval ]
    ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*makeWorkflowData*)
makeWorkflowData[ name_String? workflowNameQ, as_Association ] :=
    Enclose @ Module[ { as1, as2, data },
        as1 = Lookup[
            $namedWorkflows,
            toWorkflowName @ name,
            throwMessageFailure[ Workflow::invname, name ]
        ];

        as1  = ConfirmBy[ normalizeForYAML @ as1, AssociationQ ];
        as2  = ConfirmBy[ normalizeForYAML @ as , AssociationQ ];
        data = merger @ { as1, as2 };
        Join[ KeyTake[ data, Keys @ as2 ], data ]
    ];

makeWorkflowData[ name_String, custom_Association ] :=
    makeWorkflowData @ Prepend[ custom, "name" -> name ];

makeWorkflowData[ custom_Association ] :=
    Enclose @ ConfirmBy[ normalizeForYAML @ custom, AssociationQ ];

makeWorkflowData // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*WorkflowJob*)
WorkflowJob::invname =
"`1` is not a known job name.";

WorkflowJob::invprop =
"`1` is not a valid WorkflowJob property name.";

WorkflowJob::TokenSecret =
"Publisher tokens should not be specified directly, and should instead be \
given as a GitHubSecret.";

WorkflowJob::InvalidPublisherToken =
"Value of option PublisherToken -> `1` should be Automatic or None.";

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Options*)
WorkflowJob // Options = {
    "BuildPacletAction"      -> $buildAction,
    "CheckPacletAction"      -> $checkAction,
    "SubmitPacletAction"     -> $submitAction,
    "TestPacletAction"       -> $testAction,
    "DefaultBranch"          -> $defaultBranch,
    "DefinitionNotebookPath" -> Automatic,
    OperatingSystem          -> Automatic,
    ProcessEnvironment       -> Automatic,
    "PublisherToken"         -> Automatic,
    ResourceSystemBase       -> Automatic,
    "Target"                 -> $actionTarget,
    TimeConstraint           -> Infinity
};

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Main definition*)
WorkflowJob[
    name_String,
    as_Association,
    opts: OptionsPattern[ ]
]? sp`HoldNotValidQ := withWorkflowOptions[ Workflow, opts ] @
    Module[ { new },
        new = postProcessYAML @ makeWorkflowJobData[ name, <| as, opts |> ];
        (* TODO: always insert "name" property as first arg *)
        With[ { a = new },
            If[ FailureQ @ a,
                a,
                sp`HoldSetValid @ WorkflowJob[ name, a ]
            ]
        ]
    ];

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*Named*)
WorkflowJob[ name_String, opts: OptionsPattern[ ] ] :=
    catchTop @ withOS[
        OptionValue @ OperatingSystem,
        If[ jobNameQ @ name,
            WorkflowJob[ name, Association @ opts ],
            throwMessageFailure[ WorkflowJob::invname, name ]
        ]
    ];

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*Custom*)
WorkflowJob[ as_Association, opts: OptionsPattern[ ] ] :=
    catchTop @ withOS[
        OptionValue @ OperatingSystem,
        Module[ { new, id },
            new = makeWorkflowJobData @ Association[ as, opts ];
            id = First[ KeyTake[ new, { "id", "name" } ], CreateUUID[ ] ];
            WorkflowJob[ id, new ]
        ]
    ];

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*Merged*)
WorkflowJob[
    wf_WorkflowJob? workflowJobQ,
    as_Association,
    opts: OptionsPattern[ ]
] :=
    catchTop @ withOS[
        OptionValue @ OperatingSystem,
        Module[ { name, as1, as2, merged, data },
            name   = Lookup[ as, "Name", Lookup[ as, "name", wf[ "Name" ] ] ];
            as1    = wf[ "Data" ];
            as2    = makeWorkflowJobData[ name, <| as, opts |> ];
            merged = merger @ { as1, as2 };
            data   = Join[ KeyTake[ merged, Keys @ as1 ], merged ];
            WorkflowJob[ name, data ]
        ]
    ];

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*File*)
WorkflowJob[ File[ file_String ], opts: OptionsPattern[ ] ] :=
    catchTop @ withOS[ OptionValue @ OperatingSystem,
        Module[ { name },
            name = FileBaseName @ file;
            WorkflowJob[
                <|
                    "name"      -> name,
                    "runs-on"   -> $defaultRunner,
                    "container" -> $defaultJobContainer,
                    "env"       -> $defaultJobEnv,
                    "Steps"     -> workflowFileSteps @ file
                |>,
                opts
            ]
        ]
    ];

WorkflowJob[ File[ file_String ], as_Association, opts: OptionsPattern[ ] ] :=
    WorkflowJob[ WorkflowJob[ File @ file, opts ], as, opts ];

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*List of Steps*)
WorkflowJob[ steps_List, opts: OptionsPattern[ ] ] :=
    catchTop @ withOS[
        OptionValue @ OperatingSystem,
        WorkflowJob[
            <|
                "name"      -> "UntitledJob",
                "runs-on"   -> $defaultRunner,
                "container" -> $defaultJobContainer,
                "env"       -> $defaultJobEnv,
                "Steps"     -> steps
            |>,
            opts
        ]
    ];

WorkflowJob[ steps_List, as_Association, opts: OptionsPattern[ ] ] :=
    WorkflowJob[ WorkflowJob[ steps, opts ], as, opts ];

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*Properties*)
(job_WorkflowJob? workflowJobQ)[ prop_ ] :=
    catchTop @ workflowJobProperty[ job, prop ];

(* TODO: handle undefined and not validQ *)

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Formatting*)
WorkflowJob /: MakeBoxes[ job_WorkflowJob? workflowJobQ, fmt_ ] :=
    With[ { boxes = FormattingHelper[ job, fmt ] },
        boxes /; MatchQ[ boxes, _InterpretationBox ]
    ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*$jobNames*)
$jobNames = { "Check", "Build", "Publish", "Release", "Test", "Submit" };

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*jobNameQ*)
jobNameQ[ name_ ] := MemberQ[ $jobNames, name ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*workflowJobQ*)
workflowJobQ // Attributes = { HoldFirst };

workflowJobQ[ job: WorkflowJob[ name_? StringQ, as_? AssociationQ ] ] :=
    sp`HoldValidQ @ job;

workflowJobQ[ ___ ] := False;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*workflowJobProperty*)
workflowJobProperty[ job_, prop: $$wfProp ] := genericWFProperty[ job, prop ];

workflowJobProperty[ job_, "Needs" ] :=
    Lookup[ genericWFProperty[ job, "Data" ], "needs", None ];

workflowJobProperty[ job_, "Steps" ] :=
    Module[ { data, os, steps },
        data = genericWFProperty[ job, "Data" ];
        os = toDefaultOS @ Lookup[ data, "runs-on", "ubuntu-latest" ];
        steps = Lookup[ data, "steps", { } ];
        WorkflowStep[ #, OperatingSystem -> os ] & /@ steps
    ];

workflowJobProperty[ _, prop_ ] :=
    throwMessageFailure[ WorkflowJob::invprop, prop ];

workflowJobProperty // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*makeWorkflowJobData*)
makeWorkflowJobData[ name_String? jobNameQ, as_Association ] :=
    Enclose @ Module[ { rule, as1, as2, data },
        rule = ConfirmMatch[ normalizeJob @ name, _ -> _ ];
        as1  = ConfirmBy[ Last @ rule, AssociationQ ];
        as2  = ConfirmBy[ normalizeForYAML[ "jobs", name, as ], AssociationQ ];
        data = merger @ { as1, as2 };
        Join[ KeyTake[ data, Keys @ as2 ], data ]
    ];

makeWorkflowJobData[ name_String, custom_Association ] :=
    makeWorkflowJobData @ Prepend[ custom, "name" -> name ];

makeWorkflowJobData[ custom_Association ] :=
    Enclose @ Module[ { rule, data, id },
        rule = ConfirmMatch[ normalizeJob @ custom, _ -> _ ];
        data = ConfirmBy[ Last @ rule, AssociationQ ];
        data
    ];

makeWorkflowJobData // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*workflowFileSteps*)
workflowFileSteps[ file_ ] := workflowFileSteps[ file, $defaultOS ];

workflowFileSteps[ file_String, "Windows-x86-64"|"MacOSX-x86-64" ] := {
    "Checkout",
    "RestoreCachedWolframEngine",
    "InstallWolframEngine",
    File @ file
};

workflowFileSteps[ file_String, _ ] := { "Checkout", File @ file };

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*merger*)
merger[ as: { ___Association? AssociationQ } ] := Merge[ { as }, merger ];
merger[ stuff_List ] := Last[ stuff, Missing[ ] ];
merger // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*WorkflowStep*)
WorkflowStep::invname =
"`1` is not a known step name.";

WorkflowStep::invprop =
"`1` is not a valid WorkflowStep property name.";

WorkflowStep::TokenSecret =
"Publisher tokens should not be specified directly, and should instead be \
given as a GitHubSecret.";

WorkflowStep::InvalidPublisherToken =
"Value of option PublisherToken -> `1` should be Automatic or None.";

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Options*)
WorkflowStep // Options = {
    "BuildPacletAction"      -> $buildAction,
    "CheckPacletAction"      -> $checkAction,
    "SubmitPacletAction"     -> $submitAction,
    "TestPacletAction"       -> $testAction,
    "DefaultBranch"          -> $defaultBranch,
    "DefinitionNotebookPath" -> Automatic,
    OperatingSystem          -> Automatic,
    ProcessEnvironment       -> Automatic,
    "PublisherToken"         -> Automatic,
    ResourceSystemBase       -> Automatic,
    "Target"                 -> $actionTarget,
    TimeConstraint           -> Infinity
};

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Main definition*)
WorkflowStep[
    name_String,
    as_Association,
    opts: OptionsPattern[ ]
]? sp`HoldNotValidQ := withWorkflowOptions[ WorkflowStep, opts ] @
    Module[ { new },
        new = postProcessYAML @ makeWorkflowStepData[ name, <| as, opts |> ];
        (* TODO: always insert "name" property as first arg *)
        With[ { a = new },
            If[ FailureQ @ a,
                a,
                sp`HoldSetValid @ WorkflowStep[ name, a ]
            ]
        ]
    ];

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*Named*)
WorkflowStep[ name_String, opts: OptionsPattern[ ] ] :=
    withWorkflowOptions[ WorkflowStep, opts ] @
        If[ stepNameQ @ name,
            WorkflowStep[ name, <| opts |> ],
            throwMessageFailure[ WorkflowStep::invname, name ]
        ];

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*Custom*)
WorkflowStep[ as_Association, opts: OptionsPattern[ ] ] :=
    withWorkflowOptions[ WorkflowStep, opts ] @
        Module[ { new, id },
            new = makeWorkflowStepData @ <| as, opts |>;
            (* TODO: write a getWorkFlowID function *)
            id = First[ KeyTake[ new, { "id", "name" } ], CreateUUID[ ] ];
            WorkflowStep[ id, new ]
        ];

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*Merged*)
WorkflowStep[
    wf_WorkflowStep? workflowStepQ,
    as_Association,
    opts: OptionsPattern[ ]
] := withWorkflowOptions[ WorkflowStep, opts ] @
    Module[ { name, as1, as2, merged, data },
        name = wf[ "Name" ];
        as1 = wf[ "Data" ];
        as2 = makeWorkflowStepData[ name, <| as, opts |> ];
        merged = merger @ { as1, as2 };
        data = Join[ KeyTake[ merged, Keys @ as1 ], merged ];
        WorkflowStep[ name, data ]
    ];

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*File*)
WorkflowStep[ File[ file_String ], opts: OptionsPattern[ ] ] :=
    withWorkflowOptions[ WorkflowStep, opts ] @
        Module[ { name, run },
            name = FileBaseName @ file;
            run = makeRunString @ file;
            WorkflowStep[
                <|
                    "name" -> name,
                    "run" -> run,
                    "env" -> Join[ $defaultJobEnv, $wolframScriptRunEnv ]
                |>,
                opts
            ]
        ];

WorkflowStep[ File[ file_String ], as_Association, opts: OptionsPattern[ ] ] :=
    WorkflowStep[ WorkflowStep[ File @ file, opts ], as, opts ];

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*Evaluate*)
WorkflowStep[
    WorkflowEvaluate[ code_, name_String ],
    opts: OptionsPattern[ ]
] :=
    withWorkflowOptions[ WorkflowStep, opts ] @
        Module[ { run },
            run = makeEvaluateString @ code;
            WorkflowStep[
                <|
                    "name" -> name,
                    "run"  -> run,
                    "env"  -> Join[ $defaultJobEnv, $wolframScriptRunEnv ]
                |>,
                opts
            ]
        ];

WorkflowStep[ e_WorkflowEvaluate, as_Association, opts: OptionsPattern[ ] ] :=
    WorkflowStep[ WorkflowStep[ e, opts ], as, opts ];

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*Properties*)
(job_WorkflowStep? workflowStepQ)[ prop_ ] :=
    catchTop @ workflowStepProperty[ job, prop ];

(* TODO: handle undefined and not validQ *)

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Formatting*)
WorkflowStep /: MakeBoxes[ step_WorkflowStep? workflowStepQ, fmt_ ] :=
    With[ { boxes = FormattingHelper[ step, fmt ] },
        boxes /; MatchQ[ boxes, _InterpretationBox ]
    ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*$stepNames*)
$stepNames = {
    "Build",
    "Check",
    "Checkout",
    "CreateRelease",
    "Download",
    "DownloadCompilationArtifacts",
    "InstallWolframEngine",
    "Publish",
    "RestoreCachedWolframEngine",
    "Submit",
    "Test",
    "Upload",
    "UploadBuildArtifacts",
    "UploadRelease"
};

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*stepNameQ*)
stepNameQ[ name_ ] := MemberQ[ $stepNames, name ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*workflowStepQ*)
workflowStepQ // Attributes = { HoldFirst };

workflowStepQ[ job: WorkflowStep[ name_? StringQ, as_? AssociationQ ] ] :=
    sp`HoldValidQ @ job;

workflowStepQ[ ___ ] := False;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*workflowStepProperty*)
workflowStepProperty[ job_, prop: $$wfProp ] := genericWFProperty[ job, prop ];

workflowStepProperty[ step_, "Action"     ] := getStepAction @ step;
workflowStepProperty[ step_, "ActionURL"  ] := getStepActionURL @ step;
workflowStepProperty[ step_, "ActionLink" ] := getStepActionLink @ step;

workflowStepProperty[ step_, "Command" ] :=
    Lookup[ step[ "Data" ], "run", Missing[ "NotAvailable" ] ];

workflowStepProperty[ _, prop_ ] :=
    throwMessageFailure[ WorkflowStep::invprop, prop ];

workflowStepProperty // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*getStepAction*)
getStepAction[ step_WorkflowStep ] := getStepAction @ step[ "Data" ];
getStepAction[ KeyValuePattern[ "uses" -> uses_String ] ] := uses;
getStepAction[ ___ ] := Missing[  "NotAvailable" ];

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*getStepActionURL*)
getStepActionURL[ step_WorkflowStep ] :=
    getStepActionURL @ getStepAction @ step;

getStepActionURL[ uses_String ] :=
    Module[ { lbl },
        lbl = StringDelete[ uses, "@" ~~ __ ~~ EndOfString ];
        URLBuild @ { "https://github.com", lbl }
    ];

getStepActionURL[ ___ ] := Missing[ "NotAvailable" ];

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*getStepActionLink*)
getStepActionLink[ step_WorkflowStep ] :=
    getStepActionLink[ getStepAction @ step, getStepActionURL @ step ];

getStepActionLink[ lbl_String, url_String ] :=
    Hyperlink[ lbl, url ];

getStepActionLink[ ___ ] := Missing[ "NotAvailable" ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*makeWorkflowStepData*)
makeWorkflowStepData[ name_String? stepNameQ, as_Association ] :=
    Enclose @ Module[ { rule, as1, as2, data },
        as1  = ConfirmBy[ normalizeStep @ name, AssociationQ ];
        as2  = ConfirmBy[ normalizeStep @ as, AssociationQ ];
        data = merger @ { as1, as2 };
        Join[ KeyTake[ data, Keys @ as2 ], data ]
    ];

makeWorkflowStepData[ name_String, custom_Association ] :=
    makeWorkflowStepData @ Prepend[ custom, "name" -> name ];

makeWorkflowStepData[ custom_Association ] :=
    Enclose @ ConfirmBy[ normalizeStep @ custom, AssociationQ ];

makeWorkflowStepData // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*$wolframScriptRunEnv*)
$wolframScriptRunEnv /; $defaultOS === "MacOSX-x86-64" := <|
    "WOLFRAMENGINE_EXECUTABLES_DIRECTORY" -> "\"${{ env.WOLFRAMENGINE_INSTALLATION_DIRECTORY }}/Contents/Resources/Wolfram Player.app/Contents/MacOS\"",
    "WOLFRAMSCRIPT_KERNELPATH"            -> "\"${{ env.WOLFRAMENGINE_INSTALLATION_DIRECTORY }}/Contents/MacOS/WolframKernel\""
|>;

$wolframScriptRunEnv /; $defaultOS === "Windows-x86-64" := <|
    "WOLFRAMENGINE_INSTALLATION_DIRECTORY" -> "'${{ runner.temp }}\\WolframEngine'",
    "WOLFRAMINIT" -> "\"-pwfile !cloudlm.wolfram.com -entitlement ${{ secrets.WOLFRAMSCRIPT_ENTITLEMENTID }}\""
|>;

$wolframScriptRunEnv /; True := <| |>;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*makeRunString*)
makeRunString[ file_String ] /; $defaultOS === "MacOSX-x86-64" := "\
export PATH=\"${{ env.WOLFRAMENGINE_EXECUTABLES_DIRECTORY }}:$PATH\"
wolframscript -debug -verbose -script " <> file;

makeRunString[ file_String ] /; $defaultOS === "Windows-x86-64" := "\
$env:Path += ';${{ env.WOLFRAMENGINE_INSTALLATION_DIRECTORY }}\\'
wolfram -script " <> file;

makeRunString[ file_String ] /; True := "wolframscript " <> file;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*makeEvaluateString*)
makeEvaluateString // Attributes = { HoldFirst };

makeEvaluateString[ command_String ] := command;

makeEvaluateString[ File[ file_String ] ] := makeRunString @ file;

makeEvaluateString[ code_ ] :=
    Block[ { $Context = "System`", $ContextPath = { "System`" } },
        StringJoin[
            "wolframscript -code '",
            ToString[ Unevaluated @ code, InputForm ],
            "'"
        ]
    ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*WorkflowExport*)
WorkflowExport::defbranch =
"Expected a string instead of `1` for the default branch.";

WorkflowExport::wfname =
"`1` is not a recognized workflow name.";

WorkflowExport::invspec =
"Invalid workflow specification: `1`";

WorkflowExport::invtimeout =
"Invalid TimeConstraint: `1`";

WorkflowExport::yamlfail =
"Failed to export workflow to YAML format.";

WorkflowExport::ymlconv =
"Unable to convert `1` to valid YAML.";

WorkflowExport::export =
"Failed to export YAML to the file `1`.";

WorkflowExport::entitlement = "`1`";

WorkflowExport::invaction =
"`1` is not a valid action specification.";

WorkflowExport::target =
"`1` is not a valid target specification.";

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Options*)
WorkflowExport // Options = {
    "BuildPacletAction"      -> $buildAction,
    "CheckPacletAction"      -> $checkAction,
    "TestPacletAction"       -> $testAction,
    "DefaultBranch"          -> $defaultBranch,
    "DefinitionNotebookPath" -> Automatic,
    OperatingSystem          -> Automatic,
    ProcessEnvironment       -> Automatic,
    ResourceSystemBase       -> Automatic,
    "Target"                 -> $actionTarget,
    TimeConstraint           -> Infinity
};

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Main definition*)
WorkflowExport[ pac_, spec_, opts: OptionsPattern[ ] ] :=
    catchTop @ Module[ { workflow },
        workflow = workflowExport[
            spec,
            toDefNBLocation @ pac,
            toDefaultBranch @ OptionValue[ "DefaultBranch" ],
            toTimeConstraint @ OptionValue[ TimeConstraint ],
            toBuildPacletAction @ OptionValue[ "BuildPacletAction" ],
            toCheckPacletAction @ OptionValue[ "CheckPacletAction" ],
            toActionTarget @ OptionValue[ "Target" ]
        ];
        exportWorkflow[ pac, workflow ]
    ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*workflowExport*)
workflowExport[
    spec_,
    defNotebookLocation_,
    branch_,
    timeConstraint_,
    buildPacletAction_,
    checkPacletAction_,
    actionTarget_
] :=
    Block[
        {
            $defaultBranch   = branch,
            $timeConstraint  = timeConstraint,
            $buildAction     = buildPacletAction,
            $checkAction     = checkPacletAction,
            $defNotebookPath = defNotebookLocation,
            $actionTarget    = actionTarget
        },
        workflowExport0 @ spec
    ];

workflowExport // catchUndefined;


workflowExport0[ wf_Workflow? workflowQ ] := workflowExport0 @ wf[ "Data" ];

workflowExport0[ name_String ] :=
    workflowExport0 @ Lookup[
        $namedWorkflows,
        toWorkflowName @ name,
        throwMessageFailure[ WorkflowExport::wfname, name ]
    ];

workflowExport0[ spec_Association ] :=
    Module[ { workflow },
        workflow = normalizeForYAML @ spec;
        If[ TrueQ @ validValueQ @ workflow,
            workflow,
            throwMessageFailure[ WorkflowExport::invspec, spec ]
        ]
    ];

workflowExport0 // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*toActionTarget*)
toActionTarget[ str_String? StringQ ] := str;
toActionTarget[ Automatic ] := $actionTarget;

toActionTarget[ str: Except[ _String? StringQ ] ] :=
    throwMessageFailure[ WorkflowExport::target, str ];

toActionTarget // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*toDefaultBranch*)
toDefaultBranch[ branch_String? StringQ ] := branch;

toDefaultBranch[ branch: Except[ _String? StringQ ] ] :=
    throwMessageFailure[ WorkflowExport::defbranch, branch ];

toDefaultBranch // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*toDefNBLocation*)
toDefNBLocation[ dir_? DirectoryQ ] := Enclose[
    Module[ { file },
        file = ConfirmBy[ findDefinitionNotebook @ dir, FileExistsQ ];
        ConfirmBy[ relativePath[ dir, file ], StringQ ]
    ],
    $defNotebookPath &
];

toDefNBLocation[ pac_PacletObject ] := toDefNBLocation @ pac[ "Location" ];

toDefNBLocation[ file_? defNBQ ] :=
    Module[ { root },
        root = getRootGitDirectory @ file;
        If[ DirectoryQ @ root,
            relativePath[ root, file ],
            $defNotebookPath
        ]
    ];

toDefNBLocation[ file_ ] /; ToLowerCase @ FileExtension @ file === "yml" :=
    Module[ { root, nb },
        root = getRootGitDirectory @ file;
        nb   = findDefinitionNotebook @ root;
        If[ FileExistsQ @ nb,
            relativePath[ root, nb ],
            $defNotebookPath
        ]
    ];

toDefNBLocation[ File[ file_String ] ] := toDefNBLocation @ file;

toDefNBLocation[ file_String? relativePathQ ] :=
    "./" <> StringDelete[
        StringReplace[ file, "\\" -> "/" ],
        StartOfString~~"./"
    ];

toDefNBLocation[ file_String ] := file;

toDefNBLocation // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*getRootGitDirectory*)
getRootGitDirectory[ dir_? DirectoryQ ] :=
    With[ { root = getRootGitDirectory0 @ dir },
        If[ DirectoryQ @ root, root, dir ]
    ];

getRootGitDirectory[ file_ ] := getRootGitDirectory0 @ file;

getRootGitDirectory // catchUndefined;

getRootGitDirectory0[ file_ ] := Enclose[
    Module[ { dir, dirs, root, nb },
        dir  = ConfirmBy[ DirectoryName @ file, StringQ ];
        dirs = FixedPointList[ DirectoryName, dir ];
        root = SelectFirst[ dirs, FileNames[ ".git", # ] =!= { } & ];
        If[ DirectoryQ @ root,
            ConfirmBy[ root, DirectoryQ ],
            ConfirmBy[ SelectFirst[ dirs, pacletDirectoryQ ], DirectoryQ ]
        ]
    ],
    Missing[ "NotFound" ] &
];

getRootGitDirectory0 // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*toBuildPacletAction*)
toBuildPacletAction[ Automatic ] := normalizeActionName @ $buildAction;
toBuildPacletAction[ name_     ] := normalizeActionName @ name;
toBuildPacletAction // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*toCheckPacletAction*)
toCheckPacletAction[ Automatic ] := normalizeActionName @ $checkAction;
toCheckPacletAction[ name_     ] := normalizeActionName @ name;
toCheckPacletAction // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*toTestPacletAction*)
toTestPacletAction[ Automatic ] := normalizeActionName @ $testAction;
toTestPacletAction[ name_     ] := normalizeActionName @ name;
toTestPacletAction // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*normalizeActionName*)
normalizeActionName[ name_String? StringQ ] :=
    normalizeActionName[ name, StringSplit[ name, c: "/" | "@" :> c ] ];

normalizeActionName[ name_, { owner_, "/", repo_ } ] :=
    normalizeActionName[ name, { owner, "/", repo, "@", "latest" } ];

normalizeActionName[ name_, { owner_, "/", repo_, "@", "latest" } ] :=
    latestActionName[ name, owner, repo ];

normalizeActionName[ name_, { owner_, "/", repo_, "@", ref_ } ] :=
    StringJoin[ owner, "/", repo, "@", ref ];

normalizeActionName[ name_, _ ] :=
    throwMessageFailure[ WorkflowExport::invaction, name ];

normalizeActionName // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*latestActionName*)
latestActionName[ name_, owner_, repo_ ] := Enclose[
    Module[ { data, tag, new },
        data = ghAPI[ "repos", owner, repo, "releases", "latest" ];
        data = ConfirmBy[ data, AssociationQ ];
        tag  = ConfirmBy[ Lookup[ data, "tag_name" ], StringQ ];
        new  = owner <> "/" <> repo <> "@" <> tag;
        latestActionName[ name, owner, repo ] = new
    ],
    name &
];

latestActionName // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*toTimeConstraint*)
toTimeConstraint[ t_? NumberQ ] := toTimeConstraint @ Quantity[ t, "Seconds" ];
toTimeConstraint[ Infinity    ] := toTimeConstraint @ Quantity[ 6, "Hours" ];

toTimeConstraint[ q_Quantity ] :=
    QuantityMagnitude @ UnitConvert[ q, "Minutes" ];

toTimeConstraint[ other_ ] :=
    throwMessageFailure[ WorkflowExport::invtimeout, other ];

toTimeConstraint // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*exportWorkflow*)
exportWorkflow[ pac_PacletObject, workflow_ ] :=
    exportWorkflow[ getRootGitDirectory @ pac[ "Location" ], workflow ];

exportWorkflow[ dir_? DirectoryQ, workflow_ ] :=
    Module[ { wfDir, file },
        wfDir  = toWorkFlowDir @ dir;
        file   = FileNameJoin @ { wfDir, workflowFileName @ workflow };
        exportWorkflow[ file, workflow ]
    ];

exportWorkflow[ file_, workflow_ ] := Enclose[
    Module[ { yaml, exported },
        yaml = ConfirmBy[ toYAMLString @ workflow, StringQ ];
        exported = Export[ file, yaml, "String" ];
        If[ FileExistsQ @ exported,
            entitlementWarning[ ];
            File @ exported,
            throwMessageFailure[ WorkflowExport::export, file ]
        ]
    ],
    throwMessageFailure[ WorkflowExport::ymlconv ] &
];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*$hasLicenseEntitlements*)
$hasLicenseEntitlements :=
    MatchQ[ LicenseEntitlements[ ], { ___, _LicenseEntitlementObject, ___ } ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*entitlementWarning*)
entitlementWarning[ ] := entitlementWarning[ ] =
    If[ TrueQ @ suppressedEntitlementWarning[ ],
        Null,
        Block[ { $MessagePrePrint = Identity },
            messageFailure[ WorkflowExport::entitlement, $licenseWarningText ]
        ]
    ];

suppressedEntitlementWarning[ ] :=
    suppressedEntitlementWarning[ $thisPacletVersion, $suppressWarningFile ];

suppressedEntitlementWarning[ ver_String, file_? FileExistsQ ] :=
    suppressedEntitlementWarning[ ver, Get @ file ];

suppressedEntitlementWarning[ ver_String, ver_String ] := True;

suppressedEntitlementWarning[ ___ ] := False;

$suppressWarningFile :=
    FileNameJoin @ {
        $UserBaseDirectory,
        "ApplicationData",
        $thisPacletName,
        "SuppressWarning.wl"
    };

$licenseWarningText := $licenseWarningText = TemplateApply[ "\
Warning: An on-demand license entitlement is required for this workflow to run.
See the `LicenseEntitlementDocs` for details on creating license entitlements.\
`DisableLink`",
<|
    "LicenseEntitlementDocs" -> $leTutorial,
    "DisableLink" -> $disableGHSecretWarning
|>
];

$leTutorial :=
    If[ $Notebooks,
        ToString[
            Hyperlink[
                "license entitlement tutorial",
                "paclet:Wolfram/PacletCICD/tutorial/LicenseEntitlementsAndRepositorySecrets"
            ],
            StandardForm
        ],
        "license entitlement tutorial"
    ];

$disableGHSecretWarning :=
    With[ { file = $suppressWarningFile, ver = $thisPacletVersion },
        If[ $Notebooks,
            " " <> hyperlinkButtonString[
                "Don't show this message again \[RightGuillemet]",
                GeneralUtilities`EnsureDirectory @ DirectoryName @ file;
                Put[ ver, file ];
                NotebookDelete @ EvaluationCell[ ]
            ],
            ""
        ]
    ];

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*hyperlinkButtonString*)
hyperlinkButtonString // Attributes = { HoldRest };

hyperlinkButtonString[ label_, code_ ] :=
    ToString[
        RawBoxes @ TagBox[
            ButtonBox[
                StyleBox[
                    ToBoxes @ label,
                    ShowStringCharacters -> False
                ],
                BaseStyle      -> "Hyperlink",
                ButtonFunction :> code,
                Evaluator      -> Automatic,
                Method         -> "Queued"
            ],
            MouseAppearanceTag[ "LinkHand" ]
        ],
        StandardForm
    ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*toWorkFlowDir*)
toWorkFlowDir[ dir_ ] := toWorkFlowDir @ FileNameSplit @ ExpandFileName @ dir;
toWorkFlowDir[ { p__, ".github", "workflows" } ] := toWorkFlowDir @ { p };
toWorkFlowDir[ { p__, ".github" } ] := toWorkFlowDir @ { p };
toWorkFlowDir[ { p__ } ] := ensureDirectory @ { p, ".github", "workflows" };
toWorkFlowDir // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*workflowFileName*)
workflowFileName[ KeyValuePattern[ "name" -> name_String ] ] :=
    workflowFileName @ name;

workflowFileName[ KeyValuePattern[ "id" -> id_String ] ] :=
    workflowFileName @ id;

workflowFileName[ name_String ] :=
    StringDelete[ name, Except[ LetterCharacter ] ] <> ".yml";

workflowFileName[ ___ ] := "workflow.yml";

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*$namedWorkflows*)
$namedWorkflows := <|
    "Release" -> <|
        "name" -> "Release",
        "on" -> <|
            "push"              -> <| "branches" -> { "release/*" } |>,
            "workflow_dispatch" -> True
        |>,
        "jobs" -> {
            "Check",
            "Test",
            { "Release", <| "needs" -> { "Check", "Test" } |> }
        }
    |>
    ,
    "Build" -> <|
        "name" -> "Build",
        "on"   -> <|
            "push"              -> <| "branches" -> $defaultBranch |>,
            "pull_request"      -> <| "branches" -> $defaultBranch |>,
            "workflow_dispatch" -> True
        |>,
        "jobs" -> "Build"
    |>
    ,
    "Check" -> <|
        "name" -> "Check",
        "on"   -> <|
            "push"              -> <| "branches" -> $defaultBranch |>,
            "pull_request"      -> <| "branches" -> $defaultBranch |>,
            "workflow_dispatch" -> True
        |>,
        "jobs" -> "Check"
    |>
    ,
    "Test" -> <|
        "name" -> "Test",
        "on"   -> <|
            "push"              -> <| "branches" -> $defaultBranch |>,
            "pull_request"      -> <| "branches" -> $defaultBranch |>,
            "workflow_dispatch" -> True
        |>,
        "jobs" -> "Test"
    |>
    ,
    "Submit" -> <|
        "name" -> "Submit",
        "on"   -> <|
            "push"              -> <| "branches" -> { "release/*" } |>,
            "workflow_dispatch" -> True
        |>,
        "jobs" -> "Submit"
    |>
    ,
    "Compile" -> <|
        "name" -> "Compile",
        "on"   -> <|
            "push"              -> <| "branches" -> $defaultBranch |>,
            "pull_request"      -> <| "branches" -> $defaultBranch |>,
            "workflow_dispatch" -> True
        |>,
        "jobs" -> "Compile"
    |>
|>;

toWorkflowName[ name_String ] :=
    Module[ { lc, wf },
        lc = ToLowerCase @ name;
        wf = toWorkflowName0 @ StringDelete[ lc, Except[ LetterCharacter ] ];
        If[ StringQ @ wf, wf, name ]
    ];

toWorkflowName0[ "release"       ] := "Release";
toWorkflowName0[ "releasepaclet" ] := "Release";
toWorkflowName0[ "build"         ] := "Build";
toWorkflowName0[ "buildpaclet"   ] := "Build";
toWorkflowName0[ "check"         ] := "Check";
toWorkflowName0[ "checkpaclet"   ] := "Check";
toWorkflowName0[ "submit"        ] := "Submit";
toWorkflowName0[ "submitpaclet"  ] := "Submit";
toWorkflowName0[ "publish"       ] := "Submit";
toWorkflowName0[ "publishpaclet" ] := "Submit";
toWorkflowName0[ "test"          ] := "Test";
toWorkflowName0[ "testpaclet"    ] := "Test";

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*validValueQ*)
validValueQ[ str_String? StringQ ] := True;
validValueQ[ num_? NumberQ       ] := True;
validValueQ[ Null | True | False ] := True;
validValueQ[ list_List           ] := AllTrue[ list, validValueQ ];

validValueQ[ as_Association ] :=
    validValueQ @ Keys @ as && validValueQ @ Values @ as;

validValueQ[ ___ ] := False;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*normalizeForYAML*)
$$ymlKey  = _String|_Integer|Automatic;
$$ymlKeys = $$ymlKey...;

$$rootProp = Alternatives[
    "name",
    "on",
    "env",
    "defaults",
    "concurrency",
    "jobs",
    "permissions"
];

normalizeForYAML[ "operating_system" -> _ ] := Nothing;
normalizeForYAML[ "jobs", _, "operating_system" -> _ ] := Nothing;
normalizeForYAML[ "jobs", _, "steps", _, "operating_system" -> _ ] := Nothing;

normalizeForYAML[ key: Except[ $$rootProp ] -> val_ ] :=
    With[ { p = toCanonicalProp @ key },
        If[ MatchQ[ p, $$rootProp ],
            normalizeForYAML[ p -> val ],
            throwFailure[ "`1` is not a valid property", key ]
        ]
    ];

normalizeForYAML[ k___, key_ -> val_ ] :=
    With[ { p = toCanonicalProp[ k, key ] },
        normalizeForYAML[ k, p -> val ] /; p =!= key
    ];

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*toCanonicalProp*)
toCanonicalProp[ k___, "Environment"|"ProcessEnvironment" ] := "env";
toCanonicalProp[ k___, "environment"|"processenvironment" ] := "env";
toCanonicalProp[ "jobs", job_String ] := job;
toCanonicalProp[ k___, "with", var_String ] := var;
toCanonicalProp[ k___, "env", var_String ] := var;
toCanonicalProp[ k___, s_String ] := fromCamelCase @ s;
toCanonicalProp[ k___, s_Symbol ] := toCanonicalProp[ k, SymbolName @ s ];
toCanonicalProp[ k___, other_ ] := other;

fromCamelCase[ str_String ] :=
    ToLowerCase @ StringReplace[
        str,
        a_? LowerCaseQ ~~ b_? UpperCaseQ :> a <> "_" <> b
    ];

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*Name*)
normalizeForYAML[ "name"|"Name" -> name_String ] := "name" -> name;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*On*)
normalizeForYAML[ "On" -> on_ ] := normalizeForYAML[ "on" -> on ];

normalizeForYAML[ "on", "WorkflowDispatch" -> wd_ ] :=
    normalizeForYAML[ "on", "workflow_dispatch" -> wd ];

normalizeForYAML[ "on", "Push" -> push_ ] :=
    normalizeForYAML[ "on", "push" -> push ];

normalizeForYAML[ "on", "PullRequest"|"pullrequest" -> pr_ ] :=
    normalizeForYAML[ "on", "pull_request" -> pr ];

normalizeForYAML[ "on", key_String -> True ] := key -> Null;

normalizeForYAML[ "on", on_, "Branches" -> br_ ] :=
    normalizeForYAML[ "on", on, "branches" -> br ];

normalizeForYAML[ "on", on_, "branches" -> branch_String ] :=
    "branches" -> { branch };

normalizeForYAML[ "on", on_, "branches" -> branches: { __String } ] :=
    "branches" -> branches;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*Jobs*)
normalizeForYAML[ "Jobs" -> jobs_ ] :=
    normalizeForYAML[ "jobs" -> jobs ];

normalizeForYAML[ "jobs" -> jobs_List ] :=
    "jobs" -> normalizeJobs @ jobs;

normalizeForYAML[ "jobs" -> job_String ] :=
    normalizeForYAML[ "jobs" -> { job } ];

(* ::**********************************************************************:: *)
(* ::Subsubsubsection::Closed:: *)
(*Job*)
normalizeForYAML[ "jobs", _, "name"|"Name" -> name_String ] :=
    "name" -> name;

normalizeForYAML[ "jobs", _, "id"|"ID" -> id_String ] :=
    "id" -> id;

normalizeForYAML[ "jobs", _, "runs-on"|"RunsOn" -> str_String ] :=
    "runs-on" -> str;

normalizeForYAML[ "jobs", name_, "Needs" -> needs_ ] :=
    normalizeForYAML[ "jobs", name, "needs" -> needs ];

normalizeForYAML[ "jobs", name_, "needs" -> str_String ] :=
    normalizeForYAML[ "jobs", name, "needs" -> Flatten @ { str } ];

normalizeForYAML[ "jobs", name_, "needs" -> names: { __String } ] :=
    "needs" -> names;

normalizeForYAML[ "jobs", name_, "needs" -> { } ] :=
    "needs" -> $noValue;

normalizeForYAML[
    "jobs",
    name_,
    "container",
    "options" -> opts: _String | { ___String }
] :=
    "options" -> StringRiffle[ Flatten @ { opts } ];

(* ::**********************************************************************:: *)
(* ::Subsubsubsection::Closed:: *)
(*Steps*)
normalizeForYAML[ "jobs", job_, "Steps" -> steps_ ] :=
    normalizeForYAML[ "jobs", job, "steps" -> steps ];

normalizeForYAML[ "jobs", job_, "steps", idx_Integer, step_ ] :=
    normalizeStep[ "jobs", job, "steps", idx, step ];

normalizeForYAML[ "jobs", job_, "steps" -> step: Except[ _List ] ] :=
    normalizeForYAML[ "jobs", job, "steps" -> { step } ];

normalizeForYAML[
    "jobs",
    job_,
    "steps",
    step_,
    "parameters" -> as_Association
] :=
    normalizeForYAML[
        "jobs",
        job,
        "steps",
        step,
        "with" -> KeyMap[ toLowerCase, as ]
    ];

(* ::**********************************************************************:: *)
(* ::Subsubsubsection::Closed:: *)
(*TimeConstraint*)
$$tcKey = "timeout-minutes"|"time_constraint";

normalizeForYAML[ keys___, $$tcKey -> Infinity | Automatic | None ] :=
    "timeout-minutes" -> $noValue;

normalizeForYAML[ keys___, $$tcKey -> q_Quantity ] :=
    Module[ { minutes },
        minutes = QuantityMagnitude @ UnitConvert[ q, "Minutes" ];
        "timeout-minutes" -> If[ IntegerQ @ minutes, minutes, N @ minutes ]
    ];

normalizeForYAML[ keys___, "time_constraint" -> t_? NumericQ ] :=
    normalizeForYAML[ keys, "time_constraint" -> Quantity[ t, "Seconds" ] ];

normalizeForYAML[ keys___, "time_constraint" -> other_ ] :=
    normalizeForYAML[ keys, "timeout-minutes" -> other ];

(* ::**********************************************************************:: *)
(* ::Subsubsubsection::Closed:: *)
(*Environment*)
normalizeForYAML[ keys___, "env", key_ -> None ] :=
    normalizeForYAML[ keys, "env", key -> $noValue ];

normalizeForYAML[ keys___, key_String :> Environment[ env_String ] ] :=
    normalizeForYAML[ keys, key -> "${{ env."<>env<>" }}" ];

normalizeForYAML[ keys___, key_String -> GitHubSecret[ sec_String ] ] :=
    normalizeForYAML[ keys, key -> "${{ secrets."<>sec<>" }}" ];

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*General*)
normalizeForYAML[ keys___, as_Association ] :=
    AssociationMap[ normalizeForYAML[ keys, #1 ] &, as ];

normalizeForYAML[ keys___, list_List ] :=
    MapIndexed[ normalizeForYAML[ keys, First[ #2 ], #1 ] &, list ];

normalizeForYAML[ ___, wf_Workflow? workflowQ ] :=
    normalizeForYAML @ wf[ "Data" ];

normalizeForYAML[ ___, job_WorkflowJob? workflowJobQ ] :=
    normalizeForYAML @ job[ "Data" ];

normalizeForYAML[ ___, step_WorkflowStep? workflowStepQ ] :=
    normalizeForYAML @ step[ "Data" ];

normalizeForYAML[ keys___, key_ -> as_Association ] :=
    key -> normalizeForYAML[ keys, key, as ];

normalizeForYAML[ keys___, key_ -> list_List ] :=
    key -> MapIndexed[ normalizeForYAML[ keys, key, First[ #2 ], #1 ] &, list ];

normalizeForYAML[ keys___, key_ -> None ] :=
    key -> None;

normalizeForYAML[ k___, key_ -> $noValue ] :=
    key -> $noValue;

normalizeForYAML[ keys___, key_String -> value_? validValueQ ] :=
    key -> value;

normalizeForYAML[ keys___, key_ -> val_ ] :=
    throwError[ "Invalid value `1` (`2`)", val, keySeqDocLink[ keys, key ] ];

normalizeForYAML // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*normalizeJobs*)
normalizeJobs[ job_WorkflowJob ] := normalizeJobs @ { job };
normalizeJobs[ job_String      ] := normalizeJobs @ { job };
normalizeJobs[ jobs_List       ] := Association[ normalizeJob /@ jobs ];
normalizeJobs[ jobs_Association? validValueQ ] := jobs; (* TODO: validate against schema *)
normalizeJobs // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*normalizeJob*)
normalizeJob[ "check" | "checkpaclet" ] :=
    "Check" -> <|
        "name"            -> "Check",
        "runs-on"         -> $defaultRunner,
        "container"       -> $defaultJobContainer,
        "env"             -> $defaultJobEnv,
        "timeout-minutes" -> $timeConstraint,
        "steps"           -> {
            normalizeStep[ "Checkout"    ],
            normalizeStep[ "CheckPaclet" ]
        }
    |>;

normalizeJob[ "build" | "buildpaclet" ] :=
    "Build" -> <|
        "name"            -> "Build",
        "runs-on"         -> $defaultRunner,
        "container"       -> $defaultJobContainer,
        "env"             -> $defaultJobEnv,
        "timeout-minutes" -> $timeConstraint,
        "steps"           -> {
            normalizeStep[ "Checkout"             ],
            normalizeStep[ "BuildPaclet"          ],
            normalizeStep[ "UploadBuildArtifacts" ]
        }
    |>;

normalizeJob[ "release" | "releasepaclet" ] :=
    "Release" -> <|
        "name"            -> "Release",
        "runs-on"         -> $defaultRunner,
        "container"       -> $defaultJobContainer,
        "env"             -> $defaultJobEnv,
        "timeout-minutes" -> $timeConstraint,
        "steps"           -> {
            normalizeStep[ "Checkout"             ],
            normalizeStep[ "BuildPaclet"          ],
            normalizeStep[ "UploadBuildArtifacts" ],
            normalizeStep[ "CreateRelease"        ],
            normalizeStep[ "UploadRelease"        ]
        }
    |>;

normalizeJob[ "submit" | "submitpaclet" | "publish" | "publishpaclet" ] :=
    "Submit" -> <|
        "name"            -> "Submit",
        "runs-on"         -> $defaultRunner,
        "container"       -> $defaultJobContainer,
        "env"             -> $defaultJobEnv,
        "timeout-minutes" -> $timeConstraint,
        "steps"           -> {
            normalizeStep[ "Checkout"             ],
            normalizeStep[ "SubmitPaclet"         ],
            normalizeStep[ "UploadBuildArtifacts" ]
        }
    |>;

normalizeJob[ "test" | "testpaclet" ] :=
    "Test" -> <|
        "name"            -> "Test",
        "runs-on"         -> $defaultRunner,
        "container"       -> $defaultJobContainer,
        "env"             -> $defaultJobEnv,
        "timeout-minutes" -> $timeConstraint,
        "steps"           -> {
            normalizeStep[ "Checkout"             ],
            normalizeStep[ "TestPaclet"           ],
            normalizeStep[ "UploadBuildArtifacts" ]
        }
    |>;

normalizeJob[ "compile" ] := normalizeJob @ { "compile" };
normalizeJob[ { "compile", config___ } ] := normalizeCompilationJobs @ config;

normalizeJob[ job_WorkflowJob? workflowJobQ ] :=
    job[ "Name" ] -> job[ "Data" ];

normalizeJob[ { job_String, rest___ } ] :=
    With[ { wfj = Quiet @ catch @ WorkflowJob[ job, rest ] },
        (wfj[ "Name" ] -> wfj[ "Data" ]) /; workflowJobQ @ wfj
    ];

normalizeJob[ { job_String, rest___ } ] :=
    With[ { lc = StringDelete[ ToLowerCase @ job, "-" | "_" ] },
        normalizeJob @ { lc, rest } /; lc =!= job
    ];

normalizeJob[ job_String ] :=
    With[ { lc = StringDelete[ ToLowerCase @ job, "-" | "_" ] },
        normalizeJob @ lc /; lc =!= job
    ];

normalizeJob[ as: KeyValuePattern[ "id"|"ID" -> id_ ] ] :=
    normalizeForYAML[ "jobs", id -> as ];

normalizeJob[ as: KeyValuePattern[ "name"|"Name" -> name_String ] ] :=
    normalizeForYAML[ "jobs", name -> as ];

normalizeJob // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*normalizeCompilationJobs*)
normalizeCompilationJobs[ rules: (_Rule|_RuleDelayed)... ] :=
    normalizeCompilationJobs @ <| rules |>;

normalizeCompilationJobs[ jobs_List ] := normalizeCompilationJob /@ jobs;

normalizeCompilationJobs[ as_Association? AssociationQ ] :=
    Module[ { tgts, jobs },
        tgts = Lookup[ as, "Platforms", $compilationTargets ];
        jobs = normalizeCompilationJob[ #, as ] & /@ tgts;
        Association[ jobs, buildCompiledPacletJob @ as ]
    ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*normalizeCompilationJob*)
normalizeCompilationJob[ as_Association? AssociationQ ] :=
    normalizeCompilationJob[ Lookup[ as, "SystemID" ], as ];

normalizeCompilationJob[ sys_String, as_Association ] :=
    normalizeCompilationJob[ sys, Lookup[ as, "Name", "Compile" ], as ];

normalizeCompilationJob[ sys_String, name_String, as_Association ] :=
    name<>"-"<>sys -> normalizeCompilationJob0[ sys, as ];

normalizeCompilationJob // catchUndefined;

normalizeCompilationJob0[ "Windows-x86-64", as_Association ] := <|
    "name"            -> "Compile (Windows-x86-64)",
    "runs-on"         -> "windows-latest",
    "env"             -> compilationEnv[ "Windows-x86-64" ],
    "timeout-minutes" -> $timeConstraint,
    "steps" -> {
        normalizeStep[ "Checkout" ],
        $windowsCacheRestoreStep,
        $windowsInstallWLStep,
        windowsCompileStep @ as,
        windowsUploadCompiledStep @ as
    }
|>;

normalizeCompilationJob0[ "MacOSX-x86-64", as_Association ] := <|
    "name"            -> "Compile (MacOSX-x86-64)",
    "runs-on"         -> "macos-latest",
    "env"             -> compilationEnv[ "MacOSX-x86-64" ],
    "timeout-minutes" -> $timeConstraint,
    "steps" -> {
        normalizeStep[ "Checkout" ],
        $macCacheRestoreStep,
        $macInstallWLStep,
        macCompileStep @ as,
        macUploadCompiledStep @ as
    }
|>;

normalizeCompilationJob0[ "Linux-x86-64", as_Association ] := <|
    "name"            -> "Compile (Linux-x86-64)",
    "runs-on"         -> "ubuntu-latest",
    "container"       -> $defaultJobContainer,
    "env"             -> compilationEnv[ "Linux-x86-64" ],
    "timeout-minutes" -> $timeConstraint,
    "steps" -> {
        normalizeStep[ "Checkout" ],
        linuxInstallBuildStep @ as,
        linuxCompileStep @ as,
        linuxUploadCompiledStep @ as
    }
|>;

normalizeCompilationJob0 // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*buildCompiledPacletJob*)
buildCompiledPacletJob[ as_Association ] :=
    "BuildPaclet" -> <|
        "name"            -> "Build Paclet",
        "runs-on"         -> "ubuntu-latest",
        "container"       -> $defaultJobContainer,
        "env"             -> $defaultJobEnv,
        "timeout-minutes" -> $timeConstraint,
        "needs"           -> {
            "Compile-Windows-x86-64",
            "Compile-MacOSX-x86-64",
            "Compile-Linux-x86-64"
        },
        "steps"           -> {
            normalizeStep[ "Checkout"                     ],
            normalizeStep[ "DownloadCompilationArtifacts" ],
            normalizeStep[ "BuildPaclet"                  ],
            normalizeStep[ "UploadBuildArtifacts"         ]
        }
    |>;

buildCompiledPacletJob // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Windows Compilation Parameters*)

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*$windowsCacheRestoreStep*)
$windowsCacheRestoreStep := <|
    "name" -> "RestoreCachedWolframEngine",
    "id"   -> "cache-restore-step",
    "uses" -> "actions/cache@v2",
    "env"  -> <|
        "WOLFRAM_SYSTEM_ID"                    -> "Windows-x86-64",
        "WOLFRAMSCRIPT_ENTITLEMENTID"          -> "${{ secrets.WOLFRAMSCRIPT_ENTITLEMENTID }}",
        "WOLFRAMENGINE_INSTALLATION_DIRECTORY" -> "'${{ runner.temp }}\\WolframEngine'",
        "WOLFRAMENGINE_CACHE_KEY"              -> "WolframEngine-A"
    |>,
    "with" -> <|
        "path" -> "${{ env.WOLFRAMENGINE_INSTALLATION_DIRECTORY }}",
        "key"  -> "wolframengine-${{ env.WOLFRAM_SYSTEM_ID }}-${{ env.WOLFRAMENGINE_CACHE_KEY }}"
    |>
|>;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*$windowsInstallWLStep*)
$windowsInstallWLStep := <|
    "name" -> "InstallWolframEngine",
    "if"   -> "steps.cache-restore-step.outputs.cache-hit != 'true'",
    "env"  -> <|
        "WOLFRAM_SYSTEM_ID"                      -> "Windows-x86-64",
        "WOLFRAMSCRIPT_ENTITLEMENTID"            -> "${{ secrets.WOLFRAMSCRIPT_ENTITLEMENTID }}",
        "WOLFRAMENGINE_INSTALL_MSI_DOWNLOAD_URL" -> "https://files.wolframcdn.com/packages/winget/13.0.0.0/WolframEngine_13.0.0_WIN.msi",
        "WOLFRAMENGINE_INSTALLATION_DIRECTORY"   -> "'${{ runner.temp }}\\WolframEngine'",
        "WOLFRAMENGINE_INSTALL_MSI_PATH"         -> "'${{ runner.temp }}\\WolframEngine-Install.msi'",
        "WOLFRAMENGINE_INSTALL_LOG_PATH"         -> "'${{ runner.temp }}\\WolframEngine-Install.log'"
    |>,
    "run" -> $windowsInstallWLString
|>;

$windowsInstallWLString = "\
echo 'Downloading Wolfram Engine installer...'
$msiFile = '${{ env.WOLFRAMENGINE_INSTALL_MSI_PATH }}'
$logFile = '${{ env.WOLFRAMENGINE_INSTALL_LOG_PATH }}'
Import-Module BitsTransfer
Start-BitsTransfer '${{ env.WOLFRAMENGINE_INSTALL_MSI_DOWNLOAD_URL }}' $msiFile
echo 'Downloaded Wolfram Engine installer.'
$DataStamp = get-date -Format yyyyMMddTHHmmss
$MSIArguments = @(
    \"/i\"
    ('\"{0}\"' -f $msiFile)
    'INSTALLLOCATION=\"${{ env.WOLFRAMENGINE_INSTALLATION_DIRECTORY }}\"'
    \"/qn\"
    \"/norestart\"
    \"/L*v\"
    $logFile
)
echo 'Installing Wolfram Engine...'
Start-Process \"msiexec.exe\" -ArgumentList $MSIArguments -Wait -NoNewWindow
echo 'Installed Wolfram Engine.'
Set-Alias -Name wolframscript -Value wolfram";

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*windowsCompileStep*)
windowsCompileStep[ as_ ] := <|
    "name" -> "Compile libraries",
    "env" -> <|
        "WOLFRAMENGINE_INSTALLATION_DIRECTORY" -> "'${{ runner.temp }}\\WolframEngine'",
        "WOLFRAMINIT" -> "\"-pwfile !cloudlm.wolfram.com -entitlement ${{ secrets.WOLFRAMSCRIPT_ENTITLEMENTID }}\""
    |>,
    "run" -> "\
$env:Path += ';${{ env.WOLFRAMENGINE_INSTALLATION_DIRECTORY }}\\'
wolfram -script ${{ env.WOLFRAM_LIBRARY_BUILD_SCRIPT }}"
|>;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*windowsUploadCompiledStep*)
windowsUploadCompiledStep[ as_ ] := <|
    "name" -> "Archive compiled libraries",
    "uses" -> "actions/upload-artifact@v2",
    "with" -> <|
        "name" -> "${{ env.WOLFRAM_SYSTEM_ID }}",
        "path" -> "${{ env.WOLFRAM_LIBRARY_BUILD_OUTPUT }}/${{ env.WOLFRAM_SYSTEM_ID }}"
    |>
|>;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Mac Compilation Parameters*)

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*$macCacheRestoreStep*)
$macCacheRestoreStep := <|
    "name" -> "RestoreCachedWolframEngine",
    "id"   -> "cache-restore-step",
    "uses" -> "actions/cache@v2",
    "with" -> <|
        "path" -> "${{ env.WOLFRAMENGINE_INSTALLATION_DIRECTORY }}",
        "key"  -> "wolframengine-${{ env.WOLFRAM_SYSTEM_ID }}-${{ env.WOLFRAMENGINE_CACHE_KEY }}"
    |>
|>;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*$macInstallWLStep*)
$macInstallWLStep := <|
    "name" -> "InstallWolframEngine",
    "if"   -> "steps.cache-restore-step.outputs.cache-hit != 'true'",
    "run"  -> $macInstallWLString
|>;

$macInstallWLString = "\
echo 'Installing Wolfram Engine...'
brew install --cask wolfram-engine
echo 'Installed Wolfram Engine.'";

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*macCompileStep*)
macCompileStep[ as_ ] := <|
    "name" -> "Compile libraries",
    "env" -> <|
        "WOLFRAMENGINE_EXECUTABLES_DIRECTORY" -> "\"${{ env.WOLFRAMENGINE_INSTALLATION_DIRECTORY }}/Contents/Resources/Wolfram Player.app/Contents/MacOS\"",
        "WOLFRAMSCRIPT_KERNELPATH"            -> "\"${{ env.WOLFRAMENGINE_INSTALLATION_DIRECTORY }}/Contents/MacOS/WolframKernel\""
    |>,
    "run" -> "\
export PATH=\"${{ env.WOLFRAMENGINE_EXECUTABLES_DIRECTORY }}:$PATH\"
wolframscript -debug -verbose -script ${{ env.WOLFRAM_LIBRARY_BUILD_SCRIPT }}"
|>;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*macUploadCompiledStep*)
macUploadCompiledStep[ as_ ] := <|
    "name" -> "Archive compiled libraries",
    "uses" -> "actions/upload-artifact@v2",
    "with" -> <|
        "name" -> "${{ env.WOLFRAM_SYSTEM_ID }}",
        "path" -> "${{ env.WOLFRAM_LIBRARY_BUILD_OUTPUT }}/${{ env.WOLFRAM_SYSTEM_ID }}"
    |>
|>;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Linux Compilation Parameters*)

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*$linuxCacheRestoreStep*)
$linuxCacheRestoreStep := <|
    "name" -> "RestoreCachedWolframEngine",
    "id"   -> "cache-restore-step",
    "run"  -> "echo \"::notice::RestoreCachedWolframEngine skipped (WolframEngine already installed)\""
|>;

$linuxInstallWLStep := <|
    "name" -> "InstallWolframEngine",
    "id"   -> "install-wl-step",
    "run"  -> "echo \"::notice::InstallWolframEngine skipped (WolframEngine already installed)\""
|>;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*linuxInstallBuildStep*)
linuxInstallBuildStep[ as_ ] := <|
    "name" -> "Install build tools",
    "run" -> "\
apt-get -y update
apt-get -y install build-essential"
|>;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*linuxCompileStep*)
linuxCompileStep[ as_ ] := <|
    "name" -> "Compile libraries",
    "run"  -> "wolframscript -script ${{ env.WOLFRAM_LIBRARY_BUILD_SCRIPT }}"
|>;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*linuxUploadCompiledStep*)
linuxUploadCompiledStep[ as_ ] := <|
    "name" -> "Archive compiled libraries",
    "uses" -> "actions/upload-artifact@v2",
    "with" -> <|
        "name" -> "${{ env.WOLFRAM_SYSTEM_ID }}",
        "path" -> "${{ env.WOLFRAM_LIBRARY_BUILD_OUTPUT }}/${{ env.WOLFRAM_SYSTEM_ID }}"
    |>
|>;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*compilationEnv*)
compilationEnv[ "Windows-x86-64" ] := <|
    "WOLFRAMSCRIPT_ENTITLEMENTID"             -> "${{ secrets.WOLFRAMSCRIPT_ENTITLEMENTID }}",
    "WOLFRAM_LIBRARY_BUILD_SCRIPT"            -> "./Scripts/Compile.wls",
    "WOLFRAM_LIBRARY_BUILD_OUTPUT"            -> "LibraryResources/",
    "WOLFRAM_SYSTEM_ID"                       -> "Windows-x86-64",
    "WOLFRAMENGINE_INSTALL_MSI_DOWNLOAD_URL"  -> "https://files.wolframcdn.com/packages/winget/13.0.0.0/WolframEngine_13.0.0_WIN.msi",
    "WOLFRAMENGINE_CACHE_KEY"                 -> "WolframEngine-A"
|>;

compilationEnv[ "MacOSX-x86-64" ] := <|
    "WOLFRAMSCRIPT_ENTITLEMENTID"          -> "${{ secrets.WOLFRAMSCRIPT_ENTITLEMENTID }}",
    "WOLFRAM_LIBRARY_BUILD_SCRIPT"         -> "./Scripts/Compile.wls",
    "WOLFRAM_LIBRARY_BUILD_OUTPUT"         -> "LibraryResources/",
    "WOLFRAM_SYSTEM_ID"                    -> "MacOSX-x86-64",
    "WOLFRAMENGINE_CACHE_KEY"              -> "WolframEngine-A",
    "WOLFRAMENGINE_INSTALLATION_DIRECTORY" -> "\"/Applications/Wolfram Engine.app\""
|>;

compilationEnv[ "Linux-x86-64" ] := <|
    "WOLFRAMSCRIPT_ENTITLEMENTID"  -> "${{ secrets.WOLFRAMSCRIPT_ENTITLEMENTID }}",
    "WOLFRAM_LIBRARY_BUILD_SCRIPT" -> "./Scripts/Compile.wls",
    "WOLFRAM_LIBRARY_BUILD_OUTPUT" -> "LibraryResources/",
    "WOLFRAM_SYSTEM_ID"            -> "Linux-x86-64"
|>;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*$compilationTargets*)
$compilationTargets = { "Windows-x86-64", "MacOSX-x86-64", "Linux-x86-64" };

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*$defaultJobContainer*)
$defaultJobContainer :=
    If[ $defaultOS === "Linux-x86-64",
        <|
            "image"   -> "wolframresearch/wolframengine:latest",
            "options" -> "--user root"
        |>,
        $noValue
    ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*$defaultJobEnv*)
$defaultJobEnv /; $defaultOS === "MacOSX-x86-64" := takeEnvStrings @ <|
    "WOLFRAM_SYSTEM_ID"                    -> "MacOSX-x86-64",
    "WOLFRAMSCRIPT_ENTITLEMENTID"          -> "${{ secrets.WOLFRAMSCRIPT_ENTITLEMENTID }}",
    "RESOURCE_PUBLISHER_TOKEN"             -> $publisherToken,
    "WOLFRAMENGINE_CACHE_KEY"              -> "WolframEngine-A",
    "WOLFRAMENGINE_INSTALLATION_DIRECTORY" -> "\"/Applications/Wolfram Engine.app\""
|>;

$defaultJobEnv /; $defaultOS === "Windows-x86-64" := takeEnvStrings @ <|
    "WOLFRAM_SYSTEM_ID"                      -> "Windows-x86-64",
    "WOLFRAMSCRIPT_ENTITLEMENTID"            -> "${{ secrets.WOLFRAMSCRIPT_ENTITLEMENTID }}",
    "RESOURCE_PUBLISHER_TOKEN"               -> $publisherToken,
    "WOLFRAMENGINE_INSTALL_MSI_DOWNLOAD_URL" -> "https://files.wolframcdn.com/packages/winget/13.0.0.0/WolframEngine_13.0.0_WIN.msi",
    "WOLFRAMENGINE_CACHE_KEY"                -> "WolframEngine-A"
|>;

$defaultJobEnv /; True := takeEnvStrings @ <|
    "WOLFRAM_SYSTEM_ID"           -> "Linux-x86-64",
    "WOLFRAMSCRIPT_ENTITLEMENTID" -> "${{ secrets.WOLFRAMSCRIPT_ENTITLEMENTID }}",
    "RESOURCE_PUBLISHER_TOKEN"    -> $publisherToken
|>;

takeEnvStrings[ as_ ] :=
    Select[
        ReplaceAll[
            as,
            GitHubSecret[ sec_String? StringQ ] :>
                "${{ secrets." <> sec <> " }}"
        ],
        StringQ
    ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*$latestPacletCICDVersion*)
$latestPacletCICDVersion := getPacletCICDReleaseVersion[ ];

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*getPacletCICDReleaseVersion*)
getPacletCICDReleaseVersion[ ] :=
    getPacletCICDReleaseVersion @ $thisPacletVersion;

getPacletCICDReleaseVersion[ ver_String ] :=
    getPacletCICDReleaseVersion[ $thisRepository, ver ];

getPacletCICDReleaseVersion[ repo_String, ver_String ] := Enclose[
    Module[ { data, tags, sel },
        data = ghAPI[ "repos", repo, "releases" ];
        data = ConfirmMatch[ data, { __Association } ];
        tags = ConfirmMatch[ Lookup[ data, "tag_name" ], { __String } ];
        sel  = ConfirmBy[ chooseMatchingTagVersion[ tags, "v"<>ver ], StringQ ];
        getPacletCICDReleaseVersion[ repo, ver ] = sel
    ],
    "latest" &
];

getPacletCICDReleaseVersion // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*chooseMatchingTagVersion*)
chooseMatchingTagVersion[ { ___, ver_, ___ }, ver_ ] :=
    StringDelete[ ver, StartOfString~~"v" ];

chooseMatchingTagVersion[ tags_List, tag_String ] :=
    Module[ { sorted, sel },
        sorted = Sort[ Append[ tags, tag ], versionOrder ];
        sel =
            Replace[
                sorted,
                {
                    { ___, v_, tag, ___ } :> v,
                    { ___, tag, v_, ___ } :> v
                }
            ];
        StringDelete[ sel, StartOfString~~"v" ] /; StringQ @ sel
    ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*normalizeSteps*)
normalizeSteps[ step_String ] := normalizeSteps @ { step };
normalizeSteps[ steps_List ] := normalizeStep /@ steps;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*normalizeStep*)
normalizeStep[ ___, "checkout"|"checkoutcode" ] := <|
    "name" -> "Checkout",
    "id"   -> "checkout-code-step",
    "uses" -> "actions/checkout@v2"
|>;

normalizeStep[ ___, "check"|"checkpaclet" ] := <|
    "name" -> "Check",
    "id"   -> "check-paclet-step",
    "uses" -> normalizeActionName @ $checkAction,
    "with" -> <|
        "target"               -> $actionTarget,
        "paclet_cicd_version"  -> $latestPacletCICDVersion,
        "definition_notebook"  -> $defNotebookPath,
        "resource_system_base" -> $resSystemBase
    |>
|>;

normalizeStep[ ___, "build"|"buildpaclet" ] := <|
    "name" -> "Build",
    "id"   -> "build-paclet-step",
    "uses" -> normalizeActionName @ $buildAction,
    "with" -> <|
        "target"               -> $actionTarget,
        "paclet_cicd_version"  -> $latestPacletCICDVersion,
        "definition_notebook"  -> $defNotebookPath,
        "resource_system_base" -> $resSystemBase
    |>
|>;

normalizeStep[ ___, "test"|"testpaclet" ] := <|
    "name" -> "Test",
    "id"   -> "test-paclet-step",
    "uses" -> normalizeActionName @ $testAction,
    "with" -> <|
        "target"               -> $actionTarget,
        "paclet_cicd_version"  -> $latestPacletCICDVersion,
        "definition_notebook"  -> $defNotebookPath,
        "resource_system_base" -> $resSystemBase
    |>
|>;

normalizeStep[ ___, "submit"|"submitpaclet"|"publish"|"publishpaclet" ] := <|
    "name" -> "Submit",
    "id"   -> "submit-paclet-step",
    "uses" -> normalizeActionName @ $submitAction,
    "with" -> <|
        "paclet_cicd_version"  -> $latestPacletCICDVersion,
        "definition_notebook"  -> $defNotebookPath,
        "resource_system_base" -> $resSystemBase
    |>
|>;

normalizeStep[ ___, "upload" ] := <|
    "name" -> "Upload",
    "id"   -> "upload-artifacts-step",
    "uses" -> "actions/upload-artifact@v2",
    "with" -> <| "path" -> ".", "if-no-files-found" -> "error" |>
|>;

normalizeStep[ ___, "download" ] := <|
    "name" -> "Download",
    "id"   -> "download-artifacts-step",
    "uses" -> "actions/download-artifact@v2",
    "with" -> <| "path" -> "." |>
|>;

normalizeStep[
    ___,
    Alternatives[
        "uploadbuildartifacts",
        "uploadartifacts",
        "uploadbuildartifact",
        "uploadartifact",
        "uploadbuild"
    ]
] := <|
    "name" -> "UploadArtifact",
    "id"   -> "upload-build-artifacts-step",
    "uses" -> "actions/upload-artifact@v2",
    "with" -> <|
        "path"              -> "${{ env.PACLET_BUILD_DIR }}",
        "if-no-files-found" -> "ignore"
    |>
|>;

normalizeStep[ ___, "createrelease" ] := <|
    "name" -> "CreateRelease",
    "id"   -> "create-release-step",
    "uses" -> "actions/create-release@v1",
    "env"  -> <| "GITHUB_TOKEN" -> "${{ secrets.GITHUB_TOKEN }}" |>,
    "with" -> <|
        "tag_name"     -> "${{ env.PACLET_RELEASE_TAG }}",
        "release_name" -> "Release ${{ env.PACLET_RELEASE_TAG }}",
        "draft"        -> False,
        "prerelease"   -> False
    |>
|>;

normalizeStep[ ___, "uploadrelease"|"uploadreleaseasset" ] := <|
    "name" -> "UploadRelease",
    "id"   -> "upload-release-asset-step",
    "uses" -> "actions/upload-release-asset@v1",
    "env"  -> <| "GITHUB_TOKEN" -> "${{ secrets.GITHUB_TOKEN }}" |>,
    "with" -> <|
        "upload_url" -> "${{ steps.create-release-step.outputs.upload_url }}",
        "asset_path" -> "${{ env.PACLET_PATH }}",
        "asset_name" -> "${{ env.PACLET_FILE }}",
        "asset_content_type" -> "application/zip"
    |>
|>;

normalizeStep[ ___, "downloadcompilationartifacts" ] := <|
    "name" -> "DownloadCompilationArtifacts",
    "id"   -> "download-compilation-artifacts-step",
    "uses" -> "actions/download-artifact@v2",
    "with" -> <| "path" -> "LibraryResources" |>
|>;

normalizeStep[ ___, "restorecachedwolframengine" ] :=
    Switch[ $defaultOS,
            "Windows-x86-64", $windowsCacheRestoreStep,
            "MacOSX-x86-64" , $macCacheRestoreStep,
            _               , $linuxCacheRestoreStep
    ];

normalizeStep[ ___, "installwolframengine" ] :=
    Switch[ $defaultOS,
            "Windows-x86-64", $windowsInstallWLStep,
            "MacOSX-x86-64" , $macInstallWLStep,
            _               , $linuxInstallWLStep
    ];

normalizeStep[ keys___, step_String ] :=
    With[ { lc = StringDelete[ ToLowerCase @ step, "-" | "_" ] },
        normalizeStep[ keys, lc ] /; lc =!= step
    ];

normalizeStep[ keys___, step_String ] :=
    throwError[
        "Invalid step name \"`1`\" (`2`)",
        step,
        keySeqDocLink @ keys
    ];

normalizeStep[ as_Association ] :=
    normalizeForYAML[
        "jobs",
        Automatic,
        "steps",
        Automatic,
        KeyMap[ toLowerCase, as ]
    ];

normalizeStep[ keys__, as_Association ] :=
    With[ { lc = KeyMap[ toLowerCase, as ] },
        If[ lc =!= as,
            normalizeForYAML[ keys, lc ],
            lc
        ]
    ];

normalizeStep[ keys___, step_WorkflowStep? workflowStepQ ] :=
    step[ "Data" ];

normalizeStep[ keys___, code: _File|_WorkflowEvaluate ] :=
    normalizeStep[ keys, WorkflowStep @ code ];

keySeqDocLink[ keys___ ] :=
    Module[ { str, url },
        str = keySeqString @ keys;

        url =
            URLBuild @ <|
                "Scheme" -> "https",
                "Domain" -> "docs.github.com",
                "Path" -> {
                    "",
                    "en",
                    "actions",
                    "using-workflows",
                    "workflow-syntax-for-github-actions"
                },
                "Fragment" ->
                    StringDelete[ str, Except[ LetterCharacter | "_" | "-" ] ]
            |>;

        ToString[ Hyperlink[ str, url ], StandardForm ]
    ];

keySeqString[ "jobs", None|Automatic, keys___ ] :=
    keySeqString[ "jobs", "<job_id>", keys ];

keySeqString[ a___, key_String, int_Integer, b___ ] :=
    keySeqString[ a, key <> "[" <> ToString[ int ] <> "]", b ];

keySeqString[ a___ ] := StringRiffle[ { a }, "." ];

toLowerCase[ str_String ] := ToLowerCase @ str;
toLowerCase[ other_ ] := other;

(*TODO: check against schema *)

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*$workflowSchema*)
$workflowSchema := $workflowSchema =
    URLExecute[
        "https://json.schemastore.org/github-workflow.json",
        "RawJSON"
    ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*YAML Export*)

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*toYAMLString*)
toYAMLString[ expr0_ ] := Enclose[ \
    Module[ { expr, string, lines },
        expr   = <| "YAML" -> expr0 |>;
        string = ConfirmBy[ toYAMLString1 @ expr, StringQ ];
        lines  = Rest @ StringSplit[ string, "\n" ];
        StringRiffle[ StringDelete[ lines, StartOfString ~~ "  " ], "\n" ]
    ],
    throwMessageFailure[ WorkflowExport::yamlfail, expr0 ] &
];

toYAMLString1[ expr_ ] :=
    Enclose[
        StringReplace[
            ConfirmBy[ toYAMLString0 @ expr, StringQ ],
            StringExpression[
                a: ("\n" ~~ WhitespaceCharacter.. ~~ "-"),
                WhitespaceCharacter..,
                b: Except[ WhitespaceCharacter ]
            ] :>
                a <> " " <> b
        ],
        throwMessageFailure[ WorkflowExport::yamlfail, expr ] &
    ];

toYAMLString1 // catchUndefined;


toYAMLString0[ as_Association ] :=
    stringJoin @ Riffle[
        KeyValueMap[ toYAMLString0, DeleteCases[ as, None ] ],
        "\n"
    ];

toYAMLString0[ key_String, as_Association ] :=
    stringJoin[ toYAMLKey @ key, "\n", descend @ toYAMLString0 @ as ];

toYAMLString0[ key_String, vals_List ] :=
    Module[ { strs, str },
        strs = toYAMLString0 /@ vals;
        str  = StringRiffle[ strs, ", " ];
        stringJoin[ toYAMLKey @ key, " [", str, "]" ] /;
            FreeQ[ strs, toYAMLString0 ] && StringLength @ str < 80 - $depth*2
    ];

toYAMLString0[ key_String, vals_List ] :=
    stringJoin[
        toYAMLKey @ key,
        Map[
            stringJoin[ "\n", $indent, "- ", descend @ toYAMLString0 @ # ] &,
            vals
        ]
    ];

toYAMLString0[ key_String, Null ] := $indent <> key <> ":";

toYAMLString0[ key_String, val_ ] :=
    stringJoin[ $indent, key, ": ", toYAMLString0 @ val ];

toYAMLString0[ list_List ] :=
    descend @ stringJoin @ Riffle[
        stringJoin[ "- ", toYAMLString0 @ # ] & /@ list,
        "\n"
    ];

toYAMLString0[ val_String  ] := checkMultilineString @ val;
toYAMLString0[ int_Integer ] := ToString @ int;
toYAMLString0[ r_Real      ] := TextString @ r;
toYAMLString0[ True        ] := "true";
toYAMLString0[ False       ] := "false";
toYAMLString0[ Null        ] := "null";

toYAMLString0[ other_ ] :=
    throwMessageFailure[ WorkflowExport::ymlconv, other ];

toYAMLString0 // catchUndefined;


(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*checkMultilineString*)
checkMultilineString[ str_String ] /; StringFreeQ[ str, "\n" ] := str;
checkMultilineString[ str_String ] :=
    Module[ { lines },
        lines = StringSplit[ str, "\r\n" | "\n" ];
        StringJoin[
            "|",
            descend[ (stringJoin[ "\n", $indent, #1 ] &) /@ lines ]
        ]
    ];

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*toYAMLKey*)
toYAMLKey[ key_ ] := stringJoin[ $indent, yamlKeyString @ key, ": " ];
toYAMLKey // catchUndefined;

yamlKeyString[ key_String ] := StringReplace[ key, WhitespaceCharacter -> "-" ];
yamlKeyString[ other_ ] := yamlKeyString @ ToString @ other;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*Indentation*)
$depth   = 0;
$indent := StringJoin @ ConstantArray[ "  ", $depth ];

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*descend*)
descend // Attributes = { HoldFirst };
descend[ eval_ ] := Internal`InheritedBlock[ { $depth }, $depth += 1; eval ];

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*stringJoin*)
stringJoin[ str_String ] := str;
stringJoin[ a___, b_String, c__String, d___ ] := stringJoin[ a, b <> c, d ];
stringJoin[ a___, { b___ }, c___ ] := stringJoin[ a, b, c ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Misc Utilities*)

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*withWorkflowOptions*)
withWorkflowOptions[ s_, opts___ ] :=
    Function[
        eval,
        catchTop @ Block[
            { $wfHead = s },
            withWorkflowOptions0[ s, { opts }, eval ]
        ],
        HoldAllComplete
    ];

withWorkflowOptions // catchUndefined;

withWorkflowOptions0 // Attributes = { HoldRest };

withWorkflowOptions0[ s_, { opts___ }, eval_ ] :=
    Block[
        {
            $buildAction     = wfOpt[ s, opts, "BuildPacletAction"           ],
            $checkAction     = wfOpt[ s, opts, "CheckPacletAction"           ],
            $testAction      = wfOpt[ s, opts, "TestPacletAction"            ],
            $defaultBranch   = wfOpt[ s, opts, "DefaultBranch"               ],
            $timeConstraint  = wfOpt[ s, opts, "TimeConstraint"              ],
            $actionTarget    = wfOpt[ s, opts, "Target"                      ],
            $defNotebookPath = wfOpt[ s, opts, "DefinitionNotebookPath"      ],
            $defaultOS       = wfOpt[ s, opts, "OperatingSystem"             ],
            $defaultRunner   = wfOpt[ s, opts, "OperatingSystem" -> "Runner" ],
            $resSystemBase   = wfOpt[ s, opts, "ResourceSystemBase"          ],
            $publisherToken  = wfOpt[ s, opts, "PublisherToken"              ]
        },
        eval
    ];

withWorkflowOptions0 // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*wfOptFail*)
wfOptFail // Attributes = { HoldFirst };

wfOptFail[ MessageName[ _, tag_String ], args___ ] :=
    wfOptFail[ tag, args ];

wfOptFail[ tag_String, args___ ] :=
    With[ { s = $wfHead },
        If[ StringQ @ MessageName[ s, tag ],
            throwMessageFailure[ MessageName[ s, tag ], args ],
            throwError[ "Invalid option specification.", args ]
        ]
    ];

(* TODO: define these message tags for all workflow symbols:
    InvalidResourceSystemBase
    InvalidDefinitionNotebookPath
*)

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*wfOpt*)
wfOpt[ sym_, opts___, opt_Symbol ] :=
    wfOpt[ sym, opts, SymbolName @ opt ];

wfOpt[ sym_, opts___, name_String ] :=
    wfOptFunc[ name ] @ OptionValue[ sym, { opts }, name ];

wfOpt[ sym_, opts___, from_String -> name_String ] :=
    wfOptFunc[ name ] @ OptionValue[ sym, { opts }, from ];

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*wfOptFunc*)
wfOptFunc[ "BuildPacletAction"      ] := toBuildPacletAction;
wfOptFunc[ "CheckPacletAction"      ] := toCheckPacletAction;
wfOptFunc[ "DefaultBranch"          ] := toDefaultBranch;
wfOptFunc[ "DefinitionNotebookPath" ] := toDefinitionNotebookPath;
wfOptFunc[ "OperatingSystem"        ] := toDefaultOS;
wfOptFunc[ "PublisherToken"         ] := toPublisherToken;
wfOptFunc[ "ResourceSystemBase"     ] := toResourceSystemBase;
wfOptFunc[ "Runner"                 ] := toDefaultRunner;
wfOptFunc[ "Target"                 ] := toActionTarget;
wfOptFunc[ "TestPacletAction"       ] := toTestPacletAction;
wfOptFunc[ "TimeConstraint"         ] := toTimeConstraint;

wfOptFunc[ other_   ] := throwError[ "`1` is not a valid option."   , other ];
wfOptFunc[ other___ ] := throwError[ "An unexpected error occurred.", other ];

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*toResourceSystemBase*)
toResourceSystemBase[ Automatic ] := toResourceSystemBase @ $ResourceSystemBase;
toResourceSystemBase[ url_String ] := url;
toResourceSystemBase[ URL[ url_ ] ] := toResourceSystemBase @ url;
toResourceSystemBase[ HoldPattern @ $ResourceSystemBase ] := $resSystemBase;

toResourceSystemBase[ other_ ] :=
    wfOptFail[ "InvalidResourceSystemBase", other ];

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*toPublisherToken*)
toPublisherToken[ Automatic       ] := $publisherToken;
toPublisherToken[ None            ] := $noValue;
toPublisherToken[ s_GitHubSecret  ] := s;
toPublisherToken[ _? actualTokenQ ] := wfOptFail[ "TokenSecret" ];
toPublisherToken[ e_              ] := wfOptFail[ "InvalidPublisherToken", e ];

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*actualTokenQ*)
actualTokenQ[ expr_ ] := (
    LoadSubPackage[ "PublisherTokens" ];
    ! FreeQ[ Hold @ expr,
             (_String|_ByteArray|_PublisherTokenObject)? actualTokenQ0
      ]
);

actualTokenQ // catchUndefined;

actualTokenQ0[ str_String ] := StringContainsQ[ str, s__ /; tokenStringQ @ s ];
actualTokenQ0[ bytes_ByteArray ] := actualTokenQ0 @ ByteArrayToString @ bytes;
actualTokenQ0[ token: HoldPattern[ _PublisherTokenObject ] ] :=
    publisherTokenObjectQ @ Evaluate @ token;

actualTokenQ0[ ___ ] := False;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*toDefinitionNotebookPath*)
toDefinitionNotebookPath[ Automatic ] := $defNotebookPath;
toDefinitionNotebookPath[ path_String ] := toDefNBLocation @ path;
toDefinitionNotebookPath[ File[ path_ ] ] := toDefinitionNotebookPath @ path;

toDefinitionNotebookPath[ other_ ] :=
    wfOptFail[ "InvalidDefinitionNotebookPath", other ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*withOS*)
withOS // Attributes = { HoldRest };

withOS[ os_, eval_ ] :=
    Block[
        {
            $defaultOS     = toDefaultOS @ os,
            $defaultRunner = toDefaultRunner @ os
        },
        eval
    ];

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*toDefaultOS*)
toDefaultOS[ Automatic                      ] := $defaultOS;
toDefaultOS[ "Linux"|"Unix"|"Linux-x86-64"  ] := "Linux-x86-64";
toDefaultOS[ "Windows"|"Windows-x86-64"     ] := "Windows-x86-64";
toDefaultOS[ "Mac"|"MacOSX"|"MacOSX-x86-64" ] := "MacOSX-x86-64";

toDefaultOS[ s_String? StringQ ] :=
    Module[ { startsWith },
        startsWith = StringStartsQ[ s, #~~WordBoundary, IgnoreCase -> True ] &;
        Which[ startsWith[ "Windows"        ], "Windows-x86-64",
               startsWith[ "Ubuntu"|"Linux" ], "Linux-x86-64",
               startsWith[ "Mac"|"MacOS"    ], "MacOSX-x86-64",
               True,
               throwError[
                   "`1` is not a valid OperatingSystem specification",
                   s
               ]
        ]
    ];

toDefaultOS[ other_ ] :=
    throwError[ "`1` is not a valid OperatingSystem specification", other ];

toDefaultOS // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*toDefaultRunner*)
toDefaultRunner[ Automatic                      ] := $defaultRunner;
toDefaultRunner[ "Linux"|"Unix"|"Linux-x86-64"  ] := "ubuntu-latest";
toDefaultRunner[ "Windows"|"Windows-x86-64"     ] := "windows-latest";
toDefaultRunner[ "Mac"|"MacOSX"|"MacOSX-x86-64" ] := "macos-latest";

toDefaultRunner[ s_String? StringQ ] :=
    Module[ { startsWith },
        startsWith = StringStartsQ[ s, #~~WordBoundary, IgnoreCase -> True ] &;
        Which[ startsWith[ "Windows"        ], windowsRunner @ s,
               startsWith[ "Ubuntu"|"Linux" ], linuxRunner @ s,
               startsWith[ "Mac"|"MacOS"    ], macRunner @ s,
               True,
               throwError[
                   "`1` is not a valid runner specification",
                   s
               ]
        ]
    ];

toDefaultRunner[ other_ ] :=
    throwError[ "`1` is not a valid runner specification", other ];

toDefaultRunner // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsubsubsection::Closed:: *)
(*windowsRunner*)
windowsRunner[ spec_String ] :=
    Module[ { runner },
        runner = windowsRunner0 @@ StringSplit[ ToLowerCase @ spec, " " | "-" ];
        If[ StringQ @ runner,
            runner,
            throwError[ "`1` is not a valid runner specification", spec ]
        ]
    ];

windowsRunner // catchUndefined;

windowsRunner0[ "windows" ] := "windows-latest";
windowsRunner0[ "windows", "latest" ] := "windows-latest";
windowsRunner0[ "windows", "server", a___ ] := windowsRunner0[ "windows", a ];
windowsRunner0[ "windows", year_String ] := "windows-" <> year;

(* ::**********************************************************************:: *)
(* ::Subsubsubsection::Closed:: *)
(*linuxRunner*)
linuxRunner[ spec_String ] :=
    Module[ { runner },
        runner = linuxRunner0 @@ StringSplit[ ToLowerCase @ spec, " " | "-" ];
        If[ StringQ @ runner,
            runner,
            throwError[ "`1` is not a valid runner specification", spec ]
        ]
    ];

linuxRunner // catchUndefined;

linuxRunner0[ "linux"|"unix", a___ ] := linuxRunner0[ "ubuntu", a ];
linuxRunner0[ "ubuntu" ] := "ubuntu-latest";
linuxRunner0[ "ubuntu", "latest" ] := "ubuntu-latest";
linuxRunner0[ "ubuntu", version_String ] := "ubuntu-" <> version;

(* ::**********************************************************************:: *)
(* ::Subsubsubsection::Closed:: *)
(*macRunner*)
macRunner[ spec_String ] :=
    Module[ { runner },
        runner = macRunner0 @@ StringSplit[ ToLowerCase @ spec, " " | "-" ];
        If[ StringQ @ runner,
            runner,
            throwError[ "`1` is not a valid runner specification", spec ]
        ]
    ];

macRunner // catchUndefined;

macRunner0[ "mac"|"macosx", a___ ] := macRunner0[ "macos", a ];
macRunner0[ "macos" ] := "macos-latest";
macRunner0[ "macos", "latest" ] := "macos-latest";
macRunner0[ "macos", version_String ] := "macos-" <> version;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*postProcessYAML*)
postProcessYAML[ expr_ ] := envReduce @ DeleteCases[ expr, $noValue, Infinity ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*envReduce*)
envReduce[ env_Association, data_ ] :=
    ReplaceRepeated[
        Replace[
            data,
            a: KeyValuePattern[ "env" -> e_Association ] :>
                With[ { ec = Complement[ e, env ], ej = Join[ env, e ] },
                    envReduce[ ej, Insert[ a, "env" -> ec, Key[ "env" ] ] ]
                ],
            { 1, Infinity }
        ],
        a: KeyValuePattern[ "env" -> <| |> ] :>
            RuleCondition @ KeyDrop[ a, "env" ]
    ];

envReduce[ data_ ] :=
    ReplaceAll[
        data,
        a: KeyValuePattern[ "env" -> env_Association ] :>
            RuleCondition @ envReduce[ env, a ]
    ];

envReduce // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Footer*)
End[ ];
EndPackage[ ];