(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Header*)
BeginPackage[ "Wolfram`PacletCICD`" ];

CreateWorkflow;
GitHubSecret;

Begin[ "`Private`" ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*CreateWorkflow*)
CreateWorkflow::defbranch =
"Expected a string instead of `1` for the default branch.";

CreateWorkflow::wfname =
"`1` is not a recognized workflow name.";

CreateWorkflow::invspec =
"Invalid workflow specification: `1`";

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Options*)
CreateWorkflow // Options = {
    OverwriteTarget -> False,
    "DefaultBranch" -> "main"
};

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Main definition*)
CreateWorkflow[ pac_, spec_, opts: OptionsPattern[ ] ] :=
    catchTop @ Module[ { workflow },
        workflow = createWorkflow[ spec, OptionValue[ "DefaultBranch" ] ];
        exportWorkflow[ pac, workflow ]
    ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*createWorkflow*)
createWorkflow[ spec_, branch_String ] :=
    Block[ { $defaultBranch = branch },
        createWorkflow0 @ spec
    ];

createWorkflow[ spec_, branch: Except[ _String? StringQ ] ] :=
    throwMessageFailure[ CreateWorkflow::defbranch, branch ];

createWorkflow0[ name_String ] :=
    createWorkflow0 @ Lookup[
        $namedWorkflows,
        name,
        throwMessageFailure[ CreateWorkflow::wfname, name ]
    ];

createWorkflow0[ spec_Association ] :=
    Module[ { workflow },
        workflow = normalizeForYAML @ spec;
        If[ TrueQ @ validValueQ @ workflow,
            workflow,
            throwMessageFailure[ CreateWorkflow::invspec, spec ]
        ]
    ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*exportWorkflow*)
exportWorkflow[ pac_PacletObject, workflow_ ] :=
    Module[ { pacDir, wfDir, file },
        pacDir = pac[ "Location" ];
        wfDir  = ensureDirectory @ { pacDir, ".github", "workflows" };
        file   = FileNameJoin @ { wfDir, workflowFileName @ workflow };
        exportWorkflow[ pac, file, workflow ] (* TODO *)
    ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*workflowFileName*)
workflowFileName[ KeyValuePattern[ "name" -> name_String ] ] :=
    StringDelete[ name, Except[ LetterCharacter ] ] <> ".yml";

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*$namedWorkflows*)
$namedWorkflows := <|
    "Release" -> <|
        "Name" -> "Release Paclet",
        "On" -> <|
            "Push"             -> <| "Branches" -> { "release/*" } |>,
            "WorkflowDispatch" -> True
        |>,
        "Jobs" -> "Release"
    |>
    ,
    "Build" -> <|
        "Name" -> "Build Paclet",
        "On"   -> <|
            "Push"             -> <| "Branches" -> $defaultBranch |>,
            "PullRequest"      -> <| "Branches" -> $defaultBranch |>,
            "WorkflowDispatch" -> True
        |>,
        "Jobs" -> "Build"
    |>
    ,
    "Check" -> <|
        "Name" -> "Check Paclet",
        "On"   -> <|
            "Push"             -> <| "Branches" -> $defaultBranch |>,
            "PullRequest"      -> <| "Branches" -> $defaultBranch |>,
            "WorkflowDispatch" -> True
        |>,
        "Jobs" -> "Check"
    |>
|>;

$defaultBranch = "main";

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
normalizeForYAML[ keys___, as_Association ] :=
    AssociationMap[ normalizeForYAML[ keys, #1 ] &, as ];

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

normalizeForYAML[ "on", "PullRequest" -> pr_ ] :=
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

normalizeForYAML[ "jobs", name_, "Container" -> container_ ] :=
    normalizeForYAML[ "jobs", name, "container" -> container ];

normalizeForYAML[ "jobs", _, "container", "image"|"Image" -> str_String ] :=
    "image" -> str;

normalizeForYAML[ "jobs", name_, "container", "Options"|Options -> opts_ ] :=
    normalizeForYAML[ "jobs", name, "container", "options" -> opts ];

normalizeForYAML[
    "jobs",
    name_,
    "container",
    "options" -> opts: _String | { ___String }
] :=
    "options" -> Flatten @ { opts };

normalizeForYAML[ "jobs", name_, "Environment"|Environment, env_ ] :=
    normalizeForYAML[ "jobs", name, "env", env ];

normalizeForYAML[ "jobs", _, "env", key_String -> val_String ] :=
    key -> val;

normalizeForYAML[ "jobs", _, "steps"|"Steps" -> steps_ ] :=
    "steps" -> normalizeSteps @ steps;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*General*)
normalizeForYAML[ keys___, key_String -> as_Association ] :=
    key -> normalizeForYAML[ keys, key, as ];

normalizeForYAML[ keys___, key_Symbol -> value_ ] :=
    normalizeForYAML[ keys, SymbolName @ key -> value ];

normalizeForYAML[ keys___, key_String -> value_? validValueQ ] :=
    key -> value;

normalizeForYAML[ keys___, key_String :> Environment[ env_String ] ] :=
    normalizeForYAML[ keys, key -> "${{ env."<>env<>" }}" ];

normalizeForYAML[ keys___, key_String -> GitHubSecret[ sec_String ] ] :=
    normalizeForYAML[ keys, key -> "${{ secrets."<>sec<>" }}" ];

normalizeForYAML // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*normalizeJobs*)
normalizeJobs[ job_String ] := normalizeJobs @ { job };
normalizeJobs[ jobs_List  ] := Association[ normalizeJob /@ jobs ];
normalizeJobs[ jobs_Association? validValueQ ] := jobs; (* TODO: validate against schema *)
normalizeJobs // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*normalizeJob*)
normalizeJob[ "check" | "checkpaclet" ] :=
    "CheckPaclet" -> <|
        "name"      -> "Check Paclet",
        "id"        -> "check-paclet-job",
        "runs-on"   -> "ubuntu-latest",
        "container" -> $defaultJobContainer,
        "env"       -> $defaultJobEnv,
        "steps"     -> {
            normalizeStep[ "Checkout"    ],
            normalizeStep[ "CheckPaclet" ]
        }
    |>;

normalizeJob[ "build" | "buildpaclet" ] :=
    "BuildPaclet" -> <|
        "name"      -> "Build Paclet",
        "id"        -> "build-paclet-job",
        "runs-on"   -> "ubuntu-latest",
        "container" -> $defaultJobContainer,
        "env"       -> $defaultJobEnv,
        "steps"     -> {
            normalizeStep[ "Checkout"             ],
            normalizeStep[ "BuildPaclet"          ],
            normalizeStep[ "UploadBuildArtifacts" ]
        }
    |>;

normalizeJob[ "release" | "releasepaclet" ] :=
    "ReleasePaclet" -> <|
        "name"      -> "Release Paclet",
        "id"        -> "release-paclet-job",
        "runs-on"   -> "ubuntu-latest",
        "container" -> $defaultJobContainer,
        "env"       -> $defaultJobEnv,
        "steps"     -> {
            normalizeStep[ "Checkout"             ],
            normalizeStep[ "BuildPaclet"          ],
            normalizeStep[ "UploadBuildArtifacts" ],
            normalizeStep[ "CreateRelease"        ],
            normalizeStep[ "UploadRelease"        ]
        }
    |>;

normalizeJob[ job_String ] :=
    With[ { lc = StringDelete[ ToLowerCase @ job, "-" | "_" ] },
        normalizeJob @ lc /; lc =!= job
    ];

normalizeJob[ as: KeyValuePattern[ "id"|"ID" -> id_ ] ] :=
    id -> as;

normalizeJob // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*$defaultJobContainer*)
$defaultJobContainer = <|
    "image"   -> "wolframresearch/wolframengine:latest",
    "options" -> { " --user root" }
|>;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*name*)
$defaultJobEnv = <|
    "WOLFRAM_SYSTEM_ID"           -> "Linux-x86-64",
    "WOLFRAMSCRIPT_ENTITLEMENTID" -> "${{ secrets.WOLFRAMSCRIPT_ENTITLEMENTID }}"
|>;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*normalizeSteps*)
normalizeSteps[step_String] := normalizeSteps@{step};
normalizeSteps[steps_List] := Association[normalizeStep /@ steps];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*normalizeStep*)
normalizeStep[ "checkout" ] := <|
    "name" -> "Checkout Code",
    "id"   -> "checkout-code",
    "uses" -> "actions/checkout@v2"
|>;

normalizeStep[ "check" | "checkpaclet" ] := <|
    "name" -> "Check Paclet",
    "id"   -> "check-paclet-step",
    "uses" -> "rhennigan/check-paclet@latest"
|>;

normalizeStep[ "build" | "buildpaclet" ] := <|
    "name" -> "Build Paclet",
    "id"   -> "build-paclet-step",
    "uses" -> "rhennigan/build-paclet@latest"
|>;

normalizeStep[ "uploadbuildartifacts" | "uploadartifacts" | "uploadbuild" ] := <|
    "name" -> "Upload Build Artifacts",
    "id" -> "upload-build-artifacts-step",
    "uses" -> "actions/upload-artifact@v2",
    "with" -> <|
        "path"              -> "${{ env.BUILD_DIR }}",
        "if-no-files-found" -> "error"
    |>
|>;

normalizeStep[ "createrelease" ] := <|
    "name" -> "Create Release",
    "id" -> "create-release-step",
    "uses" -> "actions/create-release@v1",
    "env" -> <| "GITHUB_TOKEN" -> "${{ secrets.GITHUB_TOKEN }}" |>,
    "with" -> <|
        "tag_name"     -> "${{ env.RELEASE_TAG }}",
        "release_name" -> "Release ${{ env.RELEASE_TAG }}",
        "draft"        -> False,
        "prerelease"   -> False
    |>
|>;

normalizeStep[ "uploadrelease" | "uploadreleaseasset" ] := <|
    "name" -> "Upload Release Asset",
    "id" -> "upload-release-asset-step",
    "uses" -> "actions/upload-release-asset@v1",
    "env" -> <| "GITHUB_TOKEN" -> "${{ secrets.GITHUB_TOKEN }}" |>,
    "with" -> <|
        "upload_url" -> "${{ steps.create_release.outputs.upload_url }}",
        "asset_path" -> "${{ env.PACLET_PATH }}",
        "asset_name" -> "${{ env.PACLET_FILE }}",
        "asset_content_type" -> "application/zip"
    |>
|>;

normalizeStep[ step_String ] :=
    With[ { lc = StringDelete[ ToLowerCase @ step, "-" | "_" ] },
        normalizeStep @ lc /; lc =!= step
    ];

normalizeStep[ as_Association ] := as;

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
(*Package Footer*)
End[ ];
EndPackage[ ];