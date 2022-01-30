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

CreateWorkflow::invtimeout =
"Invalid TimeConstraint: `1`";

CreateWorkflow::yamlfail =
"Failed to export workflow to YAML format.";

CreateWorkflow::ymlconv =
"Unable to convert `1` to valid YAML.";

CreateWorkflow::export =
"Failed to export YAML to the file `1`.";

CreateWorkflow::entitlement = "`1`";

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Options*)
CreateWorkflow // Options = {
    "DefaultBranch" -> "main",
    OverwriteTarget -> False,
    TimeConstraint  -> Quantity[ 10, "Minutes" ]
};

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Main definition*)
CreateWorkflow[ pac_, spec_, opts: OptionsPattern[ ] ] :=
    catchTop @ Module[ { workflow },
        workflow = createWorkflow[
            spec,
            OptionValue[ "DefaultBranch" ],
            OptionValue[ TimeConstraint ]
        ];
        exportWorkflow[ pac, workflow ]
    ];

$defaultBranch         = "main";
$defaultTimeConstraint = 10;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*createWorkflow*)
createWorkflow[ spec_, branch_String, timeConstraint_ ] :=
    Block[
        {
            $defaultBranch = branch,
            $defaultTimeConstraint = toTimeConstraint @ timeConstraint
        },
        createWorkflow0 @ spec
    ];

createWorkflow[ spec_, branch: Except[ _String? StringQ ] ] :=
    throwMessageFailure[ CreateWorkflow::defbranch, branch ];

createWorkflow // catchUndefined;


createWorkflow0[ name_String ] :=
    createWorkflow0 @ Lookup[
        $namedWorkflows,
        toWorkflowName @ name,
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

createWorkflow0 // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*toTimeConstraint*)
toTimeConstraint[ t_? NumberQ ] := toTimeConstraint @ Quantity[ t, "Seconds" ];
toTimeConstraint[ Infinity    ] := toTimeConstraint @ Quantity[ 6, "Hours" ];

toTimeConstraint[ q_Quantity ] :=
    QuantityMagnitude @ UnitConvert[ q, "Minutes" ];

toTimeConstraint[ other_ ] :=
    throwMessageFailure[ CreateWorkflow::invtimeout, other ];

toTimeConstraint // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*exportWorkflow*)
exportWorkflow[ pac_PacletObject, workflow_ ] :=
    exportWorkflow[ pac[ "Location" ], workflow ];

exportWorkflow[ dir_? DirectoryQ, workflow_ ] :=
    Module[ { wfDir, file },
        wfDir  = toWorkFlowDir @ dir;
        file   = FileNameJoin @ { wfDir, workflowFileName @ workflow };
        exportWorkflow[ file, workflow ] (* TODO *)
    ];

exportWorkflow[ file_, workflow_ ] := Enclose[
    Module[ { yaml, exported },
        yaml = ConfirmBy[ toYAMLString @ workflow, StringQ ];
        exported = Export[ file, yaml, "String" ];
        If[ FileExistsQ @ exported,
            entitlementWarning[ ];
            File @ exported,
            throwMessageFailure[ CreateWorkflow::export, file ]
        ]
    ],
    throwMessageFailure[ CreateWorkflow::ymlconv ] &
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
        messageFailure[ CreateWorkflow::entitlement, $licenseWarningText ]
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
        "Wolfram",
        "PacletCICD",
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
                Put[ ver, file ]
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

normalizeForYAML[ "jobs", name_, "Environment"|Environment -> env_ ] :=
    normalizeForYAML[ "jobs", name, "env" -> env ];

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

normalizeForYAML[ keys___, "TimeConstraint" -> t_ ] :=
    normalizeForYAML[ keys, TimeConstraint -> t ];

normalizeForYAML[ keys___, TimeConstraint -> t_? NumberQ ] :=
    normalizeForYAML[ keys, TimeConstraint -> Quantity[ t, "Seconds" ] ];

normalizeForYAML[ keys___, TimeConstraint -> q_Quantity ] :=
    "timeout-minutes" -> QuantityMagnitude @ UnitConvert[ q, "Minutes" ];

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
        "name"            -> "Check Paclet",
        "runs-on"         -> "ubuntu-latest",
        "container"       -> $defaultJobContainer,
        "env"             -> $defaultJobEnv,
        "timeout-minutes" -> $defaultTimeConstraint,
        "steps"           -> {
            normalizeStep[ "Checkout"    ],
            normalizeStep[ "CheckPaclet" ]
        }
    |>;

normalizeJob[ "build" | "buildpaclet" ] :=
    "BuildPaclet" -> <|
        "name"            -> "Build Paclet",
        "runs-on"         -> "ubuntu-latest",
        "container"       -> $defaultJobContainer,
        "env"             -> $defaultJobEnv,
        "timeout-minutes" -> $defaultTimeConstraint,
        "steps"           -> {
            normalizeStep[ "Checkout"             ],
            normalizeStep[ "BuildPaclet"          ],
            normalizeStep[ "UploadBuildArtifacts" ]
        }
    |>;

normalizeJob[ "release" | "releasepaclet" ] :=
    "ReleasePaclet" -> <|
        "name"            -> "Release Paclet",
        "runs-on"         -> "ubuntu-latest",
        "container"       -> $defaultJobContainer,
        "env"             -> $defaultJobEnv,
        "timeout-minutes" -> $defaultTimeConstraint,
        "steps"           -> {
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
    "options" -> "--user root"
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
normalizeSteps[ step_String ] := normalizeSteps @ { step };
normalizeSteps[ steps_List ] := normalizeStep /@ steps;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*normalizeStep*)
normalizeStep[ "checkout"|"checkoutcode" ] := <|
    "name" -> "Checkout Code",
    "id"   -> "checkout-code",
    "uses" -> "actions/checkout@v2"
|>;

normalizeStep[ "check"|"checkpaclet" ] := <|
    "name" -> "Check Paclet",
    "id"   -> "check-paclet-step",
    "uses" -> "rhennigan/check-paclet@latest"
|>;

normalizeStep[ "build"|"buildpaclet" ] := <|
    "name" -> "Build Paclet",
    "id"   -> "build-paclet-step",
    "uses" -> "rhennigan/build-paclet@latest"
|>;

normalizeStep[ "uploadbuildartifacts"|"uploadartifacts"|"uploadbuild" ] := <|
    "name" -> "Upload Build Artifacts",
    "id"   -> "upload-build-artifacts-step",
    "uses" -> "actions/upload-artifact@v2",
    "with" -> <|
        "path"              -> "${{ env.BUILD_DIR }}",
        "if-no-files-found" -> "error"
    |>
|>;

normalizeStep[ "createrelease" ] := <|
    "name" -> "Create Release",
    "id"   -> "create-release-step",
    "uses" -> "actions/create-release@v1",
    "env"  -> <| "GITHUB_TOKEN" -> "${{ secrets.GITHUB_TOKEN }}" |>,
    "with" -> <|
        "tag_name"     -> "${{ env.RELEASE_TAG }}",
        "release_name" -> "Release ${{ env.RELEASE_TAG }}",
        "draft"        -> False,
        "prerelease"   -> False
    |>
|>;

normalizeStep[ "uploadrelease"|"uploadreleaseasset" ] := <|
    "name" -> "Upload Release Asset",
    "id"   -> "upload-release-asset-step",
    "uses" -> "actions/upload-release-asset@v1",
    "env"  -> <| "GITHUB_TOKEN" -> "${{ secrets.GITHUB_TOKEN }}" |>,
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
(*YAML Export*)

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*toYAMLString*)
toYAMLString[ expr_ ] :=
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
        throwMessageFailure[ CreateWorkflow::yamlfail, expr ] &
    ];

toYAMLString // catchUndefined;


toYAMLString0[ as_Association ] :=
    stringJoin @ Riffle[ KeyValueMap[ toYAMLString0, as ], "\n" ];

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

toYAMLString0[ val_String  ] := val;
toYAMLString0[ int_Integer ] := ToString @ int;
toYAMLString0[ r_Real      ] := TextString @ r;
toYAMLString0[ True        ] := "true";
toYAMLString0[ False       ] := "false";
toYAMLString0[ Null        ] := "null";

toYAMLString0[ other_ ] :=
    throwMessageFailure[ CreateWorkflow::ymlconv, other ];

toYAMLString0 // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*toYAMLKey*)
toYAMLKey[ key_ ] := stringJoin[ $indent, key, ": " ];
toYAMLKey // catchUndefined;

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
(*Package Footer*)
End[ ];
EndPackage[ ];