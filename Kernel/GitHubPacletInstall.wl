(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Header*)
BeginPackage[ "Wolfram`PacletCICD`" ];

GitHubPacletInstall;

Begin[ "`Private`" ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Messages*)
GitHubPacletInstall::spec =
"`1` is not a valid GitHub Paclet specification.";

GitHubPacletInstall::ref =
"Cannot determine a release for `1` using the ref `2`.";

GitHubPacletInstall::unimpl =
"Installing by `1` is not yet supported.";

GitHubPacletInstall::versp =
"`1` is not a valid version specification.";

GitHubPacletInstall::tagsp =
"`1` is not a valid tag specification.";

GitHubPacletInstall::brsp =
"`1` is not a valid branch specification.";

GitHubPacletInstall::unknown =
"An unexpected error occurred.";

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Options*)
GitHubPacletInstall // Options = { };

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Main definition*)
GitHubPacletInstall[ spec_String? StringQ, opts: OptionsPattern[ ] ] :=
    catchTop @ Module[ { assoc },
        assoc = fromGitHubPacletSpec @ spec;
        If[ AssociationQ @ assoc,
            GitHubPacletInstall[ assoc, opts ],
            throwMessageFailure[ GitHubPacletInstall::spec, spec ]
        ]
    ];

GitHubPacletInstall[ as_Association? AssociationQ, opts: OptionsPattern[ ] ] :=
    catchTop @ githubPacletInstall[
        Lookup[ as, "User"       ],
        Lookup[ as, "Repository" ],
        Lookup[ as, "Version"    ],
        Lookup[ as, "Tag"        ],
        Lookup[ as, "Branch"     ]
    ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Error cases*)
(* TODO *)

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Dependencies*)

$$maybeString = _String? StringQ | _Missing;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*githubPacletInstall*)
githubPacletInstall[ owner_String, repo_String, "Latest", tag_, branch_ ] :=
    ghInstallLatest[ owner, repo, tag, branch ];

githubPacletInstall[ owner_String, repo_String, ver_String, tag_, branch_ ] :=
    ghInstallVersion[ owner, repo, ver, tag, branch ];

(* Errors: *)
githubPacletInstall[ owner_, repo_, ver_Missing, tag_String, branch_ ] :=
    throwMessageFailure[ GitHubPacletInstall::unimpl, "tag" ];

githubPacletInstall[ owner_, repo_, ver_Missing, tag_Missing, branch_String ] :=
    throwMessageFailure[ GitHubPacletInstall::unimpl, "branch" ];

githubPacletInstall[ owner_, repo_, ver: Except[ $$maybeString ], _, _ ] :=
    throwMessageFailure[ GitHubPacletInstall::versp, ver ];

githubPacletInstall[ owner_, repo_, ver_, tag: Except[ $$maybeString ], _ ] :=
    throwMessageFailure[ GitHubPacletInstall::tagsp, ver ];

githubPacletInstall[ owner_, repo_, ver_, tag_, br: Except[ $$maybeString ] ] :=
    throwMessageFailure[ GitHubPacletInstall::brsp, br ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*ghInstallLatest*)
(* TODO *)
ghInstallLatest[ ___ ] := throwMessageFailure[ GitHubPacletInstall::unknown ];

(* Use this URL:
    URLBuild @ <|
        "Scheme" -> "https",
        "Domain" -> "api.github.com",
        "Path"   -> { "", "repos", owner, repo, "releases", "latest" }
    |>
*)

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*ghInstallVersion*)
(* TODO *)
ghInstallVersion[ ___ ] := throwMessageFailure[ GitHubPacletInstall::unknown ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*fromGitHubPacletSpec*)
fromGitHubPacletSpec[ spec_String? StringQ ] :=
    fromGitHubPacletSpec @ StringSplit[ spec, c: "/" | "@" :> c ];

fromGitHubPacletSpec[ { owner_, "/", repo_, "@", ref_ } ] := Enclose[
    Module[ { assoc },
        assoc = ConfirmBy[ makeSpecifierData @ ref, AssociationQ ];
        Join[ <| "User" -> owner, "Repository" -> repo |>, assoc ]
    ],
    throwMessageFailure[
        GitHubPacletInstall::ref,
        owner <> "/" <> repo,
        ref
    ] &
];

fromGitHubPacletSpec[ { owner_, "/", repo_ } ] := <|
    "User"       -> owner,
    "Repository" -> repo,
    "Version"    -> "Latest"
|>;

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Footer*)
End[ ];

EndPackage[ ];