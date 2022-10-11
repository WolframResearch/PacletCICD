(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Header*)
BeginPackage[ "Wolfram`PacletCICD`" ];

Begin[ "`Private`" ];

$ContextAliases[ "dnc`" ] = "DefinitionNotebookClient`";
$ContextAliases[ "cp`"  ] = "CodeParser`";

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Definition Notebook Utilities *)

$simpleTextMode := MatchQ[ dnc`$ConsoleType, "TTY"|"GitHub" ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*withDNCSettings*)
withDNCSettings // Attributes = { HoldRest };

withDNCSettings[ { type_, tgt_ }, eval_ ] := (
    needs[ "DefinitionNotebookClient`" -> None ];
    hiddenDirectoryFix[ ];
    Internal`InheritedBlock[ { dnc`$ConsoleType, dnc`$ClickedButton },
        dnc`$ConsoleType = type;
        dnc`$ClickedButton = tgt;
        eval
    ]
);

withDNCSettings // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*withConsoleType*)
withConsoleType // Attributes = { HoldRest };
withConsoleType[ type_, eval_ ] := (
    needs[ "DefinitionNotebookClient`" -> None ];
    hiddenDirectoryFix[ ];
    Internal`InheritedBlock[ { dnc`$ConsoleType },
        dnc`$ConsoleType = type;
        eval
    ]
);

withConsoleType // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*autoConsole*)
autoConsole // Attributes = { HoldFirst };
autoConsole[ eval_ ] := withConsoleType[ Automatic, eval ];
autoConsole // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*defNBQ*)
defNBQ[ file_? FileExistsQ ] := TrueQ @ Quiet @ defNBQ0 @ file;
defNBQ[ ___ ] := False;

defNBQ0[ file_? FileExistsQ ] := defNBQ0[ Hash @ ReadByteArray @ file, file ];
defNBQ0[ h_, file_ ] := defNBQ0[ h, file ] = defNBQ0 @ Import[ file, "NB" ];
defNBQ0[ Notebook[ ___, TaggingRules -> tags_, ___ ] ] := defNBQ0 @ tags;
defNBQ0[ KeyValuePattern[ "ResourceType" -> "Paclet" ] ] := True;
defNBQ0[ ___ ] := False;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*findDefinitionNotebook*)
findDefinitionNotebook[ dir_? DirectoryQ ] :=
    findDefinitionNotebook[
        dir,
        fileNameJoin @ { ExpandFileName @ dir, "ResourceDefinition.nb" }
    ];

findDefinitionNotebook[ pac_PacletObject ] :=
    findDefinitionNotebook @ pac[ "Location" ];

findDefinitionNotebook[ dir_, file_? defNBQ ] :=
    Flatten @ File @ file;

findDefinitionNotebook[ dir_? DirectoryQ, _ ] :=
    Module[ { files, sorted },
        files  = File /@ FileNames[ "*.nb", dir, Infinity ];
        sorted = SortBy[ files, Length @* FileNameSplit ];
        SelectFirst[ sorted, defNBQ ]
    ];

findDefinitionNotebook // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*notebookCellIDIndex*)
notebookCellIDIndex[ file_ ] := notebookCellIDIndex[ file, "LineColumn" ];

(* Disable lint for KeyValuePattern/PatternSequence, since ASTPattern evaluates
   to something else:
*)
(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::KernelBug:: *)
notebookCellIDIndex[ file_? FileExistsQ, type_ ] :=
    Module[ { ast, assoc },
        Needs[ "CodeParser`" -> None ];
        ast = cp`CodeParse[ Flatten @ File @ file, "SourceConvention" -> type ];

        assoc = Association @ Cases[
            ast,
            ASTPattern[
                cell: Cell[ ___, CellID -> id_Integer, ___ ],
                KeyValuePattern[ cp`Source -> src_ ]
            ] :> (
                FromAST @ id -> <|
                    "Position"   -> src,
                    "Expression" -> FromAST @ cell
                |>
            ),
            Infinity
        ];

        (notebookCellIDIndex[ file, type ] = assoc) /; AssociationQ @ assoc
    ];
(* :!CodeAnalysis::EndBlock:: *)

notebookCellIDIndex // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*simpleCellExpression*)
simpleCellExpression[ Cell[ a___, CellID -> id_, b___ ] ] :=
    Append[ simpleCellExpression @ Cell[ a, b ], CellID -> id ];

simpleCellExpression[ Cell[ a_, style___String, Except[ _String ]... ] ] :=
    Cell[ a, style ];

simpleCellExpression // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*GitHub Actions*)

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*ghAPI*)
ghAPI[ { path__String } ] := Enclose[
    Module[ { url, resp, data, code },
        url  = URLBuild @ { "https://api.github.com", path };
        resp = URLRead[ url, Authentication -> $ghTokenAuth ];
        data = Confirm @ importHTTPResponse @ resp;
        code = ConfirmBy[ resp[ "StatusCode" ], IntegerQ ];

        If[ code =!= 200,
            ghAPIError @ data,
            checkGHRateLimits @ resp;
            data
        ]
    ],
    throwError[ "Failed to retrieve data from the GitHub API.", # ] &
];

ghAPI[ path__String ] := ghAPI @ Flatten @ StringSplit[ { path }, "/" ];

ghAPI // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*ghAPIError*)
ghAPIError[ KeyValuePattern[ "message" -> message_String ] ] :=
    throwError[ "GitHub API Error: `1`", message ];

ghAPIError[ other_ ] :=
    throwError[ "Failed to retrieve data from the GitHub API.", other ];

ghAPIError // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*importHTTPResponse*)
importHTTPResponse[ resp_HTTPResponse ] := Enclose[
    Module[ { mime, fmts },
        mime = ConfirmBy[ resp[ "ContentType" ], StringQ ];
        fmts = ConfirmMatch[ MIMETypeToFormatList @ mime, { __String } ];
        If[ MemberQ[ fmts, "JSON" ],
            Confirm @ Developer`ReadRawJSONString @ resp[ "Body" ],
            FirstCase[
                fmts,
                f_ :> With[ { e = Import[ resp, f ] }, e /; ! FailureQ @ e ],
                Import[ resp, Automatic ]
            ]
        ]
    ],
    Import[ resp, Automatic ] &
];

importHTTPResponse // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*checkGHRateLimits*)
checkGHRateLimits[ resp_HTTPResponse ] :=
    Enclose @ Module[ { str, num },
        str = lookupHeaders[ resp, "X-RateLimit-Remaining" ];
        ConfirmAssert[ StringQ @ str && StringMatchQ[ str, DigitCharacter.. ] ];
        num = ConfirmBy[ ToExpression @ str, IntegerQ ];
        checkGHRateLimits @ num;
        num
    ];

checkGHRateLimits[ num_Integer ] /; num <= 5 :=
    generalMessage[
        "warning",
        "Warning: only `1` GitHub API requests remaining before reaching the rate limit.",
        num
    ];

checkGHRateLimits[ num_Integer ] :=
    If[ TrueQ @ $gitHub,
        ConsoleDebug[ "GitHub API requests remaining: " <> ToString @ num ],
        Null
    ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*lookupHeaders*)
lookupHeaders // Attributes = { HoldRest };

lookupHeaders[ resp_, keys_ ] :=
    lookupHeaders[ resp, keys, Missing[ "NotFound" ] ];

lookupHeaders[ resp_HTTPResponse, keys_, default_ ] :=
    lookupHeaders[ resp[ "Headers" ], keys, default ];

lookupHeaders[ headers: { (_String -> _String).. }, keys_, default_ ] :=
    Lookup[ MapAt[ normalHeaderKey, headers, { All, 1 } ],
            normalHeaderKey @ keys,
            default
    ];

lookupHeaders // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*normalHeaderKey*)
normalHeaderKey[ key: _String | { ___String } ] :=
    StringDelete[ ToLowerCase @ key, "-" ];

normalHeaderKey // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*$ghTokenAuth*)
$ghTokenAuth :=
    Module[ { user, token },
        user  = $ghTokenUser;
        token = Environment[ "GITHUB_TOKEN" ];
        If[ StringQ @ user && StringQ @ token,
            <| "Username" -> user, "Password" -> token |>,
            Automatic
        ]
    ];

$ghTokenUser :=
    SelectFirst[
        Values @ GetEnvironment @ {
            "GITHUB_REPOSITORY_OWNER",
            "GITHUB_TOKEN_USER"
        },
        StringQ
    ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*$gitHub*)
$gitHub := TrueQ @ Or[
    DirectoryQ @ $gitHubWorkspace,
    dnc`$ConsoleType === "GitHub"
];

$gitHubWorkspace := Environment[ "GITHUB_WORKSPACE" ];

$gitHubEnvironment :=
    Enclose @ Module[ { data, keys, desc, vals },
        data = Confirm @ $gitHubEnvironmentData;
        keys = data[[ All, 1 ]];
        desc = data[[ All, 2 ]];
        vals = Values @ GetEnvironment @ keys;
        Association @ Apply[
            #1 -> <| "Value" -> #2, "Description" -> #3 |> &,
            Transpose @ { keys, vals, desc },
            { 1 }
        ]
    ];

$gitHubEnvironmentData := Enclose[
    $gitHubEnvironmentData =
        ConfirmMatch[
            Get @ ConfirmBy[
                FileNameJoin @ {
                    $thisPaclet[ "AssetLocation", "Resources" ],
                    "GitHubEnvironmentData.wl"
                },
                FileExistsQ
            ],
            { { _String, _String }.. }
        ]
];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*$ghCommitRootURL*)
$ghCommitRootURL := $ghCommitRootURL = Enclose[
    Module[ { env, server, repo, sha },
        env    = ConfirmBy[ Environment[ # ], StringQ ] &;
        server = env[ "GITHUB_SERVER_URL" ];
        repo   = env[ "GITHUB_REPOSITORY" ];
        sha    = env[ "GITHUB_SHA" ];
        URLBuild @ Flatten @ { server, repo, "blob", sha }
    ],
    None &
];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*ghCommitFileURL*)
ghCommitFileURL[ file_? FileExistsQ ] := ghCommitFileURL[ file ] = Enclose[
    Module[ { base, ws, rel, split, url },
        base  = ConfirmBy[ $ghCommitRootURL, StringQ ];
        ws    = ConfirmBy[ Environment[ "GITHUB_WORKSPACE" ], StringQ ];
        rel   = ConfirmBy[ relativePath[ ws, file ], StringQ ];
        split = DeleteCases[ FileNameSplit @ rel, "." ];
        url   = ConfirmBy[ URLBuild @ Flatten @ { base, split }, StringQ ];
        StringTrim[ url, "/" ]
    ],
    None &
];

ghCommitFileURL[ file_, KeyValuePattern[ "SourceInformation" -> src_ ] ] :=
    ghCommitFileURL[ file, src ];

ghCommitFileURL[ _, KeyValuePattern @ { "File" -> f_, "Position" -> p_ } ] :=
    ghCommitFileURL[ f, p ];

ghCommitFileURL[ file_, KeyValuePattern[ "Position" -> pos_ ] ] :=
    ghCommitFileURL[ file, pos ];

ghCommitFileURL[
    file_,
    { KeyValuePattern[ "Line" -> l1_ ], KeyValuePattern[ "Line" -> l2_ ] }
] := ghCommitFileURL[ file, { l1, l2 } ];

ghCommitFileURL[ file_, _Association ] := ghCommitFileURL @ file;
ghCommitFileURL[ file_, _Missing     ] := ghCommitFileURL @ file;

ghCommitFileURL[ file_, { { l1_Integer, _ }, { l2_Integer, _ } } ] :=
    ghCommitFileURL[ file, { l1, l2 } ];

ghCommitFileURL[ file_, { l1_Integer, l2_Integer } ] := Enclose[
    Module[ { url },
        url = ConfirmBy[ ghCommitFileURL @ file, StringQ ];
        url <> "#L" <> ToString @ l1 <> "-L" <> ToString @ l2
    ],
    None &
];

ghCommitFileURL[ KeyValuePattern[ "SourcePosition" -> src_ ] ] :=
    ghCommitFileURL @ src;

ghCommitFileURL[ KeyValuePattern @ { "File" -> f_, "Position" -> p_ } ] :=
    ghCommitFileURL[ f, p ];

ghCommitFileURL[ file_ ] := None;
ghCommitFileURL[ file_, _ ] := None;

ghCommitFileURL // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*ghRelativePath*)
ghRelativePath[ file_ ] := Enclose[
    Module[ { ws },
        ws = Environment[ "GITHUB_WORKSPACE" ];
        If[ StringQ @ ws,
            ConfirmBy[ relativePath[ ws, file ], StringQ ],
            ConfirmBy[ relativePath[ Directory[ ], file ], StringQ ]
        ]
    ],
    throwError[ "Could not determine relative path for file `1`", file ] &
];

ghRelativePath // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*setOutput*)
setOutput[ name_, value_ ] :=
    If[ TrueQ @ $gitHub, setOutput[ $gitHubEnv, name, value ] ];

setOutput[ str_OutputStream, name_, value_ ] := (

    ConsoleDebug @ StringJoin[
        "Setting GitHub environment variable ",
        ToString @ name,
        "=",
        ToString @ value
    ];

    dnc`ConsolePrint @ StringJoin[
        "::set-output name=",
        ToString @ name,
        "::",
        ToString @ value
    ];

    WriteLine[ str, ToString @ name <> "=" <> ToString @ value ]
);

setOutput[ _, name_, value_ ] := (
    ConsoleDebug @ StringJoin[
        "Setting GitHub environment variable using fallback ",
        ToString @ name,
        "=",
        ToString @ value
    ];

    dnc`ConsolePrint @ StringJoin[
        "::set-output name=",
        ToString @ name,
        "::",
        ToString @ value
    ]
);

setOutput // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*appendStepSummary*)
appendStepSummary[ md_ ] :=
    If[ TrueQ @ $gitHub, appendStepSummary[ $gitHubStepSummary, md ] ];

appendStepSummary[ stream_OutputStream, md_ ] :=
    With[ { string = ToString @ md },
        ConsoleDebug[ "Appending to summary markdown: " <> string ];
        If[ StringEndsQ[ string, "\n" ],
            WriteString[ stream, string ],
            WriteString[ stream, string, "\n" ]
        ]
    ];

appendStepSummary // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*autoTrueWhenGH*)
autoTrueWhenGH[ Automatic ] := TrueQ @ $gitHub;
autoTrueWhenGH[ other_    ] := TrueQ @ other;
autoTrueWhenGH // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsubsubsection::Closed:: *)
(*$gitHubEnv*)
$gitHubEnv := getGitHubEnv[ ];

getGitHubEnv[ ] := getGitHubEnv @ EchoEvaluation @ Environment[ "GITHUB_ENV" ];
getGitHubEnv[ e_String ] := getGitHubEnv @ First[ EchoEvaluation @ Streams @ e, OpenAppend @ e ];
getGitHubEnv[ s_OutputStream ] := $gitHubEnv = s;
getGitHubEnv[ ___ ] := $Failed;

(* ::**********************************************************************:: *)
(* ::Subsubsubsection::Closed:: *)
(*$gitHubStepSummary*)
$gitHubStepSummary := getGitHubSS[ ];

getGitHubSS[ ] := getGitHubSS @ Environment[ "GITHUB_STEP_SUMMARY" ];
getGitHubSS[ e_String ] := getGitHubSS @ First[ Streams @ e, OpenAppend @ e ];
getGitHubSS[ s_OutputStream ] := $gitHubStepSummary = s;
getGitHubSS[ ___ ] := $Failed;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*ghSetWFOutput*)
ghSetWFOutput[ name_String? StringQ, expr_ ] /; $ghSetWFOutput :=
    With[ { tag = ghWFTag @ name },
        ConsoleDebug @ TemplateApply[
            "Setting WorkflowValue \"`1`\" to `2`.",
            { name, shortString @ expr }
        ];
        WorkflowValue[ tag ] = expr
    ];

ghSetWFOutput // catchUndefined;

$ghSetWFOutput = True;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*ghWFTag*)
ghWFTag[ name_String? StringQ ] :=
    With[ { job = Environment[ "GITHUB_JOB" ] },
        StringRiffle[ { "PacletCICD", job, name }, "/" ] /; StringQ @ job
    ];

ghWFTag // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Paclet Resources*)

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*wlResource*)
wlResource[ name_String? StringQ ] :=
    Enclose @ Module[ { dir, base, file },
        dir  = ConfirmBy[ $resourceDirectory, DirectoryQ ];
        base = StringDelete[ name, ".wl"~~EndOfString, IgnoreCase -> True ];
        file = ConfirmBy[ FileNameJoin @ { dir, base <> ".wl" }, FileExistsQ ];
        wlResource[ name ] = Confirm @ Get @ file
    ];

wlResource // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*wxfResource*)
wxfResource[ name_String? StringQ ] :=
    Enclose @ Module[ { dir, base, file },
        dir  = ConfirmBy[ $resourceDirectory, DirectoryQ ];
        base = StringDelete[ name, ".wxf"~~EndOfString, IgnoreCase -> True ];
        file = ConfirmBy[ FileNameJoin @ { dir, base <> ".wxf" }, FileExistsQ ];
        wxfResource[ name ] = Confirm @ Developer`ReadWXFFile @ file
    ];

wxfResource // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*$resourceDirectory*)
$resourceDirectory := Enclose[
    $resourceDirectory = ConfirmBy[
        $thisPaclet[ "AssetLocation", "Resources" ],
        DirectoryQ
    ]
];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*General File Utilities*)

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*nonEmptyDirectoryQ*)
nonEmptyDirectoryQ[ dir_? DirectoryQ ] :=
    MatchQ[ FileNames[ All, dir ], { __ } ];

nonEmptyDirectoryQ[ ___ ] := False;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*parentPacletDirectory*)
parentPacletDirectory[ file_, default_: None ] := Enclose[
    Module[ { expanded, dir, parent },
        expanded = ConfirmBy[ ExpandFileName @ file, StringQ ];
        parent = parentPacletDirectory0[ expanded, default ];
        ConfirmMatch[ parent, default | _?DirectoryQ ]
    ],
    throwError[
        "Cannot determine parent paclet directory of `1`.",
        file, ##
    ] &
];

parentPacletDirectory // catchUndefined;

parentPacletDirectory0[ file_, default_ ] :=
    Quiet[ SelectFirst[ FixedPointList[ DirectoryName, file, 50 ],
                        pacletDirectoryQ,
                        default
           ],
           PacletManager`CreatePaclet::badarg
    ];

parentPacletDirectory0 // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*pacletDirectoryQ*)
pacletDirectoryQ[ "" ] := False;

pacletDirectoryQ[ dir_? DirectoryQ ] :=
    Quiet @ PacletObjectQ @ PacletObject @ Flatten @ File @ dir;

pacletDirectoryQ[ ___ ] := False;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*pacletInfoFileQ*)
pacletInfoFileQ[ "" ] := False;

pacletInfoFileQ[ dir_? DirectoryQ ] := False;

pacletInfoFileQ[ file_? FileExistsQ ] :=
    TrueQ @ And[
        StringMatchQ[ FileBaseName @ file, "PacletInfo", IgnoreCase -> True ],
        StringMatchQ[ FileExtension @ file, ("m"|"wl"), IgnoreCase -> True ],
        pacletInfoFileQ[ ExpandFileName @ file, Hash @ ReadByteArray @ file ]
    ];

pacletInfoFileQ[ file_? FileExistsQ, hash_Integer ] :=
    pacletInfoFileQ[ file, hash ] =
        TrueQ @Quiet @ PacletObjectQ @ PacletObject @ Flatten @ File @ file;

pacletInfoFileQ[ ___ ] := False;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*ensureDirectory*)
ensureDirectory[ dir_ ] := GeneralUtilities`EnsureDirectory @ dir;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*fileNameJoin*)
fileNameJoin[ list_List ] :=
    With[ { joined = fileNameJoin0 @ list },
        If[ StringQ @ joined,
            StringReplace[ joined, "\\" -> "/" ],
            $Failed
        ]
    ];

fileNameJoin0[ list: { ___, _File, ___ } ] :=
  fileNameJoin0 @ Replace[ list, File[ file_ ] :> file, { 1 } ];

fileNameJoin0[ list_ ] := FileNameJoin @ list;

fileNameJoin // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*relativePath*)
relativePath[ path1_ ][ path2_ ] := relativePath[ path1, path2 ];

relativePath[ path1_, path2_ ] := Enclose[
    Module[
        {
            full1, full2, split1, split2, first1, first2,
            prefix, common, drop1, drop2, dots, rel
        },

        full1  = ConfirmBy[ expandFileName @ path1, StringQ ];
        full2  = ConfirmBy[ expandFileName @ path2, StringQ ];
        split1 = ConfirmBy[ FileNameSplit @ full1, ListQ ];
        split2 = ConfirmBy[ FileNameSplit @ full2, ListQ ];
        first1 = First[ split1, "" ];
        first2 = First[ split2, "" ];

        If[ $OperatingSystem === "Windows" && first1 =!= first2,
            Throw[ full2, $tag ]
        ];

        prefix = ConfirmBy[ longestCommonPrefix[ split1, split2 ], ListQ ];
        common = Length @ prefix;
        drop1  = Drop[ split1, common ];
        drop2  = Drop[ split2, common ];
        dots   = Join[ ConstantArray[ "..", Length @ drop1 ], drop2 ];
        rel    = If[ drop1 === { }, Prepend[ dots, "." ], dots ];

        ConfirmBy[ fileNameJoin @ rel, StringQ ]

    ] ~Catch~ $tag
    ,
    throwError[
        "Cannot determine relative path for `1` and `2`.",
        path1, path2, ##
    ] &
];

e: relativePath[ PatternSequence[ ] | PatternSequence[ _, _, __ ] ] :=
    throwMessageFailure[
        PacletCICD::undefined,
        HoldForm @ relativePath,
        HoldForm @ e,
        DownValues
    ];

relativePath ~catchUndefined~ SubValues;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*relativePathQ*)
relativePathQ[ file_ ] :=
    With[ { list = Quiet @ FileNameSplit @ file },
        If[ ListQ @ list,
            relativePathListQ @ list,
            False
        ]
    ];

relativePathQ // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*relativePathListQ*)
relativePathListQ[ { "", ___ } ] := False;
relativePathListQ[ { c_? driveLetterQ, ___ } ] := False;
relativePathListQ[ { ___String } ] := True;
relativePathListQ // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*driveLetterQ*)
driveLetterQ[ c_String ] /; $OperatingSystem === "Windows" :=
    StringMatchQ[ c, LetterCharacter ~~ ":" ];

driveLetterQ[ ___ ] := False;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*expandFileName*)
expandFileName[ file_String /; StringStartsQ[ file, "file://" ] ] := Enclose[
    Module[ { path },
        LocalObject;
        path = ConfirmBy[ LocalObjects`URIToPath @ file, StringQ ];
        ConfirmAssert[ ! StringStartsQ[ path, "file://" ] ];
        expandFileName @ path
    ],
    throwError[ "Cannot expand file `1`.", file, ## ] &
];

expandFileName[ obj: HoldPattern[ _CloudObject ] ] := expandCloudObject @ obj;

expandFileName[ url_URL ] := expandURL @ url;

expandFileName[ file_ ] :=
    With[ { expanded = ExpandFileName @ file },
        If[ StringQ @ expanded,
            expanded,
            throwError[
                "Cannot expand file `file`.",
                <| "file" -> file, "expanded" -> expanded |>
            ]
        ]
    ];

expandFileName // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*expandCloudObject*)
$$uuidCOPath = KeyValuePattern[ "Path" -> { "", _, _? uuidQ } ];
$$objCOPath  = KeyValuePattern[ "Path" -> { "", "obj", _, __ } ];

expandCloudObject[ obj: HoldPattern[ _CloudObject ] ]:=
    expandCloudObject[ obj, URLParse @ obj ];

expandCloudObject[ obj_, $$objCOPath ] := obj;

expandCloudObject[ obj_, $$uuidCOPath ] :=
    Module[ { expanded },
        expanded = CloudObject[ obj, CloudObjectNameFormat -> "UserURLBase" ];
        If[ expandedCloudObjectQ @ expanded,
            expanded,
            throwError[ "Cannot expand cloud object `1`.", obj ]
        ]
    ];

expandCloudObject // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsubsubsection::Closed:: *)
(*expandedCloudObjectQ*)
expandedCloudObjectQ[
    obj: HoldPattern @ CloudObject[ _? StringQ, OptionsPattern[ ] ]
] := MatchQ[ URLParse @ obj, $$objCOPath ];

expandedCloudObjectQ[ ___ ] := False;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*expandURL*)
expandURL[ obj: HoldPattern[ _CloudObject|_URL ] ] :=
    Module[ { url },
        url = URLBuild @ URLParse @ obj;
        If[ ! StringQ @ url,
            throwError[ "Cannot expand URL `1`.", obj ],
            url
        ]
    ];

expandURL // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Misc Programming Utilities*)

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*shortString*)
shortString[ a___ ] := ToString[ Unevaluated @ Short @ a, PageWidth -> 80 ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*symbolQ*)
symbolQ // Attributes = { HoldAllComplete };

symbolQ[ s_Symbol ] :=
    TrueQ @ And[
        AtomQ @ Unevaluated @ s,
        ! Internal`RemovedSymbolQ @ Unevaluated @ s,
        Unevaluated @ s =!= Internal`$EFAIL
    ];

symbolQ[ ___ ] := False;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*versionOrder*)
versionOrder[ v1_, v2_ ] :=
    Module[ { l1, l2, len, p1, p2 },

        l1  = toVersionList @ v1;
        l2  = toVersionList @ v2;
        len = Max[ Length @ l1, Length @ l2 ];
        p1  = PadRight[ l1, len, 0 ];
        p2  = PadRight[ l2, len, 0 ];

        Which[ p1 === p2            ,  0,
               OrderedQ @ { p1, p2 },  1,
               OrderedQ @ { p2, p1 }, -1,
               True                 ,  0
        ]
    ];

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*toVersionList*)
toVersionList[ v_ ] :=
    StringCases[
        ToString @ N @ v,
        {
            d: Longest[ DigitCharacter..  ] :> ToExpression @ d,
            c: Longest[ LetterCharacter.. ] :> c
        }
    ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*uuidQ*)
uuidQ[ str_String? StringQ ] := StringMatchQ[ str, $uuidRegex ];
uuidQ[ ___ ] := False;

$uuidRegex := $uuidRegex =
    Module[ { hex, blocks, string },
        hex = "[a-fA-F0-9]{" <> ToString[ #1 ] <> "}" &;
        blocks = hex /@ { 8, 4, 4, 4, 12 };
        string = StringRiffle[ blocks, "-" ];
        RegularExpression @ string
    ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*optionValue*)
optionValue[ sym_Symbol, opts_, name_ ] :=
    OptionValue[ sym, FilterRules[ opts, Options @ sym ], name ];

optionValue // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*filterOptions*)
filterOptions[ sym_Symbol, opts: (Rule|RuleDelayed)[ _, _ ]... ] :=
    Sequence @@ FilterRules[ Flatten @ { opts }, Options @ sym ];

filterOptions[ { syms___Symbol }, opts: (Rule|RuleDelayed)[ _, _ ]... ] :=
    Apply[
        Sequence,
        FilterRules[
            Flatten @ { opts },
            Flatten @ Cases[ HoldComplete @ syms, s_ :> Options @ s ]
        ]
    ];

filterOptions[
    Verbatim[ OptionsPattern ][ syms: { ___ } ],
    opts: (Rule|RuleDelayed)[ _, _ ]...
] :=
    filterOptions[ syms, opts ];

filterOptions /: (sym_Symbol)[
        args___,
        filterOptions[ opts1: (Rule|RuleDelayed)[ _, _ ]... ],
        opts2: (Rule|RuleDelayed)[ _, _ ]...
    ] :=
    With[ { filtered = filterOptions[ sym, opts1 ] },
        sym[ args, filtered, opts2 ]
    ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*optionsAssociation*)
optionsAssociation[ sym_Symbol, opts___ ] :=
    KeyMap[ ToString, Association @ Flatten @ { Options @ sym, opts } ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*longestCommonPrefix*)
longestCommonPrefix[ list_List ] :=
    list;

longestCommonPrefix[ lists__List ] :=
    With[ { prefix = longestCommonPrefix0[ { } ][ { lists } ] },
        If[ ListQ @ prefix,
            prefix,
            throwError[
                "Cannot determine common prefix for lists `1`.",
                { lists },
                prefix
            ]
        ]
    ];

longestCommonPrefix[ strings__String ] :=
    Module[ { chars, string },
        chars  = Characters @ { strings };
        string = StringJoin[ longestCommonPrefix @@ chars ];
        If[ StringQ @ string,
            string,
            throwError[
                "Cannot determine common prefix for strings `strings`.",
                { strings },
                <| "chars" -> chars, "string" -> string |>
            ]
        ]
    ];

longestCommonPrefix // catchUndefined;


longestCommonPrefix0[ { xs___ } ][ { y: { a_, ___ }, ys: { a_, ___ }.. } ] :=
    longestCommonPrefix0[ { xs, a } ][ Rest /@ { y, ys } ];

longestCommonPrefix0[ xs_List ][ ___ ] := xs;

longestCommonPrefix0 ~catchUndefined~ UpValues;

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Hacks*)

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*needs*)
needs[ ctx_String   ] := Quiet[ Needs[ ctx -> None ], General::shdw ];
needs[ ctx_ -> None ] := needs @ ctx;
needs[ other___     ] := Needs @ other;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*ccPromptFix*)
ccPromptFix // Attributes = { HoldFirst };

ccPromptFix[ eval_ ] /; StringQ @ Environment[ "GITHUB_WORKFLOW" ] :=
    ccPromptFix[ eval, "Script" ];

ccPromptFix[ eval_ ] := ccPromptFix[ eval, $EvaluationEnvironment ];

ccPromptFix[ eval_, True|"Script"|"Subkernel"|"BatchJob" ] := (
    CloudObject;
    Internal`InheritedBlock[ { CloudObject`Private`hiddenOptions },

        DownValues[ CloudObject`Private`hiddenOptions ] =
            Replace[
                DownValues @ CloudObject`Private`hiddenOptions,
                HoldPattern[ "Prompt" -> Automatic ] :> ("Prompt" -> False),
                { 3 }
            ];

        eval
    ]
);

ccPromptFix[ eval_, _ ] := eval;

ccPromptFix // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*disableCloudConnect*)
disableCloudConnect // Attributes = { HoldFirst };
disableCloudConnect[ eval_ ] := ccPromptFix[ eval, True ];
disableCloudConnect // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*usingFrontEnd*)
usingFrontEnd // Attributes = { HoldFirst };
usingFrontEnd[ eval_ ] :=
    Block[ { usingFrontEnd = UsingFrontEnd },
        UsingFrontEnd @ usingFrontEnd0 @ eval
    ];

(* workaround for bug(427434) *)
usingFrontEnd0 // Attributes = { HoldFirst };

usingFrontEnd0[ eval_ ] /; DownValues @ FE`Evaluate === { } :=
    Internal`InheritedBlock[ { FE`Evaluate },
        Unprotect @ FE`Evaluate;
        FE`Evaluate // Attributes = { HoldFirst };
        FE`Evaluate[ e_ ] := MathLink`CallFrontEndHeld @ FrontEnd`Value @ e;
        FE`Evaluate[ e_, h_ ] :=
            With[ { fe = $ParentLink },
                LinkWriteHeld[ fe, Hold @ FrontEnd`Value @ e ];
                LinkRead[ fe, h ]
            ];

        eval
    ];

usingFrontEnd0[ eval_ ] := eval;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*hiddenDirectoryFix*)
hiddenDirectoryFix[ ] := hiddenDirectoryFix[ ] = (
    needs[ "DefinitionNotebookClient`" -> None ];
    SubValues[ DefinitionNotebookClient`TemplateCells`PackagePrivate`expandDir ] =
        ReplaceAll[
            SubValues @ DefinitionNotebookClient`TemplateCells`PackagePrivate`expandDir,
            HoldPattern @ Select[ FileNames[ "*", a_ ], ! StringMatchQ[ #1, b_ ] & ] :>
                Select[
                    FileNames[ "*", a ],
                    ! StringMatchQ[ FileNameTake[ #1 ], b ] &
                ]
        ];
);

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Footer*)
End[ ];
EndPackage[ ];
