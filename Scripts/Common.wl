BeginPackage[ "Wolfram`PacletCICD`Scripts`" ];

(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::SuspiciousSessionSymbol:: *)

Wolfram`PacletCICD`$Debug = True;

(* Off[ DocumentationBuild`Utils`Localized::nokey ]; *)

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Initialization*)
$messageHistoryLength = 10;
$messageNumber        = 0;
$messageHistory       = <| |>;
$stackHistory         = <| |>;

Internal`AddHandler[ "Message", messageHandler ];

$testingHeads = HoldPattern @ Alternatives[
    TestReport,
    VerificationTest,
    Testing`Private`extractUnevaluated
];

$testStack = With[ { h = $testingHeads }, HoldForm[ h[ ___ ] ] ];

messageHandler[ Hold[ msg_, True ] ] /; $messageNumber < $messageHistoryLength :=
    StackInhibit @ Module[ { stack, keys, limit, drop },
        stack = Stack[ _ ];
        If[ MemberQ[ stack, $testStack ], Throw[ Null, $tag ] ];

        $messageNumber += 1;
        $messageHistory[ $messageNumber ] = HoldForm @ msg;
        $stackHistory[   $messageNumber ] = stack;

        keys  = Union[ Keys @ $messageHistory, Keys @ $stackHistory ];
        limit = $messageNumber - $messageHistoryLength;
        drop  = Select[ keys, ! TrueQ[ # > limit ] & ];

        KeyDropFrom[ $messageHistory, drop ];
        KeyDropFrom[ $stackHistory  , drop ];

        messagePrint @ msg;
    ] ~Catch~ $tag;


messagePrint // Attributes = { HoldFirst };

messagePrint[ Message[ msg_, args___ ] ] :=
    messagePrint[ msg, args ];

messagePrint[ msg_MessageName, args___ ] :=
    Print[ "::warning::",
           ToString @ Unevaluated @ msg <> ": " <> messageString[ msg, args ]
    ];


messageString[ template_String, args___ ] :=
    ToString[ StringForm[ template, Sequence @@ Short /@ { args } ],
              OutputForm,
              PageWidth -> 80
    ];

messageString[ HoldPattern @ MessageName[ f_, tag_ ], args___ ] :=
    With[ { template = MessageName[ General, tag ] },
        messageString[ template, args ] /; StringQ @ template
    ];

messageString[ ___ ] := "-- Message text not found --";

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Definitions*)

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*gitCommand*)
gitCommand[ { cmd__String }, dir_ ] :=
    Enclose @ Module[ { res },
        res = RunProcess[ { "git", cmd }, ProcessDirectory -> dir ];
        ConfirmAssert[ res[ "ExitCode" ] === 0 ];
        StringTrim @ ConfirmBy[ res[ "StandardOutput" ], StringQ ]
    ];

gitCommand[ cmd_String, dir_ ] := gitCommand[ { cmd }, dir ];

gitCommand[ cmd_ ] := gitCommand[ cmd, Directory[ ] ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*releaseID*)
releaseID[ dir_ ] :=
    With[ { sha = Environment[ "GITHUB_SHA" ] },
        If[ StringQ @ sha,
            sha,
            gitCommand[ { "rev-parse", "HEAD" }, dir ]
        ]
    ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*releaseURL*)
releaseURL[ file_ ] :=
    Enclose @ Module[ { pac, ver },
        pac = PacletObject @ Flatten @ File @ file;
        ver = ConfirmBy[ pac[ "Version" ], StringQ ];
        TemplateApply[
            "https://github.com/rhennigan/PacletCICD/releases/tag/v`1`",
            ver
        ]
    ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*updatePacletInfo*)
updatePacletInfo[ dir_ ] := Enclose[
    Module[ { cs, file, string, id, date, url, new },
        cs     = ConfirmBy[ #, StringQ ] &;
        file   = cs @ FileNameJoin @ { dir, "PacletInfo.wl" };
        string = cs @ ReadString @ file;
        id     = cs @ releaseID @ dir;
        date   = cs @ DateString[ "ISODateTime", TimeZone -> 0 ];
        date   = StringTrim[ date, "Z" ] <> "Z";
        url    = cs @ releaseURL @ file;

        new = cs @ StringReplace[
            string,
            {
                "\r\n"           -> "\n",
                "$RELEASE_ID$"   -> id,
                "$RELEASE_DATE$" -> date,
                "$RELEASE_URL$"  -> url
            }
        ];

        Print[ "Updating PacletInfo" ];
        Print[ "    ReleaseID: "  , id   ];
        Print[ "    ReleaseDate: ", date ];

        Confirm @ WithCleanup[ BinaryWrite[ file, new ],
                               Close @ file
                  ]
    ],
    Function[
        Print[ "::error::Failed to update PacletInfo template parameters." ];
        Exit[ 1 ]
    ]
];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*setResourceSystemBase*)
setResourceSystemBase[ ] := (
    Needs[ "ResourceSystemClient`" -> None ];
    $ResourceSystemBase =
        With[ { rsBase = Environment[ "RESOURCE_SYSTEM_BASE" ] },
            If[ StringQ @ rsBase, rsBase, $ResourceSystemBase ]
        ]
);

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*checkResult*)
checkResult // Attributes = { HoldFirst };

checkResult[ eval: (sym_Symbol)[ args___ ] ] :=
    Module[ { result, ctx, name, stacks, stackName, full },

        result = noExit @ eval;
        ctx    = Context @ Unevaluated @ sym;
        name   = SymbolName @ Unevaluated @ sym;
        full   = ctx <> name;

        If[ $messageNumber > 0
            ,
            stackName = name <> "StackHistory";
            stacks = ExpandFileName[ stackName <> ".wxf" ];
            Print[ "::notice::Exporting stack data: ", stacks ];
            Export[
                stacks,
                <|
                    "Stacks"      -> $stackHistory,
                    "Environment" -> GetEnvironment[ ]
                |>,
                "WXF",
                PerformanceGoal -> "Size"
            ];
            EchoEvaluation @ setOutput[ "PACLET_STACK_HISTORY", stacks    ];
            EchoEvaluation @ setOutput[ "PACLET_STACK_NAME"   , stackName ];
        ];

        If[ MatchQ[ Head @ result, HoldPattern @ sym ]
            ,
            Print[ "::error::" <> full <> " not defined" ];
            Exit[ 1 ]
        ];

        If[ FailureQ @ result,
            Print[ "::error::" <> full <> " failed" ];
            Exit[ 1 ]
        ]
    ];

noExit    := Wolfram`PacletCICD`Private`noExit;
setOutput := Wolfram`PacletCICD`Private`setOutput;

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Setup*)
$pacDir = DirectoryName[ $InputFileName, 2 ];
Print[ "Paclet Directory: ", $pacDir ];

updatePacletInfo @ $pacDir;
PacletDirectoryLoad @ $pacDir;
Needs[ "Wolfram`PacletCICD`" -> None ];

setResourceSystemBase[ ];
Print[ "ResourceSystemBase: ", $ResourceSystemBase ];

$defNB = File @ FileNameJoin @ { $pacDir, "ResourceDefinition.nb" };
Print[ "Definition Notebook: ", $defNB ];

(* :!CodeAnalysis::EndBlock:: *)

EndPackage[ ];
