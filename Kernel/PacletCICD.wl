BeginPackage[ "Wolfram`PacletCICD`" ];
EndPackage[ ];

Wolfram`PacletCICD`$Debug;
Wolfram`PacletCICD`$ExamplesLocation;
Wolfram`PacletCICD`AnnotateTestIDs;
Wolfram`PacletCICD`ASTCondition;
Wolfram`PacletCICD`ASTConditionValue;
Wolfram`PacletCICD`ASTPattern;
Wolfram`PacletCICD`ASTPatternTest;
Wolfram`PacletCICD`BuildPaclet;
Wolfram`PacletCICD`CheckDependencies;
Wolfram`PacletCICD`CheckPaclet;
Wolfram`PacletCICD`CompileLibraryResources;
Wolfram`PacletCICD`ConsoleError;
Wolfram`PacletCICD`ConsoleLog;
Wolfram`PacletCICD`ConsoleNotice;
Wolfram`PacletCICD`ConsoleType;
Wolfram`PacletCICD`ConsoleWarning;
Wolfram`PacletCICD`CreatePublisherToken;
Wolfram`PacletCICD`DeletePublisherToken;
Wolfram`PacletCICD`DeployPaclet;
Wolfram`PacletCICD`EquivalentNodeQ;
Wolfram`PacletCICD`ExampleDirectory;
Wolfram`PacletCICD`FormatNotebooks;
Wolfram`PacletCICD`FormattingHelper;
Wolfram`PacletCICD`FromAST;
Wolfram`PacletCICD`GitHubPacletInstall;
Wolfram`PacletCICD`GitHubSecret;
Wolfram`PacletCICD`LoadSubPackage;
Wolfram`PacletCICD`MessageFailure;
Wolfram`PacletCICD`PacletCICD;
Wolfram`PacletCICD`PublisherTokenObject;
Wolfram`PacletCICD`PublisherTokens;
Wolfram`PacletCICD`ReplacePacletInfo;
Wolfram`PacletCICD`ResetExampleDirectory;
Wolfram`PacletCICD`SetContextLoad;
Wolfram`PacletCICD`SetPacletInfo;
Wolfram`PacletCICD`SubmitPaclet;
Wolfram`PacletCICD`TestPaclet;
Wolfram`PacletCICD`Workflow;
Wolfram`PacletCICD`WorkflowEvaluate;
Wolfram`PacletCICD`WorkflowExport;
Wolfram`PacletCICD`WorkflowJob;
Wolfram`PacletCICD`WorkflowJobQ;
Wolfram`PacletCICD`WorkflowQ;
Wolfram`PacletCICD`WorkflowStep;
Wolfram`PacletCICD`WorkflowStepQ;
Wolfram`PacletCICD`WorkflowValue;

Wolfram`PacletCICD`Internal`$MXFile =
    FileNameJoin @ {
        DirectoryName @ $InputFileName,
        ToString @ $SystemWordLength <> "Bit",
        "PacletCICD.mx"
    };

If[ FileExistsQ[ Wolfram`PacletCICD`Internal`$MXFile ],
    Get[ Wolfram`PacletCICD`Internal`$MXFile ],
    Quiet[
        Block[ { $ContextPath },
            Get[ "Wolfram`PacletCICD`Config`" ];
            Get[ "Wolfram`PacletCICD`ErrorHandling`" ];
            Get[ "Wolfram`PacletCICD`Loading`" ];
            Get[ "Wolfram`PacletCICD`Utilities`" ];
        ],
        General::shdw
    ]
];

If[ $VersionNumber < 13.1 && StringQ @ Environment[ "GITHUB_WORKFLOW" ],
    Quiet[
        PacletInstall[ "https://wolfr.am/11FhS453R" ];
        PacletInstall[ "https://wolfr.am/11FhS4xyd" ];
        PacletInstall[ "https://wolfr.am/11FhS4VRh" ];
        ,
        PacletInstall::samevers
    ]
];

If[ $VersionNumber < 13.1,
    Wolfram`PacletCICD`CheckDependencies[
        Wolfram`PacletCICD`Private`$thisPaclet,
        Message -> True
    ]
];