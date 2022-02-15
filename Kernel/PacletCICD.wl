BeginPackage[ "Wolfram`PacletCICD`" ];
EndPackage[ ];

Wolfram`PacletCICD`$ExamplesLocation;
Wolfram`PacletCICD`AnnotateTestIDs;
Wolfram`PacletCICD`ASTCondition;
Wolfram`PacletCICD`ASTConditionValue;
Wolfram`PacletCICD`ASTPattern;
Wolfram`PacletCICD`ASTPatternTest;
Wolfram`PacletCICD`BuildPaclet;
Wolfram`PacletCICD`CheckDependencies
Wolfram`PacletCICD`CheckPaclet;
Wolfram`PacletCICD`CompileLibraryResources;
Wolfram`PacletCICD`DeployPaclet;
Wolfram`PacletCICD`EquivalentNodeQ;
Wolfram`PacletCICD`ExampleDirectory;
Wolfram`PacletCICD`FromAST;
Wolfram`PacletCICD`GitHubPacletInstall;
Wolfram`PacletCICD`GitHubSecret;
Wolfram`PacletCICD`MessageFailure;
Wolfram`PacletCICD`PacletCICD;
Wolfram`PacletCICD`setContextLoad;
Wolfram`PacletCICD`SubmitPaclet;
Wolfram`PacletCICD`TestPaclet;
Wolfram`PacletCICD`WorkflowExport;

Wolfram`PacletCICD`Internal`ResetExampleDirectory;

Quiet[
    Block[ { $ContextPath },
        Get[ "Wolfram`PacletCICD`Config`"            ];
        Get[ "Wolfram`PacletCICD`ErrorHandling`"     ];
        Get[ "Wolfram`PacletCICD`Loading`"           ];
        Get[ "Wolfram`PacletCICD`Utilities`"         ];
        Get[ "Wolfram`PacletCICD`BuildPaclet`"       ];
        Get[ "Wolfram`PacletCICD`CheckPaclet`"       ];
    ],
    General::shdw
];

If[ $VersionNumber < 13.1,
    Wolfram`PacletCICD`CheckDependencies[
        Wolfram`PacletCICD`Private`$thisPaclet,
        Message -> True
    ]
];