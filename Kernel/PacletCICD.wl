BeginPackage[ "Wolfram`PacletCICD`" ];
EndPackage[ ];

Wolfram`PacletCICD`$ExamplesLocation;
Wolfram`PacletCICD`AnnotateTestIDs;
Wolfram`PacletCICD`ASTPattern;
Wolfram`PacletCICD`BuildPaclet;
Wolfram`PacletCICD`CheckPaclet;
Wolfram`PacletCICD`CompileLibraryResources;
Wolfram`PacletCICD`DeployPaclet;
Wolfram`PacletCICD`EquivalentNodeQ;
Wolfram`PacletCICD`ExampleDirectory;
Wolfram`PacletCICD`FromAST;
Wolfram`PacletCICD`GitHubPacletInstall;
Wolfram`PacletCICD`GitHubSecret;
Wolfram`PacletCICD`PacletCICD;
Wolfram`PacletCICD`SubmitPaclet;
Wolfram`PacletCICD`TestPaclet;
Wolfram`PacletCICD`WorkflowExport;

Quiet[
    Block[ { $ContextPath },
        Get[ "Wolfram`PacletCICD`Config`"              ];
        Get[ "Wolfram`PacletCICD`ErrorHandling`"       ];
        Get[ "Wolfram`PacletCICD`Loading`"             ];
        Get[ "Wolfram`PacletCICD`Utilities`"           ];
        Get[ "Wolfram`PacletCICD`BuildPaclet`"         ];
        Get[ "Wolfram`PacletCICD`CheckPaclet`"         ];
    ],
    General::shdw
];