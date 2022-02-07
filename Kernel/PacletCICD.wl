BeginPackage[ "Wolfram`PacletCICD`" ];
EndPackage[ ];

Wolfram`PacletCICD`$ExamplesLocation;
Wolfram`PacletCICD`AnnotateTestIDs;
Wolfram`PacletCICD`ASTPattern;
Wolfram`PacletCICD`BuildPaclet;
Wolfram`PacletCICD`CheckPaclet;
Wolfram`PacletCICD`CompileLibraryResources;
Wolfram`PacletCICD`DeployPaclet;
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
        Get[ "Wolfram`PacletCICD`Utilities`"           ];
        Get[ "Wolfram`PacletCICD`BuildPaclet`"         ];
        Get[ "Wolfram`PacletCICD`CheckPaclet`"         ];
        Get[ "Wolfram`PacletCICD`Compilation`"         ];
        Get[ "Wolfram`PacletCICD`DeployPaclet`"        ];
        Get[ "Wolfram`PacletCICD`GitHubPacletInstall`" ];
        Get[ "Wolfram`PacletCICD`SubmitPaclet`"        ];
        Get[ "Wolfram`PacletCICD`TestPaclet`"          ];
        Get[ "Wolfram`PacletCICD`Workflows`"           ];
    ],
    General::shdw
];