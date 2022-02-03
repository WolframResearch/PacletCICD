BeginPackage[ "Wolfram`PacletCICD`" ];
EndPackage[ ];

Wolfram`PacletCICD`$ExamplesLocation;
Wolfram`PacletCICD`BuildPaclet;
Wolfram`PacletCICD`CheckPaclet;
Wolfram`PacletCICD`CreateWorkflow;
Wolfram`PacletCICD`DeployPaclet;
Wolfram`PacletCICD`ExampleDirectory;
Wolfram`PacletCICD`GitHubPacletInstall;
Wolfram`PacletCICD`GitHubSecret;
Wolfram`PacletCICD`PacletCICD;
Wolfram`PacletCICD`SubmitPaclet;
Wolfram`PacletCICD`TestPaclet;

Quiet[
    Block[ { $ContextPath },
        Get[ "Wolfram`PacletCICD`Config`"              ];
        Get[ "Wolfram`PacletCICD`ErrorHandling`"       ];
        Get[ "Wolfram`PacletCICD`Utilities`"           ];
        Get[ "Wolfram`PacletCICD`BuildPaclet`"         ];
        Get[ "Wolfram`PacletCICD`CheckPaclet`"         ];
        Get[ "Wolfram`PacletCICD`DeployPaclet`"        ];
        Get[ "Wolfram`PacletCICD`GitHubPacletInstall`" ];
        Get[ "Wolfram`PacletCICD`SubmitPaclet`"        ];
        Get[ "Wolfram`PacletCICD`TestPaclet`"          ];
        Get[ "Wolfram`PacletCICD`Workflows`"           ];
    ],
    General::shdw
];