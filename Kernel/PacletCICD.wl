BeginPackage[ "Wolfram`PacletCICD`" ];
EndPackage[ ];

Wolfram`PacletCICD`BuildPaclet;
Wolfram`PacletCICD`CheckPaclet;
Wolfram`PacletCICD`DeployPaclet;
Wolfram`PacletCICD`PacletCICD;
Wolfram`PacletCICD`SubmitPaclet;
Wolfram`PacletCICD`TestPaclet;

Quiet[
    Block[ { $ContextPath },
        Get[ "Wolfram`PacletCICD`BuildPaclet`"  ];
        Get[ "Wolfram`PacletCICD`CheckPaclet`"  ];
        Get[ "Wolfram`PacletCICD`DeployPaclet`" ];
        Get[ "Wolfram`PacletCICD`SubmitPaclet`" ];
        Get[ "Wolfram`PacletCICD`TestPaclet`"   ];
        Get[ "Wolfram`PacletCICD`Utilities`"    ];
    ],
    General::shdw
];