(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Header*)
BeginPackage[ "Wolfram`PacletCICD`" ];
EndPackage[ ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Install Dependencies*)
If[ StringQ @ Environment[ "GITHUB_WORKFLOW" ] && $VersionNumber < 14,
    Quiet[
        PacletInstall[ "https://github.com/WolframResearch/PacletCICD/releases/download/DefinitionNotebookClient-1.18.0/DefinitionNotebookClient-1.18.0.paclet" ];
        PacletInstall[ "https://github.com/WolframResearch/PacletCICD/releases/download/PacletResource-1.6.0/PacletResource-1.6.0.paclet"                       ];
        ,
        PacletInstall::samevers
    ]
];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Load Package*)
Wolfram`PacletCICD`Internal`$MXFile =
    FileNameJoin @ {
        DirectoryName @ $InputFileName,
        ToString @ $SystemWordLength <> "Bit",
        "PacletCICD.mx"
    };

If[ FileExistsQ[ Wolfram`PacletCICD`Internal`$MXFile ]
    ,
    Wolfram`PacletCICD`Internal`$MX = True;
    Get[ Wolfram`PacletCICD`Internal`$MXFile ]
    ,
    Wolfram`PacletCICD`Internal`$MX = False;
    Quiet[
        Block[ { $ContextPath },
            Get[ "Wolfram`PacletCICD`Symbols`"       ];
            Get[ "Wolfram`PacletCICD`Config`"        ];
            Get[ "Wolfram`PacletCICD`ErrorHandling`" ];
            Get[ "Wolfram`PacletCICD`Loading`"       ];
            Get[ "Wolfram`PacletCICD`Utilities`"     ];
        ],
        General::shdw
    ]
];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Initialize Workflow Values*)
If[ StringQ @ Environment[ "GITHUB_WORKFLOW" ],
    Wolfram`PacletCICD`InitializeWorkflowValues[ ]
];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Check Dependencies*)
If[ $VersionNumber < 13.2,
    Wolfram`PacletCICD`CheckDependencies[
        PacletObject @ File @ DirectoryName[ $InputFileName, 2 ],
        Message -> True
    ]
];
