(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Header*)
BeginPackage[ "Wolfram`PacletCICD`" ];
EndPackage[ ];

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
(*Check Dependencies*)
If[ $VersionNumber < 13.1,
    Wolfram`PacletCICD`CheckDependencies[
        PacletObject @ File @ DirectoryName[ $InputFileName, 2 ],
        Message -> True
    ]
];
