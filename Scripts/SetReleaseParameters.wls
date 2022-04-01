#!/usr/bin/env wolframscript

(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::SuspiciousSessionSymbol:: *)

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Initialization*)
$pacDir = DirectoryName[ $InputFileName, 2 ];
PacletDirectoryLoad @ $pacDir;
Get[ "Wolfram`PacletCICD`" ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Get Release Directory*)
$releaseDir = FileNameJoin @ { $pacDir, "Release" };

If[ ! DirectoryQ @ $releaseDir,
    Print[ "::error::Release directory ", $releaseDir, " does not exist!" ];
    Exit[ 1 ],
    Print @ StringJoin[
        "Release Files: \n\t",
        StringRiffle[ FileNames[ All, $releaseDir, Infinity ], "\n\t" ]
    ]
];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Get Paclet Archive*)
$releasePaclet = First[ FileNames[ "*.paclet", $releaseDir ], $Failed ];

If[ ! FileExistsQ @ $releasePaclet,
    Print[ "::error::Release paclet file not found!" ];
    Exit[ 1 ]
];

$releaseFile = FileNameTake @ $releasePaclet;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Get Release Version*)
$paclet = PacletObject @ File @ $pacDir;
If[ ! PacletObjectQ @ $paclet,
    Print[ "::error::Paclet directory failure: " <> ToString @ $pacDir ];
    Exit[ 1 ]
];

$version = $paclet[ "Version" ];

If[ ! StringQ @ $version && ! StringMatchQ[ $version, (DigitCharacter|".").. ],
    Print[ "::error::Paclet version failure: " <> ToString @ $version ];
    Exit[ 1 ]
];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Set Parameters*)
Wolfram`PacletCICD`Private`setOutput[ "PACLET_BUILD_DIR"  , $releaseDir    ];
Wolfram`PacletCICD`Private`setOutput[ "PACLET_PATH"       , $releasePaclet ];
Wolfram`PacletCICD`Private`setOutput[ "PACLET_FILE"       , $releaseFile   ];
Wolfram`PacletCICD`Private`setOutput[ "PACLET_RELEASE_TAG", "v"<>$version  ];

(* :!CodeAnalysis::EndBlock:: *)
