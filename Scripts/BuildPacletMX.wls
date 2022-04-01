#!/usr/bin/env wolframscript

(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::SuspiciousSessionSymbol:: *)

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Initialization*)
$pacDir = DirectoryName[ $InputFileName, 2 ];
$resDir = FileNameJoin @ { $pacDir, "Resources" };
PacletDirectoryLoad @ $pacDir;
Get[ "Wolfram`PacletCICD`" ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Pre-cache Values*)

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*normalizeActionName*)
LoadSubPackage[ "Workflows" ];

Print[ "Caching values for normalizeActionName:" ];
Scan[
    Echo @* Wolfram`PacletCICD`Private`normalizeActionName,
    {
        "rhennigan/build-paclet@latest",
        "rhennigan/check-paclet@latest",
        "rhennigan/submit-paclet@latest",
        "rhennigan/test-paclet@latest"
    }
];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*wlResource*)
Print[ "Caching values for wlResource:" ];
Block[ { Wolfram`PacletCICD`Private`$resourceDirectory = $resDir },
    Scan[ Echo @* Wolfram`PacletCICD`Private`wlResource @* FileBaseName ,
          FileNames[ "*.wl", $resDir ]
    ]
];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*wxfResource*)
Print[ "Caching values for wxfResource:" ];
Block[ { Wolfram`PacletCICD`Private`$resourceDirectory = $resDir },
    Scan[ Echo @* Wolfram`PacletCICD`Private`wxfResource @* FileBaseName ,
          FileNames[ "*.wxf", $resDir ]
    ]
];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*actionIcon*)
LoadSubPackage[ "Formatting" ];

Print[ "Caching values for actionIcon:" ];
Scan[
    Echo @* Wolfram`PacletCICD`Private`actionIcon,
    {
        "actions/cache",
        "actions/checkout",
        "actions/create-release",
        "actions/download-artifact",
        "actions/upload-artifact",
        "actions/upload-release-asset",
        "rhennigan/build-paclet",
        "rhennigan/check-paclet",
        "rhennigan/submit-paclet",
        "rhennigan/test-paclet"
    }
];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Build*)
Print @ Wolfram`PacletCICD`Internal`BuildMX[ ];
(* :!CodeAnalysis::EndBlock:: *)
