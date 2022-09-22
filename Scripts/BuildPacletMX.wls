#!/usr/bin/env wolframscript

(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::SuspiciousSessionSymbol:: *)

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Initialization*)
$pacDir = DirectoryName[ $InputFileName, 2 ];
$mxDir = FileNameJoin @ { $pacDir, "Kernel", ToString @ $SystemWordLength <> "Bit" };
If[ DirectoryQ @ $mxDir, DeleteDirectory[ $mxDir, DeleteContents -> True ] ];

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
    Wolfram`PacletCICD`Private`normalizeActionName,
    {
        "WolframResearch/build-paclet@latest",
        "WolframResearch/check-paclet@latest",
        "WolframResearch/submit-paclet@latest",
        "WolframResearch/test-paclet@latest"
    }
];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*wlResource*)
Print[ "Caching values for wlResource:" ];
Block[ { Wolfram`PacletCICD`Private`$resourceDirectory = $resDir },
    Scan[ Wolfram`PacletCICD`Private`wlResource @* FileBaseName ,
          FileNames[ "*.wl", $resDir ]
    ]
];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*wxfResource*)
Print[ "Caching values for wxfResource:" ];
Block[ { Wolfram`PacletCICD`Private`$resourceDirectory = $resDir },
    Scan[ Wolfram`PacletCICD`Private`wxfResource @* FileBaseName ,
          FileNames[ "*.wxf", $resDir ]
    ]
];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*actionIcon*)
LoadSubPackage[ "Formatting" ];

Print[ "Caching values for actionIcon:" ];
Scan[
    Wolfram`PacletCICD`Private`actionIcon,
    {
        "actions/cache",
        "actions/checkout",
        "actions/create-release",
        "actions/download-artifact",
        "actions/upload-artifact",
        "actions/upload-release-asset",
        "WolframResearch/build-paclet",
        "WolframResearch/check-paclet",
        "WolframResearch/submit-paclet",
        "WolframResearch/test-paclet"
    }
];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Build*)
Print @ Wolfram`PacletCICD`Internal`BuildMX[ ];
(* :!CodeAnalysis::EndBlock:: *)
