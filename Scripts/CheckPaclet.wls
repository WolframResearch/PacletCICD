#!/usr/bin/env wolframscript

(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::SuspiciousSessionSymbol:: *)

$pacDir = DirectoryName[ $InputFileName, 2 ];

PacletDirectoryLoad @ $pacDir;

Needs[ "Wolfram`PacletCICD`" ];
Needs[ "ResourceSystemClient`" -> None ];
$ResourceSystemBase = "https://www.wolframcloud.com/obj/rhennigan/api/1.0";

Print[ "GITHUB_ENV=", Environment[ "GITHUB_ENV" ] ];
Print[ "RESOURCE_PUBLISHER_TOKEN=", ResourceSystemClient`$PublisherToken ];

result = Wolfram`PacletCICD`CheckPaclet[
    File @ FileNameJoin @ { $pacDir, "ResourceDefinition.nb" },
    "Target" -> "Submit"
];

If[ MatchQ[ result, _Wolfram`PacletCICD`CheckPaclet ],
    Print[ "::error::Wolfram`PacletCICD`CheckPaclet not defined" ];
    Exit[ 1 ],
    Print @ result
];

(* :!CodeAnalysis::EndBlock:: *)