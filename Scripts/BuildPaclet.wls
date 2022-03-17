#!/usr/bin/env wolframscript

(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::SuspiciousSessionSymbol:: *)

$pacDir = DirectoryName[ $InputFileName, 2 ];

PacletDirectoryLoad @ $pacDir;

Needs[ "Wolfram`PacletCICD`" ];
Needs[ "ResourceSystemClient`" -> None ];

Print[ "GITHUB_ENV=", Environment[ "GITHUB_ENV" ] ];
Print[ "RESOURCE_PUBLISHER_TOKEN=", ResourceSystemClient`$PublisherToken ];

result = Wolfram`PacletCICD`BuildPaclet[
    File @ FileNameJoin @ { $pacDir, "ResourceDefinition.nb" },
    "Check"  -> False,
    "Target" -> "Submit"
];

If[ MatchQ[ result, _Wolfram`PacletCICD`BuildPaclet ],
    Print[ "::error::Wolfram`PacletCICD`BuildPaclet not defined" ];
    Exit[ 1 ],
    Print @ result
];

(* :!CodeAnalysis::EndBlock:: *)