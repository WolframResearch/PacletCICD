#!/usr/bin/env wolframscript

(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::SuspiciousSessionSymbol:: *)

$pacDir = DirectoryName[ $InputFileName, 2 ];

PacletDirectoryLoad @ $pacDir;

Needs[ "Wolfram`PacletCICD`" ];
Needs[ "ResourceSystemClient`" -> None ];

Print[ "GITHUB_ENV=", Environment[ "GITHUB_ENV" ] ];
Print[ "RESOURCE_PUBLISHER_TOKEN=", ResourceSystemClient`$PublisherToken ];

result = Wolfram`PacletCICD`TestPaclet[ FileNameJoin @ { $pacDir, "Tests" } ];

If[ MatchQ[ result, _Wolfram`PacletCICD`TestPaclet ],
    print[ "::error::Wolfram`PacletCICD`TestPaclet not defined" ];
    Exit[ 1 ],
    print @ result
];

(* :!CodeAnalysis::EndBlock:: *)