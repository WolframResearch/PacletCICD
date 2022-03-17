#!/usr/bin/env wolframscript

(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::SuspiciousSessionSymbol:: *)

$pacDir = DirectoryName[ $InputFileName, 2 ];

PacletDirectoryLoad @ $pacDir;

Needs[ "Wolfram`PacletCICD`" ];
Needs[ "ResourceSystemClient`" -> None ];
$ResourceSystemBase = Environment[ "RESOURCE_SYSTEM_BASE" ];

result = Wolfram`PacletCICD`SubmitPaclet[
    File @ FileNameJoin @ { $pacDir, "ResourceDefinition.nb" }
];

If[ MatchQ[ result, _Wolfram`PacletCICD`SubmitPaclet ],
    Print[ "::error::Wolfram`PacletCICD`SubmitPaclet not defined" ];
    Exit[ 1 ],
    Print @ result
];

(* :!CodeAnalysis::EndBlock:: *)