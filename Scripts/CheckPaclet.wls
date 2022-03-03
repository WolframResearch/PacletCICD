#!/usr/bin/env wolframscript

(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::SuspiciousSessionSymbol:: *)

$pacDir = DirectoryName[ $InputFileName, 2 ];

PacletDirectoryLoad @ $pacDir;

Needs[ "Wolfram`PacletCICD`" ];

result = Wolfram`PacletCICD`CheckPaclet[
    File @ FileNameJoin @ { $pacDir, "ResourceDefinition.nb" },
    "Target" -> "Submit"
];

If[ MatchQ[ result, _Wolfram`PacletCICD`CheckPaclet ],
    print[ "::error::Wolfram`PacletCICD`CheckPaclet not defined" ];
    Exit[ 1 ],
    print @ result
];

(* :!CodeAnalysis::EndBlock:: *)