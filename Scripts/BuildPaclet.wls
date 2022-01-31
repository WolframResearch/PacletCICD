#!/usr/bin/env wolframscript

(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::SuspiciousSessionSymbol:: *)

$pacDir = DirectoryName[ $InputFileName, 2 ];

PacletDirectoryLoad @ $pacDir;

Needs[ "Wolfram`PacletCICD`" ];

Print[ "GITHUB_ENV=", Environment[ "GITHUB_ENV" ] ];

result = Wolfram`PacletCICD`BuildPaclet[
    File @ FileNameJoin @ { $pacDir, "DefinitionNotebook.nb" },
    "Check"  -> False,
    "Target" -> "Submit"
];

If[ MatchQ[ result, _Wolfram`PacletCICD`BuildPaclet ],
    print[ "::error::Wolfram`PacletCICD`BuildPaclet not defined" ];
    Exit[ 1 ],
    print @ result
];

(* :!CodeAnalysis::EndBlock:: *)