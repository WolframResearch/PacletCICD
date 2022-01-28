#!/usr/bin/env wolframscript

(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::SuspiciousSessionSymbol:: *)

$pacDir = DirectoryName[ $InputFileName, 2 ];

Quiet[
(* Temporary *)
PacletInstall[ "https://www.wolframcloud.com/obj/rhennigan/Paclets/DefinitionNotebookClient.paclet" ];
PacletInstall[ "https://www.wolframcloud.com/obj/rhennigan/Paclets/PacletResource.paclet" ];
PacletDirectoryLoad @ $pacDir;
];

Needs[ "Wolfram`PacletCICD`" ];

Print[ "GITHUB_ENV=", Environment[ "GITHUB_ENV" ] ];

EchoEvaluation @ Wolfram`PacletCICD`BuildPaclet[
    File @ FileNameJoin @ { $pacDir, "DefinitionNotebook.nb" },
    "Preflight"   -> True,
    "Target"      -> "Submit",
    "ConsoleType" -> "GitHub"
]

(* :!CodeAnalysis::EndBlock:: *)