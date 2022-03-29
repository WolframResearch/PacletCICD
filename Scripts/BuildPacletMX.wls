#!/usr/bin/env wolframscript

(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::SuspiciousSessionSymbol:: *)
PacletDirectoryLoad @ DirectoryName[ $InputFileName, 2 ];
Get[ "Wolfram`PacletCICD`" ];
Print @ Wolfram`PacletCICD`Internal`BuildMX[ ];
(* :!CodeAnalysis::EndBlock:: *)
