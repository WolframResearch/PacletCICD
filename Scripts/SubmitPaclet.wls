#!/usr/bin/env wolframscript

(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::SuspiciousSessionSymbol:: *)

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Definitions*)
Get @ FileNameJoin @ { DirectoryName @ $InputFileName, "Common.wl" };

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Run*)
checkResult @ Wolfram`PacletCICD`SubmitPaclet @ $defNB;

(* :!CodeAnalysis::EndBlock:: *)