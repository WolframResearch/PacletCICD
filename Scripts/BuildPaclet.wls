#!/usr/bin/env wolframscript

Get @ FileNameJoin @ { DirectoryName @ $InputFileName, "Common.wl" };
checkResult @ Wolfram`PacletCICD`BuildPaclet[
    $defNB,
    "Check"      -> False,
    "ExitOnFail" -> True,
    "Target"     -> "Submit"
];