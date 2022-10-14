#!/usr/bin/env wolframscript

BeginPackage[ "Wolfram`PacletCICD`Scripts`" ];

Get @ FileNameJoin @ { DirectoryName @ $InputFileName, "Common.wl" };
checkResult @ Wolfram`PacletCICD`CheckPaclet[
    $defNB,
    "Target"           -> "Submit",
    "FailureCondition" -> { "Warning", "Error" }
];

Print[ "::notice::Finished CheckPaclet" ];

EndPackage[ ];

Print[ "::notice::Exiting ", $InputFileName ];
Exit[ 0 ];