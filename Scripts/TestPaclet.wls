#!/usr/bin/env wolframscript

BeginPackage[ "Wolfram`PacletCICD`Scripts`" ];

Get @ FileNameJoin @ { DirectoryName @ $InputFileName, "Common.wl" };

checkResult @ Wolfram`PacletCICD`TestPaclet[
    FileNameJoin @ { $pacDir, "Tests" },
    "Target"          -> "Submit",
    "AnnotateTestIDs" -> True
];

EndPackage[ ];
