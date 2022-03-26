#!/usr/bin/env wolframscript

BeginPackage[ "Wolfram`PacletCICD`Scripts`" ];

Get @ FileNameJoin @ { DirectoryName @ $InputFileName, "Common.wl" };

1/0;

checkResult @ Wolfram`PacletCICD`TestPaclet[
    FileNameJoin @ { $pacDir, "Tests" },
    "Target"          -> "Submit",
    "AnnotateTestIDs" -> True
];

EndPackage[ ];
