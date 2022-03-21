#!/usr/bin/env wolframscript

Off[ General::shdw ];
Get @ FileNameJoin @ { DirectoryName @ $InputFileName, "Common.wl" };
checkResult @ Wolfram`PacletCICD`TestPaclet[
    FileNameJoin @ { $pacDir, "Tests" },
    "Target"          -> "Submit",
    "AnnotateTestIDs" -> True
];