#!/usr/bin/env wolframscript

Get @ FileNameJoin @ { DirectoryName @ $InputFileName, "Common.wl" };
checkResult @ Wolfram`PacletCICD`TestPaclet[
    FileNameJoin @ { $pacDir, "Tests" },
    "Target"          -> "Submit",
    "AnnotateTestIDs" -> True
];