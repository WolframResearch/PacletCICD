#!/usr/bin/env wolframscript

Get @ FileNameJoin @ { DirectoryName @ $InputFileName, "Common.wl" };
checkResult @ Wolfram`PacletCICD`SubmitPaclet[
    $defNB,
    "ExitOnFail" -> True
];