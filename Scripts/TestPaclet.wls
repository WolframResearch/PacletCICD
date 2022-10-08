#!/usr/bin/env wolframscript

BeginPackage[ "Wolfram`PacletCICD`Scripts`" ];

Get @ FileNameJoin @ { DirectoryName @ $InputFileName, "Common.wl" };
checkResult @ Wolfram`PacletCICD`TestPaclet[
    FileNameJoin @ { $pacDir, "Tests" },
    "Target"          -> "Submit",
    "AnnotateTestIDs" -> True
];

Wolfram`PacletCICD`$WorkflowValueScope = "Workflow";
Wolfram`PacletCICD`WorkflowValue[ "TestValue" ][ "MachineName" ] = $MachineName;
Wolfram`PacletCICD`WorkflowValue[ "TestValue" ][ "SystemID"    ] = $SystemID;
Wolfram`PacletCICD`WorkflowValue[ "TestValue" ][ "Job"         ] = Environment[ "GITHUB_JOB" ];

EndPackage[ ];
