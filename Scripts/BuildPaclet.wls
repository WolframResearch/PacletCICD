#!/usr/bin/env wolframscript

BeginPackage[ "Wolfram`PacletCICD`Scripts`" ];

Get @ FileNameJoin @ { DirectoryName @ $InputFileName, "Common.wl" };
checkResult @ Wolfram`PacletCICD`BuildPaclet[
    $defNB,
    "Check"      -> False,
    "ExitOnFail" -> True,
    "Target"     -> "Submit"
];

Wolfram`PacletCICD`$WorkflowValueScope = "Workflow";
Wolfram`PacletCICD`WorkflowValue[ "TestValue" ][ "MachineName" ] = $MachineName;
Wolfram`PacletCICD`WorkflowValue[ "TestValue" ][ "SystemID"    ] = $SystemID;
Wolfram`PacletCICD`WorkflowValue[ "TestValue" ][ "Job"         ] = Environment[ "GITHUB_JOB" ];

EndPackage[ ];
