#!/usr/bin/env wolframscript

BeginPackage[ "Wolfram`PacletCICD`Scripts`" ];

Get @ FileNameJoin @ { DirectoryName @ $InputFileName, "Common.wl" };
checkResult @ Wolfram`PacletCICD`CheckPaclet[
    $defNB,
    "Target"           -> "Submit",
    "FailureCondition" -> { "Warning", "Error" }
];

 Wolfram`PacletCICD`WorkflowValue[ "TestValue", "Workflow" ][ "MachineName" ] = $MachineName;
 Wolfram`PacletCICD`WorkflowValue[ "TestValue", "Workflow" ][ "SystemID"    ] = $SystemID;
 Wolfram`PacletCICD`WorkflowValue[ "TestValue", "Workflow" ][ "Timestamp"   ] = Now;

EndPackage[ ];
