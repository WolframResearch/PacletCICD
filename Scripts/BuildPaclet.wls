#!/usr/bin/env wolframscript

BeginPackage[ "Wolfram`PacletCICD`Scripts`" ];

Get @ FileNameJoin @ { DirectoryName @ $InputFileName, "Common.wl" };
checkResult @ Wolfram`PacletCICD`BuildPaclet[
    $defNB,
    "Check"      -> False,
    "ExitOnFail" -> True,
    "Target"     -> "Submit"
];

 Wolfram`PacletCICD`WorkflowValue[ "TestValue", "Workflow" ][ "MachineName" ] = $MachineName;
 Wolfram`PacletCICD`WorkflowValue[ "TestValue", "Workflow" ][ "SystemID"    ] = $SystemID;
 Wolfram`PacletCICD`WorkflowValue[ "TestValue", "Workflow" ][ "Timestamp"   ] = Now;

EndPackage[ ];
