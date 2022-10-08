#!/usr/bin/env wolframscript

BeginPackage[ "Wolfram`PacletCICD`Scripts`" ];

Get @ FileNameJoin @ { DirectoryName @ $InputFileName, "Common.wl" };
checkResult @ Wolfram`PacletCICD`CheckPaclet[
    $defNB,
    "Target"           -> "Submit",
    "FailureCondition" -> { "Warning", "Error" }
];

job  = Environment[ "GITHUB_JOB" ];
import = <|
    "Check" -> Wolfram`PacletCICD`WorkflowValue[ "Check_Data" ],
    "Build" -> Wolfram`PacletCICD`WorkflowValue[ "Build_Data" ],
    "TestValue" -> Wolfram`PacletCICD`WorkflowValue[ "TestValue" ]
|>;
data = <| "MachineName" -> $MachineName, "SystemID" -> $SystemID, "Job" -> job, "Imported" -> import |>;
Wolfram`PacletCICD`$WorkflowValueScope = "Workflow";
AppendTo[ Wolfram`PacletCICD`WorkflowValue[ "TestValue" ], data ];
Wolfram`PacletCICD`WorkflowValue[ job<>"_Data" ] = data;

EndPackage[ ];
