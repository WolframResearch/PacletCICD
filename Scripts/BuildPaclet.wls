#!/usr/bin/env wolframscript

BeginPackage[ "Wolfram`PacletCICD`Scripts`" ];

Get @ FileNameJoin @ { DirectoryName @ $InputFileName, "Common.wl" };
checkResult @ Wolfram`PacletCICD`BuildPaclet[
    $defNB,
    "Check"      -> False,
    "ExitOnFail" -> True,
    "Target"     -> "Submit"
];

now = Now;
AppendTo[ Wolfram`PacletCICD`WorkflowValue[ "Debug/StepTest"    , "Step"      ], now ];
AppendTo[ Wolfram`PacletCICD`WorkflowValue[ "Debug/JobTest"     , "Job"       ], now ];
AppendTo[ Wolfram`PacletCICD`WorkflowValue[ "Debug/WorkflowTest", "Workflow"  ], now ];

EndPackage[ ];
