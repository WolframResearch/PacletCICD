#!/usr/bin/env wolframscript

BeginPackage[ "Wolfram`PacletCICD`Scripts`" ];

Get @ FileNameJoin @ { DirectoryName @ $InputFileName, "Common.wl" };
checkResult @ Wolfram`PacletCICD`CheckPaclet[
    $defNB,
    "Target"           -> "Submit",
    "FailureCondition" -> { "Warning", "Error" }
];

Print[ "::notice::Finished CheckPaclet" ];

EndPackage[ ];


Print[ "::notice::Killing any remaining FE process" ];
fePID = UsingFrontEnd @ SystemInformation[ "FrontEnd", "ProcessID" ];
feProc = First[ SystemProcesses[ "PID" -> fePID ], $Failed ];
KillProcess @ feProc;

Print[ "::notice::Exiting ", $InputFileName ];
Exit[ 0 ];