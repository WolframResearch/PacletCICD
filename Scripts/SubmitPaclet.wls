#!/usr/bin/env wolframscript

BeginPackage[ "Wolfram`PacletCICD`Scripts`" ];

Get @ FileNameJoin @ { DirectoryName @ $InputFileName, "Common.wl" };

Needs[ "PacletResource`" -> None ];
SetOptions[ PacletResource`Notebooks`ProcessNotebookForEmbedding, "EmbeddedHTMLImages" -> True ];

checkResult @ Wolfram`PacletCICD`SubmitPaclet[ $defNB, "ExitOnFail" -> True ];

EndPackage[ ];
