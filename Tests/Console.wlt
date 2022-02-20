(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Initialization*)
VerificationTest[
    PacletObjectQ @ PacletObject @ File[
        $pacletDir = DirectoryName[ $TestFileName, 2 ]
    ],
    TestID -> "Initialize-Paclet"
]

VerificationTest[
    PacletDirectoryLoad @ $pacletDir,
    { ___, $pacletDir, ___ },
    SameTest -> MatchQ,
    TestID -> "Initialize-Paclet-Directory"
]

VerificationTest[
    Needs[ "Wolfram`PacletCICD`" ],
    Null,
    TestID -> "Initialize-Needs"
]

(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::SuspiciousSessionSymbol:: *)
VerificationTest[
    traceWrites // Attributes = { HoldFirst };
    traceWrites[ eval_ ] :=
        Module[ { $writes, res, writes },
            $writes = Internal`Bag[ ];
            Block[ { WriteString, Print },

                WriteString =
                    Function[
                        Internal`StuffBag[
                            $writes,
                            HoldComplete @ WriteString @ ##1
                        ]
                    ];


                Print =
                    Internal`StuffBag[ $writes, HoldComplete @ Print @ ##1 ] &;

                res = eval;
                writes = Internal`BagPart[ $writes, All ];
                <| "Result" -> res, "Writes" -> writes |>
            ]
        ];,
    Null,
    TestID -> "Definition-TraceWrites"
]

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Context Check*)
VerificationTest[
    Context @ ConsoleError,
    "Wolfram`PacletCICD`",
    TestID -> "ConsoleError-Context"
]

VerificationTest[
    Context @ ConsoleWarning,
    "Wolfram`PacletCICD`",
    TestID -> "ConsoleWarning-Context"
]

VerificationTest[
    Context @ ConsoleNotice,
    "Wolfram`PacletCICD`",
    TestID -> "ConsoleNotice-Context"
]

VerificationTest[
    Context @ ConsoleLog,
    "Wolfram`PacletCICD`",
    TestID -> "ConsoleLog-Context"
]

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*ConsoleError*)
VerificationTest[
    traceWrites @ ConsoleError[ "error", "ConsoleType" -> "Notebook" ],
    KeyValuePattern @ {
        "Result" -> Null,
        "Writes" -> { HoldComplete @ Print[ __, "error" ] }
    },
    SameTest -> MatchQ,
    TestID   -> "ConsoleError-Notebook"
]

VerificationTest[
    traceWrites @ ConsoleError[ "error", "ConsoleType" -> "TTY" ],
    KeyValuePattern @ {
        "Result" -> Null,
        "Writes" -> {
            HoldComplete @ WriteString[
                OutputStream[ "stderr", _ ],
                ___,
                "error",
                "\n"
            ]
        }
    },
    SameTest -> MatchQ,
    TestID   -> "ConsoleError-TTY"
]

VerificationTest[
    traceWrites @ ConsoleError[ "message", "ConsoleType" -> "GitHub" ],
    KeyValuePattern @ {
        "Result" -> Null,
        "Writes" -> {
            HoldComplete @ WriteString[
                OutputStream[ "stdout", _ ],
                "::error ::message",
                "\n"
            ]
        }
    },
    SameTest -> MatchQ,
    TestID   -> "ConsoleError-GitHub"
]

VerificationTest[
    traceWrites @ ConsoleError[
        "message",
        "path/to/file.wl",
        "ConsoleType" -> "GitHub"
    ],
    KeyValuePattern @ {
        "Result" -> Null,
        "Writes" -> {
            HoldComplete @ WriteString[
                OutputStream[ "stdout", _ ],
                "::error file=path/to/file.wl::message",
                "\n"
            ]
        }
    },
    SameTest -> MatchQ,
    TestID   -> "ConsoleError-GitHub-File"
]

VerificationTest[
    traceWrites @ ConsoleError[
        "message",
        "path/to/file.wl",
        123,
        "ConsoleType" -> "GitHub"
    ],
    KeyValuePattern @ {
        "Result" -> Null,
        "Writes" -> {
            HoldComplete @ WriteString[
                OutputStream[ "stdout", _ ],
                "::error file=path/to/file.wl,line=123,endLine=123,col=1::message",
                "\n"
            ]
        }
    },
    SameTest -> MatchQ,
    TestID   -> "ConsoleError-GitHub-File-Line"
]

VerificationTest[
    traceWrites @ ConsoleError[
        "message",
        "path/to/file.wl",
        { 10, 20 },
        "ConsoleType" -> "GitHub"
    ],
    KeyValuePattern @ {
        "Result" -> Null,
        "Writes" -> {
            HoldComplete @ WriteString[
                OutputStream[ "stdout", _ ],
                "::error file=path/to/file.wl,line=10,endLine=20,col=1::message",
                "\n"
            ]
        }
    },
    SameTest -> MatchQ,
    TestID   -> "ConsoleError-GitHub-File-Lines"
]

VerificationTest[
    traceWrites @ ConsoleError[
        "message",
        "path/to/file.wl",
        { { 10, 5 }, { 20, 45 } },
        "ConsoleType" -> "GitHub"
    ],
    KeyValuePattern @ {
        "Result" -> Null,
        "Writes" -> {
            HoldComplete @ WriteString[
                OutputStream[ "stdout", _ ],
                "::error file=path/to/file.wl,line=10,endLine=20,col=5,endColumn=45::message",
                "\n"
            ]
        }
    },
    SameTest -> MatchQ,
    TestID   -> "ConsoleError-GitHub-File-Lines-Columns"
]

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*ConsoleWarning*)
VerificationTest[
    traceWrites @ ConsoleWarning[ "message text", "ConsoleType" -> "Notebook" ],
    KeyValuePattern @ {
        "Result" -> Null,
        "Writes" -> { HoldComplete @ Print[ __, "message text" ] }
    },
    SameTest -> MatchQ,
    TestID   -> "ConsoleWarning-Notebook"
]

VerificationTest[
    traceWrites @ ConsoleWarning[ "message text", "ConsoleType" -> "TTY" ],
    KeyValuePattern @ {
        "Result" -> Null,
        "Writes" -> {
            HoldComplete @ WriteString[
                OutputStream[ "stderr", _ ],
                ___,
                "message text",
                "\n"
            ]
        }
    },
    SameTest -> MatchQ,
    TestID   -> "ConsoleWarning-TTY"
]

VerificationTest[
    traceWrites @ ConsoleWarning[ "message text", "ConsoleType" -> "GitHub" ],
    KeyValuePattern @ {
        "Result" -> Null,
        "Writes" -> {
            HoldComplete @ WriteString[
                OutputStream[ "stdout", _ ],
                "::warning ::message text",
                "\n"
            ]
        }
    },
    SameTest -> MatchQ,
    TestID   -> "ConsoleWarning-GitHub"
]

VerificationTest[
    traceWrites @ ConsoleWarning[
        "message text",
        "path/to/file.wl",
        "ConsoleType" -> "GitHub"
    ],
    KeyValuePattern @ {
        "Result" -> Null,
        "Writes" -> {
            HoldComplete @ WriteString[
                OutputStream[ "stdout", _ ],
                "::warning file=path/to/file.wl::message text",
                "\n"
            ]
        }
    },
    SameTest -> MatchQ,
    TestID   -> "ConsoleWarning-GitHub-File"
]

VerificationTest[
    traceWrites @ ConsoleWarning[
        "message text",
        "path/to/file.wl",
        123,
        "ConsoleType" -> "GitHub"
    ],
    KeyValuePattern @ {
        "Result" -> Null,
        "Writes" -> {
            HoldComplete @ WriteString[
                OutputStream[ "stdout", _ ],
                "::warning file=path/to/file.wl,line=123,endLine=123,col=1::message text",
                "\n"
            ]
        }
    },
    SameTest -> MatchQ,
    TestID   -> "ConsoleWarning-GitHub-File-Line"
]

VerificationTest[
    traceWrites @ ConsoleWarning[
        "message text",
        "path/to/file.wl",
        { 10, 20 },
        "ConsoleType" -> "GitHub"
    ],
    KeyValuePattern @ {
        "Result" -> Null,
        "Writes" -> {
            HoldComplete @ WriteString[
                OutputStream[ "stdout", _ ],
                "::warning file=path/to/file.wl,line=10,endLine=20,col=1::message text",
                "\n"
            ]
        }
    },
    SameTest -> MatchQ,
    TestID   -> "ConsoleWarning-GitHub-File-Lines"
]

VerificationTest[
    traceWrites @ ConsoleWarning[
        "message text",
        "path/to/file.wl",
        { { 10, 5 }, { 20, 45 } },
        "ConsoleType" -> "GitHub"
    ],
    KeyValuePattern @ {
        "Result" -> Null,
        "Writes" -> {
            HoldComplete @ WriteString[
                OutputStream[ "stdout", _ ],
                "::warning file=path/to/file.wl,line=10,endLine=20,col=5,endColumn=45::message text",
                "\n"
            ]
        }
    },
    SameTest -> MatchQ,
    TestID   -> "ConsoleWarning-GitHub-File-Lines-Columns"
]

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*ConsoleNotice*)
VerificationTest[
    traceWrites @ ConsoleNotice[ "message text", "ConsoleType" -> "Notebook" ],
    KeyValuePattern @ {
        "Result" -> Null,
        "Writes" -> { HoldComplete @ Print[ __, "message text" ] }
    },
    SameTest -> MatchQ,
    TestID   -> "ConsoleNotice-Notebook"
]

VerificationTest[
    traceWrites @ ConsoleNotice[ "message text", "ConsoleType" -> "TTY" ],
    KeyValuePattern @ {
        "Result" -> Null,
        "Writes" -> {
            HoldComplete @ WriteString[
                OutputStream[ "stderr", _ ],
                ___,
                "message text",
                "\n"
            ]
        }
    },
    SameTest -> MatchQ,
    TestID   -> "ConsoleNotice-TTY"
]

VerificationTest[
    traceWrites @ ConsoleNotice[ "message text", "ConsoleType" -> "GitHub" ],
    KeyValuePattern @ {
        "Result" -> Null,
        "Writes" -> {
            HoldComplete @ WriteString[
                OutputStream[ "stdout", _ ],
                "::notice ::message text",
                "\n"
            ]
        }
    },
    SameTest -> MatchQ,
    TestID   -> "ConsoleNotice-GitHub"
]

VerificationTest[
    traceWrites @ ConsoleNotice[
        "message text",
        "path/to/file.wl",
        "ConsoleType" -> "GitHub"
    ],
    KeyValuePattern @ {
        "Result" -> Null,
        "Writes" -> {
            HoldComplete @ WriteString[
                OutputStream[ "stdout", _ ],
                "::notice file=path/to/file.wl::message text",
                "\n"
            ]
        }
    },
    SameTest -> MatchQ,
    TestID   -> "ConsoleNotice-GitHub-File"
]

VerificationTest[
    traceWrites @ ConsoleNotice[
        "message text",
        "path/to/file.wl",
        123,
        "ConsoleType" -> "GitHub"
    ],
    KeyValuePattern @ {
        "Result" -> Null,
        "Writes" -> {
            HoldComplete @ WriteString[
                OutputStream[ "stdout", _ ],
                "::notice file=path/to/file.wl,line=123,endLine=123,col=1::message text",
                "\n"
            ]
        }
    },
    SameTest -> MatchQ,
    TestID   -> "ConsoleNotice-GitHub-File-Line"
]

VerificationTest[
    traceWrites @ ConsoleNotice[
        "message text",
        "path/to/file.wl",
        { 10, 20 },
        "ConsoleType" -> "GitHub"
    ],
    KeyValuePattern @ {
        "Result" -> Null,
        "Writes" -> {
            HoldComplete @ WriteString[
                OutputStream[ "stdout", _ ],
                "::notice file=path/to/file.wl,line=10,endLine=20,col=1::message text",
                "\n"
            ]
        }
    },
    SameTest -> MatchQ,
    TestID   -> "ConsoleNotice-GitHub-File-Lines"
]

VerificationTest[
    traceWrites @ ConsoleNotice[
        "message text",
        "path/to/file.wl",
        { { 10, 5 }, { 20, 45 } },
        "ConsoleType" -> "GitHub"
    ],
    KeyValuePattern @ {
        "Result" -> Null,
        "Writes" -> {
            HoldComplete @ WriteString[
                OutputStream[ "stdout", _ ],
                "::notice file=path/to/file.wl,line=10,endLine=20,col=5,endColumn=45::message text",
                "\n"
            ]
        }
    },
    SameTest -> MatchQ,
    TestID   -> "ConsoleNotice-GitHub-File-Lines-Columns"
]

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*ConsoleLog*)
VerificationTest[
    traceWrites @ ConsoleLog[ "message text", "ConsoleType" -> "Notebook" ],
    KeyValuePattern @ {
        "Result" -> Null,
        "Writes" -> { HoldComplete @ Print[ "", "", "message text" ] }
    },
    SameTest -> MatchQ,
    TestID   -> "ConsoleLog-Notebook"
]

VerificationTest[
    traceWrites @ ConsoleLog[ "message text", "ConsoleType" -> "TTY" ],
    KeyValuePattern @ {
        "Result" -> Null,
        "Writes" -> {
            HoldComplete @ WriteString[
                OutputStream[ "stderr", _ ],
                "",
                "",
                "message text",
                "\n"
            ]
        }
    },
    SameTest -> MatchQ,
    TestID   -> "ConsoleLog-TTY"
]

VerificationTest[
    traceWrites @ ConsoleLog[ "message text", "ConsoleType" -> "GitHub" ],
    KeyValuePattern @ {
        "Result" -> Null,
        "Writes" -> {
            HoldComplete @ WriteString[
                OutputStream[ "stdout", _ ],
                "message text",
                "\n"
            ]
        }
    },
    SameTest -> MatchQ,
    TestID   -> "ConsoleLog-GitHub"
]

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*GitHub Annotations*)
VerificationTest[
    traceWrites @ ConsoleLog[
        "message text",
        "path/to/file.wl",
        "ConsoleType" -> "GitHub"
    ],
    KeyValuePattern @ {
        "Result" -> Null,
        "Writes" -> {
            HoldComplete @ WriteString[
                OutputStream[ "stdout", _ ],
                "message text",
                "\n"
            ]
        }
    },
    SameTest -> MatchQ,
    TestID   -> "ConsoleLog-GitHub-File"
]

VerificationTest[
    traceWrites @ ConsoleLog[
        "message text",
        "path/to/file.wl",
        123,
        "ConsoleType" -> "GitHub"
    ],
    KeyValuePattern @ {
        "Result" -> Null,
        "Writes" -> {
            HoldComplete @ WriteString[
                OutputStream[ "stdout", _ ],
                "message text",
                "\n"
            ]
        }
    },
    SameTest -> MatchQ,
    TestID   -> "ConsoleLog-GitHub-File-Line"
]

VerificationTest[
    traceWrites @ ConsoleLog[
        "message text",
        "path/to/file.wl",
        { 10, 20 },
        "ConsoleType" -> "GitHub"
    ],
    KeyValuePattern @ {
        "Result" -> Null,
        "Writes" -> {
            HoldComplete @ WriteString[
                OutputStream[ "stdout", _ ],
                "message text",
                "\n"
            ]
        }
    },
    SameTest -> MatchQ,
    TestID   -> "ConsoleLog-GitHub-File-Lines"
]

VerificationTest[
    traceWrites @ ConsoleLog[
        "message text",
        "path/to/file.wl",
        { { 10, 5 }, { 20, 45 } },
        "ConsoleType" -> "GitHub"
    ],
    KeyValuePattern @ {
        "Result" -> Null,
        "Writes" -> {
            HoldComplete @ WriteString[
                OutputStream[ "stdout", _ ],
                "message text",
                "\n"
            ]
        }
    },
    SameTest -> MatchQ,
    TestID   -> "ConsoleLog-GitHub-File-Lines-Columns"
]

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*ConsoleLog Equivalence*)
VerificationTest[
    traceWrites @ ConsoleLog[
        "message text",
        "path/to/file.wl",
        { { 10, 5 }, { 20, 45 } },
        "ConsoleType" -> "GitHub",
        "Level"       -> "Error"
    ],
    traceWrites @ ConsoleError[
        "message text",
        "path/to/file.wl",
        { { 10, 5 }, { 20, 45 } },
        "ConsoleType" -> "GitHub"
    ],
    SameTest -> SameQ,
    TestID   -> "ConsoleLog-GitHub-Error"
]

VerificationTest[
    traceWrites @ ConsoleLog[
        "message text",
        "path/to/file.wl",
        { { 10, 5 }, { 20, 45 } },
        "ConsoleType" -> "GitHub",
        "Level"       -> "Warning"
    ],
    traceWrites @ ConsoleWarning[
        "message text",
        "path/to/file.wl",
        { { 10, 5 }, { 20, 45 } },
        "ConsoleType" -> "GitHub"
    ],
    SameTest -> SameQ,
    TestID   -> "ConsoleLog-GitHub-Warning"
]

VerificationTest[
    traceWrites @ ConsoleLog[
        "message text",
        "path/to/file.wl",
        { { 10, 5 }, { 20, 45 } },
        "ConsoleType" -> "GitHub",
        "Level"       -> "Notice"
    ],
    traceWrites @ ConsoleNotice[
        "message text",
        "path/to/file.wl",
        { { 10, 5 }, { 20, 45 } },
        "ConsoleType" -> "GitHub"
    ],
    SameTest -> SameQ,
    TestID   -> "ConsoleLog-GitHub-Notice"
]

(* :!CodeAnalysis::EndBlock:: *)