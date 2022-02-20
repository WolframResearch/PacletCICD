(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Initialization*)
VerificationTest[
    PacletObjectQ @ PacletObject @ File[
        $pacletDir = DirectoryName[ $TestFileName, 2 ]
    ],
    TestID -> "Initialize-Paclet@@Tests/Console.wlt:4,1-9,2"
]

VerificationTest[
    PacletDirectoryLoad @ $pacletDir,
    { ___, $pacletDir, ___ },
    SameTest -> MatchQ,
    TestID -> "Initialize-Paclet-Directory@@Tests/Console.wlt:11,1-16,2"
]

VerificationTest[
    Needs[ "Wolfram`PacletCICD`" ],
    Null,
    TestID -> "Initialize-Needs@@Tests/Console.wlt:18,1-22,2"
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
    TestID -> "Definition-TraceWrites@@Tests/Console.wlt:26,1-52,2"
]

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Context Check*)
VerificationTest[
    Context @ ConsoleError,
    "Wolfram`PacletCICD`",
    TestID -> "ConsoleError-Context@@Tests/Console.wlt:57,1-61,2"
]

VerificationTest[
    Context @ ConsoleWarning,
    "Wolfram`PacletCICD`",
    TestID -> "ConsoleWarning-Context@@Tests/Console.wlt:63,1-67,2"
]

VerificationTest[
    Context @ ConsoleNotice,
    "Wolfram`PacletCICD`",
    TestID -> "ConsoleNotice-Context@@Tests/Console.wlt:69,1-73,2"
]

VerificationTest[
    Context @ ConsoleLog,
    "Wolfram`PacletCICD`",
    TestID -> "ConsoleLog-Context@@Tests/Console.wlt:75,1-79,2"
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
    TestID   -> "ConsoleError-Notebook@@Tests/Console.wlt:84,1-92,2"
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
    TestID   -> "ConsoleError-TTY@@Tests/Console.wlt:94,1-109,2"
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
    TestID   -> "ConsoleError-GitHub@@Tests/Console.wlt:111,1-125,2"
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
    TestID   -> "ConsoleError-GitHub-File@@Tests/Console.wlt:127,1-145,2"
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
    TestID   -> "ConsoleError-GitHub-File-Line@@Tests/Console.wlt:147,1-166,2"
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
    TestID   -> "ConsoleError-GitHub-File-Lines@@Tests/Console.wlt:168,1-187,2"
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
    TestID   -> "ConsoleError-GitHub-File-Lines-Columns@@Tests/Console.wlt:189,1-208,2"
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
    TestID   -> "ConsoleWarning-Notebook@@Tests/Console.wlt:213,1-221,2"
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
    TestID   -> "ConsoleWarning-TTY@@Tests/Console.wlt:223,1-238,2"
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
    TestID   -> "ConsoleWarning-GitHub@@Tests/Console.wlt:240,1-254,2"
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
    TestID   -> "ConsoleWarning-GitHub-File@@Tests/Console.wlt:256,1-274,2"
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
    TestID   -> "ConsoleWarning-GitHub-File-Line@@Tests/Console.wlt:276,1-295,2"
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
    TestID   -> "ConsoleWarning-GitHub-File-Lines@@Tests/Console.wlt:297,1-316,2"
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
    TestID   -> "ConsoleWarning-GitHub-File-Lines-Columns@@Tests/Console.wlt:318,1-337,2"
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
    TestID   -> "ConsoleNotice-Notebook@@Tests/Console.wlt:342,1-350,2"
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
    TestID   -> "ConsoleNotice-TTY@@Tests/Console.wlt:352,1-367,2"
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
    TestID   -> "ConsoleNotice-GitHub@@Tests/Console.wlt:369,1-383,2"
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
    TestID   -> "ConsoleNotice-GitHub-File@@Tests/Console.wlt:385,1-403,2"
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
    TestID   -> "ConsoleNotice-GitHub-File-Line@@Tests/Console.wlt:405,1-424,2"
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
    TestID   -> "ConsoleNotice-GitHub-File-Lines@@Tests/Console.wlt:426,1-445,2"
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
    TestID   -> "ConsoleNotice-GitHub-File-Lines-Columns@@Tests/Console.wlt:447,1-466,2"
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
    TestID   -> "ConsoleLog-Notebook@@Tests/Console.wlt:471,1-479,2"
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
    TestID   -> "ConsoleLog-TTY@@Tests/Console.wlt:481,1-497,2"
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
    TestID   -> "ConsoleLog-GitHub@@Tests/Console.wlt:499,1-513,2"
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
    TestID   -> "ConsoleLog-GitHub-File@@Tests/Console.wlt:518,1-536,2"
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
    TestID   -> "ConsoleLog-GitHub-File-Line@@Tests/Console.wlt:538,1-557,2"
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
    TestID   -> "ConsoleLog-GitHub-File-Lines@@Tests/Console.wlt:559,1-578,2"
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
    TestID   -> "ConsoleLog-GitHub-File-Lines-Columns@@Tests/Console.wlt:580,1-599,2"
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
    TestID   -> "ConsoleLog-GitHub-Error@@Tests/Console.wlt:604,1-620,2"
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
    TestID   -> "ConsoleLog-GitHub-Warning@@Tests/Console.wlt:622,1-638,2"
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
    TestID   -> "ConsoleLog-GitHub-Notice@@Tests/Console.wlt:640,1-656,2"
]

(* :!CodeAnalysis::EndBlock:: *)