(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Initialization*)
VerificationTest[
    PacletObjectQ @ PacletObject @ File[
        $pacletDir =
            Module[ { root, mx },
                root = DirectoryName[ $TestFileName, 2 ];
                mx = FileNameJoin @ { root, "MXBuild", "Wolfram__PacletCICD" };
                If[ DirectoryQ @ mx, mx, root ]
            ]
    ],
    TestID -> "Initialize-PacletObject@@Tests/Console.wlt:4,1-14,2"
]

(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::SuspiciousSessionSymbol:: *)
VerificationTest[
    Echo[ $TestFileName, "TestFileName" ];
    PacletDirectoryLoad @ Echo[ $pacletDir, "PacletDirectory" ],
    { ___, $pacletDir, ___ },
    SameTest -> MatchQ,
    TestID   -> "Initialize-PacletDirectoryLoad@@Tests/Console.wlt:18,1-24,2"
]
(* :!CodeAnalysis::EndBlock:: *)

VerificationTest[
    Needs[ "Wolfram`PacletCICD`" ],
    Null,
    TestID -> "Initialize-Needs@@Tests/Console.wlt:27,1-31,2"
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
    TestID -> "Definition-TraceWrites@@Tests/Console.wlt:35,1-61,2"
]

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Context Check*)
VerificationTest[
    Context @ ConsoleError,
    "Wolfram`PacletCICD`",
    TestID -> "ConsoleError-Context@@Tests/Console.wlt:66,1-70,2"
]

VerificationTest[
    Context @ ConsoleWarning,
    "Wolfram`PacletCICD`",
    TestID -> "ConsoleWarning-Context@@Tests/Console.wlt:72,1-76,2"
]

VerificationTest[
    Context @ ConsoleNotice,
    "Wolfram`PacletCICD`",
    TestID -> "ConsoleNotice-Context@@Tests/Console.wlt:78,1-82,2"
]

VerificationTest[
    Context @ ConsoleLog,
    "Wolfram`PacletCICD`",
    TestID -> "ConsoleLog-Context@@Tests/Console.wlt:84,1-88,2"
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
    TestID   -> "ConsoleError-Notebook@@Tests/Console.wlt:93,1-101,2"
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
    TestID   -> "ConsoleError-TTY@@Tests/Console.wlt:103,1-118,2"
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
    TestID   -> "ConsoleError-GitHub@@Tests/Console.wlt:120,1-134,2"
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
    TestID   -> "ConsoleError-GitHub-File@@Tests/Console.wlt:136,1-154,2"
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
    TestID   -> "ConsoleError-GitHub-File-Line@@Tests/Console.wlt:156,1-175,2"
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
    TestID   -> "ConsoleError-GitHub-File-Lines@@Tests/Console.wlt:177,1-196,2"
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
    TestID   -> "ConsoleError-GitHub-File-Lines-Columns@@Tests/Console.wlt:198,1-217,2"
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
    TestID   -> "ConsoleWarning-Notebook@@Tests/Console.wlt:222,1-230,2"
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
    TestID   -> "ConsoleWarning-TTY@@Tests/Console.wlt:232,1-247,2"
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
    TestID   -> "ConsoleWarning-GitHub@@Tests/Console.wlt:249,1-263,2"
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
    TestID   -> "ConsoleWarning-GitHub-File@@Tests/Console.wlt:265,1-283,2"
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
    TestID   -> "ConsoleWarning-GitHub-File-Line@@Tests/Console.wlt:285,1-304,2"
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
    TestID   -> "ConsoleWarning-GitHub-File-Lines@@Tests/Console.wlt:306,1-325,2"
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
    TestID   -> "ConsoleWarning-GitHub-File-Lines-Columns@@Tests/Console.wlt:327,1-346,2"
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
    TestID   -> "ConsoleNotice-Notebook@@Tests/Console.wlt:351,1-359,2"
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
    TestID   -> "ConsoleNotice-TTY@@Tests/Console.wlt:361,1-376,2"
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
    TestID   -> "ConsoleNotice-GitHub@@Tests/Console.wlt:378,1-392,2"
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
    TestID   -> "ConsoleNotice-GitHub-File@@Tests/Console.wlt:394,1-412,2"
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
    TestID   -> "ConsoleNotice-GitHub-File-Line@@Tests/Console.wlt:414,1-433,2"
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
    TestID   -> "ConsoleNotice-GitHub-File-Lines@@Tests/Console.wlt:435,1-454,2"
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
    TestID   -> "ConsoleNotice-GitHub-File-Lines-Columns@@Tests/Console.wlt:456,1-475,2"
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
    TestID   -> "ConsoleLog-Notebook@@Tests/Console.wlt:480,1-488,2"
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
    TestID   -> "ConsoleLog-TTY@@Tests/Console.wlt:490,1-506,2"
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
    TestID   -> "ConsoleLog-GitHub@@Tests/Console.wlt:508,1-522,2"
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
    TestID   -> "ConsoleLog-GitHub-File@@Tests/Console.wlt:527,1-545,2"
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
    TestID   -> "ConsoleLog-GitHub-File-Line@@Tests/Console.wlt:547,1-566,2"
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
    TestID   -> "ConsoleLog-GitHub-File-Lines@@Tests/Console.wlt:568,1-587,2"
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
    TestID   -> "ConsoleLog-GitHub-File-Lines-Columns@@Tests/Console.wlt:589,1-608,2"
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
    TestID   -> "ConsoleLog-GitHub-Error@@Tests/Console.wlt:613,1-629,2"
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
    TestID   -> "ConsoleLog-GitHub-Warning@@Tests/Console.wlt:631,1-647,2"
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
    TestID   -> "ConsoleLog-GitHub-Notice@@Tests/Console.wlt:649,1-665,2"
]

(* :!CodeAnalysis::EndBlock:: *)