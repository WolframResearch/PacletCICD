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
    TestID -> "Initialize-PacletObject@@Tests/ErrorHandling.wlt:4,1-14,2"
]

(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::SuspiciousSessionSymbol:: *)
VerificationTest[
    Echo[ $TestFileName, "TestFileName" ];
    PacletDirectoryLoad @ Echo[ $pacletDir, "PacletDirectory" ],
    { ___, $pacletDir, ___ },
    SameTest -> MatchQ,
    TestID   -> "Initialize-PacletDirectoryLoad@@Tests/ErrorHandling.wlt:18,1-24,2"
]
(* :!CodeAnalysis::EndBlock:: *)

VerificationTest[
    Needs[ "Wolfram`PacletCICD`" ];
    PacletCICD;
    MessageFailure;
    ,
    Null,
    TestID -> "Initialize@@Tests/ErrorHandling.wlt:27,1-34,2"
]

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Context Check*)
VerificationTest[
    Context @ PacletCICD,
    "Wolfram`PacletCICD`",
    TestID -> "PacletCICD-Context@@Tests/ErrorHandling.wlt:39,1-43,2"
]

VerificationTest[
    Context @ MessageFailure,
    "Wolfram`PacletCICD`",
    TestID -> "MessageFailure-Context@@Tests/ErrorHandling.wlt:45,1-49,2"
]

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*catchUndefined*)
VerificationTest[
    ExampleDirectory;
    Wolfram`PacletCICD`Private`tryFetchExampleData[ 1, 2, 3, 4 ],
    Failure[
        "PacletCICD::undefined",
        <|
            "MessageParameters" :> {
                HoldForm @ Wolfram`PacletCICD`Private`tryFetchExampleData,
                HoldForm @ Wolfram`PacletCICD`Private`tryFetchExampleData[
                    1,
                    2,
                    3,
                    4
                ],
                DownValues
            },
            "MessageTemplate" :> PacletCICD::undefined
        |>
    ],
    { PacletCICD::undefined },
    TestID -> "catchUndefined@@Tests/ErrorHandling.wlt:54,1-75,2"
]

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*throwError*)
VerificationTest[
    Wolfram`PacletCICD`Private`throwError[ "Error message: `1`", 1, 2, 3 ],
    Failure[
        "PacletCICD::error",
        <|
            "MessageParameters" :> { "Error message: 1", 1, 2, 3 },
            "MessageTemplate"   :> PacletCICD::error
        |>
    ],
    { PacletCICD::error },
    TestID -> "throwError@@Tests/ErrorHandling.wlt:80,1-91,2"
]

VerificationTest[
    Wolfram`PacletCICD`Private`throwError[
        "Error message: `expr`",
        <| "expr" -> { 1, 2, 3 }, "other" -> 123 |>
    ],
    Failure[
        "PacletCICD::error",
        <|
            "MessageParameters" :> Evaluate @ {
                "Error message: {1, 2, 3}",
                <| "expr" -> { 1, 2, 3 }, "other" -> 123 |>
            },
            "MessageTemplate" :> PacletCICD::error
        |>
    ],
    { PacletCICD::error },
    TestID -> "throwError@@Tests/ErrorHandling.wlt:93,1-110,2"
]

VerificationTest[
    Wolfram`PacletCICD`Private`catchTop[
        Wolfram`PacletCICD`Private`throwError[ "a" ];
        Wolfram`PacletCICD`Private`throwError[ "b" ]
    ],
    Failure[
        "PacletCICD::error",
        <|
            "MessageParameters" :> { "a" },
            "MessageTemplate"   :> PacletCICD::error
        |>
    ],
    { PacletCICD::error },
    TestID -> "throwError@@Tests/ErrorHandling.wlt:112,1-126,2"
]

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*catchTop*)
VerificationTest[
    Wolfram`PacletCICD`Private`catchTop[

        Wolfram`PacletCICD`Private`catch @
            Wolfram`PacletCICD`Private`throwError[ "a" ];

        Wolfram`PacletCICD`Private`throwGeneralMessage[ "warning", "b" ]
    ],
    Failure[
        "PacletCICD::warning",
        <|
            "MessageParameters" :> { "b" },
            "MessageTemplate"   :> PacletCICD::warning
        |>
    ],
    {
        PacletCICD::error,
        PacletCICD::warning
    },
    TestID -> "catchTop@@Tests/ErrorHandling.wlt:131,1-151,2"
]

VerificationTest[
    Wolfram`PacletCICD`Private`catchTop[

        Wolfram`PacletCICD`Private`catchQuiet @
            Wolfram`PacletCICD`Private`throwError[ "a" ];

        Wolfram`PacletCICD`Private`throwGeneralMessage[ "warning", "b" ]
    ],
    Failure[
        "PacletCICD::warning",
        <|
            "MessageParameters" :> { "b" },
            "MessageTemplate"   :> PacletCICD::warning
        |>
    ],
    { PacletCICD::warning },
    TestID -> "catchTop@@Tests/ErrorHandling.wlt:153,1-170,2"
]

VerificationTest[
    Wolfram`PacletCICD`Private`catchTop @ {
        Wolfram`PacletCICD`Private`throwError[ "a" ],
        Wolfram`PacletCICD`Private`throwError[ "b" ]
    },
    Failure[
        "PacletCICD::error",
        <|
            "MessageParameters" :> { "a" },
            "MessageTemplate"   :> PacletCICD::error
        |>
    ],
    { PacletCICD::error },
    TestID -> "catchTop@@Tests/ErrorHandling.wlt:172,1-186,2"
]

VerificationTest[
    Wolfram`PacletCICD`Private`catchTop @ {
        Wolfram`PacletCICD`Private`catchTop @
            Wolfram`PacletCICD`Private`throwError[ "a" ],
        Wolfram`PacletCICD`Private`catchTop @
            Wolfram`PacletCICD`Private`throwError[ "b" ]
    },
    Failure[
        "PacletCICD::error",
        <|
            "MessageParameters" :> { "a" },
            "MessageTemplate"   :> PacletCICD::error
        |>
    ],
    { PacletCICD::error },
    TestID -> "catchTop@@Tests/ErrorHandling.wlt:188,1-204,2"
]

VerificationTest[
    Wolfram`PacletCICD`Private`catchTop @ {
        Wolfram`PacletCICD`Private`catch @
            Wolfram`PacletCICD`Private`throwError[ "a" ],
        Wolfram`PacletCICD`Private`catch @
            Wolfram`PacletCICD`Private`throwError[ "b" ]
    },
    {
        Failure[
            "PacletCICD::error",
            <|
                "MessageParameters" :> { "a" },
                "MessageTemplate"   :> PacletCICD::error
            |>
        ],
        Failure[
            "PacletCICD::error",
            <|
                "MessageParameters" :> { "b" },
                "MessageTemplate"   :> PacletCICD::error
            |>
        ]
    },
    {
        PacletCICD::error,
        PacletCICD::error
    },
    TestID -> "catchTop@@Tests/ErrorHandling.wlt:206,1-234,2"
]
