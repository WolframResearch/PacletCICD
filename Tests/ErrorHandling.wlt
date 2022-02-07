(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Initialization*)
VerificationTest[
    PacletObjectQ @ PacletObject @ File[
        $pacletDir = DirectoryName[ $TestFileName, 2 ]
    ],
    TestID -> "Initialize@@Tests/ErrorHandling.wlt:4,1-9,2"
]

VerificationTest[
    PacletDirectoryLoad @ $pacletDir,
    { ___, $pacletDir, ___ },
    SameTest -> MatchQ,
    TestID -> "Initialize@@Tests/ErrorHandling.wlt:11,1-16,2"
]

VerificationTest[
    Needs[ "Wolfram`PacletCICD`" ],
    Null,
    TestID -> "Initialize@@Tests/ErrorHandling.wlt:18,1-22,2"
]

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Context Check*)
VerificationTest[
    Context @ PacletCICD,
    "Wolfram`PacletCICD`",
    TestID -> "PacletCICD-Context@@Tests/ErrorHandling.wlt:27,1-31,2"
]

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*catchUndefined*)
VerificationTest[
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
    TestID -> "catchUndefined@@Tests/ErrorHandling.wlt:36,1-56,2"
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
    TestID -> "throwError@@Tests/ErrorHandling.wlt:61,1-72,2"
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
    TestID -> "throwError@@Tests/ErrorHandling.wlt:74,1-91,2"
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
    TestID -> "throwError@@Tests/ErrorHandling.wlt:93,1-107,2"
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
    TestID -> "catchTop@@Tests/ErrorHandling.wlt:112,1-132,2"
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
    TestID -> "catchTop@@Tests/ErrorHandling.wlt:134,1-151,2"
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
    TestID -> "catchTop@@Tests/ErrorHandling.wlt:153,1-167,2"
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
    TestID -> "catchTop@@Tests/ErrorHandling.wlt:169,1-185,2"
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
    TestID -> "catchTop@@Tests/ErrorHandling.wlt:187,1-215,2"
]
