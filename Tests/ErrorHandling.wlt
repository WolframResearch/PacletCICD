(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Initialization*)
VerificationTest[
    PacletObjectQ @ PacletObject @ File[
        Wolfram`PacletCICD`Tests`$pacletDir = DirectoryName[
            System`$TestFileName,
            2
        ]
    ],
    TestID -> "Initialize@@Tests/ErrorHandling.wlt:4,1-12,2"
]

VerificationTest[
    PacletDirectoryLoad @ Wolfram`PacletCICD`Tests`$pacletDir,
    { ___, Wolfram`PacletCICD`Tests`$pacletDir, ___ },
    SameTest -> MatchQ,
    TestID -> "Initialize@@Tests/ErrorHandling.wlt:14,1-19,2"
]

VerificationTest[
    Block[ { $ContextPath }, Needs[ "Wolfram`PacletCICD`" ] ],
    Null,
    TestID -> "Initialize@@Tests/ErrorHandling.wlt:21,1-25,2"
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
                HoldForm @ Wolfram`PacletCICD`Private`tryFetchExampleData[ 1, 2, 3, 4 ],
                DownValues
            },
            "MessageTemplate" :> Wolfram`PacletCICD`PacletCICD::undefined
        |>
    ],
    { Wolfram`PacletCICD`PacletCICD::undefined },
    TestID -> "catchUndefined@@Tests/ErrorHandling.wlt:30,1-45,2"
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
            "MessageTemplate"   :> Wolfram`PacletCICD`PacletCICD::error
        |>
    ],
    { Wolfram`PacletCICD`PacletCICD::error },
    TestID -> "throwError@@Tests/ErrorHandling.wlt:50,1-61,2"
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
            "MessageTemplate" :> Wolfram`PacletCICD`PacletCICD::error
        |>
    ],
    { Wolfram`PacletCICD`PacletCICD::error },
    TestID -> "throwError@@Tests/ErrorHandling.wlt:63,1-80,2"
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
            "MessageTemplate"   :> Wolfram`PacletCICD`PacletCICD::error
        |>
    ],
    { Wolfram`PacletCICD`PacletCICD::error },
    TestID -> "throwError@@Tests/ErrorHandling.wlt:82,1-97,2"
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
            "MessageTemplate"   :> Wolfram`PacletCICD`PacletCICD::warning
        |>
    ],
    {
        Wolfram`PacletCICD`PacletCICD::error,
        Wolfram`PacletCICD`PacletCICD::warning
    },
    TestID -> "catchTop@@Tests/ErrorHandling.wlt:102,1-123,2"
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
            "MessageTemplate"   :> Wolfram`PacletCICD`PacletCICD::warning
        |>
    ],
    { Wolfram`PacletCICD`PacletCICD::warning },
    TestID -> "catchTop@@Tests/ErrorHandling.wlt:125,1-143,2"
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
            "MessageTemplate"   :> Wolfram`PacletCICD`PacletCICD::error
        |>
    ],
    { Wolfram`PacletCICD`PacletCICD::error },
    TestID -> "catchTop@@Tests/ErrorHandling.wlt:145,1-160,2"
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
            "MessageTemplate"   :> Wolfram`PacletCICD`PacletCICD::error
        |>
    ],
    { Wolfram`PacletCICD`PacletCICD::error },
    TestID -> "catchTop@@Tests/ErrorHandling.wlt:162,1-179,2"
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
                "MessageTemplate"   :> Wolfram`PacletCICD`PacletCICD::error
            |>
        ],
        Failure[
            "PacletCICD::error",
            <|
                "MessageParameters" :> { "b" },
                "MessageTemplate"   :> Wolfram`PacletCICD`PacletCICD::error
            |>
        ]
    },
    {
        Wolfram`PacletCICD`PacletCICD::error,
        Wolfram`PacletCICD`PacletCICD::error
    },
    TestID -> "catchTop@@Tests/ErrorHandling.wlt:181,1-211,2"
]