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


 VerificationTest[1+1, 3]

 VerificationTest[1/0, ComplexInfinity,{First::argx},TestID->None]

 VerificationTest[1/0, ComplexInfinity,{First::argx},
SameTest-> SameQ
 ]

VerificationTest   @
    FailureQ[ $Failed/ 0]

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
            "MessageTemplate" :> Wolfram`PacletCICD`PacletCICD::undefined
        |>
    ],
    { Wolfram`PacletCICD`PacletCICD::undefined },
    TestID -> "catchUndefined@@Tests/ErrorHandling.wlt:30,1-50,2"
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
    TestID -> "throwError@@Tests/ErrorHandling.wlt:55,1-66,2"
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
    TestID -> "throwError@@Tests/ErrorHandling.wlt:68,1-85,2"
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
    TestID -> "throwError@@Tests/ErrorHandling.wlt:87,1-101,2"
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
    TestID -> "catchTop@@Tests/ErrorHandling.wlt:106,1-126,2"
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
    TestID -> "catchTop@@Tests/ErrorHandling.wlt:128,1-145,2"
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
    TestID -> "catchTop@@Tests/ErrorHandling.wlt:147,1-161,2"
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
    TestID -> "catchTop@@Tests/ErrorHandling.wlt:163,1-179,2"
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
    TestID -> "catchTop@@Tests/ErrorHandling.wlt:181,1-209,2"
]
