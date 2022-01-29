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
    TestID -> "Utilities-Initialize-1"
]

VerificationTest[
    PacletDirectoryLoad @ Wolfram`PacletCICD`Tests`$pacletDir,
    { ___, Wolfram`PacletCICD`Tests`$pacletDir, ___ },
    SameTest -> MatchQ,
    TestID -> "Utilities-Initialize-2"
]

VerificationTest[
    Block[ { $ContextPath }, Needs[ "Wolfram`PacletCICD`" ] ],
    Null,
    TestID -> "Utilities-Initialize-3"
]

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*catchUndefined*)
VerificationTest[
    Wolfram`PacletCICD`Private`tryFetchExampleData[ 1, 2, 3, 4 ],
    Failure[
        "PacletCICD::undefined",
        <|
            "MessageTemplate" :> Wolfram`PacletCICD`PacletCICD::undefined,
            "MessageParameters" :> {
                HoldForm @ Wolfram`PacletCICD`Private`tryFetchExampleData,
                HoldForm @ Wolfram`PacletCICD`Private`tryFetchExampleData[ 1, 2, 3, 4 ],
                DownValues
            }
        |>
    ],
    { Wolfram`PacletCICD`PacletCICD::undefined },
    TestID -> "Utilities-catchUndefined-1"
]

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*relativePath*)
VerificationTest[
    Wolfram`PacletCICD`Private`relativePath[
        "path/to/file",
        "path/another/file"
    ],
    "../../another/file",
    TestID -> "Utilities-relativePath-1"
]

VerificationTest[
    Wolfram`PacletCICD`Private`relativePath[ "path/to/file", "path/to/file" ],
    ".",
    TestID -> "Utilities-relativePath-2"
]

VerificationTest[
    Wolfram`PacletCICD`Private`relativePath[ "path/to/file" ][ "path/another/file" ],
    "../../another/file",
    TestID -> "Utilities-relativePath-3"
]

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*throwError*)
VerificationTest[
    Wolfram`PacletCICD`Private`throwError[ "Error message: `1`", 1, 2, 3 ],
    Failure[
        "PacletCICD::error",
        <|
            "MessageTemplate"   :> Wolfram`PacletCICD`PacletCICD::error,
            "MessageParameters" :> { "Error message: 1", 1, 2, 3 }
        |>
    ],
    { Wolfram`PacletCICD`PacletCICD::error },
    TestID -> "Utilities-throwError-1"
]

VerificationTest[
    Wolfram`PacletCICD`Private`throwError[
        "Error message: `expr`",
        <| "expr" -> { 1, 2, 3 }, "other" -> 123 |>
    ],
    Failure[
        "PacletCICD::error",
        <|
            "MessageTemplate" :> Wolfram`PacletCICD`PacletCICD::error,
            "MessageParameters" :> Evaluate @ {
                "Error message: {1, 2, 3}",
                <| "expr" -> { 1, 2, 3 }, "other" -> 123 |>
            }
        |>
    ],
    { Wolfram`PacletCICD`PacletCICD::error },
    TestID -> "Utilities-throwError-2"
]

VerificationTest[
    Wolfram`PacletCICD`Private`catchTop[
        Wolfram`PacletCICD`Private`throwError[ "a" ];
        Wolfram`PacletCICD`Private`throwError[ "b" ]
    ],
    Failure[
        "PacletCICD::error",
        <|
            "MessageTemplate" :> Wolfram`PacletCICD`PacletCICD::error,
            "MessageParameters" :> { "a" }
        |>
    ],
    { Wolfram`PacletCICD`PacletCICD::error },
    TestID -> "Utilities-throwError-3"
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
            "MessageTemplate" :> Wolfram`PacletCICD`PacletCICD::warning,
            "MessageParameters" :> { "b" }
        |>
    ],
    {
        Wolfram`PacletCICD`PacletCICD::error,
        Wolfram`PacletCICD`PacletCICD::warning
    },
    TestID -> "Utilities-catchTop"
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
            "MessageTemplate" :> Wolfram`PacletCICD`PacletCICD::warning,
            "MessageParameters" :> { "b" }
        |>
    ],
    { Wolfram`PacletCICD`PacletCICD::warning },
    TestID -> "Utilities-catchTop"
]

VerificationTest[
    Wolfram`PacletCICD`Private`catchTop @ {
        Wolfram`PacletCICD`Private`throwError[ "a" ],
        Wolfram`PacletCICD`Private`throwError[ "b" ]
    },
    Failure[
        "PacletCICD::error",
        <|
            "MessageTemplate" :> Wolfram`PacletCICD`PacletCICD::error,
            "MessageParameters" :> { "a" }
        |>
    ],
    { Wolfram`PacletCICD`PacletCICD::error },
    TestID -> "Utilities-catchTop"
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
            "MessageTemplate" :> Wolfram`PacletCICD`PacletCICD::error,
            "MessageParameters" :> { "a" }
        |>
    ],
    { Wolfram`PacletCICD`PacletCICD::error },
    TestID -> "Utilities-catchTop"
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
                "MessageTemplate" :> Wolfram`PacletCICD`PacletCICD::error,
                "MessageParameters" :> { "a" }
            |>
        ],
        Failure[
            "PacletCICD::error",
            <|
                "MessageTemplate" :> Wolfram`PacletCICD`PacletCICD::error,
                "MessageParameters" :> { "b" }
            |>
        ]
    },
    {
        Wolfram`PacletCICD`PacletCICD::error,
        Wolfram`PacletCICD`PacletCICD::error
    },
    TestID -> "Utilities-catchTop"
]