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
    TestID -> "Initialize-PacletObject"
]

(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::SuspiciousSessionSymbol:: *)
VerificationTest[
    Echo[ $TestFileName, "TestFileName" ];
    PacletDirectoryLoad @ Echo[ $pacletDir, "PacletDirectory" ],
    { ___, $pacletDir, ___ },
    SameTest -> MatchQ,
    TestID   -> "Initialize-PacletDirectoryLoad"
]
(* :!CodeAnalysis::EndBlock:: *)

VerificationTest[
    Needs[ "Wolfram`PacletCICD`" ],
    Null,
    TestID -> "Initialize-Paclet-Needs"
]

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Context Check*)
VerificationTest[
    Context @ BytesToQuantity,
    "Wolfram`PacletCICD`",
    TestID -> "BytesToQuantity-Context"
]

VerificationTest[
    Context @ SecondsToQuantity,
    "Wolfram`PacletCICD`",
    TestID -> "SecondsToQuantity-Context"
]

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*SecondsToQuantity*)
VerificationTest[
    SecondsToQuantity[ 12345 ],
    Quantity[
        MixedMagnitude @ { 3, 25, 45 },
        MixedUnit @ { "Hours", "Minutes", "Seconds" }
    ]
]

VerificationTest[
    q = Quantity[ 1.2346*^8, "Seconds" ],
    Quantity[ 1.2346*^8, "Seconds" ]
]

VerificationTest[
    SecondsToQuantity @ q,
    Quantity[
        MixedMagnitude @ { 3, 10, 29.731 },
        MixedUnit @ { "Years", "Months", "Days" }
    ]
]

VerificationTest[ SecondsToQuantity @ f @ x, Quantity[ f @ x, "Seconds" ] ]

VerificationTest[
    SecondsToQuantity @ Quantity[ f @ x, "Minutes" ],
    Quantity[ 60 * f @ x, "Seconds" ]
]

VerificationTest[
    SecondsToQuantity[ 12346.0 ],
    Quantity[
        MixedMagnitude @ { 3, 25, 45.679 },
        MixedUnit @ { "Hours", "Minutes", "Seconds" }
    ]
]

VerificationTest[
    SecondsToQuantity[ 12346.0, "MixedUnits" -> False ],
    Quantity[ 3.4294, "Hours" ]
]

VerificationTest[
    SecondsToQuantity[ 6052.4 ],
    Quantity[
        MixedMagnitude @ { 1, 40, 52.402 },
        MixedUnit @ { "Hours", "Minutes", "Seconds" }
    ]
]

VerificationTest[
    SecondsToQuantity[ 12346.0, "MaxMixedUnits" -> 2 ],
    Quantity[
        MixedMagnitude @ { 3, 25.761 },
        MixedUnit @ { "Hours", "Minutes" }
    ]
]

VerificationTest[
    SecondsToQuantity[ 12346.0, "MaxMixedUnits" -> 1 ],
    SecondsToQuantity[ 12346.0, "MixedUnits" -> False ]
]

VerificationTest[
    SecondsToQuantity[ 12346.0, "MaxMixedUnits" -> invalid ],
    _,
    { ResourceFunction::usermessage },
    SameTest -> MatchQ
]