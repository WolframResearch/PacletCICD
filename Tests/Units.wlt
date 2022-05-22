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
    ],
    TestID -> "SecondsToQuantity-1"
]

VerificationTest[
    q = Quantity[ 1.2346*^8, "Seconds" ],
    Quantity[ 1.2346*^8, "Seconds" ],
    TestID -> "SecondsToQuantity-2"
]

VerificationTest[
    SecondsToQuantity @ q,
    Quantity[
        MixedMagnitude @ { 3, 10, 29.731 },
        MixedUnit @ { "Years", "Months", "Days" }
    ],
    TestID -> "SecondsToQuantity-3"
]

VerificationTest[
    SecondsToQuantity @ f @ x,
    Quantity[ f @ x, "Seconds" ],
    TestID -> "SecondsToQuantity-4"
]

VerificationTest[
    SecondsToQuantity @ Quantity[ f @ x, "Minutes" ],
    Quantity[ 60 * f @ x, "Seconds" ],
    TestID -> "SecondsToQuantity-5"
]

VerificationTest[
    SecondsToQuantity[ 12346.0 ],
    Quantity[
        MixedMagnitude @ { 3, 25, 45.679 },
        MixedUnit @ { "Hours", "Minutes", "Seconds" }
    ],
    TestID -> "SecondsToQuantity-6"
]

VerificationTest[
    SecondsToQuantity[ 12346.0, "MixedUnits" -> False ],
    Quantity[ 3.4294, "Hours" ],
    TestID -> "SecondsToQuantity-7"
]

VerificationTest[
    SecondsToQuantity[ 6052.4 ],
    Quantity[
        MixedMagnitude @ { 1, 40, 52.402 },
        MixedUnit @ { "Hours", "Minutes", "Seconds" }
    ],
    TestID -> "SecondsToQuantity-8"
]

VerificationTest[
    SecondsToQuantity[ 12346.0, "MaxMixedUnits" -> 2 ],
    Quantity[
        MixedMagnitude @ { 3, 25.761 },
        MixedUnit @ { "Hours", "Minutes" }
    ],
    TestID -> "SecondsToQuantity-9"
]

VerificationTest[
    SecondsToQuantity[ 12346.0, "MaxMixedUnits" -> 1 ],
    SecondsToQuantity[ 12346.0, "MixedUnits" -> False ],
    TestID -> "SecondsToQuantity-10"
]

VerificationTest[
    SecondsToQuantity[ 12346.0, "MaxMixedUnits" -> invalid ],
    _,
    { ResourceFunction::usermessage },
    SameTest -> MatchQ,
    TestID -> "SecondsToQuantity-11"
]