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
    TestID -> "Initialize-PacletObject@@Tests/Units.wlt:4,1-14,2"
]

(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::SuspiciousSessionSymbol:: *)
VerificationTest[
    Echo[ $TestFileName, "TestFileName" ];
    PacletDirectoryLoad @ Echo[ $pacletDir, "PacletDirectory" ],
    { ___, $pacletDir, ___ },
    SameTest -> MatchQ,
    TestID   -> "Initialize-PacletDirectoryLoad@@Tests/Units.wlt:18,1-24,2"
]
(* :!CodeAnalysis::EndBlock:: *)

VerificationTest[
    Needs[ "Wolfram`PacletCICD`" ],
    Null,
    TestID -> "Initialize-Paclet-Needs@@Tests/Units.wlt:27,1-31,2"
]

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Context Check*)
VerificationTest[
    Context @ BytesToQuantity,
    "Wolfram`PacletCICD`",
    TestID -> "BytesToQuantity-Context@@Tests/Units.wlt:36,1-40,2"
]

VerificationTest[
    Context @ SecondsToQuantity,
    "Wolfram`PacletCICD`",
    TestID -> "SecondsToQuantity-Context@@Tests/Units.wlt:42,1-46,2"
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
    TestID -> "SecondsToQuantity-1@@Tests/Units.wlt:51,1-58,2"
]

VerificationTest[
    q = Quantity[ 1.2346*^8, "Seconds" ],
    Quantity[ 1.2346*^8, "Seconds" ],
    TestID -> "SecondsToQuantity-2@@Tests/Units.wlt:60,1-64,2"
]

VerificationTest[
    SecondsToQuantity @ q,
    Quantity[
        MixedMagnitude @ { 3, 10, 29.768518518518494 },
        MixedUnit @ { "Years", "Months", "Days" }
    ],
    TestID -> "SecondsToQuantity-3@@Tests/Units.wlt:66,1-73,2"
]

VerificationTest[
    SecondsToQuantity @ f @ x,
    Quantity[ f @ x, "Seconds" ],
    TestID -> "SecondsToQuantity-4@@Tests/Units.wlt:75,1-79,2"
]

VerificationTest[
    SecondsToQuantity @ Quantity[ f @ x, "Minutes" ],
    Quantity[ 60 * f @ x, "Seconds" ],
    TestID -> "SecondsToQuantity-5@@Tests/Units.wlt:81,1-85,2"
]

VerificationTest[
    SecondsToQuantity[ 12346.0 ],
    Quantity[
        MixedMagnitude @ { 3, 25, 46.00000000000037 },
        MixedUnit @ { "Hours", "Minutes", "Seconds" }
    ],
    TestID -> "SecondsToQuantity-6@@Tests/Units.wlt:87,1-94,2"
]

VerificationTest[
    SecondsToQuantity[ 12346.0, "MixedUnits" -> False ],
    Quantity[ 3.4294444444444445, "Hours" ],
    TestID -> "SecondsToQuantity-7@@Tests/Units.wlt:96,1-100,2"
]

VerificationTest[
    SecondsToQuantity[ 6052.4 ],
    Quantity[
        MixedMagnitude @ { 1, 40, 52.399999999999665 },
        MixedUnit @ { "Hours", "Minutes", "Seconds" }
    ],
    TestID -> "SecondsToQuantity-8@@Tests/Units.wlt:102,1-109,2"
]

VerificationTest[
    SecondsToQuantity[ 12346.0, "MaxMixedUnits" -> 2 ],
    Quantity[
        MixedMagnitude @ { 3, 25.76666666666667 },
        MixedUnit @ { "Hours", "Minutes" }
    ],
    TestID -> "SecondsToQuantity-9@@Tests/Units.wlt:111,1-118,2"
]

VerificationTest[
    SecondsToQuantity[ 12346.0, "MaxMixedUnits" -> 1 ],
    SecondsToQuantity[ 12346.0, "MixedUnits" -> False ],
    TestID -> "SecondsToQuantity-10@@Tests/Units.wlt:120,1-124,2"
]

VerificationTest[
    SecondsToQuantity[ 12346.0, "MaxMixedUnits" -> invalid ],
    _,
    { SecondsToQuantity::mmu },
    SameTest -> MatchQ,
    TestID   -> "SecondsToQuantity-11@@Tests/Units.wlt:126,1-132,2"
]