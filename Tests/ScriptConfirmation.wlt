(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::SuspiciousSessionSymbol:: *)

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
    TestID -> "Initialize-PacletObject@@Tests/ScriptConfirmation.wlt:7,1-17,2"
]

VerificationTest[
    Echo[ $TestFileName, "TestFileName" ];
    PacletDirectoryLoad @ Echo[ $pacletDir, "PacletDirectory" ],
    { ___, $pacletDir, ___ },
    SameTest -> MatchQ,
    TestID   -> "Initialize-PacletDirectoryLoad@@Tests/ScriptConfirmation.wlt:19,1-25,2"
]

VerificationTest[
    Needs[ "Wolfram`PacletCICD`" ],
    Null,
    TestID -> "Initialize-Paclet-Needs@@Tests/ScriptConfirmation.wlt:27,1-31,2"
]

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Context Check*)
VerificationTest[
    Context @ ScriptConfirm,
    "Wolfram`PacletCICD`",
    TestID -> "ScriptConfirm-Context@@Tests/ScriptConfirmation.wlt:36,1-40,2"
]

VerificationTest[
    Context @ ScriptConfirmAssert,
    "Wolfram`PacletCICD`",
    TestID -> "ScriptConfirmAssert-Context@@Tests/ScriptConfirmation.wlt:42,1-46,2"
]

VerificationTest[
    Context @ ScriptConfirmBy,
    "Wolfram`PacletCICD`",
    TestID -> "ScriptConfirmBy-Context@@Tests/ScriptConfirmation.wlt:48,1-52,2"
]

VerificationTest[
    Context @ ScriptConfirmMatch,
    "Wolfram`PacletCICD`",
    TestID -> "ScriptConfirmMatch-Context@@Tests/ScriptConfirmation.wlt:54,1-58,2"
]

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Definitions*)
traceSideEffects // Attributes = { HoldFirst };

traceSideEffects[ eval_, env_ ] :=
    Block[ { $EvaluationEnvironment = env }, traceSideEffects @ eval ];

traceSideEffects[ eval_ ] :=
    Module[ { bag, result, effects },
        bag = Internal`Bag[ ];
        Block[ { WriteString, Print, Exit, Abort },
            WriteString = Internal`StuffBag[ bag, HoldComplete @ WriteString @ ## ] &;
            Print       = Internal`StuffBag[ bag, HoldComplete @ Print @ ##       ] &;
            Exit        = Throw[ $exit @ ##   , $tag ] &;
            Abort       = Throw[ $aborted @ ##, $tag ] &;
            result      = Catch[ eval, $tag ];
            effects     = Internal`BagPart[ bag, All ];

            <|
                "Result"      -> result,
                "SideEffects" -> effects
            |>
        ]
    ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*ScriptConfirm*)
VerificationTest[
    traceSideEffects[ ScriptConfirm @ $Failed, "Script" ],
    KeyValuePattern @ {
        "Result"      -> $exit[ 1 ],
        "SideEffects" -> { HoldComplete[
            (Print|WriteString)[
                ___,
                "ScriptConfirm: $Failed encountered.",
                ___
            ]
        ] }
    },
    SameTest -> MatchQ,
    TestID   -> "ScriptConfirm-1@@Tests/ScriptConfirmation.wlt:89,1-103,2"
]

VerificationTest[
    traceSideEffects[ ScriptConfirm[ 1 + 1 ], "Script" ],
    KeyValuePattern @ {
        "Result"      -> 2,
        "SideEffects" -> { }
    },
    SameTest -> MatchQ,
    TestID   -> "ScriptConfirm-2@@Tests/ScriptConfirmation.wlt:105,1-113,2"
]

VerificationTest[
    traceSideEffects[ ScriptConfirm[ Identity @ $Failed, "`Input`" ], "Script" ],
    KeyValuePattern @ {
        "Result"      -> $exit[ 1 ],
        "SideEffects" -> { HoldComplete[
            (Print|WriteString)[ ___, "Identity[$Failed]", ___ ]
        ] }
    },
    SameTest -> MatchQ,
    TestID   -> "ScriptConfirm-3@@Tests/ScriptConfirmation.wlt:115,1-125,2"
]

VerificationTest[
    traceSideEffects[
        ScriptConfirm[ Identity @ $Failed, "`Input`", 123 ],
        "Script"
    ],
    KeyValuePattern @ {
        "Result"      -> $exit[ 123 ],
        "SideEffects" -> { HoldComplete[
            (Print|WriteString)[ ___, "Identity[$Failed]", ___ ]
        ] }
    },
    SameTest -> MatchQ,
    TestID   -> "ScriptConfirm-4@@Tests/ScriptConfirmation.wlt:127,1-140,2"
]

VerificationTest[
    traceSideEffects[
        ScriptConfirm[ $Canceled, Automatic, 2 ];
        ScriptConfirm[ $Aborted , Automatic, 3 ];
        ,
        "Script"
    ],
    KeyValuePattern @ {
        "Result"      -> $exit[ 2 ],
        "SideEffects" -> { HoldComplete[
            (Print|WriteString)[
                ___,
                "ScriptConfirm: $Canceled encountered.",
                ___
            ]
        ] }
    },
    SameTest -> MatchQ,
    TestID   -> "ScriptConfirm-5@@Tests/ScriptConfirmation.wlt:142,1-161,2"
]

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Messages*)
VerificationTest[
    traceSideEffects[ ScriptConfirm @ $Failed, "Session" ],
    KeyValuePattern @ {
        "Result"      -> $aborted[ ],
        "SideEffects" -> { HoldComplete[
            (Print|WriteString)[
                ___,
                "ScriptConfirm: $Failed encountered.",
                ___
            ]
        ] }
    },
    { ScriptConfirm::Session },
    SameTest -> MatchQ,
    TestID   -> "ScriptConfirm-6@@Tests/ScriptConfirmation.wlt:166,1-181,2"
]

VerificationTest[
    ScriptConfirm[ ],
    Failure[ "ScriptConfirm::ArgumentCount", _Association ],
    { ScriptConfirm::ArgumentCount },
    SameTest -> MatchQ,
    TestID   -> "ScriptConfirm-7@@Tests/ScriptConfirmation.wlt:183,1-189,2"
]

VerificationTest[
    ScriptConfirm[ 1, 2, 3, 4 ],
    Failure[ "ScriptConfirm::ArgumentCount", _Association ],
    { ScriptConfirm::ArgumentCount },
    SameTest -> MatchQ,
    TestID   -> "ScriptConfirm-8@@Tests/ScriptConfirmation.wlt:191,1-197,2"
]

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*ScriptConfirmAssert*)
VerificationTest[
    traceSideEffects[ ScriptConfirmAssert @ False, "Script" ],
    KeyValuePattern @ {
        "Result"      -> $exit[ 1 ],
        "SideEffects" -> { HoldComplete[
            (Print|WriteString)[
                ___,
                "ScriptConfirmAssert: Assertion False failed.",
                ___
            ]
        ] }
    },
    SameTest -> MatchQ,
    TestID   -> "ScriptConfirmAssert-1@@Tests/ScriptConfirmation.wlt:202,1-216,2"
]

VerificationTest[
    traceSideEffects[ ScriptConfirmAssert @ True, "Script" ],
    KeyValuePattern @ {
        "Result"      -> Null,
        "SideEffects" -> { }
    },
    SameTest -> MatchQ,
    TestID   -> "ScriptConfirmAssert-2@@Tests/ScriptConfirmation.wlt:218,1-226,2"
]

VerificationTest[
    traceSideEffects[
        ScriptConfirmAssert[ Identity @ $Failed, "`Input`" ],
        "Script"
    ],
    KeyValuePattern @ {
        "Result"      -> $exit[ 1 ],
        "SideEffects" -> { HoldComplete[
            (Print|WriteString)[ ___, "Identity[$Failed]", ___ ]
        ] }
    },
    SameTest -> MatchQ,
    TestID   -> "ScriptConfirmAssert-3@@Tests/ScriptConfirmation.wlt:228,1-241,2"
]

VerificationTest[
        traceSideEffects[
        ScriptConfirmAssert[ Identity @ $Failed, "`Input`", 123 ],
        "Script"
    ],
    KeyValuePattern @ {
        "Result"      -> $exit[ 123 ],
        "SideEffects" -> { HoldComplete[
            (Print|WriteString)[ ___, "Identity[$Failed]", ___ ]
        ] }
    },
    SameTest -> MatchQ,
    TestID   -> "ScriptConfirmAssert-4@@Tests/ScriptConfirmation.wlt:243,1-256,2"
]

VerificationTest[
    traceSideEffects[
        ScriptConfirmAssert[ $Canceled, Automatic, 2 ];
        ScriptConfirmAssert[ $Aborted , Automatic, 3 ];
        ,
        "Script"
    ],
    KeyValuePattern @ {
        "Result"      -> $exit[ 2 ],
        "SideEffects" -> { HoldComplete[
            (Print|WriteString)[
                ___,
                "ScriptConfirmAssert: Assertion $Canceled failed.",
                ___
            ]
        ] }
    },
    SameTest -> MatchQ,
    TestID   -> "ScriptConfirmAssert-5@@Tests/ScriptConfirmation.wlt:258,1-277,2"
]

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Messages*)
VerificationTest[
    traceSideEffects[ ScriptConfirmAssert @ $Failed, "Session" ],
    KeyValuePattern @ {
        "Result"      -> $aborted[ ],
        "SideEffects" -> { HoldComplete[
            (Print|WriteString)[
                ___,
                "ScriptConfirmAssert: Assertion $Failed failed.",
                ___
            ]
        ] }
    },
    { ScriptConfirmAssert::Session },
    SameTest -> MatchQ,
    TestID   -> "ScriptConfirmAssert-6@@Tests/ScriptConfirmation.wlt:282,1-297,2"
]

VerificationTest[
    ScriptConfirmAssert[ ],
    Failure[ "ScriptConfirmAssert::ArgumentCount", _Association ],
    { ScriptConfirmAssert::ArgumentCount },
    SameTest -> MatchQ,
    TestID   -> "ScriptConfirmAssert-7@@Tests/ScriptConfirmation.wlt:299,1-305,2"
]

VerificationTest[
    ScriptConfirmAssert[ 1, 2, 3, 4 ],
    Failure[ "ScriptConfirmAssert::ArgumentCount", _Association ],
    { ScriptConfirmAssert::ArgumentCount },
    SameTest -> MatchQ,
    TestID   -> "ScriptConfirmAssert-8@@Tests/ScriptConfirmation.wlt:307,1-313,2"
]

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*ScriptConfirmBy*)
VerificationTest[
    traceSideEffects[ ScriptConfirmBy[ 1.5, IntegerQ ], "Script" ],
    KeyValuePattern @ {
        "Result"      -> $exit[ 1 ],
        "SideEffects" -> { HoldComplete[
            (Print|WriteString)[
                ___,
                "ScriptConfirmBy: IntegerQ[1.5] did not return True.",
                ___
            ]
        ] }
    },
    SameTest -> MatchQ,
    TestID   -> "ScriptConfirmBy-1@@Tests/ScriptConfirmation.wlt:318,1-332,2"
]

VerificationTest[
    traceSideEffects[ ScriptConfirmBy[ 1, IntegerQ ], "Script" ],
    KeyValuePattern @ {
        "Result"      -> 1,
        "SideEffects" -> { }
    },
    SameTest -> MatchQ,
    TestID   -> "ScriptConfirmBy-2@@Tests/ScriptConfirmation.wlt:334,1-342,2"
]

VerificationTest[
    traceSideEffects[
        ScriptConfirmBy[ Identity[ 1.5 ], IntegerQ, "`Input` `Function`" ],
        "Script"
    ],
    KeyValuePattern @ {
        "Result"      -> $exit[ 1 ],
        "SideEffects" -> { HoldComplete[
            (Print|WriteString)[ ___, "Identity[1.5] IntegerQ", ___ ]
        ] }
    },
    SameTest -> MatchQ,
    TestID   -> "ScriptConfirmBy-3@@Tests/ScriptConfirmation.wlt:344,1-357,2"
]

VerificationTest[
        traceSideEffects[
        ScriptConfirmBy[ Identity[ 1.5 ], IntegerQ, "`Input` `Function`", 123 ],
        "Script"
    ],
    KeyValuePattern @ {
        "Result"      -> $exit[ 123 ],
        "SideEffects" -> { HoldComplete[
            (Print|WriteString)[ ___, "Identity[1.5] IntegerQ", ___ ]
        ] }
    },
    SameTest -> MatchQ,
    TestID   -> "ScriptConfirmBy-4@@Tests/ScriptConfirmation.wlt:359,1-372,2"
]

VerificationTest[
    traceSideEffects[
        ScriptConfirmBy[ 1.5, IntegerQ, Automatic, 2 ];
        ScriptConfirmBy[ 2.5, IntegerQ, Automatic, 3 ];
        ,
        "Script"
    ],
    KeyValuePattern @ {
        "Result"      -> $exit[ 2 ],
        "SideEffects" -> { HoldComplete[
            (Print|WriteString)[
                ___,
                "ScriptConfirmBy: IntegerQ[1.5] did not return True.",
                ___
            ]
        ] }
    },
    SameTest -> MatchQ,
    TestID   -> "ScriptConfirmBy-5@@Tests/ScriptConfirmation.wlt:374,1-393,2"
]

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Messages*)
VerificationTest[
    traceSideEffects[ ScriptConfirmBy[ 1.5, IntegerQ ], "Session" ],
    KeyValuePattern @ {
        "Result"      -> $aborted[ ],
        "SideEffects" -> { HoldComplete[
            (Print|WriteString)[
                ___,
                "ScriptConfirmBy: IntegerQ[1.5] did not return True.",
                ___
            ]
        ] }
    },
    { ScriptConfirmBy::Session },
    SameTest -> MatchQ,
    TestID   -> "ScriptConfirmBy-6@@Tests/ScriptConfirmation.wlt:398,1-413,2"
]

VerificationTest[
    ScriptConfirmBy[ ],
    Failure[ "ScriptConfirmBy::ArgumentCount", _Association ],
    { ScriptConfirmBy::ArgumentCount },
    SameTest -> MatchQ,
    TestID   -> "ScriptConfirmBy-7@@Tests/ScriptConfirmation.wlt:415,1-421,2"
]

VerificationTest[
    ScriptConfirmBy[ 1 ],
    Failure[ "ScriptConfirmBy::ArgumentCount", _Association ],
    { ScriptConfirmBy::ArgumentCount },
    SameTest -> MatchQ,
    TestID   -> "ScriptConfirmBy-8@@Tests/ScriptConfirmation.wlt:423,1-429,2"
]

VerificationTest[
    ScriptConfirmBy[ 1, 2, 3, 4, 5 ],
    Failure[ "ScriptConfirmBy::ArgumentCount", _Association ],
    { ScriptConfirmBy::ArgumentCount },
    SameTest -> MatchQ,
    TestID   -> "ScriptConfirmBy-9@@Tests/ScriptConfirmation.wlt:431,1-437,2"
]

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*ScriptConfirmMatch*)
VerificationTest[
    traceSideEffects[ ScriptConfirmMatch[ 1.5, _Integer ], "Script" ],
    KeyValuePattern @ {
        "Result"      -> $exit[ 1 ],
        "SideEffects" -> { HoldComplete[
            (Print|WriteString)[
                ___,
                "ScriptConfirmMatch: 1.5 does not match _Integer.",
                ___
            ]
        ] }
    },
    SameTest -> MatchQ,
    TestID   -> "ScriptConfirmMatch-1@@Tests/ScriptConfirmation.wlt:442,1-456,2"
]

VerificationTest[
    traceSideEffects[ ScriptConfirmMatch[ 1, _Integer ], "Script" ],
    KeyValuePattern @ {
        "Result"      -> 1,
        "SideEffects" -> { }
    },
    SameTest -> MatchQ,
    TestID   -> "ScriptConfirmMatch-2@@Tests/ScriptConfirmation.wlt:458,1-466,2"
]

VerificationTest[
    traceSideEffects[
        ScriptConfirmMatch[ Identity[ 1.5 ], _Integer, "`Input` `Pattern`" ],
        "Script"
    ],
    KeyValuePattern @ {
        "Result"      -> $exit[ 1 ],
        "SideEffects" -> { HoldComplete[
            (Print|WriteString)[ ___, "Identity[1.5] _Integer", ___ ]
        ] }
    },
    SameTest -> MatchQ,
    TestID   -> "ScriptConfirmMatch-3@@Tests/ScriptConfirmation.wlt:468,1-481,2"
]

VerificationTest[
        traceSideEffects[
        ScriptConfirmMatch[ Identity[ 1.5 ], _Integer, "`Input` `Pattern`", 123 ],
        "Script"
    ],
    KeyValuePattern @ {
        "Result"      -> $exit[ 123 ],
        "SideEffects" -> { HoldComplete[
            (Print|WriteString)[ ___, "Identity[1.5] _Integer", ___ ]
        ] }
    },
    SameTest -> MatchQ,
    TestID   -> "ScriptConfirmMatch-4@@Tests/ScriptConfirmation.wlt:483,1-496,2"
]

VerificationTest[
    traceSideEffects[
        ScriptConfirmMatch[ 1.5, _Integer, Automatic, 2 ];
        ScriptConfirmMatch[ 2.5, _Integer, Automatic, 3 ];
        ,
        "Script"
    ],
    KeyValuePattern @ {
        "Result"      -> $exit[ 2 ],
        "SideEffects" -> { HoldComplete[
            (Print|WriteString)[
                ___,
                "ScriptConfirmMatch: 1.5 does not match _Integer.",
                ___
            ]
        ] }
    },
    SameTest -> MatchQ,
    TestID   -> "ScriptConfirmMatch-5@@Tests/ScriptConfirmation.wlt:498,1-517,2"
]

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Messages*)
VerificationTest[
    traceSideEffects[ ScriptConfirmMatch[ 1.5, _Integer ], "Session" ],
    KeyValuePattern @ {
        "Result"      -> $aborted[ ],
        "SideEffects" -> { HoldComplete[
            (Print|WriteString)[
                ___,
                "ScriptConfirmMatch: 1.5 does not match _Integer.",
                ___
            ]
        ] }
    },
    { ScriptConfirmMatch::Session },
    SameTest -> MatchQ,
    TestID   -> "ScriptConfirmMatch-6@@Tests/ScriptConfirmation.wlt:522,1-537,2"
]

VerificationTest[
    ScriptConfirmMatch[ ],
    Failure[ "ScriptConfirmMatch::ArgumentCount", _Association ],
    { ScriptConfirmMatch::ArgumentCount },
    SameTest -> MatchQ,
    TestID   -> "ScriptConfirmMatch-7@@Tests/ScriptConfirmation.wlt:539,1-545,2"
]

VerificationTest[
    ScriptConfirmMatch[ 1 ],
    Failure[ "ScriptConfirmMatch::ArgumentCount", _Association ],
    { ScriptConfirmMatch::ArgumentCount },
    SameTest -> MatchQ,
    TestID   -> "ScriptConfirmMatch-8@@Tests/ScriptConfirmation.wlt:547,1-553,2"
]

VerificationTest[
    ScriptConfirmMatch[ 1, 2, 3, 4, 5 ],
    Failure[ "ScriptConfirmMatch::ArgumentCount", _Association ],
    { ScriptConfirmMatch::ArgumentCount },
    SameTest -> MatchQ,
    TestID   -> "ScriptConfirmMatch-9@@Tests/ScriptConfirmation.wlt:555,1-561,2"
]

(* :!CodeAnalysis::EndBlock:: *)