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
    TestID -> "Initialize-PacletObject"
]

VerificationTest[
    Echo[ $TestFileName, "TestFileName" ];
    PacletDirectoryLoad @ Echo[ $pacletDir, "PacletDirectory" ],
    { ___, $pacletDir, ___ },
    SameTest -> MatchQ,
    TestID   -> "Initialize-PacletDirectoryLoad"
]

VerificationTest[
    Needs[ "Wolfram`PacletCICD`" ],
    Null,
    TestID -> "Initialize-Paclet-Needs"
]

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Context Check*)
VerificationTest[
    Context @ ScriptConfirm,
    "Wolfram`PacletCICD`",
    TestID -> "ScriptConfirm-Context"
]

VerificationTest[
    Context @ ScriptConfirmAssert,
    "Wolfram`PacletCICD`",
    TestID -> "ScriptConfirmAssert-Context"
]

VerificationTest[
    Context @ ScriptConfirmBy,
    "Wolfram`PacletCICD`",
    TestID -> "ScriptConfirmBy-Context"
]

VerificationTest[
    Context @ ScriptConfirmMatch,
    "Wolfram`PacletCICD`",
    TestID -> "ScriptConfirmMatch-Context"
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
    TestID   -> "ScriptConfirm-1"
]

VerificationTest[
    traceSideEffects[ ScriptConfirm[ 1 + 1 ], "Script" ],
    KeyValuePattern @ {
        "Result"      -> 2,
        "SideEffects" -> { }
    },
    SameTest -> MatchQ,
    TestID   -> "ScriptConfirm-2"
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
    TestID   -> "ScriptConfirm-3"
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
    TestID   -> "ScriptConfirm-4"
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
    TestID   -> "ScriptConfirm-5"
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
    TestID   -> "ScriptConfirm-6"
]

VerificationTest[
    ScriptConfirm[ ],
    Failure[ "ScriptConfirm::ArgumentCount", _Association ],
    { ScriptConfirm::ArgumentCount },
    SameTest -> MatchQ,
    TestID   -> "ScriptConfirm-7"
]

VerificationTest[
    ScriptConfirm[ 1, 2, 3, 4 ],
    Failure[ "ScriptConfirm::ArgumentCount", _Association ],
    { ScriptConfirm::ArgumentCount },
    SameTest -> MatchQ,
    TestID   -> "ScriptConfirm-8"
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
    TestID   -> "ScriptConfirmAssert-1"
]

VerificationTest[
    traceSideEffects[ ScriptConfirmAssert @ True, "Script" ],
    KeyValuePattern @ {
        "Result"      -> Null,
        "SideEffects" -> { }
    },
    SameTest -> MatchQ,
    TestID   -> "ScriptConfirmAssert-2"
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
    TestID   -> "ScriptConfirmAssert-3"
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
    TestID   -> "ScriptConfirmAssert-4"
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
    TestID   -> "ScriptConfirmAssert-5"
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
    TestID   -> "ScriptConfirmAssert-6"
]

VerificationTest[
    ScriptConfirmAssert[ ],
    Failure[ "ScriptConfirmAssert::ArgumentCount", _Association ],
    { ScriptConfirmAssert::ArgumentCount },
    SameTest -> MatchQ,
    TestID   -> "ScriptConfirmAssert-7"
]

VerificationTest[
    ScriptConfirmAssert[ 1, 2, 3, 4 ],
    Failure[ "ScriptConfirmAssert::ArgumentCount", _Association ],
    { ScriptConfirmAssert::ArgumentCount },
    SameTest -> MatchQ,
    TestID   -> "ScriptConfirmAssert-8"
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
    TestID   -> "ScriptConfirmBy-1"
]

VerificationTest[
    traceSideEffects[ ScriptConfirmBy[ 1, IntegerQ ], "Script" ],
    KeyValuePattern @ {
        "Result"      -> 1,
        "SideEffects" -> { }
    },
    SameTest -> MatchQ,
    TestID   -> "ScriptConfirmBy-2"
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
    TestID   -> "ScriptConfirmBy-3"
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
    TestID   -> "ScriptConfirmBy-4"
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
    TestID   -> "ScriptConfirmBy-5"
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
    TestID   -> "ScriptConfirmBy-6"
]

VerificationTest[
    ScriptConfirmBy[ ],
    Failure[ "ScriptConfirmBy::ArgumentCount", _Association ],
    { ScriptConfirmBy::ArgumentCount },
    SameTest -> MatchQ,
    TestID   -> "ScriptConfirmBy-7"
]

VerificationTest[
    ScriptConfirmBy[ 1 ],
    Failure[ "ScriptConfirmBy::ArgumentCount", _Association ],
    { ScriptConfirmBy::ArgumentCount },
    SameTest -> MatchQ,
    TestID   -> "ScriptConfirmBy-8"
]

VerificationTest[
    ScriptConfirmBy[ 1, 2, 3, 4, 5 ],
    Failure[ "ScriptConfirmBy::ArgumentCount", _Association ],
    { ScriptConfirmBy::ArgumentCount },
    SameTest -> MatchQ,
    TestID   -> "ScriptConfirmBy-9"
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
    TestID   -> "ScriptConfirmMatch-1"
]

VerificationTest[
    traceSideEffects[ ScriptConfirmMatch[ 1, _Integer ], "Script" ],
    KeyValuePattern @ {
        "Result"      -> 1,
        "SideEffects" -> { }
    },
    SameTest -> MatchQ,
    TestID   -> "ScriptConfirmMatch-2"
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
    TestID   -> "ScriptConfirmMatch-3"
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
    TestID   -> "ScriptConfirmMatch-4"
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
    TestID   -> "ScriptConfirmMatch-5"
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
    TestID   -> "ScriptConfirmMatch-6"
]

VerificationTest[
    ScriptConfirmMatch[ ],
    Failure[ "ScriptConfirmMatch::ArgumentCount", _Association ],
    { ScriptConfirmMatch::ArgumentCount },
    SameTest -> MatchQ,
    TestID   -> "ScriptConfirmMatch-7"
]

VerificationTest[
    ScriptConfirmMatch[ 1 ],
    Failure[ "ScriptConfirmMatch::ArgumentCount", _Association ],
    { ScriptConfirmMatch::ArgumentCount },
    SameTest -> MatchQ,
    TestID   -> "ScriptConfirmMatch-8"
]

VerificationTest[
    ScriptConfirmMatch[ 1, 2, 3, 4, 5 ],
    Failure[ "ScriptConfirmMatch::ArgumentCount", _Association ],
    { ScriptConfirmMatch::ArgumentCount },
    SameTest -> MatchQ,
    TestID   -> "ScriptConfirmMatch-9"
]

(* :!CodeAnalysis::EndBlock:: *)