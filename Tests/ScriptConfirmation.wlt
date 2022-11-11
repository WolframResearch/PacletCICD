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

printedMessage[ msg_ ] := HoldComplete[
    (Print|WriteString)[ ___, _String? (StringEndsQ @ msg), ___ ]
];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*ScriptConfirm*)
VerificationTest[
    traceSideEffects[ ScriptConfirm @ $Failed, "Script" ],
    KeyValuePattern @ {
        "Result"      -> $exit[ 1 ],
        "SideEffects" -> { printedMessage[ "ScriptConfirm: $Failed encountered." ] }
    },
    SameTest -> MatchQ,
    TestID   -> "ScriptConfirm-1@@Tests/ScriptConfirmation.wlt:93,1-101,2"
]

VerificationTest[
    traceSideEffects[ ScriptConfirm[ 1 + 1 ], "Script" ],
    KeyValuePattern @ {
        "Result"      -> 2,
        "SideEffects" -> { }
    },
    SameTest -> MatchQ,
    TestID   -> "ScriptConfirm-2@@Tests/ScriptConfirmation.wlt:103,1-111,2"
]

VerificationTest[
    traceSideEffects[ ScriptConfirm[ Identity @ $Failed, "`Input`" ], "Script" ],
    KeyValuePattern @ {
        "Result"      -> $exit[ 1 ],
        "SideEffects" -> { printedMessage[ "Identity[$Failed]" ] }
    },
    SameTest -> MatchQ,
    TestID   -> "ScriptConfirm-3@@Tests/ScriptConfirmation.wlt:113,1-121,2"
]

VerificationTest[
    traceSideEffects[
        ScriptConfirm[ Identity @ $Failed, "`Input`", 123 ],
        "Script"
    ],
    KeyValuePattern @ {
        "Result"      -> $exit[ 123 ],
        "SideEffects" -> { printedMessage[ "Identity[$Failed]" ] }
    },
    SameTest -> MatchQ,
    TestID   -> "ScriptConfirm-4@@Tests/ScriptConfirmation.wlt:123,1-134,2"
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
        "SideEffects" -> { printedMessage[ "ScriptConfirm: $Canceled encountered." ] }
    },
    SameTest -> MatchQ,
    TestID   -> "ScriptConfirm-5@@Tests/ScriptConfirmation.wlt:136,1-149,2"
]

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Messages*)
VerificationTest[
    traceSideEffects[ ScriptConfirm @ $Failed, "Session" ],
    KeyValuePattern @ {
        "Result"      -> $aborted[ ],
        "SideEffects" -> { printedMessage[ "ScriptConfirm: $Failed encountered." ] }
    },
    { ScriptConfirm::Session },
    SameTest -> MatchQ,
    TestID   -> "ScriptConfirm-6@@Tests/ScriptConfirmation.wlt:154,1-163,2"
]

VerificationTest[
    ScriptConfirm[ ],
    Failure[ "ScriptConfirm::ArgumentCount", _Association ],
    { ScriptConfirm::ArgumentCount },
    SameTest -> MatchQ,
    TestID   -> "ScriptConfirm-7@@Tests/ScriptConfirmation.wlt:165,1-171,2"
]

VerificationTest[
    ScriptConfirm[ 1, 2, 3, 4 ],
    Failure[ "ScriptConfirm::ArgumentCount", _Association ],
    { ScriptConfirm::ArgumentCount },
    SameTest -> MatchQ,
    TestID   -> "ScriptConfirm-8@@Tests/ScriptConfirmation.wlt:173,1-179,2"
]

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*ScriptConfirmAssert*)
VerificationTest[
    traceSideEffects[ ScriptConfirmAssert @ False, "Script" ],
    KeyValuePattern @ {
        "Result"      -> $exit[ 1 ],
        "SideEffects" -> { printedMessage[ "ScriptConfirmAssert: Assertion False failed." ] }
    },
    SameTest -> MatchQ,
    TestID   -> "ScriptConfirmAssert-1@@Tests/ScriptConfirmation.wlt:184,1-192,2"
]

VerificationTest[
    traceSideEffects[ ScriptConfirmAssert @ True, "Script" ],
    KeyValuePattern @ {
        "Result"      -> Null,
        "SideEffects" -> { }
    },
    SameTest -> MatchQ,
    TestID   -> "ScriptConfirmAssert-2@@Tests/ScriptConfirmation.wlt:194,1-202,2"
]

VerificationTest[
    traceSideEffects[
        ScriptConfirmAssert[ Identity @ $Failed, "`Input`" ],
        "Script"
    ],
    KeyValuePattern @ {
        "Result"      -> $exit[ 1 ],
        "SideEffects" -> { printedMessage[ "Identity[$Failed]" ] }
    },
    SameTest -> MatchQ,
    TestID   -> "ScriptConfirmAssert-3@@Tests/ScriptConfirmation.wlt:204,1-215,2"
]

VerificationTest[
        traceSideEffects[
        ScriptConfirmAssert[ Identity @ $Failed, "`Input`", 123 ],
        "Script"
    ],
    KeyValuePattern @ {
        "Result"      -> $exit[ 123 ],
        "SideEffects" -> { printedMessage[ "Identity[$Failed]" ] }
    },
    SameTest -> MatchQ,
    TestID   -> "ScriptConfirmAssert-4@@Tests/ScriptConfirmation.wlt:217,1-228,2"
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
        "SideEffects" -> { printedMessage[ "ScriptConfirmAssert: Assertion $Canceled failed." ] }
    },
    SameTest -> MatchQ,
    TestID   -> "ScriptConfirmAssert-5@@Tests/ScriptConfirmation.wlt:230,1-243,2"
]

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Messages*)
VerificationTest[
    traceSideEffects[ ScriptConfirmAssert @ $Failed, "Session" ],
    KeyValuePattern @ {
        "Result"      -> $aborted[ ],
        "SideEffects" -> { printedMessage[ "ScriptConfirmAssert: Assertion $Failed failed." ] }
    },
    { ScriptConfirmAssert::Session },
    SameTest -> MatchQ,
    TestID   -> "ScriptConfirmAssert-6@@Tests/ScriptConfirmation.wlt:248,1-257,2"
]

VerificationTest[
    ScriptConfirmAssert[ ],
    Failure[ "ScriptConfirmAssert::ArgumentCount", _Association ],
    { ScriptConfirmAssert::ArgumentCount },
    SameTest -> MatchQ,
    TestID   -> "ScriptConfirmAssert-7@@Tests/ScriptConfirmation.wlt:259,1-265,2"
]

VerificationTest[
    ScriptConfirmAssert[ 1, 2, 3, 4 ],
    Failure[ "ScriptConfirmAssert::ArgumentCount", _Association ],
    { ScriptConfirmAssert::ArgumentCount },
    SameTest -> MatchQ,
    TestID   -> "ScriptConfirmAssert-8@@Tests/ScriptConfirmation.wlt:267,1-273,2"
]

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*ScriptConfirmBy*)
VerificationTest[
    traceSideEffects[ ScriptConfirmBy[ 1.5, IntegerQ ], "Script" ],
    KeyValuePattern @ {
        "Result"      -> $exit[ 1 ],
        "SideEffects" -> { printedMessage[ "ScriptConfirmBy: IntegerQ[1.5] did not return True." ] }
    },
    SameTest -> MatchQ,
    TestID   -> "ScriptConfirmBy-1@@Tests/ScriptConfirmation.wlt:278,1-286,2"
]

VerificationTest[
    traceSideEffects[ ScriptConfirmBy[ 1, IntegerQ ], "Script" ],
    KeyValuePattern @ {
        "Result"      -> 1,
        "SideEffects" -> { }
    },
    SameTest -> MatchQ,
    TestID   -> "ScriptConfirmBy-2@@Tests/ScriptConfirmation.wlt:288,1-296,2"
]

VerificationTest[
    traceSideEffects[
        ScriptConfirmBy[ Identity[ 1.5 ], IntegerQ, "`Input` `Function`" ],
        "Script"
    ],
    KeyValuePattern @ {
        "Result"      -> $exit[ 1 ],
        "SideEffects" -> { printedMessage[ "Identity[1.5] IntegerQ" ] }
    },
    SameTest -> MatchQ,
    TestID   -> "ScriptConfirmBy-3@@Tests/ScriptConfirmation.wlt:298,1-309,2"
]

VerificationTest[
        traceSideEffects[
        ScriptConfirmBy[ Identity[ 1.5 ], IntegerQ, "`Input` `Function`", 123 ],
        "Script"
    ],
    KeyValuePattern @ {
        "Result"      -> $exit[ 123 ],
        "SideEffects" -> { printedMessage[ "Identity[1.5] IntegerQ" ] }
    },
    SameTest -> MatchQ,
    TestID   -> "ScriptConfirmBy-4@@Tests/ScriptConfirmation.wlt:311,1-322,2"
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
        "SideEffects" -> { printedMessage[ "ScriptConfirmBy: IntegerQ[1.5] did not return True." ] }
    },
    SameTest -> MatchQ,
    TestID   -> "ScriptConfirmBy-5@@Tests/ScriptConfirmation.wlt:324,1-337,2"
]

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Messages*)
VerificationTest[
    traceSideEffects[ ScriptConfirmBy[ 1.5, IntegerQ ], "Session" ],
    KeyValuePattern @ {
        "Result"      -> $aborted[ ],
        "SideEffects" -> { printedMessage[ "ScriptConfirmBy: IntegerQ[1.5] did not return True." ] }
    },
    { ScriptConfirmBy::Session },
    SameTest -> MatchQ,
    TestID   -> "ScriptConfirmBy-6@@Tests/ScriptConfirmation.wlt:342,1-351,2"
]

VerificationTest[
    ScriptConfirmBy[ ],
    Failure[ "ScriptConfirmBy::ArgumentCount", _Association ],
    { ScriptConfirmBy::ArgumentCount },
    SameTest -> MatchQ,
    TestID   -> "ScriptConfirmBy-7@@Tests/ScriptConfirmation.wlt:353,1-359,2"
]

VerificationTest[
    ScriptConfirmBy[ 1 ],
    Failure[ "ScriptConfirmBy::ArgumentCount", _Association ],
    { ScriptConfirmBy::ArgumentCount },
    SameTest -> MatchQ,
    TestID   -> "ScriptConfirmBy-8@@Tests/ScriptConfirmation.wlt:361,1-367,2"
]

VerificationTest[
    ScriptConfirmBy[ 1, 2, 3, 4, 5 ],
    Failure[ "ScriptConfirmBy::ArgumentCount", _Association ],
    { ScriptConfirmBy::ArgumentCount },
    SameTest -> MatchQ,
    TestID   -> "ScriptConfirmBy-9@@Tests/ScriptConfirmation.wlt:369,1-375,2"
]

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*ScriptConfirmMatch*)
VerificationTest[
    traceSideEffects[ ScriptConfirmMatch[ 1.5, _Integer ], "Script" ],
    KeyValuePattern @ {
        "Result"      -> $exit[ 1 ],
        "SideEffects" -> { printedMessage[ "ScriptConfirmMatch: 1.5 does not match _Integer." ] }
    },
    SameTest -> MatchQ,
    TestID   -> "ScriptConfirmMatch-1@@Tests/ScriptConfirmation.wlt:380,1-388,2"
]

VerificationTest[
    traceSideEffects[ ScriptConfirmMatch[ 1, _Integer ], "Script" ],
    KeyValuePattern @ {
        "Result"      -> 1,
        "SideEffects" -> { }
    },
    SameTest -> MatchQ,
    TestID   -> "ScriptConfirmMatch-2@@Tests/ScriptConfirmation.wlt:390,1-398,2"
]

VerificationTest[
    traceSideEffects[
        ScriptConfirmMatch[ Identity[ 1.5 ], _Integer, "`Input` `Pattern`" ],
        "Script"
    ],
    KeyValuePattern @ {
        "Result"      -> $exit[ 1 ],
        "SideEffects" -> { printedMessage[ "Identity[1.5] _Integer" ] }
    },
    SameTest -> MatchQ,
    TestID   -> "ScriptConfirmMatch-3@@Tests/ScriptConfirmation.wlt:400,1-411,2"
]

VerificationTest[
        traceSideEffects[
        ScriptConfirmMatch[ Identity[ 1.5 ], _Integer, "`Input` `Pattern`", 123 ],
        "Script"
    ],
    KeyValuePattern @ {
        "Result"      -> $exit[ 123 ],
        "SideEffects" -> { printedMessage[ "Identity[1.5] _Integer" ] }
    },
    SameTest -> MatchQ,
    TestID   -> "ScriptConfirmMatch-4@@Tests/ScriptConfirmation.wlt:413,1-424,2"
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
        "SideEffects" -> { printedMessage[ "ScriptConfirmMatch: 1.5 does not match _Integer." ] }
    },
    SameTest -> MatchQ,
    TestID   -> "ScriptConfirmMatch-5@@Tests/ScriptConfirmation.wlt:426,1-439,2"
]

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Messages*)
VerificationTest[
    traceSideEffects[ ScriptConfirmMatch[ 1.5, _Integer ], "Session" ],
    KeyValuePattern @ {
        "Result"      -> $aborted[ ],
        "SideEffects" -> { printedMessage[ "ScriptConfirmMatch: 1.5 does not match _Integer." ] }
    },
    { ScriptConfirmMatch::Session },
    SameTest -> MatchQ,
    TestID   -> "ScriptConfirmMatch-6@@Tests/ScriptConfirmation.wlt:444,1-453,2"
]

VerificationTest[
    ScriptConfirmMatch[ ],
    Failure[ "ScriptConfirmMatch::ArgumentCount", _Association ],
    { ScriptConfirmMatch::ArgumentCount },
    SameTest -> MatchQ,
    TestID   -> "ScriptConfirmMatch-7@@Tests/ScriptConfirmation.wlt:455,1-461,2"
]

VerificationTest[
    ScriptConfirmMatch[ 1 ],
    Failure[ "ScriptConfirmMatch::ArgumentCount", _Association ],
    { ScriptConfirmMatch::ArgumentCount },
    SameTest -> MatchQ,
    TestID   -> "ScriptConfirmMatch-8@@Tests/ScriptConfirmation.wlt:463,1-469,2"
]

VerificationTest[
    ScriptConfirmMatch[ 1, 2, 3, 4, 5 ],
    Failure[ "ScriptConfirmMatch::ArgumentCount", _Association ],
    { ScriptConfirmMatch::ArgumentCount },
    SameTest -> MatchQ,
    TestID   -> "ScriptConfirmMatch-9@@Tests/ScriptConfirmation.wlt:471,1-477,2"
]

(* :!CodeAnalysis::EndBlock:: *)