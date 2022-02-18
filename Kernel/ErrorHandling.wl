(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Header*)
BeginPackage[ "Wolfram`PacletCICD`" ];

PacletCICD // ClearAll;

Begin[ "`Private`" ];

Needs[ "DefinitionNotebookClient`" -> "dnc`"  ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Definition Utilities*)

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*catchUndefined*)
catchUndefined // Attributes = { HoldFirst };

catchUndefined[ sym_Symbol ] :=
    catchUndefined [ sym, DownValues ];

catchUndefined[ sym_Symbol, DownValues ] :=
    e: sym[ ___ ] :=
        throwMessageFailure[
            PacletCICD::undefined,
            HoldForm @ sym,
            HoldForm @ e,
            DownValues
        ];

catchUndefined[ sym_Symbol, SubValues ] :=
    e: sym[ ___ ][ ___ ] :=
        throwMessageFailure[
            PacletCICD::undefined,
            HoldForm @ sym,
            HoldForm @ e,
            SubValues
        ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*PacletCICD*)
(* PacletCICD is just a symbol for attaching general messages *)
PacletCICD::unknown   =
"An internal error occurred.";

PacletCICD::undefined =
"An internal error occurred (encountered unexpected pattern for `1`).";

PacletCICD::error   = "`1`";
PacletCICD::warning = "`1`";
PacletCICD::notice  = "`1`";
PacletCICD::debug   = "`1`";

PacletCICD::empty =
"An evaluation failed.";

PacletCICD::tagged =
"A failure of type \"`1`\" occurred.";

PacletCICD::expression =
"An evaluation failed with expression `1`.";

PacletCICD::arguments =
"An evaluation failed with arguments `1`.";

PacletCICD::message =
"`1`";

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Definitions*)

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*catchTop*)
catchTop // Attributes = { HoldFirst };

catchTop[ eval_ ] /; $catching := eval;

catchTop[ eval_ ] :=
    Block[ { $catching = True, $messaged = False },
        Catch[ eval, $top ]
    ];

catchTop // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*catch*)
catch // Attributes = { HoldFirst };

catch[ eval_ ] :=
    Block[ { $catching = True, $messaged = False },
        noExit @ Catch[ eval, $top ]
    ];

catch // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*catchQuiet*)
catchQuiet // Attributes = { HoldFirst };
catchQuiet[ eval_ ] := Quiet @ catch @ eval;
catchQuiet // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*throwFailure*)
throwFailure // Attributes = { HoldFirst };
throwFailure[ args___ ] := Quiet @ throwMessageFailure @ args;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*throwMessageFailure*)
throwMessageFailure // Attributes = { HoldFirst };

throwMessageFailure[ msg_MessageName, args___ ] :=
    StackInhibit @ Module[ { failure },
        failure = messageFailure[ msg, args ];
        If[ TrueQ @ $catching,
            Throw[ failure, $top ],
            failure
        ]
    ];

throwMessageFailure[ failure_Failure ] :=
    StackInhibit[
        messageFailure @ failure;
        If[ TrueQ @ $catching, Throw[ failure, $top ], failure ]
    ];

throwMessageFailure[ msg_String ] :=
    throwMessageFailure[ PacletCICD::error, msg ];

throwMessageFailure[ args___ ] :=
    throwMessageFailure[
        PacletCICD::unknown,
        HoldForm @ throwMessageFailure @ args
    ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*messageFailure*)
messageFailure := (
    SetOptions[
        MessageFailure,
        "GeneralSymbol" -> PacletCICD,
        "MessagedFlag" :> $messaged
    ];

    messageFailure = MessageFailure
);

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*joinFailure*)
joinFailure[ Failure[ _, a_Association ], Failure[ tag_, b_Association ] ] :=
    Module[ { as },
        as = Join[ a, b ];
        If[ AssociationQ @ as,
            Failure[ tag, as ],
            throwError[ "Expected an association in `1`.", as ]
        ]
    ];

joinFailure // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*generalMessage*)
$$messageType = "error"|"warning"|"notice"|"debug";

generalMessage[ tag: $$messageType, template_, as_Association ] :=
    messageFailure[
        MessageName[ PacletCICD, tag ],
        TemplateApply[ template, as ],
        as
    ];

generalMessage[ tag: $$messageType, template_, a___ ] :=
    messageFailure[
        MessageName[ PacletCICD, tag ],
        TemplateApply[ template, { a } ],
        a
    ];

generalMessage[ template_, a___ ] := generalMessage[ "notice", template, a ];

generalMessage // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*throwGeneralMessage*)
throwGeneralMessage[ tag: $$messageType, template_, a___ ] :=
    Module[ { failure },
        failure = generalMessage[ tag, template, a ];
        If[ TrueQ @ $catching,
            Throw[ failure, $top ],
            failure
        ]
    ];

throwGeneralMessage[ template_, a___ ] :=
    throwGeneralMessage[ "error", template, a ];

throwGeneralMessage // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*throwError*)
throwError[ template_, a___ ] := throwGeneralMessage[ "error", template, a ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*exitFailure*)
exitFailure // Attributes = { HoldFirst };

(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::SuspiciousSessionSymbol:: *)

exitFailure[ msg_MessageName, code_, result_ ] := (
    If[ $EvaluationEnvironment === "Script"
        ,
        Block[ { DefinitionNotebookClient`$ConsoleType = Automatic },
            DefinitionNotebookClient`ConsolePrint[
                ToString @ Unevaluated @ msg <> ": " <> ToString @ msg,
                "Level" -> "Error"
            ]
        ];
        Print @ result
    ];
    exitOr[ code, throwMessageFailure[ msg, result ] ]
);

exitFailure[ fail_Failure, code_Integer: 1 ] := (
    If[ $EvaluationEnvironment === "Script"
        ,
        Block[ { DefinitionNotebookClient`$ConsoleType = Automatic },
            DefinitionNotebookClient`ConsolePrint[
                fail[ "Message" ],
                "Level" -> "Error"
            ]
        ];
        With[ { res = fail[ "Result" ] },
            If[ MissingQ @ res,
                Print @ fail,
                Print @ res
            ]
        ]
    ];
    exitOr[ code, throwMessageFailure @ fail ]
);

exitFailure[ tag_String, as_Association, code_Integer: 1 ] :=
    exitFailure[ Failure[ tag, as ], code ];

exitFailure // catchUndefined;

(* :!CodeAnalysis::EndBlock:: *)

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*exitOr*)
exitOr // Attributes = { HoldRest };

(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::SuspiciousSessionSymbol:: *)
exitOr[ code_, eval_ ] /; $noExit := eval;
exitOr[ code_, eval_ ] /; $evaluationEnvironment === "Script" := Exit @ code;
exitOr[ code_, eval_ ] := eval;
(* :!CodeAnalysis::EndBlock:: *)

$evaluationEnvironment := $EvaluationEnvironment;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*noExit*)
noExit // Attributes = { HoldFirst };
noExit[ eval_ ] := Block[ { $noExit = True }, eval ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Footer*)
End[ ];
EndPackage[ ];
