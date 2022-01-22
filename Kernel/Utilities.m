(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Header*)
BeginPackage[ "Wolfram`PacletCICD`" ];

PacletCICD;

Begin[ "`Private`" ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*PacletCICD*)
(* PacletCICD is just a symbol for attaching general messages *)
PacletCICD::error   = "`1`";
PacletCICD::warning = "`1`";
PacletCICD::notice  = "`1`";
PacletCICD::debug   = "`1`";
PacletCICD::unknown = "`1`";

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Error Handling*)

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*catchTop*)
catchTop // Attributes = { HoldFirst };

catchTop[ eval_ ] :=
    Block[ { $catching = True, $MessageList = { }, catchTop = # & },
        Catch[ eval, $top ]
    ];

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
    Module[ { failure },
        failure = messageFailure[ msg, args ];
        If[ TrueQ @ $catching,
            Throw[ failure, $top ],
            failure
        ]
    ];

throwMessageFailure[ msg_String ] :=
    throwMessageFailure[ PacletCICD::error, msg ];

throwMessageFailure[ args___ ] :=
    throwMessageFailure[ PacletCICD::unknown, args ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*messageFailure*)
messageFailure // Attributes = { HoldFirst };

messageFailure[ msg: MessageName[ sym_Symbol, mtag__ ], args___ ] :=
    Module[ { tag, info, failure },
        tag = StringRiffle[ { SymbolName @ Unevaluated @ sym, mtag }, "::" ];
        info = <| "MessageTemplate" :> msg, "MessageParameters" :> { args } |>;
        failure = Failure[ tag, info ];
        If[ $MessageList === { }, Message @ Evaluate @ failure ];
        failure
    ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*exitFailure*)
exitFailure // Attributes = { HoldFirst };

(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::SuspiciousSessionSymbol:: *)
exitFailure[ msg_MessageName, code_, result_ ] :=
    If[ $EvaluationEnvironment === "Script"
        ,
        Block[ { DefinitionNotebookClient`$ConsoleType = Automatic },
            DefinitionNotebookClient`ConsolePrint[
                ToString @ Unevaluated @ msg <> ": " <> ToString @ msg,
                "Level" -> "Error"
            ]
        ];
        Print @ result;
        Exit @ code
        ,
        throwMessageFailure[ msg, result ]
    ];

(* :!CodeAnalysis::EndBlock:: *)

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Definition Notebook Utilities *)

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*defNBQ*)
defNBQ[ file_? FileExistsQ ] := TrueQ @ Quiet @ defNBQ0 @ file;

defNBQ0[ file_? FileExistsQ ] := defNBQ0[ Hash @ ReadByteArray @ file, file ];
defNBQ0[ h_, file_ ] := defNBQ0[ h, file ] = defNBQ0 @ Import[ file, "NB" ];
defNBQ0[ Notebook[ ___, TaggingRules -> tags_, ___ ] ] := defNBQ0 @ tags;
defNBQ0[ KeyValuePattern[ "ResourceType" -> "Paclet" ] ] := True;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*findDefinitionNotebook*)
findDefinitionNotebook[ dir_? DirectoryQ ] :=
    findDefinitionNotebook[
        dir,
        FileNameJoin @ { ExpandFileName @ dir, "DefinitionNotebook.nb" }
    ];

findDefinitionNotebook[ dir_, file_? defNBQ ] :=
    Flatten @ File @ file;

findDefinitionNotebook[ dir_, _ ] :=
    SelectFirst[ FileNames[ "*.nb", dir ], defNBQ ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Misc Programming Utilities*)

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*filterOptions*)
filterOptions[ sym_Symbol, opts: (Rule|RuleDelayed)[ _, _ ]... ] :=
    Sequence @@ FilterRules[ Flatten @ { opts }, Options @ sym ];

filterOptions[ { syms___Symbol }, opts: (Rule|RuleDelayed)[ _, _ ]... ] :=
    Apply[
        Sequence,
        FilterRules[
            Flatten @ { opts },
            Flatten @ Cases[ HoldComplete @ syms, s_ :> Options @ s ]
        ]
    ];

filterOptions[
    Verbatim[ OptionsPattern ][ syms: { ___ } ],
    opts: (Rule|RuleDelayed)[ _, _ ]...
] :=
    filterOptions[ syms, opts ];

filterOptions /:
    (sym_Symbol)[
        args___,
        filterOptions[ opts1: (Rule|RuleDelayed)[ _, _ ]... ],
        opts2: (Rule|RuleDelayed)[ _, _ ]...
    ] :=
    With[ { filtered = filterOptions[ sym, opts1 ] },
        sym[ args, filtered, opts2 ]
    ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Footer*)
End[ ];
EndPackage[ ];
