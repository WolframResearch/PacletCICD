(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Header*)
BeginPackage[ "Wolfram`PacletCICD`" ];

ASTPattern // ClearAll;

Begin[ "`Private`" ];

Needs[ "CodeParser`" ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*ASTPattern*)
ASTPattern[ patt_ ] := catchTop @ astPattern @ patt;

(* TODO: allow second arg to bind source info? *)

ASTPattern // catchUndefined;

astPattern // Attributes = { HoldAllComplete };

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Pattern*)
astPattern[ Verbatim[ Pattern ][ sym_Symbol? symbolQ, patt_ ] ] :=
    Pattern @@ Hold[ sym, astPattern @ patt ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Special patterns*)
astPattern[ Verbatim[ Verbatim ][ a___ ] ] := a;
astPattern[ Verbatim[ HoldPattern ][ a_ ] ] := astPattern @ a;
astPattern[ Verbatim[ Alternatives ][ a___ ] ] :=
    Alternatives @@ (astPattern /@ HoldComplete @ a);

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Blanks*)
astPattern[ Verbatim[ _ ] ] := _CallNode|_LeafNode;
astPattern[ Verbatim[ __ ] ] := (_CallNode|_LeafNode)..;
astPattern[ Verbatim[ ___ ] ] := (_CallNode|_LeafNode)...;

astPattern[ Verbatim[ Blank ][ sym_? symbolQ ] ] := blank @ sym;
astPattern[ Verbatim[ BlankSequence ][ sym_? symbolQ ] ] := blank @ sym ..;
astPattern[ Verbatim[ BlankNullSequence ][ sym_? symbolQ ] ] := blank @ sym ...;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*blank*)
blank // Attributes = { HoldAllComplete };

blank[ sym_? leafHeadQ ] :=
    LeafNode[ sym, _, _ ] | CallNode[ symNamePatt @ sym, _, _ ];

blank[ sym_ ] := CallNode[ symNamePatt @ sym, _, _ ];

blank // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*leafHeadQ*)
leafHeadQ // Attributes = { HoldAllComplete };
leafHeadQ[ Complex|Integer|Rational|Real|String|Symbol ] := True;
leafHeadQ[ ___ ] := False;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Repeated*)
astPattern[ Verbatim[ Repeated ][ x_, a___ ] ] :=
    Repeated[ astPattern @ x, a ];

astPattern[ Verbatim[ RepeatedNull ][ x_, a___ ] ] :=
    RepeatedNull[ astPattern @ x, a ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*PatternTest*)
astPattern[
    Verbatim[ PatternTest ][
        Verbatim[ Blank ][ sym_? symbolQ ],
        test_
    ] ] /; leafTestQ[ sym, test ] :=
    LeafNode[ sym, _, _ ];

astPattern[
    Verbatim[ PatternTest ][
        Verbatim[ BlankSequence ][ sym_? symbolQ ],
        test_
    ]
] /; leafTestQ[ sym, test ] :=
    LeafNode[ sym, _, _ ]..;

astPattern[
    Verbatim[ PatternTest ][
        Verbatim[ BlankNullSequence ][ sym_? symbolQ ],
        test_
    ]
] /; leafTestQ[ sym, test ] :=
    LeafNode[ sym, _, _ ]...;

(* TODO: general pattern tests (convert to equivalent Condition ) *)

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*leafTestQ*)
leafTestQ // Attributes = { HoldAllComplete };

leafTestQ[ Integer, IntegerQ     ] := True;
leafTestQ[ Real, Developer`RealQ ] := True;
leafTestQ[ String, StringQ       ] := True;
leafTestQ[ _? leafHeadQ, AtomQ   ] := True;
leafTestQ[ ___                   ] := False;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Atoms*)
astPattern[ sym_Symbol? symbolQ ] := symNamePatt @ sym;

astPattern[ expr: _Integer|_Real|_String ] /; AtomQ @ Unevaluated @ expr :=
    LeafNode[ Head @ expr, ToString[ expr, InputForm ], _ ];

astPattern[ r_Rational ] /; AtomQ @ Unevaluated @ r :=
    With[ { n = Numerator @ r, d = Denominator @ r },
        CallNode[
            LeafNode[ Symbol, "Times", _ ],
            {
                astPattern @ n,
                CallNode[
                    LeafNode[ Symbol, "Power", _ ],
                    { astPattern @ d, LeafNode[ Integer, "-1", _ ] },
                    _
                ]
            },
            _
        ]
    ];

astPattern[ c_Complex ] /; AtomQ @ Unevaluated @ c :=
    With[ { r = Re @ c, i = Im @ c },
        CallNode[
            LeafNode[ Symbol, "Plus", _ ],
            {
                astPattern @ r,
                CallNode[
                    LeafNode[ Symbol, "Times", _ ],
                    { astPattern @ i, LeafNode[ Symbol, "I", _ ] },
                    _
                ]
            },
            _
        ]
    ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Condition*)
astPattern[ Verbatim[ Condition ][ patt_, test_ ] ] :=
    astConditionPattern[ astPattern @ patt, test ];

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*astConditionPattern*)
astConditionPattern // Attributes = { HoldRest };

astConditionPattern[ lhs_, rhs_ ] :=
    With[ { rules = rhsConditionRules @ lhs },
        Condition @@ HoldComplete[ lhs, Unevaluated @ rhs /. rules ]
    ];

astConditionPattern // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*rhsConditionRules*)
rhsConditionRules[ lhs_ ] :=
    Cases[
        HoldComplete @ lhs,
        Verbatim[ Pattern ][ s_Symbol? symbolQ, _ ] :>
            HoldPattern @ s :>
                RuleCondition[
                    ToExpression[
                        ToFullFormString @ s,
                        InputForm,
                        $ConditionHold
                    ],
                    True
                ],
        Infinity,
        Heads -> True
    ];

rhsConditionRules // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Normal expressions*)
astPattern[ head_[ args___ ] ] :=
    CallNode[ astPattern @ head, astPattern /@ Unevaluated @ { args }, _ ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Undefined*)
astPattern // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Misc Utilities*)

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*symNamePatt*)
symNamePatt // Attributes = { HoldAllComplete };

symNamePatt[ sym_Symbol? symbolQ ] :=
    With[
        {
            name = SymbolName @ Unevaluated @ sym,
            ctx  = Context @ Unevaluated @ sym
        },
        LeafNode[ Symbol, name | ctx <> name, _ ]
    ];

symNamePatt // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Footer*)
End[ ];
EndPackage[ ];