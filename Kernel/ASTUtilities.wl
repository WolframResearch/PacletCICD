(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Header*)
BeginPackage[ "Wolfram`PacletCICD`" ];

ASTPattern // ClearAll;
FromAST // ClearAll;

Begin[ "`Private`" ];

Needs[ "CodeParser`" ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*FromAST*)
FromAST[ ast_ ] := FromAST[ ast, ##1 & ];

FromAST[ ast: _LeafNode|_CallNode, wrapper_ ] :=
    ToExpression[ ToFullFormString @ ast, InputForm, wrapper ];

FromAST[ ContainerNode[ _, ast_List, _ ], wrapper_ ] :=
    FromAST[ ast, wrapper ];

FromAST[ ast_List, wrapper_ ] := FromAST[ #, wrapper ] & /@ ast;

FromAST // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*ASTPattern*)
ASTPattern // Attributes = { HoldFirst };

ASTPattern[ patt_ ] := catchTop @ astPattern @ patt;
ASTPattern[ patt_, meta_ ] := catchTop @ astPattern[ patt, meta ];

ASTPattern // catchUndefined;


astPattern // Attributes = { HoldAllComplete };
$astPattern // Attributes = { HoldAllComplete };

astPattern[ patt_ ] /; ! FreeQ[ Unevaluated @ patt, _ASTPattern ] :=
    Module[ { held, expanded, new },
        held     = HoldComplete @ patt;
        expanded = expandNestedASTPatterns @ held;
        new      = astPattern @@ expanded;
        new /. $astPattern[ a_ ] :> a
    ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Pattern*)
astPattern[ Verbatim[ Pattern ][ sym_Symbol? symbolQ, patt_ ] ] :=
    Pattern @@ Hold[ sym, astPattern @ patt ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Special patterns*)
astPattern[ patt_ASTPattern  ] := patt;
astPattern[ patt_$astPattern ] := patt;

astPattern[ Verbatim[ Verbatim     ][ a___ ] ] := a;
astPattern[ Verbatim[ HoldPattern  ][ a_   ] ] := astPattern @ a;
astPattern[ Verbatim[ Alternatives ][ a___ ] ] :=
    Alternatives @@ (astPattern /@ HoldComplete @ a);

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Blanks*)
astPattern[ Verbatim[ _   ] ] := (_CallNode|_LeafNode);
astPattern[ Verbatim[ __  ] ] := (_CallNode|_LeafNode)..;
astPattern[ Verbatim[ ___ ] ] := (_CallNode|_LeafNode)...;

astPattern[ Verbatim[ Blank             ][ sym_? symbolQ ] ] := blank @ sym;
astPattern[ Verbatim[ BlankSequence     ][ sym_? symbolQ ] ] := blank @ sym..;
astPattern[ Verbatim[ BlankNullSequence ][ sym_? symbolQ ] ] := blank @ sym...;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*blank*)
blank // Attributes = { HoldAllComplete };
blank[ sym_? leafHeadQ ] := leafNode @ sym | callNode @ symNamePatt @ sym;
blank[ sym_ ] := callNode @ symNamePatt @ sym;
blank // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*leafNode*)
leafNode[ a_         ] := LeafNode[ a, _, _ ];
leafNode[ a_, b_     ] := LeafNode[ a, b, _ ];
leafNode[ a_, b_, c_ ] := LeafNode[ a, b, c ];

leafNode // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*callNode*)
callNode[ a_         ] := CallNode[ a, _, _ ];
callNode[ a_, b_     ] := CallNode[ a, b, _ ];
callNode[ a_, b_, c_ ] := CallNode[ a, b, c ];

callNode // catchUndefined;

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
astPattern[ Verbatim[ PatternTest ][
    Verbatim[ Pattern ][ s_Symbol? symbolQ, patt_ ],
    test_
] ] :=
    With[ { p = astPattern @ PatternTest[ patt, test ] },
        Pattern @@ HoldComplete[ s, p ]
    ];

astPattern[
    Verbatim[ PatternTest ][
        Verbatim[ Blank ][ head_? symbolQ ],
        test_
    ] ] /; leafTestQ[ head, test ] :=
    leafNode @ head;

astPattern[
    Verbatim[ PatternTest ][
        Verbatim[ BlankSequence ][ head_? symbolQ ],
        test_
    ]
] /; leafTestQ[ head, test ] :=
    leafNode @ head..;

astPattern[
    Verbatim[ PatternTest ][
        Verbatim[ BlankNullSequence ][ head_? symbolQ ],
        test_
    ]
] /; leafTestQ[ head, test ] :=
    leafNode @ head...;

astPattern[ Verbatim[ PatternTest ][ patt_, test_ ] ] :=
    With[ { p = astPattern @ patt },
        Apply[
            PatternTest,
            HoldComplete[
                p,
                FromAST[ #, test ] &
            ]
        ]
    ];

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
    Cases[ HoldComplete @ lhs,
           Verbatim[ Pattern ][ s_Symbol? symbolQ, _ ] :>
               HoldPattern @ s :>
                   RuleCondition[ FromAST[ s, $ConditionHold ], True ],
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
(*Two argument form*)
astPattern[ patt_, meta_ ] := insertMetaPatt[ astPattern @ patt, meta ];

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*insertMetaPatt*)
insertMetaPatt[ (h: LeafNode|CallNode)[ a_, b_, _ ], meta_ ] :=
    h[ a, b, meta ];

insertMetaPatt[ Verbatim[ Pattern ][ s_, p_ ], meta_ ] :=
    With[ { ins = insertMetaPatt[ p, meta ] },
        Pattern @@ Hold[ s, ins ]
    ];

insertMetaPatt[ patt_Alternatives, meta_ ] :=
    insertMetaPatt[ #1, meta ] & /@ patt;

insertMetaPatt[ Verbatim[ Condition ][ lhs_, rhs_ ], meta_ ] :=
    With[ { ins = insertMetaPatt[ lhs, meta ] },
        Condition @@ Hold[ ins, rhs ]
    ];

insertMetaPatt // catchUndefined;

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
(* ::Subsection::Closed:: *)
(*expandNestedASTPatterns*)
expandNestedASTPatterns[ expr_ ] :=
    ReplaceAll[
        expr,
        HoldPattern @ ASTPattern[ a___ ] :>
            With[ { p = astPattern @ a }, $astPattern @ p /; True ]
    ];

expandNestedASTPatterns // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Footer*)
End[ ];
EndPackage[ ];