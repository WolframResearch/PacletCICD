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
    TestID -> "Initialize-PacletObject@@Tests/ASTUtilities.wlt:4,1-14,2"
]

(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::SuspiciousSessionSymbol:: *)
VerificationTest[
    Echo[ $TestFileName, "TestFileName" ];
    PacletDirectoryLoad @ Echo[ $pacletDir, "PacletDirectory" ],
    { ___, $pacletDir, ___ },
    SameTest -> MatchQ,
    TestID   -> "Initialize-PacletDirectoryLoad@@Tests/ASTUtilities.wlt:18,1-24,2"
]
(* :!CodeAnalysis::EndBlock:: *)

VerificationTest[
    Needs[ "Wolfram`PacletCICD`" ],
    Null,
    TestID -> "Initialize-Needs-PacletCICD@@Tests/ASTUtilities.wlt:27,1-31,2"
]

VerificationTest[
    Needs[ "CodeParser`" ],
    Null,
    TestID -> "Initialize-Needs-CodeParser@@Tests/ASTUtilities.wlt:33,1-37,2"
]

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Context Check*)
VerificationTest[
    Context @ ASTPattern,
    "Wolfram`PacletCICD`",
    TestID -> "Context-ASTPattern@@Tests/ASTUtilities.wlt:42,1-46,2"
]

VerificationTest[
    Context @ FromAST,
    "Wolfram`PacletCICD`",
    TestID -> "Context-FromAST@@Tests/ASTUtilities.wlt:48,1-52,2"
]

VerificationTest[
    Context @ EquivalentNodeQ,
    "Wolfram`PacletCICD`",
    TestID -> "Context-EquivalentNodeQ@@Tests/ASTUtilities.wlt:54,1-58,2"
]

VerificationTest[
    Context @ CodeParse,
    "CodeParser`",
    TestID -> "Context-CodeParse@@Tests/ASTUtilities.wlt:60,1-64,2"
]

VerificationTest[
    Context @ LeafNode,
    "CodeParser`",
    TestID -> "Context-LeafNode@@Tests/ASTUtilities.wlt:66,1-70,2"
]

VerificationTest[
    Context @ CallNode,
    "CodeParser`",
    TestID -> "Context-CallNode@@Tests/ASTUtilities.wlt:72,1-76,2"
]

VerificationTest[
    Context @ Source,
    "CodeParser`",
    TestID -> "Context-Source@@Tests/ASTUtilities.wlt:78,1-82,2"
]

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Test Utilities*)
testParse // Attributes = { HoldRest };

testParse[ str_ ] := (CodeParse @ str)[[ 2, 1 ]];

testParse[ str_, patt_ ] :=
    MatchQ[ testParse @ str, ASTPattern @ HoldPattern @ patt ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*ASTPattern*)
VerificationTest[
    ASTPattern[ _Integer ],
    (CallNode | LeafNode)[
        Integer | LeafNode[ Symbol, "Integer" | "System`Integer", _ ],
        _,
        _
    ],
    TestID -> "Leaf-Call@@Tests/ASTUtilities.wlt:97,1-105,2"
]

VerificationTest[
    ASTPattern[ _Integer? IntegerQ ],
    LeafNode[ Integer, _, _ ],
    TestID -> "Leaf-PatternTest@@Tests/ASTUtilities.wlt:107,1-111,2"
]

VerificationTest[
    ASTPattern @ x,
    LeafNode[ Symbol, "x" | Context @ x <> "x", _ ],
    TestID -> "Leaf-Symbol@@Tests/ASTUtilities.wlt:113,1-117,2"
]

VerificationTest[
    ASTPattern @ HoldPattern @ Identity[ _ ],
    CallNode[
        LeafNode[ Symbol, "Identity" | "System`Identity", _ ],
        { (CallNode|LeafNode)[ _, _, _ ] },
        _
    ],
    TestID -> "HoldPattern@@Tests/ASTUtilities.wlt:119,1-127,2"
]

VerificationTest[
    ASTPattern @ f[ ASTPattern[ _ ], ASTPattern[ _ ] ],
    ASTPattern @ f[ _, _ ],
    TestID -> "Invisible-Nested@@Tests/ASTUtilities.wlt:129,1-133,2"
]

VerificationTest[
    ASTPattern[ f[ ASTPattern[ _, a_ ], ASTPattern[ _, b_ ] ], c_ ],
    CodeParser`CallNode[
        CodeParser`LeafNode[ Symbol, "f" | Context @ f <> "f", _ ],
        {
            (CodeParser`CallNode | CodeParser`LeafNode)[ _, _, a_ ],
            (CodeParser`CallNode | CodeParser`LeafNode)[ _, _, b_ ]
        },
        c_
    ],
    TestID -> "Bound-Nested@@Tests/ASTUtilities.wlt:135,1-146,2"
]

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Duplicate Pattern Symbols*)
VerificationTest[
    Count[ CodeParse[ "{{1,1},{2,2}}" ], ASTPattern @ { a_, a_ }, Infinity ],
    2,
    TestID -> "Duplicate-Pattern-Symbols-1@@Tests/ASTUtilities.wlt:151,1-155,2"
]

VerificationTest[
    Count[
        CodeParse[ "{{{1,1},{2,2}},{{1,1},{2,2}}}" ],
        ASTPattern @ { a_, a_ },
        Infinity
    ],
    5,
    TestID -> "Duplicate-Pattern-Symbols-2@@Tests/ASTUtilities.wlt:157,1-165,2"
]

VerificationTest[
    Cases[
        CodeParse[ "{{1,1,1},{1,1},{1,1,1,2},{2,2,2}}" ],
        ASTPattern[ expr: { a_, a_, a_ } ] :> FromAST @ expr,
        Infinity
    ],
    { { 1, 1, 1 }, { 2, 2, 2 } },
    TestID -> "Duplicate-Pattern-Symbols-3@@Tests/ASTUtilities.wlt:167,1-175,2"
]

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Two Arguments*)
VerificationTest[
    ASTPattern[ id_String, as1_ ],
    id: (CallNode | LeafNode)[
        String | LeafNode[ Symbol, "String" | "System`String", _ ],
        _,
        as1_
    ],
    TestID -> "Two-Arguments@@Tests/ASTUtilities.wlt:180,1-188,2"
]

VerificationTest[
    Cases[
        CodeParse[ "VerificationTest[1 + 1, 2, TestID -> \"Addition\", SameTest -> SameQ]" ],
        ASTPattern[
            HoldPattern @ VerificationTest[
                __,
                TestID -> ASTPattern[ id_String, as1_ ],
                ___
            ] /; StringQ @ id,
            as2_
        ] :> Lookup[ { as1, as2 }, Source ],
        Infinity
    ],
    { { { { 1, 38 }, { 1, 48 } }, { { 1, 1 }, { 1, 68 } } } },
    TestID -> "Nested-Meta-Bindings@@Tests/ASTUtilities.wlt:190,1-205,2"
]

VerificationTest[
    ASTPattern[ id_, as1_ ],
    id: (CallNode | LeafNode)[ _, _, as1_ ],
    TestID -> "Meta-Unknown-Head@@Tests/ASTUtilities.wlt:207,1-211,2"
]

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*TestParse*)
VerificationTest[
    testParse[ "VerificationTest[x,y]", VerificationTest[ ___ ] ],
    True,
    TestID -> "TestParse-VerificationTest@@Tests/ASTUtilities.wlt:216,1-220,2"
]

VerificationTest[
    testParse[ "f[x,y]", f[ _, _ ] ],
    True,
    TestID -> "TestParse-Normal@@Tests/ASTUtilities.wlt:222,1-226,2"
]

VerificationTest[
    testParse[ "2.5", _Real ],
    True,
    TestID -> "TestParse-Atom-Real@@Tests/ASTUtilities.wlt:228,1-232,2"
]

VerificationTest[
    testParse[ "2", _Integer ],
    True,
    TestID -> "TestParse-Atom-Integer@@Tests/ASTUtilities.wlt:234,1-238,2"
]

VerificationTest[
    testParse[ "\"hello\"", _String ],
    True,
    TestID -> "TestParse-Atom-String@@Tests/ASTUtilities.wlt:240,1-244,2"
]

VerificationTest[
    testParse[ "x", _Symbol ],
    True,
    TestID -> "TestParse-Atom-Symbol@@Tests/ASTUtilities.wlt:246,1-250,2"
]

VerificationTest[
    With[ { expr = 2/3 }, testParse[ "2/3", expr ] ],
    True,
    TestID -> "TestParse-Atom-Rational@@Tests/ASTUtilities.wlt:252,1-256,2"
]

VerificationTest[
    With[ { expr = 2 + 3 I }, testParse[ "2 + 3 I", expr ] ],
    True,
    TestID -> "TestParse-Atom-Complex@@Tests/ASTUtilities.wlt:258,1-262,2"
]

VerificationTest[
    testParse[ "5", x_Integer ],
    True,
    TestID -> "TestParse-Pattern@@Tests/ASTUtilities.wlt:264,1-268,2"
]

VerificationTest[
    testParse[ "f[5]", e: f[ x_Integer ] ],
    True,
    TestID -> "TestParse-Nested-Pattern@@Tests/ASTUtilities.wlt:270,1-274,2"
]

VerificationTest[
    testParse[ "f[5]", f[ x_Integer ] /; Positive @ x ],
    True,
    TestID -> "TestParse-Condition-1@@Tests/ASTUtilities.wlt:276,1-280,2"
]

VerificationTest[
    testParse[ "f[5]", f[ x_Integer ] /; Negative @ x ],
    False,
    TestID -> "TestParse-Condition-2@@Tests/ASTUtilities.wlt:282,1-286,2"
]

VerificationTest[
    testParse[
        "VerificationTest[1+1, 2, TestID -> \"test\"]",
        VerificationTest[ __, TestID -> id_, ___ ] /; StringQ @ id
    ],
    True,
    TestID -> "TestParse-Condition-3@@Tests/ASTUtilities.wlt:288,1-295,2"
]

VerificationTest[
    testParse[
        "VerificationTest[1+1, 2, TestID -> Automatic]",
        VerificationTest[ __, TestID -> id_, ___ ] /; StringQ @ id
    ],
    False,
    TestID -> "TestParse-Condition-4@@Tests/ASTUtilities.wlt:297,1-304,2"
]

VerificationTest[
    testParse[ "5", _Integer? IntegerQ ],
    True,
    TestID -> "TestParse-PatternTest-1@@Tests/ASTUtilities.wlt:306,1-310,2"
]

VerificationTest[
    testParse[ "5", x_Integer? IntegerQ ],
    True,
    TestID -> "TestParse-PatternTest-2@@Tests/ASTUtilities.wlt:312,1-316,2"
]

VerificationTest[
    testParse[ "5", x_ /; IntegerQ @ x ],
    True,
    TestID -> "TestParse-PatternTest-3@@Tests/ASTUtilities.wlt:318,1-322,2"
]

VerificationTest[
    testParse[ "f[1.2]", f @ Except[ _Integer ] ],
    True,
    TestID -> "TestParse-Except-1@@Tests/ASTUtilities.wlt:324,1-328,2"
]

VerificationTest[
    testParse[ "f[1.2]", f @ Except[ _Real ] ],
    False,
    TestID -> "TestParse-Except-2@@Tests/ASTUtilities.wlt:330,1-334,2"
]

VerificationTest[
    testParse[ "f[1.2]", f @ Except[ _Integer, _Real ] ],
    True,
    TestID -> "TestParse-Except-3@@Tests/ASTUtilities.wlt:336,1-340,2"
]

VerificationTest[
    testParse[ "f[1.2]", f @ Except[ _Integer, _String ] ],
    False,
    TestID -> "TestParse-Except-4@@Tests/ASTUtilities.wlt:342,1-346,2"
]

VerificationTest[
    testParse[ "f[\"hello\"]", f @ Except[ _Integer, _String ] ],
    True,
    TestID -> "TestParse-Except-5@@Tests/ASTUtilities.wlt:348,1-352,2"
]

VerificationTest[
    testParse[ "{a,b,c,d,c,d,a,b}", { x__, PatternSequence[ c, d, c ], y__ } ],
    True,
    TestID -> "TestParse-PatternSequence-1@@Tests/ASTUtilities.wlt:354,1-358,2"
]

VerificationTest[
    testParse[ "{a,b,a,b,a,b,a,b,a,b}", { PatternSequence[ x_, x_ ].. } ],
    False,
    TestID -> "TestParse-PatternSequence-3@@Tests/ASTUtilities.wlt:360,1-364,2"
]

VerificationTest[
    testParse[ "{0,2,3,1,4}", { 0, OrderlessPatternSequence[ 1, 2, 3 ], 4 } ],
    True,
    TestID -> "TestParse-OrderlessPatternSequence-1@@Tests/ASTUtilities.wlt:366,1-370,2"
]

VerificationTest[
    ReplaceAll[
        testParse[ "{a,b,c,d,e,f,g}" ],
        ASTPattern @ { x__, Shortest[ y__ ] } :>
            FromAST @ { { x }, { y } }
    ],
    { { a, b, c, d, e, f }, { g } },
    TestID -> "TestParse-Shortest-1@@Tests/ASTUtilities.wlt:372,1-380,2"
]

VerificationTest[
    ReplaceAll[
        testParse[ "{a,b,c,d,e,f,g}" ],
        ASTPattern @ { Shortest[ x__ ], y__ } :>
            FromAST @ { { x }, { y } }
    ],
    { { a }, { b, c, d, e, f, g } },
    TestID -> "TestParse-Shortest-2@@Tests/ASTUtilities.wlt:382,1-390,2"
]

VerificationTest[
    ReplaceAll[
        testParse[ "{a,b,c,d,e,f,g}" ],
        ASTPattern @ { Shortest[ x__, 2 ], Shortest[ y__, 1 ] } :>
            FromAST @ { { x }, { y } }
    ],
    { { a }, { b, c, d, e, f, g } },
    TestID -> "TestParse-Shortest-3@@Tests/ASTUtilities.wlt:392,1-400,2"
]

VerificationTest[
    ReplaceAll[
        testParse[ "{a,b,c,d,e,f,g}" ],
        ASTPattern @ { Shortest[ x__, 1 ], Shortest[ y__, 2 ] } :>
            FromAST @ { { x }, { y } }
    ],
    { { a, b, c, d, e, f }, { g } },
    TestID -> "TestParse-Shortest-4@@Tests/ASTUtilities.wlt:402,1-410,2"
]

VerificationTest[
    ReplaceAll[
        testParse[ "{a,b,c,d,e,f,g}" ],
        ASTPattern @ { x__, Longest[ y__ ] } :>
            FromAST @ { { x }, { y } }
    ],
    { { a }, { b, c, d, e, f, g } },
    TestID -> "TestParse-Longest-1@@Tests/ASTUtilities.wlt:412,1-420,2"
]

VerificationTest[
    ReplaceAll[
        testParse[ "{a,b,c,d,e,f,g}" ],
        ASTPattern @ { Longest[ x__ ], y__ } :>
            FromAST @ { { x }, { y } }
    ],
    { { a, b, c, d, e, f }, { g } },
    TestID -> "TestParse-Longest-2@@Tests/ASTUtilities.wlt:422,1-430,2"
]

VerificationTest[
    ReplaceAll[
        testParse[ "{a,b,c,d,e,f,g}" ],
        ASTPattern @ { Longest[ x__, 2 ], Longest[ y__, 1 ] } :>
            FromAST @ { { x }, { y } }
    ],
    { { a, b, c, d, e, f }, { g } },
    TestID -> "TestParse-Longest-3@@Tests/ASTUtilities.wlt:432,1-440,2"
]

VerificationTest[
    ReplaceAll[
        testParse[ "{a,b,c,d,e,f,g}" ],
        ASTPattern @ { Longest[ x__, 1 ], Longest[ y__, 2 ] } :>
            FromAST @ { { x }, { y } }
    ],
    { { a }, { b, c, d, e, f, g } },
    TestID -> "TestParse-Longest-4@@Tests/ASTUtilities.wlt:442,1-450,2"
]

VerificationTest[
    testParse[ "{1,1,2,2}", ASTPattern @ { x_, x_, y_, y_ } ],
    True,
    TestID -> "Reused-Pattern-Bindings-1@@Tests/ASTUtilities.wlt:452,1-456,2"
]

VerificationTest[
    testParse[
        "1+1",
        ASTPattern @ HoldPattern[ ASTPattern[ 1 ] + ASTPattern[ 1 ] ]
    ],
    True,
    TestID -> "Nested-ASTPattern-Held@@Tests/ASTUtilities.wlt:458,1-465,2"
]

VerificationTest[
    testParse[ "{1,1}", ASTPattern[ { x_, x_ } /; IntegerQ @ x ] ],
    True,
    TestID -> "Reused-Bindings-Condition@@Tests/ASTUtilities.wlt:467,1-471,2"
]

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*FromAST*)
VerificationTest[
    Cases[
        CodeParse[ "VerificationTest[1 + 1, 2, TestID -> \"Addition\", SameTest -> SameQ]" ],
        ASTPattern[
            HoldPattern @ VerificationTest[
                __,
                TestID -> ASTPattern[ id_String, as1_ ],
                ___
            ] /; StringQ @ id,
            as2_
        ] :> FromAST @ id,
        Infinity
    ],
    { "Addition" },
    TestID -> "FromAST-Bindings@@Tests/ASTUtilities.wlt:476,1-491,2"
]

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*EquivalentNodeQ*)
VerificationTest[
    Enclose @ Apply[
        EquivalentNodeQ,
        ConfirmMatch[
            Cases[
                CodeParse[ "{f[x],f[x]}" ],
                ASTPattern[ _[ _ ] ],
                Infinity
            ],
            { _, _ }
        ]
    ],
    True,
    TestID -> "EquivalentNodeQ-1@@Tests/ASTUtilities.wlt:496,1-510,2"
]

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Regression Tests*)
VerificationTest[
    testParse[ "{a,a,a}", ASTPattern[ { x: (_).. } /; SameQ @ x ] ],
    True,
    TestID -> "FromAST-Sequence-1@@Tests/ASTUtilities.wlt:515,1-519,2"
]

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Future*)
Hold @ VerificationTest[
    testParse[ "{a,b,a,b,a,b,a,b,a,b}", { PatternSequence[ x_, y_ ].. } ],
    True,
    TestID -> "TestParse-PatternSequence-2@@Tests/ASTUtilities.wlt:524,8-528,2"
]
