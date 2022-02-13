(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Initialization*)
VerificationTest[
    PacletObjectQ @ PacletObject @ File[
        $pacletDir = DirectoryName[ $TestFileName, 2 ]
    ],
    TestID -> "Initialize-PacletObject@@Tests/ASTUtilities.wlt:4,1-9,2"
]

VerificationTest[
    PacletDirectoryLoad @ $pacletDir,
    { ___, $pacletDir, ___ },
    SameTest -> MatchQ,
    TestID -> "Initialize-PacletDirectoryLoad@@Tests/ASTUtilities.wlt:11,1-16,2"
]

VerificationTest[
    Needs[ "Wolfram`PacletCICD`" ],
    Null,
    TestID -> "Initialize-Needs-PacletCICD@@Tests/ASTUtilities.wlt:18,1-22,2"
]

VerificationTest[
    Needs[ "CodeParser`" ],
    Null,
    TestID -> "Initialize-Needs-CodeParser@@Tests/ASTUtilities.wlt:24,1-28,2"
]

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Context Check*)
VerificationTest[
    Context @ ASTPattern,
    "Wolfram`PacletCICD`",
    TestID -> "Context-ASTPattern@@Tests/ASTUtilities.wlt:33,1-37,2"
]

VerificationTest[
    Context @ FromAST,
    "Wolfram`PacletCICD`",
    TestID -> "Context-FromAST@@Tests/ASTUtilities.wlt:39,1-43,2"
]

VerificationTest[
    Context @ EquivalentNodeQ,
    "Wolfram`PacletCICD`",
    TestID -> "Context-EquivalentNodeQ@@Tests/ASTUtilities.wlt:45,1-49,2"
]

VerificationTest[
    Context @ CodeParse,
    "CodeParser`",
    TestID -> "Context-CodeParse@@Tests/ASTUtilities.wlt:51,1-55,2"
]

VerificationTest[
    Context @ LeafNode,
    "CodeParser`",
    TestID -> "Context-LeafNode@@Tests/ASTUtilities.wlt:57,1-61,2"
]

VerificationTest[
    Context @ CallNode,
    "CodeParser`",
    TestID -> "Context-CallNode@@Tests/ASTUtilities.wlt:63,1-67,2"
]

VerificationTest[
    Context @ Source,
    "CodeParser`",
    TestID -> "Context-Source@@Tests/ASTUtilities.wlt:69,1-73,2"
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
    TestID -> "Leaf-Call@@Tests/ASTUtilities.wlt:88,1-96,2"
]

VerificationTest[
    ASTPattern[ _Integer? IntegerQ ],
    LeafNode[ Integer, _, _ ],
    TestID -> "Leaf-PatternTest@@Tests/ASTUtilities.wlt:98,1-102,2"
]

VerificationTest[
    ASTPattern @ x,
    LeafNode[ Symbol, "x" | Context @ x <> "x", _ ],
    TestID -> "Leaf-Symbol@@Tests/ASTUtilities.wlt:104,1-108,2"
]

VerificationTest[
    ASTPattern @ HoldPattern @ Identity[ _ ],
    CallNode[
        LeafNode[ Symbol, "Identity" | "System`Identity", _ ],
        { (CallNode|LeafNode)[ _, _, _ ] },
        _
    ],
    TestID -> "HoldPattern@@Tests/ASTUtilities.wlt:110,1-118,2"
]

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Duplicate Pattern Symbols*)
VerificationTest[
    Count[ CodeParse[ "{{1,1},{2,2}}" ], ASTPattern @ { a_, a_ }, Infinity ],
    2,
    TestID -> "Duplicate-Pattern-Symbols-1@@Tests/ASTUtilities.wlt:123,1-127,2"
]

VerificationTest[
    Count[
        CodeParse[ "{{{1,1},{2,2}},{{1,1},{2,2}}}" ],
        ASTPattern @ { a_, a_ },
        Infinity
    ],
    5,
    TestID -> "Duplicate-Pattern-Symbols-2@@Tests/ASTUtilities.wlt:129,1-137,2"
]

VerificationTest[
    Cases[
        CodeParse[ "{{1,1,1},{1,1},{1,1,1,2},{2,2,2}}" ],
        ASTPattern[ expr: { a_, a_, a_ } ] :> FromAST @ expr,
        Infinity
    ],
    { { 1, 1, 1 }, { 2, 2, 2 } },
    TestID -> "Duplicate-Pattern-Symbols-3@@Tests/ASTUtilities.wlt:139,1-147,2"
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
    TestID -> "Two-Arguments@@Tests/ASTUtilities.wlt:152,1-160,2"
]

VerificationTest[
    Cases[
        CodeParse[ "VerificationTest[1 + 1, 2, TestID -> \"Addition\", SameTest -> SameQ]" ],
        ASTPattern[
            VerificationTest[
                __,
                TestID -> ASTPattern[ id_String, as1_ ],
                ___
            ] /; StringQ @ id,
            as2_
        ] :> Lookup[ { as1, as2 }, Source ],
        Infinity
    ],
    { { { { 1, 38 }, { 1, 48 } }, { { 1, 1 }, { 1, 68 } } } },
    TestID -> "Nested-Meta-Bindings@@Tests/ASTUtilities.wlt:162,1-177,2"
]

VerificationTest[
    ASTPattern[ id_, as1_ ],
    id: (CallNode | LeafNode)[ _, _, as1_ ],
    TestID -> "Meta-Unknown-Head@@Tests/ASTUtilities.wlt:179,1-183,2"
]

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*TestParse*)
VerificationTest[
    testParse[ "VerificationTest[x,y]", VerificationTest[ ___ ] ],
    True,
    TestID -> "TestParse-VerificationTest@@Tests/ASTUtilities.wlt:188,1-192,2"
]

VerificationTest[
    testParse[ "f[x,y]", f[ _, _ ] ],
    True,
    TestID -> "TestParse-Normal@@Tests/ASTUtilities.wlt:194,1-198,2"
]

VerificationTest[
    testParse[ "2.5", _Real ],
    True,
    TestID -> "TestParse-Atom-Real@@Tests/ASTUtilities.wlt:200,1-204,2"
]

VerificationTest[
    testParse[ "2", _Integer ],
    True,
    TestID -> "TestParse-Atom-Integer@@Tests/ASTUtilities.wlt:206,1-210,2"
]

VerificationTest[
    testParse[ "\"hello\"", _String ],
    True,
    TestID -> "TestParse-Atom-String@@Tests/ASTUtilities.wlt:212,1-216,2"
]

VerificationTest[
    testParse[ "x", _Symbol ],
    True,
    TestID -> "TestParse-Atom-Symbol@@Tests/ASTUtilities.wlt:218,1-222,2"
]

VerificationTest[
    With[ { expr = 2/3 }, testParse[ "2/3", expr ] ],
    True,
    TestID -> "TestParse-Atom-Rational@@Tests/ASTUtilities.wlt:224,1-228,2"
]

VerificationTest[
    With[ { expr = 2 + 3 I }, testParse[ "2 + 3 I", expr ] ],
    True,
    TestID -> "TestParse-Atom-Complex@@Tests/ASTUtilities.wlt:230,1-234,2"
]

VerificationTest[
    testParse[ "5", x_Integer ],
    True,
    TestID -> "TestParse-Pattern@@Tests/ASTUtilities.wlt:236,1-240,2"
]

VerificationTest[
    testParse[ "f[5]", e: f[ x_Integer ] ],
    True,
    TestID -> "TestParse-Nested-Pattern@@Tests/ASTUtilities.wlt:242,1-246,2"
]

VerificationTest[
    testParse[ "f[5]", f[ x_Integer ] /; Positive @ x ],
    True,
    TestID -> "TestParse-Condition-1@@Tests/ASTUtilities.wlt:248,1-252,2"
]

VerificationTest[
    testParse[ "f[5]", f[ x_Integer ] /; Negative @ x ],
    False,
    TestID -> "TestParse-Condition-2@@Tests/ASTUtilities.wlt:254,1-258,2"
]

VerificationTest[
    testParse[
        "VerificationTest[1+1, 2, TestID -> \"test\"]",
        VerificationTest[ __, TestID -> id_, ___ ] /; StringQ @ id
    ],
    True,
    TestID -> "TestParse-Condition-3@@Tests/ASTUtilities.wlt:260,1-267,2"
]

VerificationTest[
    testParse[
        "VerificationTest[1+1, 2, TestID -> Automatic]",
        VerificationTest[ __, TestID -> id_, ___ ] /; StringQ @ id
    ],
    False,
    TestID -> "TestParse-Condition-4@@Tests/ASTUtilities.wlt:269,1-276,2"
]

VerificationTest[
    testParse[ "5", _Integer? IntegerQ ],
    True,
    TestID -> "TestParse-PatternTest-1@@Tests/ASTUtilities.wlt:278,1-282,2"
]

VerificationTest[
    testParse[ "5", x_Integer? IntegerQ ],
    True,
    TestID -> "TestParse-PatternTest-2@@Tests/ASTUtilities.wlt:284,1-288,2"
]

VerificationTest[
    testParse[ "5", x_ /; IntegerQ @ x ],
    True,
    TestID -> "TestParse-PatternTest-3@@Tests/ASTUtilities.wlt:290,1-294,2"
]

VerificationTest[
    testParse[ "f[1.2]", f @ Except[ _Integer ] ],
    True,
    TestID -> "TestParse-Except-1@@Tests/ASTUtilities.wlt:296,1-300,2"
]

VerificationTest[
    testParse[ "f[1.2]", f @ Except[ _Real ] ],
    False,
    TestID -> "TestParse-Except-2@@Tests/ASTUtilities.wlt:302,1-306,2"
]

VerificationTest[
    testParse[ "f[1.2]", f @ Except[ _Integer, _Real ] ],
    True,
    TestID -> "TestParse-Except-3@@Tests/ASTUtilities.wlt:308,1-312,2"
]

VerificationTest[
    testParse[ "f[1.2]", f @ Except[ _Integer, _String ] ],
    False,
    TestID -> "TestParse-Except-4@@Tests/ASTUtilities.wlt:314,1-318,2"
]

VerificationTest[
    testParse[ "f[\"hello\"]", f @ Except[ _Integer, _String ] ],
    True,
    TestID -> "TestParse-Except-5@@Tests/ASTUtilities.wlt:320,1-324,2"
]

VerificationTest[
    testParse[ "{a,b,c,d,c,d,a,b}", { x__, PatternSequence[ c, d, c ], y__ } ],
    True,
    TestID -> "TestParse-PatternSequence-1@@Tests/ASTUtilities.wlt:326,1-330,2"
]

VerificationTest[
    testParse[ "{a,b,a,b,a,b,a,b,a,b}", { PatternSequence[ x_, x_ ].. } ],
    False,
    TestID -> "TestParse-PatternSequence-3@@Tests/ASTUtilities.wlt:332,1-336,2"
]

VerificationTest[
    testParse[ "{1,1,2,2}", ASTPattern @ { x_, x_, y_, y_ } ],
    True,
    TestID -> "Reused-Pattern-Bindings-1@@Tests/ASTUtilities.wlt:338,1-342,2"
]

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*FromAST*)
VerificationTest[
    Cases[
        CodeParse[ "VerificationTest[1 + 1, 2, TestID -> \"Addition\", SameTest -> SameQ]" ],
        ASTPattern[
            VerificationTest[
                __,
                TestID -> ASTPattern[ id_String, as1_ ],
                ___
            ] /; StringQ @ id,
            as2_
        ] :> FromAST @ id,
        Infinity
    ],
    { "Addition" },
    TestID -> "FromAST-Bindings@@Tests/ASTUtilities.wlt:347,1-362,2"
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
    TestID -> "EquivalentNodeQ-1@@Tests/ASTUtilities.wlt:367,1-381,2"
]

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Regression Tests*)
VerificationTest[
    testParse[ "{a,a,a}", ASTPattern[ { x: (_).. } /; SameQ @ x ] ],
    True,
    TestID -> "FromAST-Sequence-1@@Tests/ASTUtilities.wlt:386,1-390,2"
]

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Future*)
Hold @ VerificationTest[
    testParse[ "{a,b,a,b,a,b,a,b,a,b}", { PatternSequence[ x_, y_ ].. } ],
    True,
    TestID -> "TestParse-PatternSequence-2@@Tests/ASTUtilities.wlt:395,8-399,2"
]
