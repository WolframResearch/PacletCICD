(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Initialization*)
VerificationTest[
    PacletObjectQ @ PacletObject @ File[
        $pacletDir = DirectoryName[ $TestFileName, 2 ]
    ],
    TestID -> "Initialize-PacletObject@@Tests/ASTPattern.wlt:4,1-9,2"
]

VerificationTest[
    PacletDirectoryLoad @ $pacletDir,
    { ___, $pacletDir, ___ },
    SameTest -> MatchQ,
    TestID -> "Initialize-PacletDirectoryLoad@@Tests/ASTPattern.wlt:11,1-16,2"
]

VerificationTest[
    Needs[ "Wolfram`PacletCICD`" ],
    Null,
    TestID -> "Initialize-Needs-PacletCICD@@Tests/ASTPattern.wlt:18,1-22,2"
]

VerificationTest[
    Needs[ "CodeParser`" ],
    Null,
    TestID -> "Initialize-Needs-CodeParser@@Tests/ASTPattern.wlt:24,1-28,2"
]

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Context Check*)
VerificationTest[
    Context @ ASTPattern,
    "Wolfram`PacletCICD`",
    TestID -> "Context-ASTPattern@@Tests/ASTPattern.wlt:33,1-37,2"
]

VerificationTest[
    Context @ CodeParse,
    "CodeParser`",
    TestID -> "Context-CodeParse@@Tests/ASTPattern.wlt:39,1-43,2"
]

VerificationTest[
    Context @ LeafNode,
    "CodeParser`",
    TestID -> "Context-LeafNode@@Tests/ASTPattern.wlt:45,1-49,2"
]

VerificationTest[
    Context @ CallNode,
    "CodeParser`",
    TestID -> "Context-CallNode@@Tests/ASTPattern.wlt:51,1-55,2"
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
    Alternatives[
        LeafNode[ Integer, _, _ ],
        CallNode[ LeafNode[ Symbol, "Integer" | "System`Integer", _ ], _, _ ]
    ],
    TestID -> "Leaf-Call@@Tests/ASTPattern.wlt:70,1-77,2"
]

VerificationTest[
    ASTPattern[ _Integer? IntegerQ ],
    LeafNode[ Integer, _, _ ],
    TestID -> "Leaf-PatternTest@@Tests/ASTPattern.wlt:79,1-83,2"
]

VerificationTest[
    ASTPattern @ x,
    LeafNode[ Symbol, "x" | Context @ x <> "x", _ ],
    TestID -> "Leaf-Symbol@@Tests/ASTPattern.wlt:85,1-89,2"
]

VerificationTest[
    ASTPattern @ HoldPattern @ Identity[ _ ],
    CallNode[
        LeafNode[ Symbol, "Identity" | "System`Identity", _ ],
        { _CallNode | _LeafNode },
        _
    ],
    TestID -> "HoldPattern@@Tests/ASTPattern.wlt:91,1-99,2"
]

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Two Arguments*)
VerificationTest[
    ASTPattern[ id_String, as1_ ],
    id: Alternatives[
        LeafNode[ String, _, as1_ ],
        CallNode[ LeafNode[ Symbol, "String" | "System`String", _ ], _, as1_ ]
    ],
    TestID -> "Two-Arguments"
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
    TestID -> "Nested-Meta-Bindings"
]

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*TestParse*)
VerificationTest[
    testParse[ "VerificationTest[x,y]", VerificationTest[ ___ ] ],
    True,
    TestID -> "TestParse-VerificationTest@@Tests/ASTPattern.wlt:101,1-105,2"
]

VerificationTest[
    testParse[ "f[x,y]", f[ _, _ ] ],
    True,
    TestID -> "TestParse-Normal@@Tests/ASTPattern.wlt:107,1-111,2"
]

VerificationTest[
    testParse[ "2.5", _Real ],
    True,
    TestID -> "TestParse-Atom-Real@@Tests/ASTPattern.wlt:113,1-117,2"
]

VerificationTest[
    testParse[ "2", _Integer ],
    True,
    TestID -> "TestParse-Atom-Integer@@Tests/ASTPattern.wlt:119,1-123,2"
]

VerificationTest[
    testParse[ "\"hello\"", _String ],
    True,
    TestID -> "TestParse-Atom-String@@Tests/ASTPattern.wlt:125,1-129,2"
]

VerificationTest[
    testParse[ "x", _Symbol ],
    True,
    TestID -> "TestParse-Atom-Symbol@@Tests/ASTPattern.wlt:131,1-135,2"
]

VerificationTest[
    With[ { expr = 2/3 }, testParse[ "2/3", expr ] ],
    True,
    TestID -> "TestParse-Atom-Rational@@Tests/ASTPattern.wlt:137,1-141,2"
]

VerificationTest[
    With[ { expr = 2 + 3 I }, testParse[ "2 + 3 I", expr ] ],
    True,
    TestID -> "TestParse-Atom-Complex@@Tests/ASTPattern.wlt:143,1-147,2"
]

VerificationTest[
    testParse[ "5", x_Integer ],
    True,
    TestID -> "TestParse-Pattern@@Tests/ASTPattern.wlt:149,1-153,2"
]

VerificationTest[
    testParse[ "f[5]", e: f[ x_Integer ] ],
    True,
    TestID -> "TestParse-Nested-Pattern@@Tests/ASTPattern.wlt:155,1-159,2"
]

VerificationTest[
    testParse[ "f[5]", f[ x_Integer ] /; Positive @ x ],
    True,
    TestID -> "TestParse-Condition-1@@Tests/ASTPattern.wlt:161,1-165,2"
]

VerificationTest[
    testParse[ "f[5]", f[ x_Integer ] /; Negative @ x ],
    False,
    TestID -> "TestParse-Condition-2@@Tests/ASTPattern.wlt:167,1-171,2"
]

VerificationTest[
    testParse[
        "VerificationTest[1+1, 2, TestID -> \"test\"]",
        VerificationTest[ __, TestID -> id_, ___ ] /; StringQ @ id
    ],
    True,
    TestID -> "TestParse-Condition-3@@Tests/ASTPattern.wlt:173,1-180,2"
]

VerificationTest[
    testParse[
        "VerificationTest[1+1, 2, TestID -> Automatic]",
        VerificationTest[ __, TestID -> id_, ___ ] /; StringQ @ id
    ],
    False,
    TestID -> "TestParse-Condition-4@@Tests/ASTPattern.wlt:182,1-189,2"
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
    TestID -> "FromAST-Bindings"
]