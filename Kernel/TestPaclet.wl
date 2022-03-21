(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Header*)
BeginPackage[ "Wolfram`PacletCICD`" ];

ClearAll[ TestPaclet, AnnotateTestIDs ];

Begin[ "`Private`" ];

Needs[ "DefinitionNotebookClient`" -> "dnc`" ];
Needs[ "CodeParser`"               -> "cp`"  ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*TestPaclet*)

TestPaclet // Options = {
    "Target"          -> "Submit",
    "Debug"           -> False,
    "AnnotateTestIDs" -> True,
    "ConsoleType"     -> Automatic
};

(* TODO: copy paclet to temp dir and auto-annotate tests with IDs *)

TestPaclet::failures =
"Failures encountered while testing paclet.";

TestPaclet[ dir_? DirectoryQ, opts: OptionsPattern[ ] ] :=
    (* TODO: do the right stuff here *)
    catchTop @ Internal`InheritedBlock[ { dnc`$ConsoleType },
        dnc`$ConsoleType = OptionValue[ "ConsoleType" ];
        If[ TrueQ @ OptionValue[ "AnnotateTestIDs" ],
            AnnotateTestIDs[ dir, "Reparse" -> False ]
        ];
        testPaclet @ dir
    ];

TestPaclet[ file_File? defNBQ, opts: OptionsPattern[ ] ] :=
    catchTop @ TestPaclet[ parentPacletDirectory @ file, opts ];


testPaclet[ dir_? DirectoryQ ] :=
    Module[ { files, report },
        PacletDirectoryLoad @ dir;
        files  = FileNames[ "*.wlt", dir, Infinity ];
        report = testContext @ TestReport @ files;
        annotateTestResult /@ report[ "TestResults" ];
        If[ TrueQ @ report[ "AllTestsSucceeded" ],
            report,
            exitFailure[
                "TestPaclet::failures",
                Association[
                    "MessageTemplate"   :> TestPaclet::failures,
                    "MessageParameters" :> { },
                    "Result"            -> report
                ],
                1
            ]
        ]
    ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*testContext*)
testContext // Attributes = { HoldFirst };

testContext[ eval_ ] :=
    Module[ { context, contextPath },
        context     = $Context;
        contextPath = $ContextPath;
        WithCleanup[
             $Context     = "PacletCICDTest`";
             $ContextPath = { "PacletCICDTest`", "System`" };
             ,
             Block[ { $catching = False }, eval ]
             ,
             $Context     = context;
             $ContextPath = contextPath;
        ]
    ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*annotateTestResult*)
annotateTestResult[
    tro: TestResultObject[
        KeyValuePattern @ {
            "TestID" | TestID -> testID_String,
            "Outcome"         -> "Success"
        },
        ___
    ]
] :=
    dnc`ConsolePrint[ "Test passed: " <> testID ];

annotateTestResult[
    tro: TestResultObject[
        KeyValuePattern @ {
            "TestID" | TestID -> testID_String,
            "Outcome"         -> outcome: Except[ "Success" ]
        },
        ___
    ]
] := (
    dnc`ConsolePrint[ "Test failed: " <> testID ];
    annotateTestResult[ tro, testID ]
);

annotateTestResult[ tro_, testID_String ] :=
    annotateTestResult[ tro, StringSplit[ testID, $testIDDelimiter ] ];

annotateTestResult[ tro_, { testID_String, annotation_String } ] :=
    annotateTestResult[ tro, testID, StringSplit[ annotation, ":" ] ];

annotateTestResult[ tro_, testID_String, { file_String, pos_String } ] :=
    annotateTestResult[ tro, testID, file, StringSplit[ pos, "-" ] ];

annotateTestResult[
    tro_,
    testID_String,
    file_String,
    { lc1_String, lc2_String }
] :=
    annotateTestResult[
        tro,
        testID,
        file,
        StringSplit[ lc1, "," ],
        StringSplit[ lc2, "," ]
    ];

annotateTestResult[
    tro_TestResultObject,
    testID_String,
    file_String,
    p1: { _String, _String },
    p2: { _String, _String }
] :=
    dnc`ConsolePrint[
        StringJoin[
            "Test \"",
            testID,
            "\" failed with outcome: \"",
            tro[ "Outcome" ],
            "\""
        ],
        "Level" -> "Error",
        "SourceInformation" -> <|
            "Scope"    -> "PacletCICD/PacletTest",
            "File"     -> file,
            "Type"     -> "LineColumn",
            "Position" -> ToExpression @ { p1, p2 }
        |>
    ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Test Utilities (Experimental)*)

$testIDDelimiter = "@@";
$pacletRoot      = None;
$untitledTestNumber = 1;

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*AnnotateTestIDs*)
AnnotateTestIDs // Options = {
    "PacletRoot" -> Automatic,
    "Reparse"    -> True
};

AnnotateTestIDs[ dir_? DirectoryQ, opts: OptionsPattern[ ] ] :=
    Block[
        {
            $pacletRoot   = toPacletRoot[ dir, OptionValue[ "PacletRoot" ] ],
            $reparseTests = OptionValue[ "Reparse" ],
            $untitledTestNumber = 1
        },
        annotateTestIDs /@ FileNames[ "*.wlt", dir, Infinity ]
    ];

AnnotateTestIDs[ file_? FileExistsQ, opts: OptionsPattern[ ] ] :=
    Block[
        {
            $pacletRoot   = toPacletRoot[ file, OptionValue[ "PacletRoot" ] ],
            $reparseTests = OptionValue[ "Reparse" ],
            $untitledTestNumber = 1
        },
        annotateTestIDs @ file
    ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*annotateTestIDs*)
annotateTestIDs[ dir_? DirectoryQ ] :=
    annotateTestIDs /@ FileNames[ "*.wlt"|"*.mt", dir, Infinity ];

annotateTestIDs[ file_ ] :=
    Block[ { $needsReparse = False },
        Module[ { annotated },
            annotated = annotateTestIDs0 @ file;
            If[ TrueQ[ $reparseTests && $needsReparse ],
                $untitledTestNumber = 1;
                annotateTestIDs0 @ file,
                annotated
            ]
        ]
    ];

annotateTestIDs0[ file_ ] :=
    Module[ { data, string, pairs, replace, newString },
        data      = parseTestIDs @ file;
        string    = ReadString @ file;
        pairs     = makeReplacementPair[ string ] /@ data;
        replace   = StringReplacePart[ string, ##1 ] &;
        newString = replace @@ Transpose @ pairs;

        Export[ file, newString, "String" ]
    ];

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*makeReplacementPair*)
makeReplacementPair[ string_ ][ KeyValuePattern @ {
    "NewTestID"              -> id_String,
    "IDSourceCharacterIndex" -> { a_Integer, b_Integer }
} ] := { id, { a, b } };

makeReplacementPair[ string_ ][ KeyValuePattern @ {
    "NewTestID"                -> id_String,
    "IDSourceCharacterIndex"   -> None,
    "TestSourceCharacterIndex" -> { a_Integer, b_Integer }
} ] := (
    $needsReparse = True;
    {
        insertTestID[
            ToExpression @ id,
            ToExpression[
                StringTake[ string, { a, b } ],
                InputForm,
                HoldComplete
            ]
        ],
        { a, b }
    }
);

makeReplacementPair ~catchUndefined~ SubValues;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*insertTestID*)
insertTestID[ id_String, HoldComplete @ VerificationTest[ a___ ] ] :=
    StringJoin[
        "VerificationTest[\n",
        StringRiffle[
            Cases[
                Append[
                    DeleteCases[ HoldComplete @ a, TestID -> _ ],
                    TestID -> id
                ],
                e_ :> "  " <> ToString[ Unevaluated @ e, InputForm ]
            ],
            ",\n"
        ],
        "\n]"
    ];

insertTestID // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*toPacletRoot*)
toPacletRoot[ file_, Automatic ] := parentPacletDirectory @ file;
toPacletRoot[ file_, root_ ] := root;
toPacletRoot // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*parseTestIDs*)
parseTestIDs[ file_ ] :=
    Module[ { as1, as2, idPositions, base },

        as1         = parseTestIDs[ file, "SourceCharacterIndex" ];
        as2         = parseTestIDs[ file, "LineColumn" ];
        idPositions = Join @@@ Transpose[ { as1, as2 } ];
        base        = testIDFilePart @ file;

        Append[ #1, "NewTestID" -> makeTestID[ #, base ] ] & /@ idPositions
    ];

parseTestIDs[ file_, type_ ] :=
    Module[ { ast, mask, masked, all, unmasked },
        ast    = codeParseType[ file, type ];
        masked = maskNestedTests[ ast, mask ];

        all = Cases[ masked,
                     ASTPattern @ HoldPattern @ VerificationTest[ ___ ],
                     Infinity
              ];

        unmasked = all /. masked[ h_ ] :> h;
        getTestIDData @ type /@ unmasked
    ];

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*maskNestedTests*)

(* TODO: simplify parsing patterns  with ASTPattern and FromAST *)

maskNestedTests[ ast_, mask_ ] :=
    ReplaceAll[
        ast,
        cp`CallNode[
            cp`LeafNode[
                Symbol,
                s1: "VerificationTest"|"System`VerificationTest",
                as1_
            ],
            args_,
            as2_
        ] :>
            cp`CallNode[
                cp`LeafNode[ Symbol, s1, as1 ],
                ReplaceAll[
                    args,
                    cp`LeafNode[
                        Symbol,
                        s2: "VerificationTest"|"System`VerificationTest",
                        a_
                    ] :>
                        cp`LeafNode[ Symbol, mask @ s2, a ]
                ],
                as2
            ]
    ];

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*getTestIDData*)
getTestIDData[ type_ ][
    cp`CallNode[
        cp`LeafNode[ Symbol, "VerificationTest" | "Test", _ ],
        {
            __,
            cp`CallNode[
                cp`LeafNode[ Symbol, "Rule", _ ],
                {
                    Alternatives[
                        cp`LeafNode[ Symbol, "TestID", _ ],
                        cp`LeafNode[ "String", "\"TestID\"", _ ]
                    ],
                    cp`LeafNode[
                        String,
                        id_,
                        KeyValuePattern[ cp`Source -> idSrc_ ]
                    ]
                },
                _
            ],
            ___
        },
        KeyValuePattern[ cp`Source -> testSrc_ ]
    ]
] := <|
    "TestID"       -> ToExpression[ id, InputForm ],
    "ID" <> type   -> idSrc,
    "Test" <> type -> testSrc
|>;

getTestIDData[ type_ ][
    cp`CallNode[
        cp`LeafNode[ Symbol, "VerificationTest", _ ],
        _,
        KeyValuePattern[ cp`Source -> testSrc_ ]
    ]
] := <|
    "TestID"       -> "Untitled-" <> ToString[ $untitledTestNumber++ ],
    "ID" <> type   -> None,
    "Test" <> type -> testSrc
|>;

getTestIDData ~catchUndefined~ SubValues;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*testIDFilePart*)
testIDFilePart[ file_ ] :=
    If[ DirectoryQ @ $pacletRoot,
        StringDelete[
            relativePath[ $pacletRoot, file ],
            StartOfString~~("./"|"/"|".")
        ],
        FileNameTake @ file
    ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*codeParseType*)
codeParseType[ file_, type_ ] :=
    cp`CodeParse[ Flatten @ File @ file, "SourceConvention" -> type ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*makeTestID*)
makeTestID[
    KeyValuePattern @ {
        "TestID" -> id_String,
        "TestLineColumn" -> { { l1_, c1_ }, { l2_, c2_ } }
    },
    base_String
] :=
    Module[ { cleaned },
        cleaned = removeTestIDAnnotation @ id;
        ToString[
            StringJoin[
                cleaned,
                $testIDDelimiter,
                base,
                ":",
                ToString @ l1,
                ",",
                ToString @ c1,
                "-",
                ToString @ l2,
                ",",
                ToString @ c2
            ],
            InputForm
        ]
    ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*removeTestIDAnnotation*)
removeTestIDAnnotation[ id_ ] :=
    StringDelete[ id, $testIDDelimiter ~~ ___ ~~ EndOfString ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Footer*)
End[ ];

EndPackage[ ];