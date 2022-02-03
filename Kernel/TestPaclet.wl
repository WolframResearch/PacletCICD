(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Header*)
BeginPackage[ "Wolfram`PacletCICD`" ];

TestPaclet;
AnnotateTestIDs;

Begin[ "`Private`" ];

Needs[ "DefinitionNotebookClient`" -> "dnc`" ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*TestPaclet*)

TestPaclet::failures =
"Failures encountered while testing paclet.";

TestPaclet[ dir_? DirectoryQ ] :=
    (* TODO: do the right stuff here *)
    Internal`InheritedBlock[ { dnc`$ConsoleType },
        dnc`$ConsoleType = Automatic;
        testPaclet @ dir
    ];

testPaclet[ dir_? DirectoryQ ] :=
    Module[ { files, report },
        files = FileNames[ "*.wlt", dir, Infinity ];
        report = TestReport @ files;
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
] :=
    annotateTestResult[ tro, testID ];

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
        "Tag" -> testID,
        "Level" -> "Error",
        "SourceInformation" -> <|
            "Scope" -> "PacletCICD/PacletTest",
            "File" -> file,
            "Position" -> ToExpression @ { p1, p2 }
        |>
    ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Test Utilities (Experimental)*)

$testIDDelimiter = "@@";
$pacletRoot      = None;

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*AnnotateTestIDs*)
AnnotateTestIDs // Options = { "PacletRoot" -> Automatic };

AnnotateTestIDs[ dir_? DirectoryQ, opts: OptionsPattern[ ] ] :=
    Block[ { $pacletRoot = toPacletRoot[ dir, OptionValue[ "PacletRoot" ] ] },
        annotateTestIDs /@ FileNames[ "*.wlt", dir, Infinity ]
    ];

AnnotateTestIDs[ file_? FileExistsQ, opts: OptionsPattern[ ] ] :=
    Block[ { $pacletRoot = toPacletRoot[ file, OptionValue[ "PacletRoot" ] ] },
        annotateTestIDs @ file
    ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*annotateTestIDs*)
annotateTestIDs[ file_ ] :=
    Module[ { data, pairs, string, replace, newString },
        data      = parseTestIDs @ file;
        pairs     = { #NewTestID, #IDSourceCharacterIndex } & /@ data;
        string    = ReadString @ file;
        replace   = StringReplacePart[ string, ##1 ] &;
        newString = replace @@ Transpose @ pairs;

        Export[ file, newString, "String" ]
    ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*toPacletRoot*)
toPacletRoot[ file_, Automatic ] :=
    Quiet[ SelectFirst[
               FixedPointList[ DirectoryName, file ],
               Composition[ PacletObjectQ, PacletObject, Flatten, File ],
               None
           ],
           PacletManager`CreatePaclet::badarg
    ];

toPacletRoot[ file_, root_ ] := root;

toPacletRoot // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*parseTestIDs*)
parseTestIDs[ file_ ] :=
    Module[ { as1, as2, idPositions, base },

        as1         = parseTestIDs[ file, "LineColumn" ];
        as2         = parseTestIDs[ file, "SourceCharacterIndex" ];
        idPositions = Join @@@ Transpose[ { as1, as2 } ];
        base        = testIDFilePart @ file;

        Append[ #1, "NewTestID" -> makeTestID[ #, base ] ] & /@ idPositions
    ];

parseTestIDs[ file_, type_ ] :=
    Module[ { ast },

        ast = codeParseType[ file, type ];

        Cases[
            ast,
            CodeParser`CallNode[
                CodeParser`LeafNode[ Symbol, "VerificationTest"|"Test", _ ],
                {
                    __,
                    CodeParser`CallNode[
                        CodeParser`LeafNode[ Symbol, "Rule", _ ],
                        {
                            Alternatives[
                                CodeParser`LeafNode[ Symbol, "TestID", _ ],
                                CodeParser`LeafNode[ "String", "\"TestID\"", _ ]
                            ],
                            CodeParser`LeafNode[
                                String,
                                id_,
                                KeyValuePattern[
                                    CodeParser`Source -> idSrc_
                                ]
                            ]
                        },
                        _
                    ],
                    ___
                },
                KeyValuePattern[ CodeParser`Source -> testSrc_ ]
            ] :> <|
                "TestID"       -> ToExpression[ id, InputForm ],
                "ID" <> type   -> idSrc,
                "Test" <> type -> testSrc
            |>,
            Infinity
        ]
    ];

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
    CodeParser`CodeParse[ Flatten @ File @ file, "SourceConvention" -> type ];

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