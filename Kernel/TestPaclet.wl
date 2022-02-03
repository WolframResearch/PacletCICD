(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Header*)
BeginPackage[ "Wolfram`PacletCICD`" ];

TestPaclet;
AnnotateTestIDs;

Begin[ "`Private`" ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*TestPaclet*)
TestPaclet[ ___ ] := Failure[ "NotImplemented", <| |> ];

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
    SelectFirst[
        FixedPointList[ DirectoryName, file ],
        Composition[ PacletObjectQ, PacletObject, Flatten, File ],
        None
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