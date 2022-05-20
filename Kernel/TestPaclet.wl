(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Header*)
BeginPackage[ "Wolfram`PacletCICD`" ];

ClearAll[ TestPaclet, AnnotateTestIDs ];

Begin[ "`Private`" ];

$ContextAliases[ "dnc`" ] = "DefinitionNotebookClient`";
$ContextAliases[ "cp`"  ] = "CodeParser`";

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*TestPaclet*)
TestPaclet::Failures =
"Failures encountered while testing paclet.";

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Options*)
TestPaclet // Options = {
    "AnnotateTestIDs"  -> True,
    "ConsoleType"      -> Automatic,
    "Debug"            -> False,
    "MarkdownSummary"  -> True,
    "MemoryConstraint" -> Inherited,
    "SameTest"         -> Inherited,
    "Target"           -> "Submit",
    "TimeConstraint"   -> Inherited
};

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Main definition*)
(* TODO: copy paclet to temp dir and auto-annotate tests with IDs *)
TestPaclet[ dir_? DirectoryQ, opts: OptionsPattern[ ] ] :=
    catchTop @ ccPromptFix[
        needs[ "DefinitionNotebookClient`" -> None ];
        (* TODO: do the right stuff here *)
        catchTop @ Internal`InheritedBlock[ { dnc`$ConsoleType },
            dnc`$ConsoleType = OptionValue[ "ConsoleType" ];
            If[ TrueQ @ OptionValue[ "AnnotateTestIDs" ],
                AnnotateTestIDs[ dir, "Reparse" -> False ]
            ];
            testPaclet[ dir, optionsAssociation[ TestPaclet, opts ] ]
        ]
    ];

TestPaclet[ file_File? defNBQ, opts: OptionsPattern[ ] ] :=
    catchTop @ TestPaclet[ parentPacletDirectory @ file, opts ];

(* TODO: find tests from PacletInfo *)

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*testPaclet*)
testPaclet[ dir_, opts: KeyValuePattern[ "MarkdownSummary" -> False ] ] :=
    Block[ { appendStepSummary },
        testPaclet[ dir, KeyDrop[ opts, "MarkdownSummary" ] ]
    ];

testPaclet[ dir_? DirectoryQ, opts_Association ] :=
    Module[ { files, pacDir, as, reports },
        PacletDirectoryLoad @ dir;
        files   = FileNames[ "*.wlt", dir, Infinity ];
        pacDir  = parentPacletDirectory @ dir;
        as      = Append[ opts, "PacletDirectory" -> pacDir ];
        reports = testReport[ as, files ];
        makeTestResult[ dir, reports ]
    ];

testPaclet // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*testReport*)
testReport[ as_Association, files_List ] :=
    Enclose @ ConfirmBy[
        generateTestSummary @ Association[ (testReport[ as, #1 ] &) /@ files ],
        AssociationQ
    ];

testReport[ as_Association, file_? FileExistsQ ] :=
    Enclose @ Module[ { dir, rules, opts, report, rel },
        dir    = ConfirmBy[ as[ "PacletDirectory" ], DirectoryQ ];
        rules  = Sequence @@ Normal[ as, Association ];
        opts   = filterOptions[ TestReport, rules ];
        report = testContext @ TestReport[ file, opts ];
        rel    = ConfirmBy[ relativePath[ dir, file ], StringQ ];
        rel -> report
    ];

testReport // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*generateTestSummary*)
generateTestSummary[ reports_Association ] := (
    appendStepSummary @ testSummaryHeader @ reports;
    KeyValueMap[ generateTestSummary, reports ];
    generateTestDetails @ reports;
    reports
);

generateTestSummary[ file_, report_TestReportObject ] :=
    Module[ { icon, link, pass, fail, time, row, md },
        icon = testSummaryIcon @ report;
        link = testSummaryLink @ file;
        pass = testSummaryPass @ report;
        fail = testSummaryFail @ report;
        time = testSummaryTime @ report;
        row  = { icon, link, pass, fail, time };
        md   = "| " <> StringRiffle[ row, " | " ] <> " |\n";
        appendStepSummary @ md
    ];

generateTestSummary // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*generateTestDetails*)
generateTestDetails[ reports_Association ] :=
    If[ AllTrue[ reports, #[ "AllTestsSucceeded" ] & ],
        reports,
        appendStepSummary @ $testDetailsHeader;
        KeyValueMap[ generateTestDetails, reports ];
        appendStepSummary @ $testDetailsFooter;
        reports
    ];

generateTestDetails[ file_, report_ ] /; report[ "AllTestsSucceeded" ] :=
    Null;

generateTestDetails[ file_, report_TestReportObject ] :=
    Module[ { header, md, results, failed },
        header = StringRiffle[ DeleteCases[ FileNameSplit @ file, "." ], "/" ];
        md = "<details><summary><h3>" <> header <> "</h3></summary>\n\n";
        appendStepSummary @ md;
        results = report[ "TestResults" ];
        failed  = Select[ results, #[ "Outcome" ] =!= "Success" & ];
        (generateTestFailureDetails[ file, #1 ] &) /@ failed;
        appendStepSummary[ "</details>" ]
    ];

generateTestDetails // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*generateTestFailureDetails*)
generateTestFailureDetails[ file_, result_TestResultObject ] :=
    Enclose @ Module[
        {
            cs, info, id, res, icon, time, mem, link,
            input, expOut, actOut, expMsg, actMsg, md
        },

        cs     = ConfirmBy[ #, StringQ ] &;
        info   = ConfirmBy[ testIDInfo @ result, AssociationQ ];
        id     = cs @ info[ "TestID" ];
        res    = cs @ troOutcome @ result;
        icon   = cs @ testSummaryIcon @ res;
        time   = stq @ result[ "AbsoluteTimeUsed" ];
        mem    = btq @ result[ "MemoryUsed" ];
        link   = cs @ testSummaryLink[ file, ":link:", lineAnchor @ info ];
        input  = codeBlock[ "Input"            , result[ "Input"            ] ];
        expOut = codeBlock[ "Expected Output"  , result[ "ExpectedOutput"   ] ];
        actOut = codeBlock[ "Actual Output"    , result[ "ActualOutput"     ] ];
        expMsg = codeBlock[ "Expected Messages", result[ "ExpectedMessages" ] ];
        actMsg = codeBlock[ "Actual Messages"  , result[ "ActualMessages"   ] ];

        md = TemplateApply[
            $testResultTemplate,
            <|
                "Icon"             -> icon,
                "Result"           -> outcomeText @ res,
                "TestID"           -> id,
                "Duration"         -> timeText @ time,
                "Memory"           -> ToString @ mem,
                "Link"             -> link,
                "Input"            -> input,
                "ExpectedOutput"   -> expOut,
                "ActualOutput"     -> actOut,
                "ExpectedMessages" -> expMsg,
                "ActualMessages"   -> actMsg
            |>
        ];

        appendStepSummary @ md
    ];

generateTestFailureDetails // catchUndefined;

btq := btq = ResourceFunction[ "BytesToQuantity"  , "Function" ];
stq := stq = ResourceFunction[ "SecondsToQuantity", "Function" ];
rdf := rdf = ResourceFunction[ "ReadableForm"     , "Function" ];


timeText[ sec_ ] :=
    If[ TrueQ[ sec <= Quantity[ 1, "Milliseconds" ] ],
        "< 1 ms",
        TextString @ Round @ sec
    ];

outcomeText[ "Messages" ] := "Message failure";
outcomeText[ "Time"     ] := "Timed out";
outcomeText[ "Memory"   ] := "Exceeded memory limits";
outcomeText[ outcome_   ] := outcome;




troOutcome[ tro_TestResultObject    ] := troOutcome[ tro, tro[ "Outcome" ] ];
troOutcome[ tro_TestReportObject    ] := troOutcome[ tro, tro[ "Outcome" ] ];
troOutcome[ tro_, "Success"         ] := "Success";
troOutcome[ tro_, "MessagesFailure" ] := "Messages";
troOutcome[ tro_, "Failure"         ] := troFailureType @ tro;
troOutcome[ tro_, outcome_String    ] := outcome;
troOutcome // catchUndefined;


troFailureType[ r_TestResultObject ] := troFailureType @ r[ "ActualOutput" ];
troFailureType[ HoldForm @ Failure[ "TimeConstrained"  , _ ] ] := "Time";
troFailureType[ HoldForm @ Failure[ "MemoryConstrained", _ ] ] := "Memory";
troFailureType[ ___ ] := "Failure";


collapsibleSection[ md_ ] := "<details>" <> ToString @ md <> "</details>";

collapsibleSection[ lbl_, md__ ] :=
    StringJoin[
        "<details><summary>",
        ToString @ lbl,
        "</summary>",
        StringRiffle[ { md }, "\n\n" ],
        "</details>"
    ];

(* ::**********************************************************************:: *)
(* ::Subsubsubsection::Closed:: *)
(*lineAnchor*)
lineAnchor[ KeyValuePattern[ "Position" -> pos_ ] ] := lineAnchor @ pos;

lineAnchor[ { { l1_Integer, _ }, { l2_Integer, _ } } ] :=
    "L" <> ToString @ l1 <> "-L" <> ToString @ l2;

lineAnchor // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*testIDInfo*)
testIDInfo[ result_TestResultObject ] :=
    testIDInfo @ result[ "TestID" ];

testIDInfo[ id_String ] :=
    If[ StringFreeQ[ id, $testIDDelimiter ],
        <| "TestID" -> id |>,
        testIDInfo @ StringSplit[ id, $testIDDelimiter ]
    ];

testIDInfo[ { testID_String, annotation_String } ] :=
    testIDInfo[ testID, StringSplit[ annotation, ":" ] ];

testIDInfo[ testID_, { file_String, pos_String } ] :=
    testIDInfo[ testID, file, StringSplit[ pos, "-" ] ];

testIDInfo[ testID_, file_, { l1_String, l2_String } ] :=
    testIDInfo[ testID, file, StringSplit[ l1, "," ], StringSplit[ l2, "," ] ];

testIDInfo[
    testID_String,
    file_String,
    p1: { _String, _String },
    p2: { _String, _String }
] := <|
    "TestID"   -> testID,
    "Scope"    -> "PacletCICD/PacletTest",
    "File"     -> file,
    "Type"     -> "LineColumn",
    "Position" -> ToExpression @ { p1, p2 }
|>;

testIDInfo[ ___ ] := <| |>;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*testSummaryIcon*)
testSummaryIcon[ tro_TestReportObject ] := testSummaryIcon @ troOutcome @ tro;
testSummaryIcon[ tro_TestResultObject ] := testSummaryIcon @ troOutcome @ tro;
testSummaryIcon[ "Success"  ] := ":white_check_mark:";
testSummaryIcon[ "Failure"  ] := ":x:";
testSummaryIcon[ "Time"     ] := ":hourglass:";
testSummaryIcon[ "Memory"   ] := ":red_circle:";
testSummaryIcon[ "Messages" ] := ":large_orange_diamond:";
testSummaryIcon[ _String    ] := ":large_blue_circle:";
testSummaryIcon // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*testSummaryLink*)
testSummaryLink[ file_ ] :=
    testSummaryLink[
        file,
        StringRiffle[ DeleteCases[ FileNameSplit @ file, "." ], "/" ]
    ];

testSummaryLink[ file_, lbl_ ] :=
    testSummaryLink[ file, lbl, "" ];

testSummaryLink[ file_, lbl_, anchor_ ] := Enclose[
    Module[ { env, server, repo, sha, split, url, frag },
        env    = ConfirmBy[ Environment[ #1 ], StringQ ] &;
        server = env[ "GITHUB_SERVER_URL" ];
        repo   = env[ "GITHUB_REPOSITORY" ];
        sha    = env[ "GITHUB_SHA" ];
        split  = DeleteCases[ FileNameSplit @ file, "." ];
        url    = URLBuild @ Flatten @ { server, repo, "blob", sha, split };
        frag   = If[ StringQ @ anchor && anchor =!= "", "#"<>anchor, "" ];
        StringJoin[
            "<a href=\"",
            url,
            frag,
            "\" target=\"_blank\">",
            ToString @ lbl,
            "</a>"
        ]
    ],
    file &
];

testSummaryLink // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*testSummaryPass*)
testSummaryPass[ r_TestReportObject ] := ToString @ r[ "TestsSucceededCount" ];
testSummaryPass // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*testSummaryFail*)
testSummaryFail[ r_TestReportObject ] := ToString @ r[ "TestsFailedCount" ];
testSummaryFail // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*testSummaryTime*)
testSummaryTime[ r_TestReportObject ] := testSummaryTime @ r[ "TimeElapsed" ];
testSummaryTime[ HoldPattern[ t_Quantity ] ] := timeText @ stq @ t;
testSummaryTime[ s_? NumberQ ] := testSummaryTime @ Quantity[ s, "Seconds" ];
testSummaryTime // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*testSummaryHeader*)
testSummaryHeader[ reports_ ] :=
    Module[ { files, tests, time, pass, rate, res, icon },
        files = Length @ reports;
        tests = Total[ Length[ #[ "TestResults" ] ] & /@ reports ];
        time  = stq @ Total[ #[ "TimeElapsed" ] & /@ reports ];
        pass  = Total[ #[ "TestsSucceededCount" ] & /@ reports ];
        rate  = PercentForm[ pass / tests ];
        res   = If[ pass === tests, "Success", "Failure" ];
        icon  = testSummaryIcon @ res;
        TemplateApply[
            $testSummaryHeader,
            <|
                "FileCount" -> files,
                "TestCount" -> tests,
                "PassCount" -> pass,
                "PassRate"  -> TextString @ rate,
                "Time"      -> timeText @ time,
                "Result"    -> res,
                "Icon"      -> icon
            |>
        ]
    ];

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*$testSummaryHeader*)
$testSummaryHeader = "

# Test Results

## Summary

|                 |                          |
|-----------------|--------------------------|
| **Result**      | `Result` `Icon`          |
| **Total files** | `FileCount`              |
| **Total tests** | `TestCount`              |
| **Passed**      | `PassCount` (`PassRate`) |
| **Duration**    | `Time`                   |

## Test files

| | File | Passed | Failed | Duration |
|-|------|--------|--------|----------|
";

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*$testDetailsHeader*)
$testDetailsHeader = "

<details><summary><h2>Details</h2></summary>

";

$testDetailsFooter = "

</details>

";

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*$testResultTemplate*)
$testResultTemplate = "


----------------------------------------------------------------


|        | Result   | TestID   | Duration   | Memory   | Link   |
|--------|----------|----------|------------|----------|--------|
| `Icon` | `Result` | `TestID` | `Duration` | `Memory` | `Link` |

`Input`

`ExpectedOutput`

`ActualOutput`

`ExpectedMessages`

`ActualMessages`

";



mmaPre[ HoldForm[ code_ ] ] :=
    StringJoin[
        "\n\n```Mathematica\n",
        TimeConstrained[ ToString @ rdf[ Unevaluated @ code,
                                         CachePersistence -> Full
                                    ],
                         5,
                         "<< " <> ToString @ ByteCount @ code <> ">>"
        ],
        "\n```\n\n"
    ];

mmaPre[ code_ ] := mmaPre @ HoldForm @ code;


codeBlock[ label_, code_ ] :=
    Module[ { string, chars, lines, template },

        string = mmaPre @ code;
        chars  = StringLength @ string;
        lines  = Length @ StringSplit[ string, "\n" ];

        template = If[ TrueQ[ chars < 500 && lines < 5 ],
                       $codeBlockTemplate,
                       $hiddenCodeBlockTemplate
                   ];

        TemplateApply[
            template,
            <| "Label" -> label, "Code" -> string |>
        ]
    ];

$codeBlockTemplate = "
`Label`

`Code`
";

$hiddenCodeBlockTemplate = "
<details><summary>`Label`</summary>

`Code`

</details>
";

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Dependencies*)

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*makeTestResult*)
makeTestResult[ dir_, reports_Association ] :=
    makeTestResult[
        dir,
        reports,
        AllTrue[ reports, #[ "AllTestsSucceeded" ] & ]
    ];

makeTestResult[ dir_, reports_, True ] :=
    Success[ "AllTestsSucceeded",
             <|
                 "MessageTemplate"   -> "All tests successful",
                 "MessageParameters" -> { },
                 "Result"            :> reports
             |>
    ];

makeTestResult[ dir_, reports_, False ] :=
    Module[ { export, exported },
        export = fileNameJoin @ { dir, "build", "test_results.wxf" };
        GeneralUtilities`EnsureDirectory @ DirectoryName @ export;
        ConsoleNotice[ "Exporting test results: " <> export ];
        exported = Export[ export,
                           reports,
                           "WXF",
                           PerformanceGoal -> "Size"
                   ];
        setOutput[ "PACLET_TEST_RESULTS", exported ];
        exitFailure[
            "TestPaclet::Failures",
            Association[
                "MessageTemplate"   :> TestPaclet::Failures,
                "MessageParameters" :> { },
                "Result"            :> reports
            ],
            1
        ]
    ];

makeTestResult // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
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

        unmasked = all /. mask[ h_ ] :> h;
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
codeParseType[ file_, type_ ] := (
    needs[ "CodeParser`" -> None ];
    cp`CodeParse[ Flatten @ File @ file, "SourceConvention" -> type ]
);

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*makeTestID*)
makeTestID[
    KeyValuePattern @ {
        "TestID"         -> id_String,
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