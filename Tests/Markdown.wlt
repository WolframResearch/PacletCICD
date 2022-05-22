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
    TestID -> "Initialize-PacletObject"
]

(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::SuspiciousSessionSymbol:: *)
VerificationTest[
    Echo[ $TestFileName, "TestFileName" ];
    PacletDirectoryLoad @ Echo[ $pacletDir, "PacletDirectory" ],
    { ___, $pacletDir, ___ },
    SameTest -> MatchQ,
    TestID   -> "Initialize-PacletDirectoryLoad"
]
(* :!CodeAnalysis::EndBlock:: *)

VerificationTest[
    Needs[ "Wolfram`PacletCICD`" ],
    Null,
    TestID -> "Initialize-Paclet-Needs"
]

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Context Check*)
VerificationTest[
    Context @ ToMarkdownString,
    "Wolfram`PacletCICD`",
    TestID -> "ToMarkdownString-Context"
]

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*ToMarkdownString*)

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Headings*)
VerificationTest[
    Map[
        ToMarkdownString @ Cell[ "test", #1 ] &,
        {
            "Title",
            "Subtitle",
            "Section",
            "Subsection",
            "Subsubsection",
            "Subsubsubsection"
        }
    ],
    {
        "# test",
        "## test",
        "### test",
        "#### test",
        "##### test",
        "###### test"
    },
    TestID -> "ToMarkdownString-Headings-Cell"
]

VerificationTest[
    Map[
        ToMarkdownString @ Style[ "test", #1 ] &,
        {
            "Title",
            "Subtitle",
            "Section",
            "Subsection",
            "Subsubsection",
            "Subsubsubsection"
        }
    ],
    {
        "# test",
        "## test",
        "### test",
        "#### test",
        "##### test",
        "###### test"
    },
    TestID -> "ToMarkdownString-Headings-Style"
]

VerificationTest[
    ToMarkdownString @ Row @ { a, Style[ b, "Section" ] },
    "ab",
    TestID -> "ToMarkdownString-Headers-Inline"
]

VerificationTest[
    ToMarkdownString @ Style[ Row @ { a, b }, "Section" ],
    "### ab",
    TestID -> "ToMarkdownString-Headers-Top"
]

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Style*)
VerificationTest[
    ToMarkdownString[ "hello" ],
    "hello",
    TestID -> "ToMarkdownString-String"
]

VerificationTest[
    ToMarkdownString @ Style[ "hello", "Input" ],
    "`\"hello\"`",
    TestID -> "ToMarkdownString-Input-Style"
]

VerificationTest[
    ToMarkdownString @ Style[ "hello", "Output" ],
    "`hello`",
    TestID -> "ToMarkdownString-Output-Style"
]

VerificationTest[
    ToMarkdownString @ Style[ "hello", "Input", ShowStringCharacters -> False ],
    "`hello`",
    TestID -> "ToMarkdownString-Input-ShowStringCharacters"
]

VerificationTest[
    ToMarkdownString @ Style[ "hello", "Output", ShowStringCharacters -> True ],
    "`\"hello\"`",
    TestID -> "ToMarkdownString-Output-ShowStringCharacters"
]

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Style options*)

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*Bold*)
VerificationTest[
    ToMarkdownString @ Style[ "test", FontWeight -> "Bold" ],
    "**test**",
    TestID -> "ToMarkdownString-Bold-1"
]

VerificationTest[
    ToMarkdownString @ Style[ "test", FontSize -> 10, FontWeight -> Bold ],
    "**test**",
    TestID -> "ToMarkdownString-Bold-2"
]

VerificationTest[
    ToMarkdownString @ Style[ "test", Bold, FontSize -> 10 ],
    "**test**",
    TestID -> "ToMarkdownString-Bold-3"
]

VerificationTest[
    ToMarkdownString @ Cell[
        TextData @ { "This is a text ", StyleBox[ "cell", Bold ], "." },
        "Text"
    ],
    "This is a text **cell**.",
    TestID -> "ToMarkdownString-Bold-4"
]

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*Italic*)
VerificationTest[
    ToMarkdownString @ Style[ "test", FontSlant -> "Italic" ],
    "*test*",
    TestID -> "ToMarkdownString-Italic-1"
]

VerificationTest[
    ToMarkdownString @ Style[ "test", FontSize -> 10, FontSlant -> Italic ],
    "*test*",
    TestID -> "ToMarkdownString-Italic-2"
]

VerificationTest[
    ToMarkdownString @ Style[ "test", Italic, FontSize -> 10 ],
    "*test*",
    TestID -> "ToMarkdownString-Italic-3"
]

VerificationTest[
    ToMarkdownString @ Cell[
        TextData @ { "This is a text ", StyleBox[ "cell", Italic ], "." },
        "Text"
    ],
    "This is a text *cell*.",
    TestID -> "ToMarkdownString-Italic-4"
]

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*Combined*)
VerificationTest[
    ToMarkdownString @ Style[ "test", Bold, FontSlant -> "Italic" ],
    "***test***",
    TestID -> "ToMarkdownString-BoldItalic-1"
]

VerificationTest[
    ToMarkdownString @ Row @ { Style[ a, Italic ], Style[ f @ x, "Input" ] },
    "*a*`f[x]`",
    TestID -> "ToMarkdownString-Row"
]

VerificationTest[
    ToMarkdownString @ {
        Style[ "MySection", "Section" ],
        "Here is some text",
        ExpressionCell[ MyFunction[ x, y ], "Input" ]
    },
    "### MySection\n\nHere is some text\n\n```wolfram\nMyFunction[x,y]\n```",
    TestID -> "ToMarkdownString-List"
]

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Hyperlinks*)
VerificationTest[
    ToMarkdownString @ Hyperlink[ "test", "https://example.com" ],
    "<a href=\"https://example.com\" target=\"_blank\">test</a>",
    TestID -> "ToMarkdownString-Hyperlink-1"
]

VerificationTest[
    ToMarkdownString @ Hyperlink[
        "test",
        "https://example.com",
        HyperlinkAction -> "Recycled"
    ],
    "[test](https://example.com)",
    TestID -> "ToMarkdownString-Hyperlink-2"
]

VerificationTest[
    ToMarkdownString[
        Hyperlink[ "test", "https://example.com" ],
        "DefaultHyperlinkAction" -> "Recycled"
    ],
    "[test](https://example.com)",
    TestID -> "ToMarkdownString-Hyperlink-3"
]

VerificationTest[
    ToMarkdownString @ Hyperlink[ "https://example.com" ],
    "<a href=\"https://example.com\" target=\"_blank\">https://example.com</a>",
    TestID -> "ToMarkdownString-Hyperlink-4"
]

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Grid*)
VerificationTest[
    ToMarkdownString[ Grid @ { { a, b, c }, { 1, 2, 3 } }, "HTMLTables" -> False ],
    "| a | b | c |\n| --- | --- | --- |\n| 1 | 2 | 3 |",
    TestID -> "ToMarkdownString-Grid-1"
]

VerificationTest[
    ToMarkdownString[ Grid @ { { a, b, c }, { 1, 2, 3 } } ],
    "<table><tbody><tr><td>a</td><td>b</td><td>c</td></tr><tr><td>1</td><td>2</td><td>3</td></tr></tbody></table>",
    TestID -> "ToMarkdownString-Grid-2"
]

VerificationTest[
    ToMarkdownString @ Grid @ {
        { "Some Text", "Here is some long text\nwith a line break." },
        {
            "Some Code",
            ReadableForm[
                Hold[ Table[ i + 1, { i, 5 } ], Range[ 10 ] ],
                PageWidth -> 30
            ]
        }
    },
    "<table><tbody><tr><td>Some Text</td><td>Here is some long text<br>with a line break.</td></tr><tr><td>Some Code</td><td><pre lang=\"wolfram\">Hold[&#10;    Table[ i + 1, { i, 5 } ],&#10;    Range[ 10 ]&#10;]</pre></td></tr></tbody></table>",
    TestID -> "ToMarkdownString-Grid-3"
]

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*ReadableForm*)
VerificationTest[
    ToMarkdownString @ ReadableForm @ Hold[
        f[ x_ ] := Table[ i + 1, { i, x } ]
    ],
    "`Hold[ f[ x_ ] := Table[ i + 1, { i, x } ] ]`",
    TestID -> "ToMarkdownString-ReadableForm-1"
]

VerificationTest[
    ToMarkdownString[
        ReadableForm @ Hold[ f[ x_ ] := Table[ i + 1, { i, x } ] ],
        "Inline" -> False
    ],
    "```wolfram\nHold[ f[ x_ ] := Table[ i + 1, { i, x } ] ]\n```",
    TestID -> "ToMarkdownString-ReadableForm-2"
]

VerificationTest[
    evaluated = False;
    ToMarkdownString @ ReadableForm @ Unevaluated[ evaluated = True ],
    "`evaluated = True`",
    TestID -> "ToMarkdownString-ReadableForm-3"
]

VerificationTest[
    evaluated,
    False,
    TestID -> "ToMarkdownString-ReadableForm-4"
]

VerificationTest[
    ToMarkdownString @ ExpressionCell[
        Defer[ f[ x_ ] := Table[ i + 1, { i, x } ] ],
        "ReadableForm"
    ],
    "```wolfram\nf[ x_ ] := Table[ i + 1, { i, x } ]\n```",
    TestID -> "ToMarkdownString-ReadableForm-5"
]

VerificationTest[
    ToMarkdownString @ Style[
        Defer[ f[ x_ ] := Table[ i + 1, { i, x } ] ],
        "ReadableForm"
    ],
    "`f[ x_ ] := Table[ i + 1, { i, x } ]`",
    TestID -> "ToMarkdownString-ReadableForm-6"
]

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*TraditionalForm*)
VerificationTest[
    ToMarkdownString @ TraditionalForm @ Limit[ f @ x, x -> Infinity ],
    "$$\\underset{x\\to \\infty }{\\text{lim}}f(x)$$",
    TestID -> "ToMarkdownString-TraditionalForm-1"
]

VerificationTest[
    ToMarkdownString[
        TraditionalForm @ Limit[ f @ x, x -> Infinity ],
        "Inline" -> True
    ],
    "$\\underset{x\\to \\infty }{\\text{lim}}f(x)$",
    TestID -> "ToMarkdownString-TraditionalForm-2"
]

VerificationTest[
    ToMarkdownString @ Row @ {
        "Here is some inline TeX: ",
        TraditionalForm @ Limit[ f @ x, x -> Infinity ]
    },
    "Here is some inline TeX: $\\underset{x\\to \\infty }{\\text{lim}}f(x)$",
    TestID -> "ToMarkdownString-TraditionalForm-3"
]

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Error handling*)
VerificationTest[
    ToMarkdownString @ Wolfram`PacletCICD`Private`$failTest,
    Failure[ "ToMarkdownString::InternalError", _ ],
    { ToMarkdownString::InternalError },
    SameTest -> MatchQ,
    TestID   -> "ToMarkdownString-FailTest"
]

(*TODO

    Superscript/Subscript
*)


VerificationTest[
    1+1,
    3,
    TestID -> "FailTest"
]

VerificationTest[
    1/0,
    ComplexInfinity,
    TestID -> "MessageTest"
]

VerificationTest[
    Pause[5]; 1+1,
    2,
    TestID -> "TimeoutTest",
    TimeConstraint -> 1
]

VerificationTest[
    Range[10000]; 1+1,
    2,
    TestID -> "MemoryTest",
    MemoryConstraint -> 1000
]