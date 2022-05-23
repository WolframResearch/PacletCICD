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
    TestID -> "Initialize-PacletObject@@Tests/Markdown.wlt:4,1-14,2"
]

(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::SuspiciousSessionSymbol:: *)
VerificationTest[
    Echo[ $TestFileName, "TestFileName" ];
    PacletDirectoryLoad @ Echo[ $pacletDir, "PacletDirectory" ],
    { ___, $pacletDir, ___ },
    SameTest -> MatchQ,
    TestID   -> "Initialize-PacletDirectoryLoad@@Tests/Markdown.wlt:18,1-24,2"
]
(* :!CodeAnalysis::EndBlock:: *)

VerificationTest[
    Needs[ "Wolfram`PacletCICD`" ],
    Null,
    TestID -> "Initialize-Paclet-Needs@@Tests/Markdown.wlt:27,1-31,2"
]

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Context Check*)
VerificationTest[
    Context @ ToMarkdownString,
    "Wolfram`PacletCICD`",
    TestID -> "ToMarkdownString-Context@@Tests/Markdown.wlt:36,1-40,2"
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
    TestID -> "ToMarkdownString-Headings-Cell@@Tests/Markdown.wlt:49,1-70,2"
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
    TestID -> "ToMarkdownString-Headings-Style@@Tests/Markdown.wlt:72,1-93,2"
]

VerificationTest[
    ToMarkdownString @ Row @ { a, Style[ b, "Section" ] },
    "ab",
    TestID -> "ToMarkdownString-Headers-Inline@@Tests/Markdown.wlt:95,1-99,2"
]

VerificationTest[
    ToMarkdownString @ Style[ Row @ { a, b }, "Section" ],
    "### ab",
    TestID -> "ToMarkdownString-Headers-Top@@Tests/Markdown.wlt:101,1-105,2"
]

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Style*)
VerificationTest[
    ToMarkdownString[ "hello" ],
    "hello",
    TestID -> "ToMarkdownString-String@@Tests/Markdown.wlt:110,1-114,2"
]

VerificationTest[
    ToMarkdownString @ Style[ "hello", "Input" ],
    "`\"hello\"`",
    TestID -> "ToMarkdownString-Input-Style@@Tests/Markdown.wlt:116,1-120,2"
]

VerificationTest[
    ToMarkdownString @ Style[ "hello", "Output" ],
    "`hello`",
    TestID -> "ToMarkdownString-Output-Style@@Tests/Markdown.wlt:122,1-126,2"
]

VerificationTest[
    ToMarkdownString @ Style[ "hello", "Input", ShowStringCharacters -> False ],
    "`hello`",
    TestID -> "ToMarkdownString-Input-ShowStringCharacters@@Tests/Markdown.wlt:128,1-132,2"
]

VerificationTest[
    ToMarkdownString @ Style[ "hello", "Output", ShowStringCharacters -> True ],
    "`\"hello\"`",
    TestID -> "ToMarkdownString-Output-ShowStringCharacters@@Tests/Markdown.wlt:134,1-138,2"
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
    TestID -> "ToMarkdownString-Bold-1@@Tests/Markdown.wlt:147,1-151,2"
]

VerificationTest[
    ToMarkdownString @ Style[ "test", FontSize -> 10, FontWeight -> Bold ],
    "**test**",
    TestID -> "ToMarkdownString-Bold-2@@Tests/Markdown.wlt:153,1-157,2"
]

VerificationTest[
    ToMarkdownString @ Style[ "test", Bold, FontSize -> 10 ],
    "**test**",
    TestID -> "ToMarkdownString-Bold-3@@Tests/Markdown.wlt:159,1-163,2"
]

VerificationTest[
    ToMarkdownString @ Cell[
        TextData @ { "This is a text ", StyleBox[ "cell", Bold ], "." },
        "Text"
    ],
    "This is a text **cell**.",
    TestID -> "ToMarkdownString-Bold-4@@Tests/Markdown.wlt:165,1-172,2"
]

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*Italic*)
VerificationTest[
    ToMarkdownString @ Style[ "test", FontSlant -> "Italic" ],
    "*test*",
    TestID -> "ToMarkdownString-Italic-1@@Tests/Markdown.wlt:177,1-181,2"
]

VerificationTest[
    ToMarkdownString @ Style[ "test", FontSize -> 10, FontSlant -> Italic ],
    "*test*",
    TestID -> "ToMarkdownString-Italic-2@@Tests/Markdown.wlt:183,1-187,2"
]

VerificationTest[
    ToMarkdownString @ Style[ "test", Italic, FontSize -> 10 ],
    "*test*",
    TestID -> "ToMarkdownString-Italic-3@@Tests/Markdown.wlt:189,1-193,2"
]

VerificationTest[
    ToMarkdownString @ Cell[
        TextData @ { "This is a text ", StyleBox[ "cell", Italic ], "." },
        "Text"
    ],
    "This is a text *cell*.",
    TestID -> "ToMarkdownString-Italic-4@@Tests/Markdown.wlt:195,1-202,2"
]

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*Combined*)
VerificationTest[
    ToMarkdownString @ Style[ "test", Bold, FontSlant -> "Italic" ],
    "***test***",
    TestID -> "ToMarkdownString-BoldItalic-1@@Tests/Markdown.wlt:207,1-211,2"
]

VerificationTest[
    ToMarkdownString @ Row @ { Style[ a, Italic ], Style[ f @ x, "Input" ] },
    "*a*`f[x]`",
    TestID -> "ToMarkdownString-Row@@Tests/Markdown.wlt:213,1-217,2"
]

VerificationTest[
    ToMarkdownString @ {
        Style[ "MySection", "Section" ],
        "Here is some text",
        ExpressionCell[ MyFunction[ x, y ], "Input" ]
    },
    "### MySection\n\nHere is some text\n\n```wolfram\nMyFunction[x,y]\n```",
    TestID -> "ToMarkdownString-List@@Tests/Markdown.wlt:219,1-227,2"
]

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Hyperlinks*)
VerificationTest[
    ToMarkdownString @ Hyperlink[ "test", "https://example.com" ],
    "<a href=\"https://example.com\" target=\"_blank\">test</a>",
    TestID -> "ToMarkdownString-Hyperlink-1@@Tests/Markdown.wlt:232,1-236,2"
]

VerificationTest[
    ToMarkdownString @ Hyperlink[
        "test",
        "https://example.com",
        HyperlinkAction -> "Recycled"
    ],
    "[test](https://example.com)",
    TestID -> "ToMarkdownString-Hyperlink-2@@Tests/Markdown.wlt:238,1-246,2"
]

VerificationTest[
    ToMarkdownString[
        Hyperlink[ "test", "https://example.com" ],
        "DefaultHyperlinkAction" -> "Recycled"
    ],
    "[test](https://example.com)",
    TestID -> "ToMarkdownString-Hyperlink-3@@Tests/Markdown.wlt:248,1-255,2"
]

VerificationTest[
    ToMarkdownString @ Hyperlink[ "https://example.com" ],
    "<a href=\"https://example.com\" target=\"_blank\">https://example.com</a>",
    TestID -> "ToMarkdownString-Hyperlink-4@@Tests/Markdown.wlt:257,1-261,2"
]

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Grid*)
VerificationTest[
    ToMarkdownString[ Grid @ { { a, b, c }, { 1, 2, 3 } }, "HTMLTables" -> False ],
    "| a | b | c |\n| --- | --- | --- |\n| 1 | 2 | 3 |",
    TestID -> "ToMarkdownString-Grid-1@@Tests/Markdown.wlt:266,1-270,2"
]

VerificationTest[
    ToMarkdownString[ Grid @ { { a, b, c }, { 1, 2, 3 } } ],
    "<table><tbody><tr><td>a</td><td>b</td><td>c</td></tr><tr><td>1</td><td>2</td><td>3</td></tr></tbody></table>",
    TestID -> "ToMarkdownString-Grid-2@@Tests/Markdown.wlt:272,1-276,2"
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
    TestID -> "ToMarkdownString-Grid-3@@Tests/Markdown.wlt:278,1-291,2"
]

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*ReadableForm*)
VerificationTest[
    ToMarkdownString @ ReadableForm @ Hold[
        f[ x_ ] := Table[ i + 1, { i, x } ]
    ],
    "`Hold[ f[ x_ ] := Table[ i + 1, { i, x } ] ]`",
    TestID -> "ToMarkdownString-ReadableForm-1@@Tests/Markdown.wlt:296,1-302,2"
]

VerificationTest[
    ToMarkdownString[
        ReadableForm @ Hold[ f[ x_ ] := Table[ i + 1, { i, x } ] ],
        "Inline" -> False
    ],
    "```wolfram\nHold[ f[ x_ ] := Table[ i + 1, { i, x } ] ]\n```",
    TestID -> "ToMarkdownString-ReadableForm-2@@Tests/Markdown.wlt:304,1-311,2"
]

VerificationTest[
    evaluated = False;
    ToMarkdownString @ ReadableForm @ Unevaluated[ evaluated = True ],
    "`evaluated = True`",
    TestID -> "ToMarkdownString-ReadableForm-3@@Tests/Markdown.wlt:313,1-318,2"
]

VerificationTest[
    evaluated,
    False,
    TestID -> "ToMarkdownString-ReadableForm-4@@Tests/Markdown.wlt:320,1-324,2"
]

VerificationTest[
    ToMarkdownString @ ExpressionCell[
        Defer[ f[ x_ ] := Table[ i + 1, { i, x } ] ],
        "ReadableForm"
    ],
    "```wolfram\nf[ x_ ] := Table[ i + 1, { i, x } ]\n```",
    TestID -> "ToMarkdownString-ReadableForm-5@@Tests/Markdown.wlt:326,1-333,2"
]

VerificationTest[
    ToMarkdownString @ Style[
        Defer[ f[ x_ ] := Table[ i + 1, { i, x } ] ],
        "ReadableForm"
    ],
    "`f[ x_ ] := Table[ i + 1, { i, x } ]`",
    TestID -> "ToMarkdownString-ReadableForm-6@@Tests/Markdown.wlt:335,1-342,2"
]

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*TraditionalForm*)
VerificationTest[
    ToMarkdownString @ TraditionalForm @ Limit[ f @ x, x -> Infinity ],
    "$$\\underset{x\\to \\infty }{\\text{lim}}f(x)$$",
    TestID -> "ToMarkdownString-TraditionalForm-1@@Tests/Markdown.wlt:347,1-351,2"
]

VerificationTest[
    ToMarkdownString[
        TraditionalForm @ Limit[ f @ x, x -> Infinity ],
        "Inline" -> True
    ],
    "$\\underset{x\\to \\infty }{\\text{lim}}f(x)$",
    TestID -> "ToMarkdownString-TraditionalForm-2@@Tests/Markdown.wlt:353,1-360,2"
]

VerificationTest[
    ToMarkdownString @ Row @ {
        "Here is some inline TeX: ",
        TraditionalForm @ Limit[ f @ x, x -> Infinity ]
    },
    "Here is some inline TeX: $\\underset{x\\to \\infty }{\\text{lim}}f(x)$",
    TestID -> "ToMarkdownString-TraditionalForm-3@@Tests/Markdown.wlt:362,1-369,2"
]

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*OpenerView*)
VerificationTest[
    ToMarkdownString @ OpenerView @ { "label", "content" },
    "<details><summary>label</summary>\n\ncontent\n\n</details>",
    TestID -> "ToMarkdownString-OpenerView-1@@Tests/Markdown.wlt:374,1-378,2"
]

VerificationTest[
    ToMarkdownString @ OpenerView @ {
        Style[ "label", "Section" ],
        ReadableForm @ Hold[ f[ x_ ] := Table[ i + 1, { i, x } ] ]
    },
    "<details><summary><h3>label</h3></summary>\n\n```wolfram\nHold[ f[ x_ ] := Table[ i + 1, { i, x } ] ]\n```\n\n</details>",
    TestID -> "ToMarkdownString-OpenerView-2@@Tests/Markdown.wlt:380,1-387,2"
]

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Error handling*)
VerificationTest[
    ToMarkdownString @ Wolfram`PacletCICD`Private`$failTest,
    Failure[ "ToMarkdownString::InternalError", _ ],
    { ToMarkdownString::InternalError },
    SameTest -> MatchQ,
    TestID   -> "ToMarkdownString-FailTest@@Tests/Markdown.wlt:392,1-398,2"
]

(*TODO

    Superscript/Subscript
*)