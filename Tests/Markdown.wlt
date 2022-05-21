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