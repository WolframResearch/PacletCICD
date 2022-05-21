(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Header*)
BeginPackage[ "Wolfram`PacletCICD`" ];

ClearAll[
    ToMarkdownString
];

Begin[ "`Private`" ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Flags*)
$inline               = False;
$mdBoxes              = False;
$html                 = True;
$useFE                = True;
$showStringCharacters = False;

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Argument patterns*)
$$outputStyle = "Output"|"Print"|"Echo"|"Message"|"MSG"|"ExampleOutput";
$$outputCell  = Cell[ _BoxData, ___, $$outputStyle, ___ ];
$$output      = $$outputCell | StyleBox[ __, $$outputStyle, ___ ];

$$inputStyle = "Input"|"Code"|"InputOnly"|"ExampleInput";
$$inputCell  = Cell[ _BoxData, ___, $$inputStyle, ___ ];
$$input      = $$inputCell | StyleBox[ __, $$inputStyle, ___ ];

$$headingStyle = Alternatives[
                     "Title",
                     "Subtitle",
                     "Section",
                     "Subsection",
                     "Subsubsection",
                     "Subsubsubsection"
                 ];
$$headingCell  = Cell[ __, $$headingStyle, ___ ];
$$heading      = $$headingCell | StyleBox[ __, $$headingStyle, ___ ];
$$headingLevel = First /@ (Association @@ MapIndexed[ Rule, $$headingStyle ]);

$$standardFormString = StringExpression[
    FromCharacterCode[ 63425 ],
    FromCharacterCode[ 63433 ],
    FromCharacterCode[ 63432 ],
    ___,
    FromCharacterCode[ 63424 ]
];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*ToMarkdownString*)
ToMarkdownString::InvalidCharacterEncoding =
"`1` is not a valid character encoding.";

ToMarkdownString::StandardFormError =
"Unable to parse the StandardForm string `1`.";

ToMarkdownString::StringConversionError =
"Unable to convert `1` to a string.";

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Options*)
ToMarkdownString // Options = {
    "DateStringFormat"  :> $DateStringFormat,
    "TimeZone"          :> $TimeZone,
    "CharacterEncoding" -> Automatic
};

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Main definition*)
ToMarkdownString[ expr_, opts: OptionsPattern[ ] ] :=
    catchTop @ makeMarkdown[
        expr,
        optionsAssociation[ ToMarkdownString, opts ]
    ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*makeMarkdown*)
makeMarkdown // Attributes = { HoldFirst };

makeMarkdown[ str_String, as_ ] := mdString[ str, as ];

makeMarkdown[ list_List, as_ ] :=
    Block[ { $inline = False },
        StringRiffle[
            Cases[ Unevaluated @ list, e_ :> makeMarkdown[ e, as ] ],
            "\n\n"
        ]
    ];

makeMarkdown[ expr: _Notebook|_Cell, as_ ] :=
    makeMarkdownFromBoxes[ expr, as ];

makeMarkdown[ cell: _ExpressionCell|_TextCell, as_ ] :=
    makeMarkdownFromBoxes[ First @ MakeBoxes @ cell, as ];

makeMarkdown[ RawBoxes[ e_ ], a_ ] := makeMarkdownFromBoxes[ e, a ];
makeMarkdown[ Defer[ e_ ]   , a_ ] := makeMarkdownFromBoxes[ MakeBoxes @ e, a ];
makeMarkdown[ HoldForm[ e_ ], a_ ] := makeMarkdownFromBoxes[ MakeBoxes @ e, a ];
makeMarkdown[ e_            , a_ ] := makeMarkdownFromBoxes[ MakeBoxes @ e, a ];

makeMarkdown // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*mdString*)
mdString[ str_String? StringQ, as_Association ] :=
    Module[ { new, enc, rep },
        new = Block[ { $showStringCharacters = True }, cellToString @ str ];
        enc = toMDCharEncoding @ Lookup[ as, "CharacterEncoding", Automatic ];
        rep = If[ StringQ @ new,
                  StringReplace[ new, $mdStringReplacements ],
                  StringReplace[ str, $mdStringReplacements ]
              ];
        ToString[ rep, CharacterEncoding -> enc ]
    ];

$mdStringReplacements = {
    "\[IndentingNewLine]" -> "\n"
};

toMDCharEncoding[ Automatic  ] := If[ TrueQ @ $codeBlock, "ASCII", "Unicode" ];
toMDCharEncoding[ enc_String ] := enc;
toMDCharEncoding[ expr_      ] :=
    throwMessageFailure[ ToMarkdownString::InvalidCharacterEncoding, expr ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*makeMarkdownFromBoxes*)
makeMarkdownFromBoxes[ Notebook[ cells_List, ___ ], as_ ] :=
    Block[ { $inline = False },
        StringRiffle[ makeMarkdownFromBoxes[ #, as ] & /@ cells, "\n\n" ]
    ];

makeMarkdownFromBoxes[ Cell[ CellGroupData[ cells_List, _ ], ___ ], as_ ] :=
    Block[ { $inline = False },
        StringRiffle[ makeMarkdownFromBoxes[ #, as ] & /@ cells, "\n\n" ]
    ];

makeMarkdownFromBoxes[ cell: $$heading, as_ ] :=
    makeMDHeading[ cell, as ];

makeMarkdownFromBoxes[ cell: $$inputCell|$$outputCell, as_ ] :=
    makeMDCodeBlock[ cell, as ];

makeMarkdownFromBoxes[
    box: StyleBox[ __, $$inputStyle|$$outputStyle, ___ ],
    as_
] := makeMDCodeInline[ box, as ];

makeMarkdownFromBoxes[ Cell[ TextData[ text_List ], ___ ], as_ ] :=
    Block[ { $inline = True },
        StringJoin[ makeMarkdownFromBoxes[ #, as ] & /@ text ]
    ];

makeMarkdownFromBoxes[ s: StyleBox[ a_, ___ ], as_ ] := Enclose[
    Module[ { cs, str, fw, fs },
        cs  = ConfirmBy[ #, StringQ ] &;
        str = cs @ makeMarkdownFromBoxes[ a, as ];
        fw  = cs @ includeFontWeight[ str, s ];
        fs  = cs @ includeFontSlant[ fw, s ];
        fs
    ]
];

makeMarkdownFromBoxes[ boxes_, as_ ] :=
    Block[ { $inline = True }, cellToString @ boxes ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*getStyleOption*)
getStyleOption // Attributes = { HoldRest };

getStyleOption[ _[ __, (Rule|RuleDelayed)[ k_, v_ ], ___ ], k_, d_ ] := v;

getStyleOption[
    _[ __, opts: OptionsPattern[ ] ],
    name_,
    default_
] :=
    Module[ { assoc },
        assoc = KeyMap[ ToString, Association @ opts ];
        Lookup[ assoc, ToString @ name, default ]
    ];

getStyleOption[ _, _, default_ ] := default;

getStyleOption[ cell_, name_ ] :=
    getStyleOption[ cell, name, Missing[ "NotFound" ] ];

getStyleOption // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*getStyleNames*)
getStyleNames[ (Cell|StyleBox)[ _, rest___ ] ] := Cases[ { rest }, _String ];
getStyleNames // catchUndefined;



getFontWeight[ StyleBox[___, fw: Bold|Plain, ___ ] ] := ToString @ fw;
getFontWeight[ _[ ___, FontWeight|"FontWeight" -> fw_, ___ ] ] := ToString @ fw;
getFontWeight[ ___ ] := "Plain";


includeFontWeight[ s_, c_ ] := includeFontWeight[ s, c, getFontWeight @ c ];

includeFontWeight[ str_String? StringQ, cell_, "Bold" ] :=
    "**" <> str <> "**";

includeFontWeight[ str_String? StringQ, cell_, fw_ ] :=
    str;


getFontSlant[ StyleBox[___, fw: Italic|Plain, ___ ] ] := ToString @ fw;
getFontSlant[ _[ ___, FontSlant|"FontSlant" -> fw_, ___ ] ] := ToString @ fw;
getFontSlant[ ___ ] := "Plain";


includeFontSlant[ s_, c_ ] := includeFontSlant[ s, c, getFontSlant @ c ];

includeFontSlant[ str_String? StringQ, cell_, "Italic" ] :=
    "*" <> str <> "*";

includeFontSlant[ str_String? StringQ, cell_, fw_ ] :=
    str;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*makeMDHeading*)
makeMDHeading[ (Cell|StyleBox)[ a_, ___, s: $$headingStyle, ___ ], as_ ] :=
    Enclose @ StringJoin[
        ConstantArray[
            "#",
            ConfirmBy[ Lookup[ $$headingLevel, s ], IntegerQ ]
        ],
        " ",
        ConfirmBy[ makeMarkdownFromBoxes[ a, as ], StringQ ]
    ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*makeMDCodeBlock*)
makeMDCodeBlock[ cell_, as_ ] :=
    Module[ { str },
        str = Block[ { $inline = True, $html = False }, cellToString @ cell ];
        mdCodeBlock[ str, as ]
    ];

makeMDCodeBlock // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*mdCodeBlock*)
mdCodeBlock[ string_String? StringQ, as_, lang_: "wolfram" ] :=
    With[ { md = mdString[ string, as ] },
        If[ TrueQ @ $inline,
            If[ StringContainsQ[ string, "`" ],
                "`` " <> md <> " ``",
                "`" <> md <> "`"
            ],
            StringRiffle[
                { "```" <> lang, md, "```" },
                "\n"
            ]
        ] /; StringQ @ md
    ];

mdCodeBlock // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*makeMDCodeInline*)
makeMDCodeInline[ cell_, as_ ] :=
    Module[ { str },
        str = Block[ { $inline = True, $html = False }, cellToString @ cell ];
        mdCodeInline[ str, as ]
    ];

makeMDCodeInline // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*mdCodeInline*)
mdCodeInline[ string_String? StringQ, as_ ] :=
    With[ { md = mdString[ string, as ] },
        If[ StringContainsQ[ string, "`" ],
            "`` " <> md <> " ``",
            "`" <> md <> "`"
        ] /; StringQ @ md
    ];

mdCodeInline // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*cellToString*)
cellToString[ data: _TextData | _BoxData ] := cellToString @ Cell @ data;
cellToString[ Cell @ CellGroupData[ { cell_ }, _ ] ] := cellToString @ cell;
cellToString[ cells: { __CellObject } ] := cellToString /@ NotebookRead @ cells;
cellToString[ cell_CellObject ] := cellToString @ { cell };

cellToString[ c_ ] :=
    With[ { string = Catch[ cellToString0 @ c, $cellToString ] },
        If[ StringQ @ string,
            StringReplace[ string, { "\r\n" -> "\n" } ],
            string
        ]
    ];

cellToString0[ c_ ] :=
    Module[ { ssc },
        ssc = getStyleOption[ c, ShowStringCharacters, $showStringCharacters ];
        Block[ { $showStringCharacters = ssc }, cellToString1 @ c ]
    ];

cellToString1[ c_ ] :=
    Module[ { string },
        string = fasterCellToString @ c;
        If[ StringQ @ string, Throw[ string, $cellToString ] ];
        string = fastCellToString @ c;
        If[ StringQ @ string, Throw[ string, $cellToString ] ];
        slowCellToString @ c
    ];

cellToString // catchUndefined;
cellToString0 // catchUndefined;
cellToString1 // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*fasterCellToString*)
fasterCellToString[ arg_ ] :=
    Block[ { $catchingStringFail = True },
        Catch[ fasterCellToString0 @ arg, $stringFail ]
    ];

fasterCellToString // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*$stringStripHeads*)
$stringStripHeads = Alternatives[
    StyleBox,
    ButtonBox,
    TooltipBox,
    TagBox,
    FormBox,
    InterpretationBox,
    BoxData,
    TextData,
    RowBox,
    CellGroupData
];

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*$stringIgnoredHeads*)
$stringIgnoredHeads =
    GraphicsBox | Graphics3DBox | CheckboxBox | PaneSelectorBox;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*$boxOp*)
$boxOp = <| SuperscriptBox -> "sup", SubscriptBox -> "sub" |>;

$boxOperators = Alternatives @@ Keys[ $boxOp ];

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*$templateBoxRules*)
$templateBoxRules := $templateBoxRules = <|
    "DateObject"       -> First,
    "HyperlinkDefault" -> First,
    "RefLink"          -> First,
    "RowDefault"       -> Identity
|>;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*showStringCharactersQ*)
showStringCharactersQ[ "Input"|"Text" ] := True;
showStringCharactersQ[ "Output"|"Print"|"Echo"|"Message"|"MSG" ] := False;

showStringCharactersQ[ cell: (Cell|StyleBox)[ _, s_String, ___ ] ] :=
    getStyleOption[ cell, ShowStringCharacters, showStringCharactersQ @ s ];

showStringCharactersQ[ s_String ] /; $useFE :=
    showStringCharactersQ[ s ] =
        UnsameQ[
            CurrentValue @ { StyleDefinitions, s, "ShowStringCharacters" },
            False
        ];

showStringCharactersQ[ _ ] := False;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*fasterCellToString0*)
fasterCellToString0[ a_String /; StringContainsQ[ a, $$standardFormString ] ] :=
    Module[ { split, string },
        split = StringSplit[ a,
                             s: $$standardFormString :>
                                 cellToString @ stringToBoxes @ s
                ];

        string = If[ AllTrue[ split, StringQ ],
                     StringJoin @ split,
                     throwMessageFailure[
                         ToMarkdownString::StandardFormError,
                         a
                     ]
                 ];

        fasterCellToString0 @ string /;
            StringQ @ string && StringFreeQ[ string, $$standardFormString ]
    ];

fasterCellToString0[ s: _String | { ___String } ] :=
    If[ TrueQ @ $showStringCharacters,
        StringJoin @ s,
        StringJoin @ StringTrim[ s, "\"" ]
    ];

fasterCellToString0[ e: (Cell|StyleBox)[ a_, ___ ] ] :=
    Block[ { $showStringCharacters = showStringCharactersQ @ e },
        fasterCellToString0 @ a
    ];

fasterCellToString0[ $stringStripHeads[ a_, ___ ] ] := fasterCellToString0 @ a;

fasterCellToString0[ $stringIgnoredHeads[ ___ ] ] := "";

fasterCellToString0[ list_List ] :=
    With[ { strings = fasterCellToString0 /@ list },
        If[ AllTrue[ strings, StringQ ], StringJoin @ strings, strings ]
    ];

fasterCellToString0[ (box: $boxOperators)[ a_, b_ ] ] :=
    Module[ { a$, b$, t, h },
        a$ = fasterCellToString0 @ a;
        b$ = fasterCellToString0 @ b;
        t  = $boxOp @ box;
        If[ StringQ @ a$ && StringQ @ b$,
            If[ TrueQ @ $html,
                StringJoin[ a$, "<", t, ">", b$, "</", t, ">" ],
                h = StringDelete[ ToString @ box, "Box"~~EndOfString ];
                StringJoin[ h, "[", a$, ", ", b$, "]" ]
            ],
            { a$, b$ }
        ]
     ];

fasterCellToString0[ TemplateBox[ args_, name_String, ___ ] ] :=
    With[ { s = fasterCellToString0 @ $templateBoxRules[ name ][ args ] },
        s /; StringQ @ s
    ];

fasterCellToString0[ Notebook[ cells_List, ___ ] ] :=
    With[ { strings = fasterCellToString0 /@ cells },
        If[ AllTrue[ strings, StringQ ],
            StringRiffle[ strings, "\n\n" ],
            strings
        ]
    ];

fasterCellToString0[ GridBox[ grid_? MatrixQ, ___ ] ] :=
    Module[ { strings, rows },
        strings = Map[ fasterCellToString0, grid, { 2 } ];
        If[ AllTrue[ strings, StringQ, 2 ], makeGridString @ strings, strings ]
    ];

fasterCellToString0[ ___ ] /; $catchingStringFail :=
    Throw[ $Failed, $stringFail ];

fasterCellToString0 // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsubsubsection::Closed:: *)
(*makeGridString*)
makeGridString[ grid_ ] :=
    Module[ { tr, colSizes },
        tr = Transpose @ grid;
        colSizes = Max /@ Map[ StringLength, tr, { 2 } ];
        StringRiffle[
            Map[
                StringRiffle,
                Transpose[
                    StringPadRight @@@ (Transpose @ { tr, colSizes })
                ]
            ],
            "\n"
        ]
    ];

(******************************************************************************)
(* ::Subsection::Closed:: *)
(*fastCellToString*)
fastCellToString[ cell_ ] :=
    With[ { string = cell //. $cellToStringReplacementRules },
        Replace[ StringTrim[ string, WhitespaceCharacter ],
                 "" -> Missing[ "NotFound" ]
        ] /; StringQ @ string
    ];

fastCellToString[ ___ ] :=
    $Failed;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*$cellToStringReplacementRules*)
$cellToStringReplacementRules := $cellToStringReplacementRules =
    Dispatch @ {
        StyleBox[ a_String, ___ ] :> a,
        ButtonBox[ a_String, ___ ] :> a,
        TooltipBox[ a_String, ___ ] :> a,
        TagBox[ a_String, ___ ] :> a,
        SuperscriptBox[ a_String, b_String ] :>
            StringJoin[ "Superscript[", a, ", ", b, "]" ],
        SubscriptBox[ a_String, b_String ] :>
            StringJoin[ "Subscript[", a, ", ", b, "]" ],
        RowBox[ a: { ___String } ] :> StringJoin @ a,
        TemplateBox[ { a_String, ___ }, "RefLink", ___ ] :> a,
        FormBox[ a_String, ___ ] :> a,
        InterpretationBox[ a_String, ___ ] :> a,
        BoxData[ a_String ] :> a,
        BoxData[ a: { ___String } ] :> StringRiffle[ a, "\n" ],
        TextData[ a_String ] :> a,
        TextData[ a: { ___String } ] :> StringJoin @ a,
        Cell[ a_String, ___ ] :> a
        ,
        s_String /; StringContainsQ[ s, "\!" ] :>
          With[ { res = stringToBoxes @ s },
              res /; FreeQ[ res, ss_String /; StringContainsQ[ ss, "\!" ] ]
          ]
    };

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*slowCellToString*)
slowCellToString[ cell_ ] /; $useFE :=
    UsingFrontEnd @ Module[ { plain, string },
        plain = Quiet @ FrontEndExecute @ ExportPacket[ cell, "PlainText" ];

        string = Replace[
            plain,
            { { s_String? StringQ, ___ } :> s, ___ :> $Failed }
        ];

        Replace[
            StringTrim[ string, WhitespaceCharacter ],
            "" -> Missing[ "NotFound" ]
        ] /; StringQ @ string
    ];

slowCellToString[ cell_ ] :=
    throwMessageFailure[ ToMarkdownString::StringConversionError, cell ];

slowCellToString // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*stringToBoxes*)
stringToBoxes[ s_String /; StringMatchQ[ s, "\"" ~~ __ ~~ "\"" ] ] :=
    "\"" <> stringToBoxes @ StringTrim[ s, "\"" ] <> "\"";

stringToBoxes[ s_String? standardFormStringQ ] :=
    With[ { e = ToExpression[ StringTake[ s, { 4, -2 } ], InputForm ] },
        e /; ! FailureQ @ e
    ];

stringToBoxes[ s_String ] /; $useFE :=
    UsingFrontEnd @ First @ First @ MathLink`CallFrontEnd @
        FrontEnd`UndocumentedTestFEParserPacket[ s, True ];

stringToBoxes[ s_String ] := ToString @ s;

stringToBoxes // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*standardFormStringQ*)
standardFormStringQ[ s_String ] := StringMatchQ[ s, $$standardFormString ];
standardFormStringQ[ ___ ] := False;

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Footer*)
End[ ];

EndPackage[ ];