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
$inline               = Automatic;
$mdBoxes              = False;
$html                 = True;
$useFE                = True;
$showStringCharacters = False;
$hyperlinkAction      = "New";
$htmlTables           = True;
$codeLanguage         = "wolfram";

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

$$itemStyle = "Item" | "Subitem" | "Subsubitem";
$$itemCell  = Cell[ __, $$itemStyle, ___ ];
$$item      = $$itemCell | StyleBox[ __, $$itemStyle, ___ ];
$$itemLevel = First /@ (Association @@ MapIndexed[ Rule, $$itemStyle ]);

$$standardFormString = StringExpression[
    FromCharacterCode[ 63425 ],
    FromCharacterCode[ 63433 ],
    FromCharacterCode[ 63432 ],
    ___,
    FromCharacterCode[ 63424 ]
];

$$hlTemplates = "HyperlinkURL"|"HyperlinkTemplate";
$$hlTemplate  = TemplateBox[ _, $$hlTemplates, ___ ];
$$hlButton    = ButtonBox[ ___, BaseStyle -> "Hyperlink", ___ ];
$$hyperlink   = $$hlButton | $$hlTemplate;
$$hlAction    = "New"|"Recycled";

$$rfWrapper    = (Style|ExpressionCell)[ __, "ReadableForm", ___ ];
$$rfBox        = (StyleBox|Cell)[ __, "ReadableForm", ___ ];
$$rfExpr       = _ReadableForm | $$rfWrapper;

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*ToMarkdownString*)
ToMarkdownString::InvalidInline =
"`1` is not a valid setting for Inline.";

ToMarkdownString::InvalidCharacterEncoding =
"`1` is not a valid character encoding.";

ToMarkdownString::StandardFormError =
"Unable to parse the StandardForm string `1`.";

ToMarkdownString::StringConversionError =
"Unable to convert `1` to a string.";

ToMarkdownString::HyperlinkActionError =
"`1` is not a valid value for DefaultHyperlinkAction";

ToMarkdownString::InternalError =
"Markdown conversion failed due to an unexpected error.";

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Options*)
ToMarkdownString // Options = {
    "DateStringFormat"       :> $DateStringFormat,
    "TimeZone"               :> $TimeZone,
    "CharacterEncoding"      -> Automatic,
    "DefaultHyperlinkAction" -> "New",
    "HTMLTables"             -> $htmlTables,
    "Inline"                 -> $inline
};

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Main definition*)
ToMarkdownString[ expr_, opts: OptionsPattern[ ] ] :=
    catchTop @ makeMarkdown[
        expr,
        optionsAssociation[ ToMarkdownString, opts ]
    ];

(* TODO:
    Images?
    BlockQuote
    Numbered lists
    ExternalEvaluate
*)

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*makeMarkdown*)
makeMarkdown // Attributes = { HoldFirst };
makeMarkdown[ expr_, as_Association ] :=
    Block[ { $inline = toInlineOption @ Lookup[ as, "Inline", Automatic ] },
        Replace[
            makeMarkdown0[ expr, as ],
            fail: Except[ _? StringQ ] :> throwMessageFailure[
                ToMarkdownString::InternalError,
                HoldForm @ expr,
                HoldForm @ fail
            ]
        ]
    ];

makeMarkdown // catchUndefined;


makeMarkdown0 // Attributes = { HoldFirst };

makeMarkdown0[ $failTest, as_ ] := $undefined;

makeMarkdown0[ str_String, as_ ] := mdString[ str, as ];

(* TODO: this doesn't space Item cells properly *)
makeMarkdown0[ list_List, as_ ] :=
    Block[ { $inline = False },
        StringRiffle[
            Cases[ Unevaluated @ list, e_ :> makeMarkdown0[ e, as ] ],
            "\n\n"
        ]
    ];

makeMarkdown0[ Row[ list_List, ___ ], as_ ] :=
    Block[ { $inline = True },
        StringJoin @ Cases[ Unevaluated @ list, e_ :> makeMarkdown0[ e, as ] ]
    ];

makeMarkdown0[ opener: OpenerView[ { _, _ }, ___ ], as_ ] /; $openerView :=
    mdOpenerView[ opener, as ];

makeMarkdown0[ Evaluate[ expr: $$rfExpr ], as_ ] :=
    mdReadableForm[ expr, as ];

makeMarkdown0[ expr: _Notebook|_Cell, as_ ] :=
    makeMarkdownFromBoxes[ expr, as ];

makeMarkdown0[ cell: _ExpressionCell|_TextCell, as_ ] :=
    makeMarkdownFromBoxes[ First @ MakeBoxes @ cell, as ];

makeMarkdown0[ Delimiter, a_ ] :=
    If[ TrueQ @ $forceHTML, "<hr>", "\n\n***\n\n" ];

makeMarkdown0[ tr_TraditionalForm, as_ ] := makeMDTradForm[ tr, as ];

makeMarkdown0[ RawBoxes[ e_ ], a_ ] := makeMarkdownFromBoxes[ e, a ];
makeMarkdown0[ Defer[ e_ ]   , a_ ] := makeMarkdownFromBoxes[ MakeBoxes @ e, a ];
makeMarkdown0[ HoldForm[ e_ ], a_ ] := makeMarkdownFromBoxes[ MakeBoxes @ e, a ];
makeMarkdown0[ e_            , a_ ] := makeMarkdownFromBoxes[ MakeBoxes @ e, a ];

makeMarkdown0 // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*toInlineOption*)
toInlineOption[ Automatic ] := $inline;
toInlineOption[ b: True|False ] := b;
toInlineOption[ other_ ] :=
    throwMessageFailure[ ToMarkdownString::InvalidInline, other ];

toInlineOption // catchUndefined;

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

$mdStringReplacements :=
    Module[ { nl, snl },
        nl  = If[ TrueQ @ $forceHTML, "<br>",   "\n" ];
        snl = If[ TrueQ @ $forceHTML, "<br>", "  \n" ];
        {
            "\[IndentingNewLine]" -> nl,
            "\n" -> snl,
            If[ TrueQ @ $showStringCharacters,
                Nothing,
                Sequence @@ { "\\n" -> snl, "\\\"" -> "\"" }
            ]
        }
    ];

toMDCharEncoding[ Automatic  ] := If[ TrueQ @ $codeBlock, "ASCII", "Unicode" ];
toMDCharEncoding[ enc_String ] := enc;
toMDCharEncoding[ expr_      ] :=
    throwMessageFailure[ ToMarkdownString::InvalidCharacterEncoding, expr ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*makeMarkdownFromBoxes*)
makeMarkdownFromBoxes[ Notebook[ cells_List, ___ ], as_ ] :=
    makeMarkdownFromBoxList[ cells, as ];

makeMarkdownFromBoxes[ Cell[ CellGroupData[ cells_List, _ ], ___ ], as_ ] :=
    makeMarkdownFromBoxList[ cells, as ];

makeMarkdownFromBoxes[ cell: $$heading, as_ ] /; ! TrueQ @ $inline :=
    makeMDHeading[ cell, as ];

makeMarkdownFromBoxes[ cell: $$item, as_ ] /; ! TrueQ @ $inline :=
    makeMDItem[ cell, as ];

makeMarkdownFromBoxes[ box: $$rfBox, as_ ] := mdReadableForm[ box, as ];

makeMarkdownFromBoxes[ cell: $$inputCell|$$outputCell, as_ ] :=
    makeMDCodeBlock[ cell, as ];

makeMarkdownFromBoxes[
    box: StyleBox[ __, $$inputStyle|$$outputStyle, ___ ],
    as_
] := makeMDCodeInline[ box, as ];

makeMarkdownFromBoxes[ cell: Cell[ TextData[ text_List ], ___ ], as_ ] :=
    Block[ { $inline = True },
        includeStyleOptions[
            StringJoin[ makeMarkdownFromBoxes[ #, as ] & /@ text ],
            cell
        ]
    ];

makeMarkdownFromBoxes[ s: (Cell|StyleBox)[ a_, ___ ], as_ ] :=
    includeStyleOptions[ makeMarkdownFromBoxes[ a, as ], s ];

makeMarkdownFromBoxes[ box: $$hyperlink, as_ ] := makeMDHyperlink[ box, as ];

makeMarkdownFromBoxes[ TagBox[ box_GridBox, _ ], as_ ] := makeMDGrid[ box, as ];
makeMarkdownFromBoxes[ box_GridBox, as_ ] := makeMDGrid[ box, as ];

makeMarkdownFromBoxes[ TemplateBox[ row_List, "RowDefault", ___ ], as_ ] :=
    Block[ { $inline = True },
        StringJoin[ makeMarkdownFromBoxes[ #, as ] & /@ row ]
    ];

makeMarkdownFromBoxes[ boxes_, as_ ] :=
    Block[ { $inline = True }, mdString[ cellToString @ boxes, as ] ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*makeMarkdownFromBoxList*)
makeMarkdownFromBoxList[ { a___, items: Longest[ $$item.. ], b___ }, as_ ] :=
    Enclose @ Block[ { $inline = False },
        Module[ { cs, aStr, iStr, bStr },
            cs   = ConfirmBy[ #1, StringQ ] &;
            aStr = cs @ makeMarkdownFromBoxes[ { a }, as ];
            iStr = (cs @ makeMarkdownFromBoxes[ #, as ] &) /@ { items };
            bStr = cs @ makeMarkdownFromBoxes[ { b }, as ];
            StringRiffle[
                DeleteCases[ { aStr, StringRiffle[ iStr, "\n" ], bStr }, "" ],
                "\n\n"
            ]
        ]
    ];

makeMarkdownFromBoxList[ cells_List, as_ ] :=
    Block[ { $inline = False },
        StringRiffle[ makeMarkdownFromBoxes[ #, as ] & /@ cells, "\n\n" ]
    ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*includeStyleOptions*)
includeStyleOptions[ str_String? StringQ, box_ ] := Enclose[
    Module[ { cs, fw, fs },
        cs  = ConfirmBy[ #, StringQ ] &;
        fw  = cs @ includeFontWeight[ str, box ];
        fs  = cs @ includeFontSlant[ fw, box ];
        fs
    ]
];

includeStyleOptions // catchUndefined;

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
    If[ TrueQ @ $forceHTML,
        "<strong>" <> str <> "</strong>",
        "**" <> str <> "**"
    ];

includeFontWeight[ str_String? StringQ, cell_, fw_ ] := str;


getFontSlant[ StyleBox[___, fw: Italic|Plain, ___ ] ] := ToString @ fw;
getFontSlant[ _[ ___, FontSlant|"FontSlant" -> fw_, ___ ] ] := ToString @ fw;
getFontSlant[ ___ ] := "Plain";


includeFontSlant[ s_, c_ ] := includeFontSlant[ s, c, getFontSlant @ c ];

includeFontSlant[ str_String? StringQ, cell_, "Italic" ] :=
    If[ TrueQ @ $forceHTML,
        "<em>" <> str <> "</em>",
        "*" <> str <> "*"
    ];

includeFontSlant[ str_String? StringQ, cell_, fw_ ] := str;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*makeMDTradForm*)
makeMDTradForm // Attributes = { HoldFirst };

makeMDTradForm[ tr_TraditionalForm, as_ ] /; $forceHTML :=
    makeMarkdownFromBoxes[ MakeBoxes @ tr, as ];

makeMDTradForm[ tr_TraditionalForm, as_ ] := Enclose[
    Module[ { str, tex },
        str = ConfirmBy[ ExportString[ tr, "TeXFragment" ], StringQ ];
        tex = StringReplace[ StringTrim @ str, "\\["~~s___~~"\\]" :> s ];
        If[ TrueQ @ $inline,
            "$"<>tex<>"$",
            "$$"<>tex<>"$$"
        ]
    ],
    makeMarkdownFromBoxes[ MakeBoxes @ tr, as ] &
];

makeMDTradForm // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*mdOpenerView*)
mdOpenerView // Attributes = { HoldFirst };

mdOpenerView[ OpenerView[ { lbl_, body_ }, ___ ], as_ ] /; $openerView :=
    Enclose @ Block[ { $inline = False },
        Module[ { lbl$, body$ },
            lbl$  = ConfirmBy[ mdOpenerLabel[ lbl , as ], StringQ ];
            body$ = ConfirmBy[ makeMarkdown[ body , as ], StringQ ];
            StringJoin[
                "<details><summary>",
                lbl$,
                "</summary>\n\n",
                body$,
                "\n\n</details>"
            ]
        ]
    ];

mdOpenerView // catchUndefined;


mdOpenerLabel // Attributes = { HoldFirst };
mdOpenerLabel[ lbl_, as_ ] :=
    Block[ { $forceHTML = True }, makeMarkdown[ lbl , as ] ];

mdOpenerLabel // catchUndefined;

$openerView := MatchQ[ $inline, Automatic|True ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*mdReadableForm*)
mdReadableForm // Attributes = { HoldFirst };

mdReadableForm[ rf_ReadableForm, as_ ] :=
    Block[ { $inline = Replace[ $inline, Automatic -> True ] },
        mdCodeBlock[ ToString @ rf, as ]
    ];

mdReadableForm[ (h: Style|ExpressionCell)[ Defer[ expr_ ], a___ ], as_ ] :=
    mdReadableForm[ h[ expr, a ], as ];

mdReadableForm[ Style[ expr_, ___ ], as_ ] :=
    Block[ { $inline = Replace[ $inline, Automatic -> True ] },
        mdReadableForm[ ReadableForm @ Unevaluated @ expr, as ]
    ];

mdReadableForm[ ExpressionCell[ expr_, ___ ], as_ ] :=
    Block[ { $inline = Replace[ $inline, Automatic -> False ] },
        mdReadableForm[ ReadableForm @ Unevaluated @ expr, as ]
    ];

mdReadableForm[
    StyleBox[ ___, TaggingRules -> KeyValuePattern[ "String" :> str_ ], ___ ],
    as_
] /; StringQ @ str := mdCodeBlock[ str, as ];

mdReadableForm // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*makeMDGrid*)
makeMDGrid[ g: GridBox[ grid_, ___ ], as_ ] :=
    Block[ { $inline = True, $htmlTables = Lookup[ as, "HTMLTables" ] },
        makeMDGrid[ g, grid, as ]
    ];

makeMDGrid[ _, grid_? MatrixQ, as_ ] /; $htmlTables :=
    Enclose @ Block[ { $forceHTML = True },
        Module[ { tr },
            tr = ConfirmBy[ makeMDGridRow[ #, as ], StringQ ] & /@ grid;
            "<table><tbody>" <> tr <> "</tbody></table>"
        ]
    ];

makeMDGrid[ _, grid0_? MatrixQ, as_ ] /; Positive @ Length @ grid0 :=
    Enclose @ Module[ { cs, width, grid, header, rows, sep, tr },
        cs     = ConfirmBy[ #, StringQ ] &;
        width  = Last @ Dimensions @ grid0;
        grid   = If[ Length @ grid0 === 1,
                     Prepend[ grid0,
                              ConstantArray[ "", width ]
                     ],
                     grid0
                 ];
        header = cs @ makeMDGridRow[ First @ grid, as ];
        sep    = cs @ makeMDGridRow[ ConstantArray[ "---", width ], as ];
        rows   = (cs @ makeMDGridRow[ #1, as ] &) /@ Rest @ grid;
        StringRiffle[ Flatten @ { header, sep, rows }, "\n" ]
    ];

makeMDGrid // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*makeMDGridRow*)
makeMDGridRow[ row_List, as_ ] /; $htmlTables :=
    Enclose @ Module[ { str, td },
        str = ConfirmBy[ makeMarkdownFromBoxes[ #1, as ], StringQ ] & /@ row;
        td = ("<td>" <> #1 <> "</td>" &) /@ str;
        "<tr>" <> td <> "</tr>"
    ];

makeMDGridRow[ row_List, as_ ] :=
    Enclose @ Module[ { str },
        str = ConfirmBy[ makeMarkdownFromBoxes[ #1, as ], StringQ ] & /@ row;
        "| " <> StringRiffle[ str, " | " ] <> " |"
    ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*makeMDHyperlink*)
makeMDHyperlink[
    box: TemplateBox[ { label_, url_, ___ }, $$hlTemplates, ___ ],
    as_
] :=
    Block[ { $hyperlinkAction = getHyperlinkAction[ box, as ] },
        makeMDHyperlink[ label, url, as ]
    ];

makeMDHyperlink[ label_, { url_, _ }, as_ ] :=
    makeMDHyperlink[ label, url, as ];

makeMDHyperlink[ label_, URL[ url_ ], as_ ] :=
    makeMDHyperlink[ label, url, as ];

makeMDHyperlink[ label_, url_String, as_ ] :=
    Module[ { str },
        str = Block[ { $inline = True }, makeMarkdownFromBoxes[ label, as ] ];
        If[ $hyperlinkAction === "New" && TrueQ @ $html,
            "<a href=\"" <> url <> "\" target=\"_blank\">" <> str <> "</a>",
            StringJoin[ "[", str, "](", url, ")" ]
        ]
    ];

makeMDHyperlink // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*getHyperlinkAction*)
getHyperlinkAction[ _[ ___,  HyperlinkAction  -> a:$$hlAction, ___ ], _ ] := a;
getHyperlinkAction[ _[ ___, "HyperlinkAction" -> a:$$hlAction, ___ ], _ ] := a;
getHyperlinkAction[ _[ ___,  HyperlinkAction  :> a:$$hlAction, ___ ], _ ] := a;
getHyperlinkAction[ _[ ___, "HyperlinkAction" :> a:$$hlAction, ___ ], _ ] := a;
getHyperlinkAction[ TemplateBox[ a_, ___ ], b_ ] := getHyperlinkAction[ a, b ];

getHyperlinkAction[ _, as_Association ] :=
    toHyperlinkAction @ Lookup[ as, "DefaultHyperlinkAction" ];

getHyperlinkAction // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*toHyperlinkAction*)
toHyperlinkAction[ Automatic       ] := "New";
toHyperlinkAction[ act: $$hlAction ] := act;

toHyperlinkAction[ other_ ] := throwMessageFailure[
    ToMarkdownString::HyperlinkActionError,
    other
];

toHyperlinkAction // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*makeMDHeading*)
makeMDHeading[ (Cell|StyleBox)[ a_, ___, s: $$headingStyle, ___ ], as_ ] :=
    Enclose @ Module[ { lvl, md, tag },
        lvl = ConfirmBy[ Lookup[ $$headingLevel, s ], IntegerQ ];
        md  = ConfirmBy[ makeMarkdownFromBoxes[ a, as ], StringQ ];
        If[ TrueQ @ $forceHTML,
            tag = "h" <> ToString @ lvl;
            "<"<>tag<>">"<>md<>"</"<>tag<>">",
            ConstantArray[ "#", lvl ] <> " " <> md
        ]
    ];

makeMDHeading // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*makeMDItem*)
makeMDItem[ (Cell|StyleBox)[ a_, ___, s: $$itemStyle, ___ ], as_ ] :=
    Enclose @ StringJoin[
        ConstantArray[
            "  ",
            ConfirmBy[ Lookup[ $$itemLevel, s ], IntegerQ ] - 1
        ],
        "- ",
        ConfirmBy[ makeMarkdownFromBoxes[ a, as ], StringQ ]
    ];

makeMDItem // catchUndefined;

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
        If[ TrueQ[ $inline || $forceHTML ],
            toInlineCodeString[ md, lang ],
            StringRiffle[
                { "```" <> lang, md, "```" },
                "\n"
            ]
        ] /; StringQ @ md
    ];

mdCodeBlock // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*toInlineCodeString*)
toInlineCodeString[ code_ ] := toInlineCodeString[ code, $codeLanguage ];

toInlineCodeString[ code_String? inlineHTMLQ, lang_ ] :=
    Module[ { pre, br },
        pre = If[ StringQ @ lang, "<pre lang=\""<>lang<>"\">", "<pre>" ];
        br  = If[ StringQ @ lang, "&#10;", "<br>" ];
        StringJoin[
            "<pre lang=\""<>lang<>"\">",
            StringReplace[
                code,
                { "<br>" -> br, "\n" -> br, "<" -> "&lt;", ">" -> "&gt;" }
            ],
            "</pre>"
        ]
    ];

toInlineCodeString[ code_String? StringQ, lang_String ?StringQ ] :=
    If[ TrueQ @ StringContainsQ[ code, "`" ],
        "`` " <> code <> " ``",
        "`" <> code <> "`"
    ];

toInlineCodeString // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*inlineHTMLQ*)
inlineHTMLQ[ code_String? StringQ ] :=
    TrueQ[ $forceHTML || StringContainsQ[ code, "\n" ] ];

inlineHTMLQ[ ___ ] := False;

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
        toInlineCodeString @ md /; StringQ @ md
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

cellToString  // catchUndefined;
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
            If[ TrueQ[ $html || $forceHTML ],
                StringJoin[ a$, "<", t, ">", b$, "</", t, ">" ],
                h = StringDelete[ ToString @ box, "Box"~~EndOfString ];
                StringJoin[ h, "[", a$, ", ", b$, "]" ]
            ],
            { a$, b$ }
        ]
    ];

fasterCellToString0[ box: (TagBox|FormBox)[ _, TraditionalForm, ___ ] ] :=
    Module[ { expr },
        expr = Quiet @ ToExpression[ box, StandardForm, HoldComplete ];
        Replace[
            expr,
            HoldComplete[ e_ ] :>
                If[ TrueQ @ $showStringCharacters,
                    ToString[ Unevaluated @ e, InputForm ],
                    ToString @ Unevaluated @ e
                ]
        ] /; MatchQ[ expr, HoldComplete[ _ ] ]
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
    Module[ { plain, string },
        plain = UsingFrontEnd @ Quiet @ FrontEndExecute @ FrontEnd`ExportPacket[
            cell,
            "PlainText"
        ];

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