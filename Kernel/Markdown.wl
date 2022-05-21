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
$inline          = False;
$mdStringDefault = False; (* TODO change to True before release *)
$mdBoxes         = False;

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*ToMarkdownString*)

ToMarkdownString // Options = {
    "DateStringFormat" :> $DateStringFormat,
    "TimeZone"         :> $TimeZone
};

ToMarkdownString[ expr_, opts: OptionsPattern[ ] ] :=
    catchTop @ makeMarkdown[
        expr,
        optionsAssociation[ ToMarkdownString, opts ]
    ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*makeMarkdown*)
makeMarkdown // Attributes = { HoldFirst };

makeMarkdown[ str_String, as_ ] := mdString @ str;

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

makeMarkdown[ Defer[ e_ ]   , a_ ] := makeMarkdownFromBoxes[ MakeBoxes @ e, a ];
makeMarkdown[ HoldForm[ e_ ], a_ ] := makeMarkdownFromBoxes[ MakeBoxes @ e, a ];
makeMarkdown[ e_            , a_ ] := makeMarkdownFromBoxes[ MakeBoxes @ e, a ];

makeMarkdown // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*mdString*)
mdString[ str_String? StringQ ] :=
    Module[ { new },
        new = cellToString @ str;
        StringReplace[ new, $mdStringReplacements ]
    ];

$mdStringReplacements = {
    "\[IndentingNewLine]" -> "\n"
};

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

makeMarkdownFromBoxes[ Cell[ BoxData[ boxes_ ]]]

makeMarkdownFromBoxes[ boxes_, as_ ] /; $mdStringDefault :=
    cellToString[ boxes, as ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*cellToString*)
cellToString[ data: _TextData | _BoxData ] := cellToString @ Cell @ data;
cellToString[ string_String? StringQ ] := fasterCellToString @ string;
cellToString[ Cell @ CellGroupData[ { cell_ }, _ ] ] := cellToString @ cell;
cellToString[ cells: { __CellObject } ] := cellToString /@ NotebookRead @ cells;
cellToString[ cell_CellObject ] := cellToString @ { cell };

cellToString[ cell_ ] :=
    Module[ { string },
        string = fasterCellToString @ cell;
        If[ StringQ @ string, Throw[ string, $tag ] ];
        string = fastCellToString @ cell;
        If[ StringQ @ string, Throw[ string, $tag ] ];
        slowCellToString @ cell
    ] ~Catch~ $tag;

cellToString[ ___ ] := $Failed;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*fasterCellToString*)
fasterCellToString[ arg_ ] := Block[ { $catchingStringFail = True },
    Module[ { string },
        string = fasterCellToString0 @ arg;
        If[ StringQ @ string,
            Replace[ StringTrim @ string, "" -> Missing[ "NotFound" ] ],
            $Failed
        ]
    ] ~Catch~ $stringFail
];

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
(* $boxOp = <| SuperscriptBox -> "^", SubscriptBox -> "_" |>; *)

(* $boxOperators = Alternatives @@ Keys[ $boxOp ]; *)

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*$showStringCharacters*)
$showStringCharacters = True;

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
showStringCharactersQ[ Cell[ _, s_String, ___ ] ] := showStringCharactersQ @ s;

showStringCharactersQ[ s_String ] :=
    showStringCharactersQ[ s ] =
        UnsameQ[
            CurrentValue @ { StyleDefinitions, s, "ShowStringCharacters" },
            False
        ];

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*fasterCellToString0*)
fasterCellToString0[ a_String /; StringContainsQ[ a, "\!" ] ] :=
    With[ { res = stringToBoxes @ a },
        res /; FreeQ[ res, s_String /; StringContainsQ[ s, "\!" ] ]
    ];

fasterCellToString0[ a_String ] :=
    If[ TrueQ @ $showStringCharacters, a, StringTrim[ a, "\"" ] ];

fasterCellToString0[ { a___String } ] := StringJoin @ a;

fasterCellToString0[ $stringStripHeads[ a_, ___ ] ] := fasterCellToString0 @ a;

fasterCellToString0[ $stringIgnoredHeads[ ___ ] ] := "";

fasterCellToString0[ list_List ] :=
    With[ { strings = fasterCellToString0 /@ list },
        If[ AllTrue[ strings, StringQ ], StringJoin @ strings, strings ]
    ];

fasterCellToString0[ cell: Cell[ a_, ___ ] ] :=
    Block[ { $showStringCharacters = showStringCharactersQ @ cell },
        fasterCellToString0 @ a
    ];

(* fasterCellToString0[ (box: $boxOperators)[ a_, b_ ] ] :=
    Module[ { a$, b$ },
        a$ = fasterCellToString0 @ a;
        b$ = fasterCellToString0 @ b;
        If[ StringQ @ a$ && StringQ @ b$,
            StringJoin[ a$, $boxOp @ box, b$ ],
            { a$, b$ }
        ]
     ]; *)

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
        SuperscriptBox[ a_String, b_String ] :> StringJoin[ a, "^", b ],
        SubscriptBox[ a_String, b_String ] :> StringJoin[ a, "_", b ],
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
slowCellToString[ cell_ ] :=
    Module[ { plain, string },
        plain = Quiet @ FrontEndExecute @ ExportPacket[ cell, "PlainText" ];

        string = Replace[
            plain,
            { { s_String? StringQ, ___ } :> s, ___ :> $Failed }
        ];

        If[ StringQ @ string,
            Replace[
                StringTrim[ string, WhitespaceCharacter ],
                "" -> Missing[ "NotFound" ]
            ],
            string
        ]
    ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*stringToBoxes*)
stringToBoxes[ s_String /; StringMatchQ[ s, "\"" ~~ __ ~~ "\"" ] ] :=
    "\"" <> stringToBoxes @ StringTrim[ s, "\"" ] <> "\"";

stringToBoxes[ s_String ] :=
    UsingFrontEnd @ First @ First @ MathLink`CallFrontEnd @
        FrontEnd`UndocumentedTestFEParserPacket[ s, True ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Footer*)
End[ ];

EndPackage[ ];