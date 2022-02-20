(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Header*)
BeginPackage[ "Wolfram`PacletCICD`" ];

Begin[ "`Private`" ];

$ContextAliases[ "dnc`" ] = "DefinitionNotebookClient`";

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Definition Notebook Utilities *)

$simpleTextMode := MatchQ[ dnc`$ConsoleType, "TTY"|"GitHub" ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*withDNCSettings*)
withDNCSettings // Attributes = { HoldRest };

withDNCSettings[ { type_, tgt_ }, eval_ ] := (
    Needs[ "DefinitionNotebookClient`" -> None ];
    Internal`InheritedBlock[ { dnc`$ConsoleType, dnc`$ClickedButton },
        dnc`$ConsoleType = type;
        dnc`$ClickedButton = tgt;
        eval
    ]
);

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*withConsoleType*)
withConsoleType // Attributes = { HoldRest };
withConsoleType[ type_, eval_ ] := (
    Needs[ "DefinitionNotebookClient`" -> None ];
    Internal`InheritedBlock[ { dnc`$ConsoleType },
        dnc`$ConsoleType = type;
        eval
    ]
);

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*autoConsole*)
autoConsole // Attributes = { HoldFirst };
autoConsole[ eval_ ] := withConsoleType[ Automatic, eval ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*defNBQ*)
defNBQ[ file_? FileExistsQ ] := TrueQ @ Quiet @ defNBQ0 @ file;
defNBQ[ ___ ] := False;

defNBQ0[ file_? FileExistsQ ] := defNBQ0[ Hash @ ReadByteArray @ file, file ];
defNBQ0[ h_, file_ ] := defNBQ0[ h, file ] = defNBQ0 @ Import[ file, "NB" ];
defNBQ0[ Notebook[ ___, TaggingRules -> tags_, ___ ] ] := defNBQ0 @ tags;
defNBQ0[ KeyValuePattern[ "ResourceType" -> "Paclet" ] ] := True;
defNBQ0[ ___ ] := False;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*findDefinitionNotebook*)
findDefinitionNotebook[ dir_? DirectoryQ ] :=
    findDefinitionNotebook[
        dir,
        fileNameJoin @ { ExpandFileName @ dir, "DefinitionNotebook.nb" }
    ];

findDefinitionNotebook[ pac_PacletObject ] :=
    findDefinitionNotebook @ pac[ "Location" ];

findDefinitionNotebook[ dir_, file_? defNBQ ] :=
    Flatten @ File @ file;

findDefinitionNotebook[ dir_, _ ] :=
    SelectFirst[ File /@ FileNames[ "*.nb", dir ], defNBQ ];

findDefinitionNotebook // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*General File Utilities*)

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*parentPacletDirectory*)
parentPacletDirectory[ file_ ] := Enclose[
    Module[ { expanded, dir, parent },
        expanded = ConfirmBy[ ExpandFileName @ file, StringQ ];
        parent = parentPacletDirectory0 @ expanded;
        ConfirmMatch[ parent, None | _?DirectoryQ ]
    ],
    throwError[
        "Cannot determine parent paclet directory of `1`.",
        file, ##
    ] &
];

parentPacletDirectory0[ file_ ] :=
    Quiet[ SelectFirst[ FixedPointList[ DirectoryName, file, 50 ],
                        pacletDirectoryQ,
                        None
           ],
           PacletManager`CreatePaclet::badarg
    ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*pacletDirectoryQ*)
pacletDirectoryQ[ "" ] := False;

pacletDirectoryQ[ dir_? DirectoryQ ] :=
    PacletObjectQ @ PacletObject @ Flatten @ File @ dir;

pacletDirectoryQ[ ___ ] := False;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*ensureDirectory*)
ensureDirectory[ dir_ ] := GeneralUtilities`EnsureDirectory @ dir;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*fileNameJoin*)
fileNameJoin[ list_List ] :=
    With[ { joined = fileNameJoin0 @ list },
        If[ StringQ @ joined,
            StringReplace[ joined, "\\" -> "/" ],
            $Failed
        ]
    ];

fileNameJoin0[ list: { ___, _File, ___ } ] :=
  fileNameJoin0 @ Replace[ list, File[ file_ ] :> file, { 1 } ];

fileNameJoin0[ list_ ] := FileNameJoin @ list;

fileNameJoin // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*relativePath*)
relativePath[ path1_ ][ path2_ ] := relativePath[ path1, path2 ];

relativePath[ path1_, path2_ ] := Enclose[
    Module[
        {
            full1, full2, split1, split2, first1, first2,
            prefix, common, drop1, drop2, dots, rel
        },

        full1  = ConfirmBy[ expandFileName @ path1, StringQ ];
        full2  = ConfirmBy[ expandFileName @ path2, StringQ ];
        split1 = ConfirmBy[ FileNameSplit @ full1, ListQ ];
        split2 = ConfirmBy[ FileNameSplit @ full2, ListQ ];
        first1 = First[ split1, "" ];
        first2 = First[ split2, "" ];

        If[ $OperatingSystem === "Windows" && first1 =!= first2,
            Throw[ full2, $tag ]
        ];

        prefix = ConfirmBy[ longestCommonPrefix[ split1, split2 ], ListQ ];
        common = Length @ prefix;
        drop1  = Drop[ split1, common ];
        drop2  = Drop[ split2, common ];
        dots   = Join[ ConstantArray[ "..", Length @ drop1 ], drop2 ];
        rel    = If[ drop1 === { }, Prepend[ dots, "." ], dots ];

        ConfirmBy[ fileNameJoin @ rel, StringQ ]

    ] ~Catch~ $tag
    ,
    throwError[
        "Cannot determine relative path for `1` and `2`.",
        path1, path2, ##
    ] &
];

e: relativePath[ PatternSequence[ ] | PatternSequence[ _, _, __ ] ] :=
    throwMessageFailure[
        PacletCICD::undefined,
        HoldForm @ relativePath,
        HoldForm @ e,
        DownValues
    ];

relativePath ~catchUndefined~ SubValues;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*expandFileName*)
expandFileName[ file_String /; StringStartsQ[ file, "file://" ] ] := Enclose[
    Module[ { path },
        LocalObject;
        path = ConfirmBy[ LocalObjects`URIToPath @ file, StringQ ];
        ConfirmAssert[ ! StringStartsQ[ path, "file://" ] ];
        expandFileName @ path
    ],
    throwError[ "Cannot expand file `1`.", file, ## ] &
];

expandFileName[ obj: HoldPattern[ _CloudObject ] ] := expandCloudObject @ obj;

expandFileName[ url_URL ] := expandURL @ url;

expandFileName[ file_ ] :=
    With[ { expanded = ExpandFileName @ file },
        If[ StringQ @ expanded,
            expanded,
            throwError[
                "Cannot expand file `file`.",
                <| "file" -> file, "expanded" -> expanded |>
            ]
        ]
    ];

expandFileName // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*expandCloudObject*)
$$uuidCOPath = KeyValuePattern[ "Path" -> { "", _, _? uuidQ } ];
$$objCOPath  = KeyValuePattern[ "Path" -> { "", "obj", _, __ } ];

expandCloudObject[ obj: HoldPattern[ _CloudObject ] ]:=
    expandCloudObject[ obj, URLParse @ obj ];

expandCloudObject[ obj_, $$objCOPath ] := obj;

expandCloudObject[ obj_, $$uuidCOPath ] :=
    Module[ { expanded },
        expanded = CloudObject[ obj, CloudObjectNameFormat -> "UserURLBase" ];
        If[ expandedCloudObjectQ @ expanded,
            expanded,
            throwError[ "Cannot expand cloud object `1`.", obj ]
        ]
    ];

expandCloudObject // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsubsubsection::Closed:: *)
(*expandedCloudObjectQ*)
expandedCloudObjectQ[
    obj: HoldPattern @ CloudObject[ _? StringQ, OptionsPattern[ ] ]
] := MatchQ[ URLParse @ obj, $$objCOPath ];

expandedCloudObjectQ[ ___ ] := False;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*expandURL*)
expandURL[ obj: HoldPattern[ _CloudObject|_URL ] ] :=
    Module[ { url },
        url = URLBuild @ URLParse @ obj;
        If[ ! StringQ @ url,
            throwError[ "Cannot expand URL `1`.", obj ],
            url
        ]
    ];

expandURL // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Misc Programming Utilities*)

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*symbolQ*)
symbolQ // Attributes = { HoldAllComplete };

symbolQ[ s_Symbol ] :=
    TrueQ @ And[
        AtomQ @ Unevaluated @ s,
        ! Internal`RemovedSymbolQ @ Unevaluated @ s,
        Unevaluated @ s =!= Internal`$EFAIL
    ];

symbolQ[ ___ ] := False;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*versionOrder*)
versionOrder[ v1_, v2_ ] :=
    Module[ { l1, l2, len, p1, p2 },

        l1  = toVersionList @ v1;
        l2  = toVersionList @ v2;
        len = Max[ Length @ l1, Length @ l2 ];
        p1  = PadRight[ l1, len, 0 ];
        p2  = PadRight[ l2, len, 0 ];

        Which[ p1 === p2            ,  0,
               OrderedQ @ { p1, p2 },  1,
               OrderedQ @ { p2, p1 }, -1,
               True                 ,  0
        ]
    ];

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*toVersionList*)
toVersionList[ v_ ] :=
    StringCases[
        ToString @ N @ v,
        {
            d: Longest[ DigitCharacter..  ] :> ToExpression @ d,
            c: Longest[ LetterCharacter.. ] :> c
        }
    ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*uuidQ*)
uuidQ[ str_String? StringQ ] := StringMatchQ[ str, $uuidRegex ];
uuidQ[ ___ ] := False;

$uuidRegex := $uuidRegex =
    Module[ { hex, blocks, string },
        hex = "[a-fA-F0-9]{" <> ToString[ #1 ] <> "}" &;
        blocks = hex /@ { 8, 4, 4, 4, 12 };
        string = StringRiffle[ blocks, "-" ];
        RegularExpression @ string
    ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*filterOptions*)
filterOptions[ sym_Symbol, opts: (Rule|RuleDelayed)[ _, _ ]... ] :=
    Sequence @@ FilterRules[ Flatten @ { opts }, Options @ sym ];

filterOptions[ { syms___Symbol }, opts: (Rule|RuleDelayed)[ _, _ ]... ] :=
    Apply[
        Sequence,
        FilterRules[
            Flatten @ { opts },
            Flatten @ Cases[ HoldComplete @ syms, s_ :> Options @ s ]
        ]
    ];

filterOptions[
    Verbatim[ OptionsPattern ][ syms: { ___ } ],
    opts: (Rule|RuleDelayed)[ _, _ ]...
] :=
    filterOptions[ syms, opts ];

filterOptions /:
    (sym_Symbol)[
        args___,
        filterOptions[ opts1: (Rule|RuleDelayed)[ _, _ ]... ],
        opts2: (Rule|RuleDelayed)[ _, _ ]...
    ] :=
    With[ { filtered = filterOptions[ sym, opts1 ] },
        sym[ args, filtered, opts2 ]
    ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*longestCommonPrefix*)
longestCommonPrefix[ list_List ] :=
    list;

longestCommonPrefix[ lists__List ] :=
    With[ { prefix = longestCommonPrefix0[ { } ][ { lists } ] },
        If[ ListQ @ prefix,
            prefix,
            throwError[
                "Cannot determine common prefix for lists `1`.",
                { lists },
                prefix
            ]
        ]
    ];

longestCommonPrefix[ strings__String ] :=
    Module[ { chars, string },
        chars  = Characters @ { strings };
        string = StringJoin[ longestCommonPrefix @@ chars ];
        If[ StringQ @ string,
            string,
            throwError[
                "Cannot determine common prefix for strings `strings`.",
                { strings },
                <| "chars" -> chars, "string" -> string |>
            ]
        ]
    ];

longestCommonPrefix // catchUndefined;


longestCommonPrefix0[ { xs___ } ][ { y: { a_, ___ }, ys: { a_, ___ }.. } ] :=
    longestCommonPrefix0[ { xs, a } ][ Rest /@ { y, ys } ];

longestCommonPrefix0[ xs_List ][ ___ ] := xs;

longestCommonPrefix0 ~catchUndefined~ UpValues;

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Footer*)
End[ ];
EndPackage[ ];
