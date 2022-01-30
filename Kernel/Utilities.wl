(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Header*)
BeginPackage[ "Wolfram`PacletCICD`" ];

PacletCICD;
$ExamplesLocation;
ExampleDirectory;

Begin[ "`Private`" ];

Needs[ "DefinitionNotebookClient`"          -> "dnc`"  ];
Needs[ "PacletResource`DefinitionNotebook`" -> "prdn`" ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*PacletCICD*)
(* PacletCICD is just a symbol for attaching general messages *)
PacletCICD::unknown   =
"An internal error occurred.";

PacletCICD::undefined =
"An internal error occurred (encountered unexpected pattern for `1`).";

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Definition Utilities*)

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*catchUndefined*)
catchUndefined // Attributes = { HoldFirst };

catchUndefined[ sym_Symbol ] :=
    catchUndefined [ sym, DownValues ];

catchUndefined[ sym_Symbol, DownValues ] :=
    e: sym[ ___ ] :=
        throwMessageFailure[
            PacletCICD::undefined,
            HoldForm @ sym,
            HoldForm @ e,
            DownValues
        ];

catchUndefined[ sym_Symbol, SubValues ] :=
    e: sym[ ___ ][ ___ ] :=
        throwMessageFailure[
            PacletCICD::undefined,
            HoldForm @ sym,
            HoldForm @ e,
            SubValues
        ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*$ExamplesLocation*)
$ExamplesLocation::pacfail =
"Cannot find the Wolfram/PacletCICD paclet.";

$ExamplesLocation::exdir =
"Cannot find the Wolfram/PacletCICD examples directory.";

$ExamplesLocation :=
    catchTop @ Module[ { pac, dir },
        pac = PacletObject[ "Wolfram/PacletCICD" ];
        If[ ! PacletObjectQ @ pac,
            throwMessageFailure[ $ExamplesLocation::pacfail ]
        ];
        dir = pac[ "AssetLocation", "Examples" ];
        If[ ! DirectoryQ @ dir,
            throwMessageFailure[ $ExamplesLocation::exdir ]
        ];
        File @ dir
    ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*ExampleDirectory*)
ExampleDirectory::exdir =
"Cannot find the Wolfram/PacletCICD examples directory.";

ExampleDirectory::exdnf =
"No example directory with the name \"`1`\" exists.";

ExampleDirectory[ name_String ] :=
    catchTop @ Module[ { root, dir },
        root = $ExamplesLocation;

        If[ ! DirectoryQ @ root,
            throwMessageFailure @ ExampleDirectory::exdir
        ];

        root = ExpandFileName @ root;
        dir = FileNameJoin @ { root, name };

        If[ DirectoryQ @ dir,
            File @ dir,
            tryFetchExampleData[ root, name ]
        ];

        File @ dir
    ];

ExampleDirectory[ other_ ] :=
    throwMessageFailure[
        ExampleDirectory::string,
        1,
        HoldForm @ ExampleDirectory @ other
    ];

ExampleDirectory[ args___ ] :=
    throwMessageFailure[
        ExampleDirectory::argx,
        ExampleDirectory,
        Length @ HoldComplete @ args
    ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*tryFetchExampleData*)
tryFetchExampleData[ root_String, name_String ] :=
    Module[ { file },
        file = FileNameJoin @ { root, name <> ".wl" };
        If[ FileExistsQ @ file,
            fetchExampleData[ file, name ],
            throwMessageFailure[ ExampleDirectory::exdnf, name ]
        ]
    ];

tryFetchExampleData // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*fetchExampleData*)
fetchExampleData[ file_, name_ ] :=
    Module[ { tmp },
        WithCleanup[
            tmp = CreateDirectory[ ],
            fetchExampleData0[ tmp, file, name ],
            DeleteDirectory[ tmp, DeleteContents -> True ]
        ]
    ];

fetchExampleData // catchUndefined;

fetchExampleData0[ tmp_, file_, name_ ] := Enclose[
    Module[ { data, url, tgt, zip, files, top },
        data  = ConfirmBy[ Get @ file, AssociationQ ];
        url   = ConfirmBy[ Lookup[ data, "URL" ], StringQ ];
        tgt   = FileNameJoin @ { tmp, name<>".zip" };
        zip   = ConfirmBy[ URLDownload[ url, tgt ], FileExistsQ ];
        files = ConfirmBy[ ExtractArchive[ zip, tmp ], AllTrue @ StringQ ];
        top   = First[ SortBy[ files, StringLength ], Confirm @ $Failed ];
        CopyDirectory[ top, FileNameJoin @ { DirectoryName @ file, name } ]
    ],
    throwMessageFailure[ ExampleDirectory::exdnf, name ] &
];

fetchExampleData0 // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Error Handling*)

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*catchTop*)
catchTop // Attributes = { HoldFirst };

catchTop[ eval_ ] :=
    Block[ { $catching = True, $MessageList = { }, catchTop = # & },
        Catch[ eval, $top ]
    ];

catchTop // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*catch*)
catch // Attributes = { HoldFirst };

catch[ eval_ ] :=
    Block[ { $catching = True, $MessageList = { } },
        Catch[ eval, $top ]
    ];

catch // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*catchQuiet*)
catchQuiet // Attributes = { HoldFirst };
catchQuiet[ eval_ ] := Quiet @ catch @ eval;
catchQuiet // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*throwFailure*)
throwFailure // Attributes = { HoldFirst };
throwFailure[ args___ ] := Quiet @ throwMessageFailure @ args;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*throwMessageFailure*)
throwMessageFailure // Attributes = { HoldFirst };

throwMessageFailure[ msg_MessageName, args___ ] :=
    Module[ { failure },
        failure = messageFailure[ msg, args ];
        If[ TrueQ @ $catching,
            Throw[ failure, $top ],
            failure
        ]
    ];

throwMessageFailure[ msg_String ] :=
    throwMessageFailure[ PacletCICD::error, msg ];

throwMessageFailure[ args___ ] :=
    throwMessageFailure[
        PacletCICD::unknown,
        HoldForm @ throwMessageFailure @ args
    ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*messageFailure*)
messageFailure // Attributes = { HoldFirst };

messageFailure[ msg: MessageName[ sym_Symbol, mtag__ ], args___ ] :=
    Module[ { tag, info, failure },
        tag = StringRiffle[ { SymbolName @ Unevaluated @ sym, mtag }, "::" ];
        info = <| "MessageTemplate" :> msg, "MessageParameters" :> { args } |>;
        failure = Failure[ tag, info ];
        If[ $MessageList === { },
            Message @ Evaluate @ failure;
            dnc`ConsolePrint[ failure[ "Message" ], "Level" -> "Error" ];
            dnc`ConsolePrint[ SequenceForm[ "Stack trace: ", Stack[ ] ] ];
        ];

        failure
    ];

messageFailure // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*generalMessage*)
$$messageType = "error"|"warning"|"notice"|"debug";

PacletCICD::error   = "`1`";
PacletCICD::warning = "`1`";
PacletCICD::notice  = "`1`";
PacletCICD::debug   = "`1`";

generalMessage[ tag: $$messageType, template_, as_Association ] :=
    messageFailure[
        MessageName[ PacletCICD, tag ],
        TemplateApply[ template, as ],
        as
    ];

generalMessage[ tag: $$messageType, template_, a___ ] :=
    messageFailure[
        MessageName[ PacletCICD, tag ],
        TemplateApply[ template, { a } ],
        a
    ];

generalMessage[ template_, a___ ] := generalMessage[ "notice", template, a ];

generalMessage // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*throwGeneralMessage*)
throwGeneralMessage[ tag: $$messageType, template_, a___ ] :=
    Module[ { failure },
        failure = generalMessage[ tag, template, a ];
        If[ TrueQ @ $catching,
            Throw[ failure, $top ],
            failure
        ]
    ];

throwGeneralMessage[ template_, a___ ] :=
    throwGeneralMessage[ "error", template, a ];

throwGeneralMessage // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*throwError*)
throwError[ template_, a___ ] := throwGeneralMessage[ "error", template, a ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*exitFailure*)
exitFailure // Attributes = { HoldFirst };

(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::SuspiciousSessionSymbol:: *)
exitFailure[ msg_MessageName, code_, result_ ] :=
    If[ $EvaluationEnvironment === "Script"
        ,
        Block[ { DefinitionNotebookClient`$ConsoleType = Automatic },
            DefinitionNotebookClient`ConsolePrint[
                ToString @ Unevaluated @ msg <> ": " <> ToString @ msg,
                "Level" -> "Error"
            ]
        ];
        Print @ result;
        Exit @ code
        ,
        throwMessageFailure[ msg, result ]
    ];

exitFailure // catchUndefined;

(* :!CodeAnalysis::EndBlock:: *)

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Definition Notebook Utilities *)

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
        FileNameJoin @ { ExpandFileName @ dir, "DefinitionNotebook.nb" }
    ];

findDefinitionNotebook[ dir_, file_? defNBQ ] :=
    Flatten @ File @ file;

findDefinitionNotebook[ dir_, _ ] :=
    SelectFirst[ FileNames[ "*.nb", dir ], defNBQ ];

findDefinitionNotebook // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*General File Utilities*)

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
