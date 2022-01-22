#!/usr/bin/env wolframscript
(* ::Package:: *)

(*
    To configure this script to run automatically before each commit, run the
    following in the repository directory:
    ```
    git config --local core.hooksPath Scripts/.githooks
    ```
*)

(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::SuspiciousSessionSymbol:: *)

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Defaults*)
$defaultRoot   = DirectoryName[ $InputFileName, 2 ];
$defaultIgnore = FileNameJoin @ { $defaultRoot, "build" };

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Get arguments*)
$root   := $root   = makeRootString @ getArg[ "root"  , $defaultRoot   ];
$ignore := $ignore = makeIgnorePatt @ getArg[ "ignore", $defaultIgnore ];
$strict := $strict = Interpreter[ "Boolean" ][ getArg[ "strict", "True" ] ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Definitions*)

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*getArg*)
getArg[ name_String, default_: None ] :=
    SelectFirst[ Flatten @ StringCases[
                     $ScriptCommandLine,
                     "--" <> name <> "=" ~~ root___ ~~ EndOfString :> root
                 ],
                 StringQ,
                 default
    ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*enclose*)
enclose // Attributes = { HoldAll };
enclose[ eval_           ] := enclose[ eval, "An unspecified error occured." ];
enclose[ eval_, msg_     ] := enclose[ eval, msg, 1 ];
enclose[ eval_, msg_, n_ ] := Enclose[
    eval,
    (Print @ ToString @ msg; If[ $strict, Exit @ n ]) &
];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*makeRootString*)
makeRootString[ str_String ] := enclose[
    ConfirmBy[ ExpandFileName @ StringTrim[ str, "\"" ], DirectoryQ ],
    StringForm[ "`1` is not a valid directory", str ]
];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*makeIgnorePatt*)
makeIgnorePatt[ "None"|"none" ] := None;

makeIgnorePatt[ str_String ] :=
    Alternatives @@ (StringTrim[ #1, "\"" ] & /@ StringSplit[ str, "," ]);

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*getNotebookFiles*)
getNotebookFiles[             ] := getNotebookFiles @ $root;
getNotebookFiles[ root_       ] := getNotebookFiles[ root, $ignore ];
getNotebookFiles[ root_, None ] := FileNames[ "*.nb", root, Infinity ];

getNotebookFiles[ root_, ignore_ ] :=
    getNotebookFiles[ root, ignore, getNotebookArgs[ ] ];

getNotebookFiles[ root_, ignore_, files: { ___String? FileExistsQ } ] := files;

getNotebookFiles[ root_, ignore_, _ ] :=
    Select[ FileNames[ "*.nb", root, Infinity ],
            Not @* StringMatchQ[ ignore~~___ ]
    ];

getNotebookArgs[ ] := getNotebookArgs @ getArg[ "notebooks", None ];

getNotebookArgs[ nbs_String ] := getNotebookArgs @ StringSplit[ nbs, "\n" ];

getNotebookArgs[ { files___String? FileExistsQ } ] := { files };

getNotebookArgs[ ___ ] := Missing[ "NotFound" ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*print*)
print[ ind_Integer, msg__ ] :=
    Print[ StringJoin @ ConstantArray[ " ", ind ],
           DateString @ { "DateTimeShort", ".", "Millisecond" },
           ": ",
           msg
    ];

print[ msg__ ] := print[ 0, msg ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*makeUnreadable*)
makeUnreadable[ file_ ] := enclose[
    ConfirmBy[ Export[ file, Import[ file, "NB" ] ], FileExistsQ ],
    StringForm[ "Failed to unformat notebook: `1`", file ]
];

(******************************************************************************)
(* ::Subsection::Closed:: *)
(*makeReadable*)
makeReadable[ file_ ] := makeReadable[ file, Hash @ ReadByteArray @ file ];

makeReadable[ file_, hash_Integer ] /; skipFormattingQ[ file, hash ] :=
    print[ "Skipping: ", StringDelete[ file, StartOfString~~$root ] ];

makeReadable[ file_, hash_Integer ] := enclose[
    Module[ { res },
        print[ "Formatting: ", StringDelete[ file, StartOfString~~$root ] ];
        res = ConfirmBy[ makeReadable[ file, hash, $overrideFormats ],
                         FileExistsQ
              ];
        saveHash @ file;
        res
    ],
    StringForm[ "Failed to format notebook: `1`", file ]
];

(* :!CodeAnalysis::Disable::VariableError::Block:: *)
makeReadable[ file_, hash_, HoldComplete[ overrides___ ] ] :=
    Internal`InheritedBlock[ { RawArray, NumericArray, overrides },
        Unprotect[ RawArray, NumericArray, overrides ];
        ReleaseHold[ overrideFormat /@ HoldComplete @ overrides ];

        Format[ x_RawArray? rawArrayQ, InputForm ] :=
            OutputForm[ "CompressedData[\"" <> Compress @ x <> "\"]" ];


        Format[ x_NumericArray? numericArrayQ, InputForm ] :=
            OutputForm[ "CompressedData[\"" <> Compress @ x <> "\"]" ];

        ResourceFunction[ "SaveReadableNotebook" ][
            ReplaceAll[
                Import @ file,
                a: _RawArray | _NumericArray :>
                    With[ { b = Check[ a, $Failed ] }, b /; ! FailureQ @ b ]
            ],
            file,
            "ExcludedNotebookOptions" -> { WindowMargins, WindowSize }
        ]
    ];

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*skipFormattingQ*)
skipFormattingQ[ file_, hash_Integer ] :=
    skipFormattingQ[ file, hash, Hash[ file, "Expression", "HexString" ] ];

skipFormattingQ[ file_, hash_Integer, id_String ] :=
    (PersistentSymbol[ "FormatNotebooks/FileHashes/" <> id ] === hash);

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*saveHash*)
saveHash[ file_ ] := saveHash[ file, Hash @ ReadByteArray @ file ];

saveHash[ file_, hash_Integer ] :=
    saveHash[ file, hash, Hash[ file, "Expression", "HexString" ] ];

saveHash[ file_, hash_Integer, id_String ] :=
    (PersistentSymbol[ "FormatNotebooks/FileHashes/" <> id ] = hash);

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*rawArrayQ*)
rawArrayQ // Attributes = { HoldFirst };
rawArrayQ[ arr_RawArray ] := Developer`RawArrayQ @ Unevaluated @ arr;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*numericArrayQ*)
numericArrayQ // Attributes = { HoldFirst };
numericArrayQ[ arr_RawArray ] := NumericArrayQ @ Unevaluated @ arr;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*overrideFormat*)
overrideFormat // Attributes = { HoldAllComplete };

overrideFormat[ sym_Symbol ] := (
    Unprotect @ sym;
    Format[ x_sym, InputForm ] :=
        OutputForm @ ToString @ Unevaluated @ FullForm @ x
);

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*$overrideFormats*)
$overrideFormats = HoldComplete[
    Alternatives,
    Apply,
    Composition,
    Decrement,
    Increment,
    Map,
    MessageName,
    Not,
    Out,
    Part,
    Pattern,
    PatternTest,
    PreDecrement,
    PreIncrement,
    RightComposition
];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Run*)

If[ MemberQ[ $ScriptCommandLine, "--unformat" ],
    makeUnreadable /@ getNotebookFiles[ ],
    makeReadable /@ getNotebookFiles[ ]
]


(* :!CodeAnalysis::EndBlock:: *)