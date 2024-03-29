(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Header*)
BeginPackage[ "Wolfram`PacletCICD`" ];

FormatNotebooks // ClearAll;

Begin[ "`Private`" ];

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*FormatNotebooks*)
FormatNotebooks[ dir_? DirectoryQ, opts: OptionsPattern[ ] ] :=
    catchTop @ FormatNotebooks[ FileNames[ "*.nb", dir, Infinity ], opts ];

FormatNotebooks[ files_List, opts: OptionsPattern[ ] ] :=
    catchTop @ Map[ FormatNotebooks[ #, opts ] &, files ];

FormatNotebooks[ file_? notebookFileQ, opts: OptionsPattern[ ] ] :=
    catchTop @ makeReadable[ file, { opts } ];

(* ::**************************************************************************************************************:: *)
(* ::Subsection::Closed:: *)
(*notebookFileQ*)
notebookFileQ[ file_? FileExistsQ ] :=
    TrueQ @ Quiet @ Or[
        ToLowerCase @ FileExtension @ file === "nb",
        ToLowerCase @ FileFormat @ file === "nb"
    ];

notebookFileQ[ ___ ] := False;

(* ::**************************************************************************************************************:: *)
(* ::Subsection::Closed:: *)
(*makeReadable*)
makeReadable[ file_, opts: { OptionsPattern[ ] } ] := makeReadable[ file, Hash @ ReadByteArray @ file, opts ];

makeReadable[ file_, hash_Integer, opts_ ] /; skipFormattingQ[ file, hash ] :=
    Missing[ "Skipped", file ];

makeReadable[ file_, hash_Integer, opts: { OptionsPattern[ ] } ] := Enclose[
    Module[ { res },
        res = ConfirmBy[ makeReadable[ file, hash, $overrideFormats, opts ],
                         FileExistsQ
              ];
        saveHash @ file;
        res
    ],
    StringForm[ "Failed to format notebook: `1`", file ]
];

(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::VariableError::Block:: *)
makeReadable[ file_, hash_, HoldComplete[ overrides___ ], { opts: OptionsPattern[ ] } ] :=
    Internal`InheritedBlock[ { RawArray, NumericArray, overrides },
        Unprotect[ RawArray, NumericArray, overrides ];
        ReleaseHold[ overrideFormat /@ HoldComplete @ overrides ];

        Format[ x_RawArray? rawArrayQ, InputForm ] :=
            OutputForm[ "CompressedData[\"" <> Compress @ x <> "\"]" ];

        Format[ x_NumericArray? numericArrayQ, InputForm ] :=
            OutputForm[ "CompressedData[\"" <> Compress @ x <> "\"]" ];

        ResourceFunction[ "SaveReadableNotebook" ][
            ReplaceAll[
                DeleteCases[
                    Import[ file, "NB" ],
                    TaggingRules -> { },
                    Infinity
                ],
                a: _RawArray | _NumericArray :>
                    With[ { b = Check[ a, $Failed ] }, b /; ! FailureQ @ b ]
            ],
            file,
            opts
        ]
    ];

(* ::**************************************************************************************************************:: *)
(* ::Subsection::Closed:: *)
(*$overrideFormats*)
(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::SuspiciousSessionSymbol:: *)
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

(* :!CodeAnalysis::EndBlock:: *)

(* ::**************************************************************************************************************:: *)
(* ::Subsection::Closed:: *)
(*formatHashID*)
formatHashID[ id_ ] :=
    StringRiffle[
        { "Wolfram", "PacletCICD", "FormatNotebooks", "FileHashes", id },
        "/"
    ];

(* ::**************************************************************************************************************:: *)
(* ::Subsection::Closed:: *)
(*skipFormattingQ*)
skipFormattingQ[ file_, hash_Integer ] :=
    skipFormattingQ[ file, hash, Hash[ file, "Expression", "HexString" ] ];

skipFormattingQ[ file_, hash_Integer, id_String ] :=
    PersistentSymbol @ formatHashID @ id === hash;

(* ::**************************************************************************************************************:: *)
(* ::Subsection::Closed:: *)
(*saveHash*)
saveHash[ file_ ] := saveHash[ file, Hash @ ReadByteArray @ file ];

saveHash[ file_, hash_Integer ] :=
    saveHash[ file, hash, Hash[ file, "Expression", "HexString" ] ];

saveHash[ file_, hash_Integer, id_String ] :=
    Set[ PersistentSymbol[ formatHashID @ id ], hash ];

(* ::**************************************************************************************************************:: *)
(* ::Subsection::Closed:: *)
(*rawArrayQ*)
rawArrayQ // Attributes = { HoldFirst };
rawArrayQ[ arr_RawArray ] := Developer`RawArrayQ @ Unevaluated @ arr;

(* ::**************************************************************************************************************:: *)
(* ::Subsection::Closed:: *)
(*numericArrayQ*)
numericArrayQ // Attributes = { HoldFirst };
numericArrayQ[ arr_RawArray ] := NumericArrayQ @ Unevaluated @ arr;

(* ::**************************************************************************************************************:: *)
(* ::Subsection::Closed:: *)
(*overrideFormat*)
overrideFormat // Attributes = { HoldAllComplete };

overrideFormat[ sym_Symbol ] := (
    Unprotect @ sym;
    Format[ x_sym, InputForm ] :=
        OutputForm @ ToString @ Unevaluated @ FullForm @ x
);

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Footer*)
End[ ];

EndPackage[ ];