(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Header*)
BeginPackage[ "Wolfram`PacletCICD`" ];

SubmitPaclet // ClearAll;

Begin[ "`Private`" ];

Needs[ "DefinitionNotebookClient`"          -> "dnc`"  ];
Needs[ "PacletResource`DefinitionNotebook`" -> "prdn`" ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*SubmitPaclet*)
SubmitPaclet::invfile =
"`1` is not a valid definition notebook file or directory.";

SubmitPaclet::errors =
"Errors encountered while checking paclet.";

SubmitPaclet::undefined =
"Unhandled arguments for `1` in `2`.";

SubmitPaclet::archive =
"Could not find built paclet archive";

SubmitPaclet::PublisherID =
"Invalid setting for PublisherID: `1`";

SubmitPaclet::ResourceSystemBase =
"Invalid setting for ResourceSystemBase: `1`";

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Options*)
SubmitPaclet // Options = {
    "ConsoleType"      -> Automatic,
    "ExitOnFail"       -> Automatic,
    PublisherID        -> Automatic,
    ResourceSystemBase -> Automatic
};

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Argument patterns*)
$$spOpts = OptionsPattern[ SubmitPaclet ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Main definition*)
SubmitPaclet[ opts: $$spOpts ] :=
    catchTop @ SubmitPaclet[ File @ Directory[ ], opts ];

SubmitPaclet[ dir_File? DirectoryQ, opts: $$spOpts ] :=
    catchTop @ SubmitPaclet[ findDefinitionNotebook @ dir, opts ];

SubmitPaclet[ file_File? defNBQ, opts: $$spOpts ] :=
    catchTop @ UsingFrontEnd @ withDNCSettings[
        { OptionValue[ "ConsoleType" ], "Submit" },
        Block[
            {
                $PublisherID        = toPublisherID @ OptionValue @ PublisherID,
                $ResourceSystemBase = toRSB @ OptionValue @ ResourceSystemBase
            },
            ccPromptFix @ submitPaclet[ file, opts ]
        ]
    ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Error cases*)

(* Invalid file specification: *)
e: SubmitPaclet[ file: Except[ _File? defNBQ ], ___ ] :=
    throwMessageFailure[ SubmitPaclet::invfile, file, HoldForm @ e ];

(* Invalid options specification: *)
e: SubmitPaclet[
    file_File? defNBQ,
    a: OptionsPattern[ ],
    inv: Except[ OptionsPattern[ ] ],
    ___
] :=
    throwMessageFailure[
        SubmitPaclet::nonopt,
        HoldForm @ inv,
        1 + Length @ HoldComplete @ a,
        HoldForm @ e
    ];

(* Unexpected arguments: *)
e: SubmitPaclet[ ___ ] :=
    throwMessageFailure[ SubmitPaclet::undefined, SubmitPaclet, HoldForm @ e ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Dependencies*)

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*submitPaclet*)
submitPaclet[ file_File, opts___ ] :=
    Module[ { cOpts, nbo },
        cOpts = filterOptions[ $$cpOpts, "Target" -> "Submit", opts ];
        CheckPaclet[ file, cOpts ];
        nbo = First[ Notebooks @ ExpandFileName @ file, $Failed ];
        If[ MatchQ[ nbo, _NotebookObject ],
            submitPaclet[ nbo, opts ],
            openNotebookAndSubmit[ file, opts ]
        ]
    ];

submitPaclet[ nbo_NotebookObject, opts___ ] :=
    Enclose @ Module[ { built, submitted },
        LoadSubPackage[ "Wolfram`PacletCICD`BuildPaclet`" ];
        built = buildPaclet[ nbo, opts ];
        Print[ "built: ", built ];
        submitted = dnc`SubmitRepository[ "Paclet", nbo ];
        Print[ "submitted: ", submitted ];
        submitted
    ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*openNotebookAndSubmit*)
openNotebookAndSubmit[ file_, opts___ ] :=
    Module[ { nbo },
        WithCleanup[
            dnc`BeginConsoleGroup[ "SubmitPaclet" ];
            nbo = dnc`OpenTemporaryNotebook @ file,
            submitPaclet[ nbo, opts ],
            dnc`CloseTemporaryNotebook @ nbo;
            dnc`EndConsoleGroup[ "SubmitPaclet" ];
        ]
    ];

openNotebookAndSubmit // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*toPublisherID*)
toPublisherID[ pub_String ] := pub;
toPublisherID[ Automatic  ] := $PublisherID;
toPublisherID[ other_     ] := exitFailure[ SubmitPaclet::PublisherID, other ];
toPublisherID // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*toRSB*)
toRSB[ rsb_String ] := rsb;
toRSB[ Automatic  ] := $ResourceSystemBase;
toRSB[ other_     ] := exitFailure[ SubmitPaclet::ResourceSystemBase, other ];
toRSB // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Footer*)
End[ ];

EndPackage[ ];