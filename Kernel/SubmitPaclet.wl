(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Header*)
BeginPackage[ "Wolfram`PacletCICD`" ];

SubmitPaclet // ClearAll;

Begin[ "`Private`" ];

Needs[ "DefinitionNotebookClient`"          -> "dnc`"  ];
Needs[ "PacletResource`DefinitionNotebook`" -> "prdn`" ];
Needs[ "ResourceSystemClient`"              -> "rsc`"  ];

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

SubmitPaclet::PublisherToken =
"Invalid setting for PublisherToken: `1`";

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Options*)
SubmitPaclet // Options = {
    "ExitOnFail"       -> Automatic,
    ConsoleType        -> Automatic,
    PublisherID        -> Automatic,
    PublisherToken     -> Automatic,
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
        { OptionValue @ ConsoleType, "Submit" },
        Block[
            {
                $PublisherID        = toPublisherID @ OptionValue @ PublisherID,
                $ResourceSystemBase = toRSB @ OptionValue @ ResourceSystemBase,
                rsc`$PublisherToken = toPToken @ OptionValue @ PublisherToken
            },
            If[ rsc`$PublisherToken === None,
                ccPromptFix @ submitPaclet[ file, opts ],
                disableCloudConnect @ submitPaclet[ file, opts ]
            ]
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
    withTokenPublisher @ Module[ { nbo },
        nbo = First[ Notebooks @ ExpandFileName @ file, $Failed ];
        If[ MatchQ[ nbo, _NotebookObject ],
            submitPaclet[ nbo, opts ],
            openNotebookAndSubmit[ file, opts ]
        ]
    ];

submitPaclet[ nbo_NotebookObject, opts___ ] := Enclose[
    Module[ { built, submitted },
        LoadSubPackage[ "Wolfram`PacletCICD`BuildPaclet`" ];
        built = buildPaclet[ nbo, opts ];
        submitted = scrapeAndSubmit @ nbo;
        Confirm @ submitted
    ],
    exitFailure[
        "SubmitPacletFailure",
        <|
            "MessageTemplate" -> "Failed to submit paclet.",
            "Result" -> #
        |>
    ] &
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
(* ::Subsection::Closed:: *)
(*toPToken*)
toPToken[ Automatic    ] := rsc`$PublisherToken;
toPToken[ None         ] := None;
toPToken[ str_String   ] := StringToByteArray @ str;
toPToken[ ba_ByteArray ] := ba;
toPToken[ other_       ] := exitFailure[ SubmitPaclet::PublisherToken, other ];
toPToken // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*scrapeAndSubmit*)
scrapeAndSubmit[ nbo_NotebookObject ] :=
    Enclose @ Module[ { dir, pac, ver, ro },
        dir = ConfirmBy[ prdn`ScrapePacletDirectory @ nbo, DirectoryQ ];
        pac = ConfirmBy[ PacletObject @ Flatten @ File @ dir, PacletObjectQ ];
        ver = ConfirmBy[ pac[ "Version" ], StringQ ];

        ro = ConfirmMatch[
            dnc`ScrapeResource[
                "Paclet",
                nbo,
                Interactive -> False,
                "ClickedButton" -> "Submit"
            ],
            _ResourceObject
        ];

        ResourceSubmit[ ro, "Version" -> ver ]
    ];

scrapeAndSubmit // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*withTokenPublisher*)
withTokenPublisher // Attributes = { HoldFirst };

withTokenPublisher[ eval_ ] :=
    withTokenPublisher[ eval, $PublisherID, rsc`$PublisherToken ];

withTokenPublisher[ eval_, _String, _    ] := eval;
withTokenPublisher[ eval_, _      , None ] := eval;

withTokenPublisher[ eval_, _, _String|_ByteArray ] := Enclose[
    Module[ { info, publisher },
        info = ConfirmBy[ getTokenInfo[ ], AssociationQ ];
        publisher = ConfirmBy[ info[ "PublisherID" ], StringQ ];
        Block[ { $PublisherID = publisher }, eval ]
    ],
    eval &
];

withTokenPublisher // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*getTokenInfo*)
getTokenInfo[ ] :=
    rsc`ResourceSystemExecute[
        "CheckPublisherToken",
        { "Request" -> "Information" }
    ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Footer*)
End[ ];

EndPackage[ ];