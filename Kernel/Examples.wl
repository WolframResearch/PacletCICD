(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Header*)
BeginPackage[ "Wolfram`PacletCICD`" ];

ClearAll[ $ExamplesLocation, ExampleDirectory, ResetExampleDirectory ];

Begin[ "`Private`" ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*$ExamplesLocation*)
$ExamplesLocation::pacfail =
"Cannot find the Wolfram/PacletCICD paclet.";

$ExamplesLocation::exdir =
"Cannot find the Wolfram/PacletCICD examples directory.";

$ExamplesLocation :=
    catchTop @ Module[ { dir },
        dir = GeneralUtilities`EnsureDirectory @ {
            $UserBaseDirectory,
            "ApplicationData",
            $thisPacletName,
            "Examples"
        };
        If[ ! DirectoryQ @ dir,
            throwMessageFailure[ $ExamplesLocation::exdir ]
        ];
        Flatten @ File @ dir
    ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*ExampleDirectory*)
ExampleDirectory::exdir =
"Cannot find the Wolfram/PacletCICD examples directory.";

ExampleDirectory::exdnf =
"No example directory with the name \"`1`\" exists.";

ExampleDirectory[ ] := catchTop @ $exampleNames;

ExampleDirectory[ All ] := catchTop[ ExampleDirectory /@ $exampleNames ];

ExampleDirectory[ name_String ] :=
    catchTop @ Module[ { root, dir },
        root = $ExamplesLocation;

        If[ ! DirectoryQ @ root,
            throwMessageFailure @ ExampleDirectory::exdir
        ];

        root = ExpandFileName @ root;
        dir = fileNameJoin @ { root, name };

        If[ DirectoryQ @ dir,
            File @ dir,
            tryFetchExampleData @ name
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
(*$exampleNames*)
$exampleNames :=
    Map[
        FileBaseName,
        FileNames[ "*.wl", $thisPaclet[ "AssetLocation", "Examples" ] ]
    ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*tryFetchExampleData*)
tryFetchExampleData[ name_String ] :=
    Module[ { root, file },
        root = $thisPaclet[ "AssetLocation", "Examples" ];
        If[ ! DirectoryQ @ root,
            throwMessageFailure @ ExampleDirectory::exdir
        ];
        file = fileNameJoin @ { root, name <> ".wl" };
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
        tgt   = fileNameJoin @ { tmp, name<>".zip" };
        zip   = ConfirmBy[ URLDownload[ url, tgt ], FileExistsQ ];
        files = ConfirmBy[ ExtractArchive[ zip, tmp ], AllTrue @ StringQ ];
        top   = First[ SortBy[ files, StringLength ], Confirm @ $Failed ];
        CopyDirectory[ top, fileNameJoin @ { $ExamplesLocation, name } ]
    ],
    throwMessageFailure[ ExampleDirectory::exdnf, name ] &
];

fetchExampleData0 // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*ResetExampleDirectory*)
ResetExampleDirectory[ args___ ] :=
    catchTop @ resetExampleDirectory @ args;

resetExampleDirectory[ name_String ] :=
    Module[ { dir },
        dir = First[ FileNames[ name, $ExamplesLocation ], None ];
        resetExampleDirectory[ name, dir ]
    ];

resetExampleDirectory[ All ] :=
    resetExampleDirectory[ All, $ExamplesLocation ];

resetExampleDirectory[ name_, dir_? DirectoryQ ] :=
    Module[ { deleted },
        deleted = DeleteDirectory[ dir, DeleteContents -> True ];
        If[ DirectoryQ @ deleted,
            throwError[ "Failed to delete directory `1`", dir ],
            Success[
                "ResetExampleDirectory",
                <|
                    "MessageTemplate"   -> "Directory `1` deleted.",
                    "MessageParameters" -> { dir },
                    "Name"              -> name,
                    "Result"            -> deleted
                |>
            ]
        ]
    ];

resetExampleDirectory // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Footer*)
End[ ];

EndPackage[ ];