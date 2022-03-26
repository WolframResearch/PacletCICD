(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Header*)
BeginPackage[ "Wolfram`PacletCICD`" ];

ClearAll[ $ExamplesLocation, ExampleDirectory, ResetExampleDirectory ];

Begin[ "`Private`" ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*$ExamplesLocation*)
$ExamplesLocation::PacletNotFound =
"Cannot find the Wolfram/PacletCICD paclet.";

$ExamplesLocation::NoExampleDirectory =
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
            throwMessageFailure[ $ExamplesLocation::NoExampleDirectory ]
        ];
        Flatten @ File @ dir
    ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*ExampleDirectory*)
ExampleDirectory::NoExampleDirectory =
"Cannot find the Wolfram/PacletCICD examples directory.";

ExampleDirectory::NoExampleDirectories =
"No example directory information found in `1`.";

ExampleDirectory::ExampleDirectoryNotFound =
"No example directory with the name \"`1`\" exists.";

ExampleDirectory::BadExampleDirectoryData =
"Failed to get example directory data for \"`1`\".";

ExampleDirectory::BadExampleDirectoryFile =
"Failed to get example directory data for \"`1`\".";

ExampleDirectory::ExampleDirectoryDownload =
"Failed to download repository for example directory \"`1`\".";

ExampleDirectory::DirectoryExists =
"The directory `1` already exists and does not contain the expected files.";

ExampleDirectory::FileExists =
"The file `1` already exists and is not a valid example directory.";

ExampleDirectory::InternalError =
"An unexpected internal error occurred.";

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Main definition*)
ExampleDirectory[ ] := catchTop @ $exampleNames;
ExampleDirectory[ All ] := catch @* ExampleDirectory /@ $exampleNames;
ExampleDirectory[ name_String ] := catchTop @ getExampleDirectory @ name;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Error cases*)
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
(*getExampleDirectory*)
getExampleDirectory[ name_String ] := Enclose[
    Module[ { data, dir },
        data = ConfirmBy[ getExampleDirectoryData @ name, AssociationQ ];
        dir  = ConfirmMatch[ getExampleDirectoryPath @ data, _File ];

        If[ exampleDirectoryQ @ dir,
            dir,
            If[ DirectoryQ @ dir,
                DeleteDirectory[ dir, DeleteContents -> True ]
            ];
            If[ DirectoryQ @ dir,
                throwMessageFailure[ ExampleDirectory::DirectoryExists, dir ]
            ];
            If[ FileExistsQ @ dir,
                throwMessageFailure[ ExampleDirectory::FileExists, dir ]
            ];
            tryFetchExampleData[ data, dir ]
        ]
    ],
    throwMessageFailure[ ExampleDirectory::InternalError, # ] &
];

getExampleDirectory // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*getExampleDirectoryData*)
getExampleDirectoryData[ name0_String ] := Enclose[
    Module[ { fail, check, infoRoot, file, data, name, ver },
        fail = throwMessageFailure[ MessageName[ ExampleDirectory, # ], ##2 ] &;

        infoRoot = $exampleInfoDirectory;
        If[ ! DirectoryQ @ infoRoot, fail[ "NoExampleDirectory" ] ];

        file = fileNameJoin @ { infoRoot, name0 <> ".wl" };
        If[ ! FileExistsQ @ file, fail[ "ExampleDirectoryNotFound", name0 ] ];

        data = getExampleInfoFile @ file;
        name = data[ "Name" ];
        ver  = data[ "Version" ];

        If[ Or[ name =!= name0,
                ! StringQ @ ver,
                ! StringMatchQ[ ver, (DigitCharacter|".").. ]
            ],
            fail[ "BadExampleDirectoryData", name ]
        ];

        data
    ],
    throwMessageFailure[ ExampleDirectory::InternalError, # ] &
];

getExampleDirectoryData // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*getExampleDirectoryPath*)
getExampleDirectoryPath[ name_String ] :=
    getExampleDirectoryPath @ getExampleDirectoryData @ name;

getExampleDirectoryPath[ data: KeyValuePattern @ {
    "Name"    -> name_String,
    "Version" -> ver_String
} ] := Enclose[
    Module[ { root, dir },
        root = ConfirmBy[ ExpandFileName @ $ExamplesLocation, StringQ ];
        dir  = fileNameJoin @ { root, name, StringReplace[ ver, "." -> "-" ] };

        ConfirmBy[ GeneralUtilities`EnsureDirectory @ DirectoryName @ dir,
                   DirectoryQ
        ];

        File @ ConfirmBy[ ExpandFileName @ dir, StringQ ]
    ],
    throwMessageFailure[ ExampleDirectory::InternalError, # ] &
];

getExampleDirectoryPath // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*$exampleInfoDirectory*)
$exampleInfoDirectory := Enclose[
    $exampleInfoDirectory = ConfirmBy[
        $thisPaclet[ "AssetLocation", "Examples" ],
        DirectoryQ
    ]
];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*exampleDirectoryQ*)
exampleDirectoryQ[ dir_? DirectoryQ ] := TrueQ[
    And @@ KeyValueMap[ #2 @ fileNameJoin @ { dir, #1 } &,
                        $expectedExampleDirectoryFiles
           ]
];

exampleDirectoryQ[ ___ ] := False;


$expectedExampleDirectoryFiles = <|
    "Documentation"         -> nonEmptyDirectoryQ,
    "Kernel"                -> nonEmptyDirectoryQ,
    "LICENSE"               -> FileExistsQ,
    "PacletInfo.wl"         -> pacletInfoFileQ,
    "README.md"             -> FileExistsQ,
    "ResourceDefinition.nb" -> defNBQ
|>;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*$exampleNames*)
$exampleNames :=
    Module[ { fail, root, files, data, names },
        fail = throwMessageFailure[ MessageName[ ExampleDirectory, # ], ##2 ] &;
        root = $exampleInfoDirectory;
        If[ ! DirectoryQ @ root, fail[ "NoExampleDirectory" ] ];
        files = FileNames[ "*.wl", root ];
        If[ files === { }, fail[ "NoExampleDirectories", root ] ];
        data  = Map[ catch @ getExampleInfoFile @ # &, files ];
        names = Cases[ data, KeyValuePattern[ "Name" -> name_String ] :> name ];
        If[ names === { }, fail[ "NoExampleDirectories", root ] ];
        Union @ names
    ];

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*$$exampleInfo*)
$$exampleInfo = KeyValuePattern @ {
    "Name"    -> _String? StringQ,
    "Version" -> _String? StringQ,
    "Commit"  -> _String? StringQ,
    "URL"     -> _String? StringQ
};

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*getExampleInfoFile*)
getExampleInfoFile[ file_? FileExistsQ ] :=
    getExampleInfoFile[ file, Hash @ ReadByteArray @ file ];

getExampleInfoFile[ file_? FileExistsQ, hash_ ] :=
    Module[ { info },
        info = Get @ file;
        If[ MatchQ[ info, $$exampleInfo ],
            getExampleInfoFile[ file, hash ] = info,
            throwMessageFailure[
                ExampleDirectory::BadExampleDirectoryFile,
                file
            ]
        ]
    ];

getExampleInfoFile // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*tryFetchExampleData*)
tryFetchExampleData[ info_, target_ ] :=
    Module[ { tmp },
        WithCleanup[
            tmp = CreateDirectory[ ],
            tryFetchExampleData[ tmp, info, target ],
            DeleteDirectory[ tmp, DeleteContents -> True ]
        ]
    ];

tryFetchExampleData[
    tmp_,
    KeyValuePattern @ {
        "Name"    -> name_String,
        "Version" -> ver_String,
        "URL"     -> url_String
    },
    target_
] := Enclose[
    Module[ { tgt, zip, files, top, copied },
        tgt    = fileNameJoin @ { tmp, name<>".zip" };
        zip    = ConfirmBy[ URLDownload[ url, tgt ], FileExistsQ ];
        files  = ConfirmBy[ ExtractArchive[ zip, tmp ], AllTrue @ StringQ ];
        top    = First[ SortBy[ files, StringLength ], Confirm @ $Failed ];
        copied = ConfirmBy[ CopyDirectory[ top, target ], exampleDirectoryQ ];
        Flatten @ File @ ExpandFileName @ copied
    ],
    throwMessageFailure[ ExampleDirectory::ExampleDirectoryDownload, name, # ] &
];

tryFetchExampleData // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*ResetExampleDirectory*)
ResetExampleDirectory::NotExampleDirectory =
"Invalid example directory: `1`.";

ResetExampleDirectory::FailedToRemoveFiles =
"Unable to remove some files in `1`.";

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Main definition*)
ResetExampleDirectory[ args___ ] :=
    catchTop @ resetExampleDirectory @ args;

resetExampleDirectory[ name_String ] :=
    Module[ { dir, success, deleted },
        dir = getExampleDirectoryPath @ name;

        success = Function @
            Success[
                "ResetExampleDirectory",
                <|
                    "MessageTemplate"   -> #1,
                    "MessageParameters" -> { dir },
                    "Name"              -> name,
                    ##2
                |>
            ];

        If[ ! DirectoryQ @ dir,
            If[ FileExistsQ @ dir,
                throwMessageFailure[
                    ResetExampleDirectory::NotExampleDirectory,
                    dir
                ],
                Throw[
                    success[ "Directory `1` does not exist." ],
                    $tag
                ]
            ]
        ];

        deleted = DeleteDirectory[ dir, DeleteContents -> True ];
        If[ DirectoryQ @ dir,
            throwMessageFailure[
                ResetExampleDirectory::FailedToRemoveFiles,
                dir
            ],
            success[ "Directory `1` deleted.", "Result" -> deleted ]
        ]
    ] ~Catch~ $tag;

resetExampleDirectory[ All ] :=
    catch @* resetExampleDirectory /@ $exampleNames;

resetExampleDirectory // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Footer*)
End[ ];

EndPackage[ ];