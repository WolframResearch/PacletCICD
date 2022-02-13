(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Header*)
BeginPackage[ "Wolfram`PacletCICD`" ];

CompileLibraryResources // ClearAll;

Begin[ "`Private`" ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*CompileLibraryResources*)
CompileLibraryResources // Options = {
    "SourceDirectory" -> Automatic,
    "BuildDirectory"  -> Automatic,
    "LibraryName"     -> Automatic,
    "EntryPoint"      -> None (* TODO *)
};

$$paclet = _PacletObject? PacletObjectQ;

CompileLibraryResources[ pac: $$paclet, opts: OptionsPattern[ ] ] :=
    catchTop @ Module[ { dir },
        dir = pac[ "Location" ];
        compileLibraryResources[
            pac,
            toSourceDirectory[ OptionValue[ "SourceDirectory" ], dir ],
            toBuildDirectory[  OptionValue[ "BuildDirectory"  ], dir ],
            toLibraryName[     OptionValue[ "LibraryName"     ], pac ]
        ]
    ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*toSourceDirectory*)
toSourceDirectory[ dir_? DirectoryQ, _ ] := dir;

toSourceDirectory[ Automatic, pacDir_ ] :=
    SelectFirst[ Select[ FileNames[ All, pacDir ], DirectoryQ ],
                 FileNames[ "*.c", # ] =!= { } &
    ];

toSourceDirectory // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*toBuildDirectory*)
toBuildDirectory[ dir_? DirectoryQ, _ ] := dir;

toBuildDirectory[ dir_File, _ ] := dir;

toBuildDirectory[ Automatic, pacDir_ ] := (* TODO: check extension root *)
    FileNameJoin @ { pacDir, "LibraryResources", $SystemID };

toBuildDirectory // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*toLibraryName*)
toLibraryName[ name_String, _ ] := name;

toLibraryName[ Automatic, pac: $$paclet ] :=
    StringDelete[ pac[ "Name" ], StartOfString~~___~~"/" ];

toLibraryName // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*compileLibraryResources*)
compileLibraryResources[ pac_, src_, tgt_, name_ ] :=
    Module[ { files },
        Needs[ "CCompilerDriver`" -> None ];
        files = FileNames[ "*.c", src ];
        CCompilerDriver`CreateLibrary[
            files,
            name,
            "TargetDirectory" -> tgt,
            "CleanIntermediate" -> True
        ]
    ];

compileLibraryResources // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Footer*)
End[ ];
EndPackage[ ];