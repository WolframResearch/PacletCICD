(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Header*)
BeginPackage[ "Wolfram`PacletCICD`" ];

ClearAll[ ReplacePacletInfo, SetPacletInfo ];

Begin[ "`Private`" ];

$ContextAliases[ "cp`" ] = "CodeParser`";

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*ReplacePacletInfo*)
ReplacePacletInfo[ ___ ] := Failure[ "NotImplemented", <| |> ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*SetPacletInfo*)
SetPacletInfo[ ___ ] := Failure[ "NotImplemented", <| |> ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Utilities*)

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*insertPacletInfo*)
insertPacletInfo[ id_, key_ -> val_ ] := Enclose[
    Needs[ "CodeParser`" -> None ];
    With[ { file = ConfirmBy[ pacletInfoFile @ id, FileExistsQ ] },
        insertPacletInfo[
            file,
            cp`CodeParse[ file, "SourceConvention" -> "SourceCharacterIndex" ],
            key -> val
        ]
    ],
    throwError[ "Cannot find PacletInfo file for `1`.", id ] &
];

insertPacletInfo[ file_, ast_, key_ -> val_ ] := Enclose[
    Module[ { pos, string, new },
        pos = FirstCase[
            ast,
            ASTPattern @ HoldPattern[ PacletObject ][ Association[
                ___,
                key -> ASTPattern[ v_, KeyValuePattern[ cp`Source -> src_ ] ],
                ___
            ] ] :> src,
            Throw[ appendPacletInfo[ file, ast, key -> val ], $tag ],
            Infinity
        ];
        string = ReadString @ file;
        new = StringReplacePart[ string, ToString[ val, InputForm ], pos ];
        WithCleanup[
            BinaryWrite[ file, StringReplace[ new, "\r\n" -> "\n" ] ],
            Close @ file
        ]
    ] ~Catch~ $tag
];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*appendPacletInfo*)
appendPacletInfo

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*pacletInfoFile*)
pacletInfoFile[ pac_PacletObject? PacletObjectQ ] := Enclose[
    Module[ { dir, files, full },
        dir   = ConfirmBy[ pac[ "Location" ], DirectoryQ ];
        files = FileNames[ "PacletInfo."~~("wl"|"m"), dir, IgnoreCase -> True ];
        full  = File @* ExpandFileName /@ files;
        Quiet @ Confirm @ SelectFirst[ full, PacletObjectQ @* PacletObject ]
    ],
    throwError[ "Cannot find PacletInfo file for `1`.", pac ] &
];

pacletInfoFile[ file_File ] := Enclose[
    pacletInfoFile @ ConfirmBy[ PacletObject @ Flatten @ file, PacletObjectQ ],
    throwError[ "`1` does not correspond to a valid paclet.", file ] &
];

pacletInfoFile[ spec_ ] := Enclose[
    pacletInfoFile @ ConfirmBy[ PacletObject @ spec, PacletObjectQ ],
    throwError[ "`1` is not a valid paclet specification.", spec ] &
];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Footer*)
End[ ];
EndPackage[ ];