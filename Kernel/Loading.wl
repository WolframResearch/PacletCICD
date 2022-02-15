(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Header*)
BeginPackage[ "Wolfram`PacletCICD`" ];

setContextLoad;

Begin[ "`Private`" ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Setup*)
setContextLoad // Attributes = { HoldFirst };

setContextLoad[ sym_Symbol, name_String ] /; StringFreeQ[ name, "`" ] :=
    setContextLoad[ sym, "Wolfram`PacletCICD`" <> name <> "`" ];

setContextLoad[ sym_Symbol, context_String ] :=
    sym := Block[ { $ContextPath },
        sym // ClearAll;
        Quiet[ Get @ context, General::shdw ];
        sym
    ];

End[ ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Definitions*)

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*ASTUtilities*)
setContextLoad[ ASTCondition       , "ASTUtilities" ];
setContextLoad[ ASTConditionValue  , "ASTUtilities" ];
setContextLoad[ ASTPattern         , "ASTUtilities" ];
setContextLoad[ ASTPatternTest     , "ASTUtilities" ];
setContextLoad[ EquivalentNodeQ    , "ASTUtilities" ];
setContextLoad[ FromAST            , "ASTUtilities" ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*TestPaclet*)
setContextLoad[ TestPaclet     , "TestPaclet" ];
setContextLoad[ AnnotateTestIDs, "TestPaclet" ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Workflows*)
setContextLoad[ WorkflowExport, "Workflows" ];
setContextLoad[ GitHubSecret  , "Workflows" ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Other*)
setContextLoad[ CompileLibraryResources, "Compilation"         ];
setContextLoad[ DeployPaclet           , "DeployPaclet"        ];
setContextLoad[ GitHubPacletInstall    , "GitHubPacletInstall" ];
setContextLoad[ MessageFailure         , "MessageFailure"      ];
setContextLoad[ SubmitPaclet           , "SubmitPaclet"        ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Footer*)

Remove @ setContextLoad;

EndPackage[ ];