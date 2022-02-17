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
        Quiet[ Get @ Echo @ context, General::shdw ];
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
setContextLoad[ GitHubSecret  , "Workflows" ];
setContextLoad[ Workflow      , "Workflows" ];
setContextLoad[ WorkflowExport, "Workflows" ];
setContextLoad[ WorkflowJob   , "Workflows" ];
setContextLoad[ WorkflowStep  , "Workflows" ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Other*)
setContextLoad[ CheckDependencies      , "CheckDependencies"   ];
setContextLoad[ CompileLibraryResources, "Compilation"         ];
setContextLoad[ DeployPaclet           , "DeployPaclet"        ];
setContextLoad[ GitHubPacletInstall    , "GitHubPacletInstall" ];
setContextLoad[ MessageFailure         , "MessageFailure"      ];
setContextLoad[ SubmitPaclet           , "SubmitPaclet"        ];
setContextLoad[ FormattingHelper       , "Formatting"          ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Footer*)

Remove @ setContextLoad;

EndPackage[ ];