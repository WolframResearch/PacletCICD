(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Header*)
BeginPackage[ "Wolfram`PacletCICD`Internal`", { "Wolfram`PacletCICD`" } ];

ClearAll[ SetContextLoad, LoadSubPackages, $SubPackageSymbols ];

Begin[ "`Private`" ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*$SubPackageSymbols*)
$SubPackageSymbols := Association @ Internal`BagPart[ $subPackageSymbols, All ];
$subPackageSymbols = Internal`Bag[ ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*LoadSubPackages*)
LoadSubPackages[ ] := Keys @ KeyValueMap[ Rule, $SubPackageSymbols ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*SetContextLoad*)
SetContextLoad // Attributes = { HoldFirst };
SetContextLoad::context = "Warning: suspicious context in `1`.";

SetContextLoad[ sym_Symbol, name_String ] /; StringFreeQ[ name, "`" ] :=
    SetContextLoad[ sym, "Wolfram`PacletCICD`" <> name <> "`" ];

(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::SuspiciousSessionSymbol:: *)
SetContextLoad[ sym_Symbol, context_String ] :=
    With[ { full = Context @ sym <> SymbolName @ Unevaluated @ sym },
        If[ $Debug && Context @ sym =!= "Wolfram`PacletCICD`",
            Message[ SetContextLoad::context, full ]
        ];
        Internal`StuffBag[ $subPackageSymbols, full :> sym ];
        sym // ClearAll;
        sym := Block[ { $ContextPath },
                   sym // ClearAll;
                   If[ TrueQ @ $Debug,
                       Print[ "Loading: ",     full,
                              "\n\tContext: ", context,
                              "\n\tFile:    ", FindFile @ context
                       ]
                   ];
                   Quiet[ Get @ context, General::shdw ];
                   sym
               ]
    ];
(* :!CodeAnalysis::EndBlock:: *)

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Definitions*)

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*ASTUtilities*)
SetContextLoad[ ASTCondition     , "ASTUtilities" ];
SetContextLoad[ ASTConditionValue, "ASTUtilities" ];
SetContextLoad[ ASTPattern       , "ASTUtilities" ];
SetContextLoad[ ASTPatternTest   , "ASTUtilities" ];
SetContextLoad[ EquivalentNodeQ  , "ASTUtilities" ];
SetContextLoad[ FromAST          , "ASTUtilities" ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Examples*)
SetContextLoad[ $ExamplesLocation    , "Examples" ];
SetContextLoad[ ExampleDirectory     , "Examples" ];
SetContextLoad[ ResetExampleDirectory, "Examples" ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*TestPaclet*)
SetContextLoad[ TestPaclet     , "TestPaclet" ];
SetContextLoad[ AnnotateTestIDs, "TestPaclet" ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Workflows*)
SetContextLoad[ GitHubSecret  , "Workflows" ];
SetContextLoad[ Workflow      , "Workflows" ];
SetContextLoad[ WorkflowExport, "Workflows" ];
SetContextLoad[ WorkflowJob   , "Workflows" ];
SetContextLoad[ WorkflowStep  , "Workflows" ];
SetContextLoad[ WorkflowQ     , "Workflows" ];
SetContextLoad[ WorkflowJobQ  , "Workflows" ];
SetContextLoad[ WorkflowStepQ , "Workflows" ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Other*)
SetContextLoad[ BuildPaclet            , "BuildPaclet"         ];
SetContextLoad[ CheckPaclet            , "CheckPaclet"         ];
SetContextLoad[ CheckDependencies      , "CheckDependencies"   ];
SetContextLoad[ CompileLibraryResources, "Compilation"         ];
SetContextLoad[ DeployPaclet           , "DeployPaclet"        ];
SetContextLoad[ GitHubPacletInstall    , "GitHubPacletInstall" ];
SetContextLoad[ MessageFailure         , "MessageFailure"      ];
SetContextLoad[ SubmitPaclet           , "SubmitPaclet"        ];
SetContextLoad[ FormattingHelper       , "Formatting"          ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Footer*)
End[ ];
EndPackage[ ];