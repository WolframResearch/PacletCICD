(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Header*)
BeginPackage[ "Wolfram`PacletCICD`Internal`", { "Wolfram`PacletCICD`" } ];

$MXFile;

ClearAll[
    BuildMX,
    SetContextLoad,
    LoadSubPackage,
    LoadSubPackages,
    $SubPackageSymbols
];

Begin[ "`Private`" ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*$SubPackageSymbols*)
$SubPackageSymbols := Association @ Internal`BagPart[ $subPackageSymbols, All ];
$subPackageSymbols = Internal`Bag[ ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*LoadSubPackage*)

(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::SuspiciousSessionSymbol:: *)
LoadSubPackage[ name_String, label_ ] /; StringFreeQ[ name, "`" ] :=
    LoadSubPackage[ "Wolfram`PacletCICD`" <> name <> "`", label ];

LoadSubPackage[ ctx_String, label_ ] :=
    Block[ { $ContextPath },
        If[ TrueQ @ $Debug,
            Print[ "Loading: "    , label,
                   "\n\tContext: ", ctx,
                   "\n\tFile:    ", FindFile @ ctx
            ]
        ];
        LoadSubPackage[ ctx, _ ] = Quiet[ Get @ ctx, General::shdw ];
    ];

LoadSubPackage[ ctx_String ] := LoadSubPackage[ ctx, "" ];
(* :!CodeAnalysis::EndBlock:: *)

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

SetContextLoad[ sym_Symbol, context_String ] :=
    With[ { full = Context @ sym <> SymbolName @ Unevaluated @ sym },
        If[ $Debug && Context @ sym =!= "Wolfram`PacletCICD`",
            Message[ SetContextLoad::context, full ]
        ];
        Internal`StuffBag[ $subPackageSymbols, full :> sym ];
        sym // ClearAll;
        sym := (ClearAll @ sym; LoadSubPackage[ context, full ]; sym)
    ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*BuildMX*)
BuildMX[ ] := (
    LoadSubPackages[ ];
    GeneralUtilities`EnsureDirectory @ DirectoryName @ $MXFile;
    DumpSave[ $MXFile, "Wolfram`PacletCICD`", "SymbolAttributes" -> False ];
    $MXFile
);

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
SetContextLoad[ GitHubSecret    , "Workflows" ];
SetContextLoad[ Workflow        , "Workflows" ];
SetContextLoad[ WorkflowEvaluate, "Workflows" ];
SetContextLoad[ WorkflowExport  , "Workflows" ];
SetContextLoad[ WorkflowJob     , "Workflows" ];
SetContextLoad[ WorkflowJobQ    , "Workflows" ];
SetContextLoad[ WorkflowQ       , "Workflows" ];
SetContextLoad[ WorkflowStep    , "Workflows" ];
SetContextLoad[ WorkflowStepQ   , "Workflows" ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Console*)
SetContextLoad[ ConsoleDebug  , "Console" ];
SetContextLoad[ ConsoleError  , "Console" ];
SetContextLoad[ ConsoleLog    , "Console" ];
SetContextLoad[ ConsoleNotice , "Console" ];
SetContextLoad[ ConsoleWarning, "Console" ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*PublisherTokens*)
SetContextLoad[ CreatePublisherToken, "PublisherTokens" ];
SetContextLoad[ DeletePublisherToken, "PublisherTokens" ];
SetContextLoad[ PublisherTokenObject, "PublisherTokens" ];
SetContextLoad[ PublisherTokens     , "PublisherTokens" ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*PacletInformation*)
SetContextLoad[ ReplacePacletInfo, "PacletInformation" ];
SetContextLoad[ SetPacletInfo    , "PacletInformation" ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Units*)
SetContextLoad[ SecondsToQuantity, "Units" ];
SetContextLoad[ BytesToQuantity  , "Units" ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*WorkflowValue*)
SetContextLoad[ $WorkflowValueScope     , "WorkflowValue" ];
SetContextLoad[ InitializeWorkflowValues, "WorkflowValue" ];
SetContextLoad[ WorkflowValue           , "WorkflowValue" ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Other*)
SetContextLoad[ BuildPaclet            , "BuildPaclet"         ];
SetContextLoad[ CheckDependencies      , "CheckDependencies"   ];
SetContextLoad[ CheckPaclet            , "CheckPaclet"         ];
SetContextLoad[ CompileLibraryResources, "Compilation"         ];
SetContextLoad[ DeployPaclet           , "DeployPaclet"        ];
SetContextLoad[ FormatNotebooks        , "FormatNotebooks"     ];
SetContextLoad[ FormattingHelper       , "Formatting"          ];
SetContextLoad[ GitHubPacletInstall    , "GitHubPacletInstall" ];
SetContextLoad[ MessageFailure         , "MessageFailure"      ];
SetContextLoad[ SubmitPaclet           , "SubmitPaclet"        ];
SetContextLoad[ ToMarkdownString       , "Markdown"            ];
SetContextLoad[ ReadableForm           , "ReadableForm"        ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Preloading*)
WorkflowValue /: (wfv: WorkflowValue[ ___ ] = rhs_) /;
    (LoadSubPackage[ "WorkflowValue" ]; True) :=
        wfv = rhs;

WorkflowValue /: (wfv: WorkflowValue[ ___ ] := rhs_) /;
    (LoadSubPackage[ "WorkflowValue" ]; True) :=
        wfv := rhs;

WorkflowValue /: (wfv: WorkflowValue[ ___ ][ ___ ] = rhs_) /;
    (LoadSubPackage[ "WorkflowValue" ]; True) :=
        wfv = rhs;

WorkflowValue /: (wfv: WorkflowValue[ ___ ][ ___ ] := rhs_) /;
    (LoadSubPackage[ "WorkflowValue" ]; True) :=
        wfv := rhs;

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Footer*)
End[ ];
EndPackage[ ];