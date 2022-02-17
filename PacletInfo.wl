PacletObject[ <|
    "Name"             -> "Wolfram/PacletCICD",
    "Description"      -> "Continuous integration and deployment for Wolfram Language Paclets",
    "Creator"          -> "Richard Hennigan <richardh@wolfram.com>",
    "URL"              -> "https://resources.wolframcloud.com/PacletRepository/resources/Wolfram/PacletCICD",
    "SourceControlURL" -> "https://github.com/rhennigan/PacletCICD",
    "License"          -> "MIT",
    "PublisherID"      -> "Wolfram",
    "Version"          -> "0.6.0",
    "WolframVersion"   -> "13.0+",
    "Icon"             -> "Images/PacletIcon.png",
    "Dependencies"     -> {
        "DefinitionNotebookClient" -> ">=1.7.0",
        "PacletResource"           -> ">=0.9.0"
    },
    "Extensions"       -> {
        {
            "Kernel",
            "Root"    -> "Kernel",
            "Context" -> { "Wolfram`PacletCICD`" },
            "Symbols" -> {
                "Wolfram`PacletCICD`$ExamplesLocation",
                "Wolfram`PacletCICD`AnnotateTestIDs",
                "Wolfram`PacletCICD`ASTCondition",
                "Wolfram`PacletCICD`ASTConditionValue",
                "Wolfram`PacletCICD`ASTPattern",
                "Wolfram`PacletCICD`ASTPatternTest",
                "Wolfram`PacletCICD`BuildPaclet",
                "Wolfram`PacletCICD`CheckDependencies",
                "Wolfram`PacletCICD`CheckPaclet",
                "Wolfram`PacletCICD`CompileLibraryResources",
                "Wolfram`PacletCICD`DeployPaclet",
                "Wolfram`PacletCICD`EquivalentNodeQ",
                "Wolfram`PacletCICD`ExampleDirectory",
                "Wolfram`PacletCICD`FromAST",
                "Wolfram`PacletCICD`GitHubPacletInstall",
                "Wolfram`PacletCICD`GitHubSecret",
                "Wolfram`PacletCICD`MessageFailure",
                "Wolfram`PacletCICD`PacletCICD",
                "Wolfram`PacletCICD`SubmitPaclet",
                "Wolfram`PacletCICD`TestPaclet",
                "Wolfram`PacletCICD`Workflow",
                "Wolfram`PacletCICD`WorkflowExport",
                "Wolfram`PacletCICD`WorkflowJob",
                "Wolfram`PacletCICD`WorkflowStep"
            }
        },
        {
            "Documentation",
            "Root"     -> "Documentation",
            "Language" -> "English"
        },
        {
            "Asset",
            "Assets" -> {
                { "License"  , "./LICENSE"   },
                { "ReadMe"   , "./README.md" },
                { "Images"   , "./Images"    },
                { "Examples" , "./Examples"  }
            }
        }
    }
|> ]