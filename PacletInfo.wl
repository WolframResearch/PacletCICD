PacletObject[ <|
    "Name"             -> "Wolfram/PacletCICD",
    "Description"      -> "Continuous integration and deployment for Wolfram Language Paclets",
    "Creator"          -> "Richard Hennigan <richardh@wolfram.com>",
    "URL"              -> "https://resources.wolframcloud.com/PacletRepository/resources/Wolfram/PacletCICD",
    "SourceControlURL" -> "https://github.com/rhennigan/PacletCICD",
    "License"          -> "MIT",
    "PublisherID"      -> "Wolfram",
    "Version"          -> "0.10.2",
    "WolframVersion"   -> "13.0+",
    "Icon"             -> "Images/PacletIcon.png",
    "Dependencies"     -> {
        "DefinitionNotebookClient" -> ">=1.6.0",
        "PacletResource"           -> ">=0.8.0"
    },
    "Extensions"       -> {
        {
            "Kernel",
            "Root"    -> "Kernel",
            "Context" -> { "Wolfram`PacletCICD`" },
            "Symbols" -> {
                "Wolfram`PacletCICD`$ExamplesLocation",
                "Wolfram`PacletCICD`AnnotateTestIDs",
                "Wolfram`PacletCICD`BuildPaclet",
                "Wolfram`PacletCICD`CheckDependencies",
                "Wolfram`PacletCICD`CheckPaclet",
                "Wolfram`PacletCICD`DeployPaclet",
                "Wolfram`PacletCICD`ExampleDirectory",
                "Wolfram`PacletCICD`GitHubPacletInstall",
                "Wolfram`PacletCICD`GitHubSecret",
                "Wolfram`PacletCICD`PacletCICD",
                "Wolfram`PacletCICD`SubmitPaclet",
                "Wolfram`PacletCICD`TestPaclet",
                "Wolfram`PacletCICD`Workflow",
                "Wolfram`PacletCICD`WorkflowEvaluate",
                "Wolfram`PacletCICD`WorkflowExport",
                "Wolfram`PacletCICD`WorkflowJob",
                "Wolfram`PacletCICD`WorkflowJobQ",
                "Wolfram`PacletCICD`WorkflowQ",
                "Wolfram`PacletCICD`WorkflowStep",
                "Wolfram`PacletCICD`WorkflowStepQ",
                "Wolfram`PacletCICD`WorkflowValue"
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
                { "Examples" , "./Examples"  },
                { "Resources", "./Resources" }
            }
        }
    }
|> ]