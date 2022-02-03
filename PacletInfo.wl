PacletObject[ <|
    "Name"             -> "Wolfram/PacletCICD",
    "Description"      -> "CI/CD utilities for Wolfram Language Paclets",
    "Creator"          -> "Richard Hennigan <richardh@wolfram.com>",
    "URL"              -> "https://resources.wolframcloud.com/PacletRepository/resources/Wolfram/PacletCICD",
    "SourceControlURL" -> "https://github.com/rhennigan/PacletCICD",
    "License"          -> "MIT",
    "PublisherID"      -> "Wolfram",
    "Version"          -> "0.3.0",
    "WolframVersion"   -> "13.0+",
    "Icon"             -> "Images/PacletIcon.png",
    "Dependencies"     -> { "Wolfram/GitLink" },
    "Extensions"       -> {
        {
            "Kernel",
            "Root"    -> "Kernel",
            "Context" -> { "Wolfram`PacletCICD`" },
            "Symbols" -> {
                "Wolfram`PacletCICD`$ExamplesLocation",
                "Wolfram`PacletCICD`BuildPaclet",
                "Wolfram`PacletCICD`CheckPaclet",
                "Wolfram`PacletCICD`CreateWorkflow",
                "Wolfram`PacletCICD`DeployPaclet",
                "Wolfram`PacletCICD`DeployReleaseHandler",
                "Wolfram`PacletCICD`ExampleDirectory",
                "Wolfram`PacletCICD`GitHubPacletInstall",
                "Wolfram`PacletCICD`GitHubSecret",
                "Wolfram`PacletCICD`PacletCICD",
                "Wolfram`PacletCICD`SubmitPaclet",
                "Wolfram`PacletCICD`TestPaclet"
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