PacletObject[ <|
    "Name"           -> "Wolfram/PacletCICD",
    "Description"    -> "CI/CD utilities for Wolfram Language Paclets",
    "Creator"        -> "Richard Hennigan <richardh@wolfram.com>",
    "Version"        -> "0.1.4",
    "WolframVersion" -> "13+",
    "PublisherID"    -> "Wolfram",
    "Extensions"     -> {
        {
            "Kernel",
            "Root"    -> "Kernel",
            "Context" -> { "Wolfram`PacletCICD`" },
            "Symbols" -> {
                "Wolfram`PacletCICD`$ExamplesLocation",
                "Wolfram`PacletCICD`BuildPaclet",
                "Wolfram`PacletCICD`CheckPaclet",
                "Wolfram`PacletCICD`DeployPaclet",
                "Wolfram`PacletCICD`ExampleDirectory",
                "Wolfram`PacletCICD`GitHubPacletInstall",
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