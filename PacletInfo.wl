PacletObject[ <|
    "Name"           -> "Wolfram/PacletCICD",
    "Description"    -> "CI/CD utilities for Wolfram Language Paclets",
    "Creator"        -> "Richard Hennigan <richardh@wolfram.com>",
    "Version"        -> "1.0.0",
    "WolframVersion" -> "13+",
    "PublisherID"    -> "Wolfram",
    "Extensions"     -> {
        {
            "Kernel",
            "Root"    -> "Kernel",
            "Context" -> { "Wolfram`PacletCICD`", "Wolfram`PacletCICDLoader`" },
            "Symbols" -> {
                "Wolfram`PacletCICD`BuildPaclet",
                "Wolfram`PacletCICD`CheckPaclet",
                "Wolfram`PacletCICD`DeployPaclet",
                "Wolfram`PacletCICD`SubmitPaclet",
                "Wolfram`PacletCICD`TestPaclet"
            }
        },
        { "Documentation", "Language" -> "English" },
        {
            "Asset",
            "Assets" -> {
                { "License", "./LICENSE"   },
                { "ReadMe" , "./README.md" },
                { "Images" , "./Images"    }
            }
        }
    }
|> ]