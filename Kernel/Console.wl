(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Header*)
BeginPackage[ "Wolfram`PacletCICD`" ];

ClearAll[ ConsoleError, ConsoleLog, ConsoleNotice, ConsoleWarning ];

Begin[ "`Private`" ];

$ContextAliases[ "dnc`" ] = "DefinitionNotebookClient`";
$ContextAliases[ "dcc`" ] = "DefinitionNotebookClient`Console`PackagePrivate`";
$ContextAliases[ "ci`"  ] = "CodeInspector`";

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*ConsoleLog*)
ConsoleLog // Options = {
    "ConsoleType" -> Automatic,
    "IndentSize"  -> Automatic,
    "Level"       -> None,
    "Output"      -> Automatic
};

ConsoleLog[ expr_, opts: OptionsPattern[ ] ] :=
    catchTop @ ConsoleLog[ Unevaluated @ expr, None, opts ];

ConsoleLog[ e_, file_, opts: OptionsPattern[ ] ] :=
    catchTop @ ConsoleLog[ Unevaluated @ e, file, None, opts ];

ConsoleLog[ e_, f_, line_Integer, opts: OptionsPattern[ ] ] :=
    catchTop @ ConsoleLog[ Unevaluated @ e, f, { line, line }, opts ];

ConsoleLog[ e_, f_, { l1_Integer, l2_Integer }, opts: OptionsPattern[ ] ] :=
    catchTop @ ConsoleLog[
        Unevaluated @ e,
        f,
        { { l1, 1 }, { l2, -1 } },
        opts
    ];

ConsoleLog[ ins_ci`InspectionObject, opts: OptionsPattern[ ] ] :=
    catchTop @ Enclose @ Module[ { msg0, more, msg, level },
        msg   = ConfirmBy[ ins[ "Description" ], StringQ ];
        more  = ConfirmMatch[ ins[ "AdditionalDescriptions" ], { ___String } ];
        msg   = StringRiffle[ Flatten @ { msg, more }, " " ];
        level = ConfirmBy[ ins[ "Severity" ], StringQ ];

        ConsoleLog[
            StringReplace[ msg, "``" -> "`" ],
            ins,
            opts,
            "Level" -> level
        ]
    ];

ConsoleLog[ expr_, ins_ci`InspectionObject, opts: OptionsPattern[ ] ] :=
    catchTop @ ConsoleLog[ Unevaluated @ expr, ins, None, opts ];

ConsoleLog[ expr_, ins_ci`InspectionObject, None, opts: OptionsPattern[ ] ] :=
    catchTop @ Module[ { type, level, indent, output },

        type   = OptionValue[ "ConsoleType" ];
        level  = OptionValue[ "Level" ];
        indent = consoleIndentSize @ OptionValue[ "IndentSize" ];
        output = consoleOutputRule @ OptionValue[ "Output" ];

        withConsoleSettings[
            { level, type },
            dnc`ConsolePrint[
                Defer @ expr,
                output,
                "IndentSize"        -> indent,
                "Level"             -> level,
                "SourceInformation" -> ins
            ]
        ]
    ];

ConsoleLog[ expr_, file_, pos_, opts: OptionsPattern[ ] ] :=
    catchTop @ Module[ { type, level, indent, output },

        type   = OptionValue[ "ConsoleType" ];
        level  = OptionValue[ "Level" ];
        indent = consoleIndentSize @ OptionValue[ "IndentSize" ];
        output = consoleOutputRule @ OptionValue[ "Output" ];

        withConsoleSettings[
            { level, type },
            dnc`ConsolePrint[
                Defer @ expr,
                output,
                "IndentSize"        -> indent,
                "Level"             -> level,
                "SourceInformation" -> <| "File" -> file, "Position" -> pos |>
            ]
        ]
    ];

ConsoleLog // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*consoleIndentSize*)
consoleIndentSize[ Automatic ] := 2;
consoleIndentSize[ other_ ] := other;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*consoleOutputRule*)
consoleOutputRule[ Automatic ] :=
    OptionValue[
        dnc`ConsolePrint,
        { },
        "Output",
        Function[ v, "Output" :> v, HoldFirst ]
    ];

consoleOutputRule[ other_ ] := "Output" -> other;

consoleOutputRule // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*withConsoleSettings*)
withConsoleSettings // Attributes = { HoldRest };

withConsoleSettings[ { level_, type_ }, eval_ ] :=
    withConsoleType[
        type,
        withConsoleSettings0[ { level, dnc`$ConsoleType }, eval ]
    ];

withConsoleSettings // catchUndefined;


withConsoleSettings0 // Attributes = { HoldRest };

withConsoleSettings0[ { "Notice", "Notebook" }, eval_ ] :=
    Internal`InheritedBlock[ { dcc`nbLevel },
        dcc`nbLevel[ "Notice" ] := dcc`nbLevel[ "Suggestion", "Notice" ];
        eval
    ];

withConsoleSettings0[ { "Notice", "TTY" }, eval_ ] :=
    Internal`InheritedBlock[ { dcc`ttyLevel },
        dcc`ttyLevel[ "Notice" ] := dcc`ttyLevel[ "Suggestion", "Notice" ];
        eval
    ];

withConsoleSettings0[ _, eval_ ] := eval;

withConsoleSettings0 // Attributes = { HoldRest };

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*ConsoleNotice*)
ConsoleNotice // Options = { "ConsoleType" -> Automatic };

ConsoleNotice[ expr_, opts: OptionsPattern[ ] ] :=
    catchTop @ ConsoleNotice[ Unevaluated @ expr, None, opts ];

ConsoleNotice[ expr_, file_, opts: OptionsPattern[ ] ] :=
    catchTop @ ConsoleNotice[ Unevaluated @ expr, file, None, opts ];

ConsoleNotice[ expr_, file_, pos_, opts: OptionsPattern[ ] ] :=
    catchTop @ ConsoleLog[
        Unevaluated @ expr,
        file,
        pos,
        "ConsoleType" -> OptionValue[ "ConsoleType" ],
        "Level"       -> "Notice"
    ];

ConsoleNotice // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*ConsoleWarning*)
ConsoleWarning // Options = { "ConsoleType" -> Automatic };

ConsoleWarning[ expr_, opts: OptionsPattern[ ] ] :=
    catchTop @ ConsoleWarning[ Unevaluated @ expr, None, opts ];

ConsoleWarning[ expr_, file_, opts: OptionsPattern[ ] ] :=
    catchTop @ ConsoleWarning[ Unevaluated @ expr, file, None, opts ];

ConsoleWarning[ expr_, file_, pos_, opts: OptionsPattern[ ] ] :=
    catchTop @ ConsoleLog[
        Unevaluated @ expr,
        file,
        pos,
        "ConsoleType" -> OptionValue[ "ConsoleType" ],
        "Level"       -> "Warning"
    ];

ConsoleWarning // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*ConsoleError*)
ConsoleError::fatal = "Encountered a fatal error.";

ConsoleError // Options = {
    "ConsoleType" -> Automatic,
    "Fatal"       -> False
};

ConsoleError[ expr_, opts: OptionsPattern[ ] ] :=
    catchTop @ ConsoleError[ Unevaluated @ expr, None, opts ];

ConsoleError[ expr_, file_, opts: OptionsPattern[ ] ] :=
    catchTop @ ConsoleError[ Unevaluated @ expr, file, None, opts ];

ConsoleError[ expr_, file_, pos_, opts: OptionsPattern[ ] ] :=
    catchTop @ Module[ { res },

        res = ConsoleLog[
            Unevaluated @ expr,
            file,
            pos,
            "ConsoleType" -> OptionValue[ "ConsoleType" ],
            "Level"       -> "Error"
        ];

        If[ TrueQ @ OptionValue[ "Fatal" ],
            exitFailure[ ConsoleError::fatal, 1, res ],
            res
        ]
    ];

ConsoleError // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Footer*)
End[ ];

EndPackage[ ];