(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Header*)
BeginPackage[ "Wolfram`PacletCICD`" ];

ClearAll[
    ScriptConfirm,
    ScriptConfirmAssert,
    ScriptConfirmBy,
    ScriptConfirmMatch
];

Begin[ "`Private`" ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*ScriptConfirm*)
ScriptConfirm // Attributes = { HoldAll, SequenceHold };

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Messages*)
ScriptConfirm::Session =
"ScriptConfirm encountered a failure in an evaluation environment other than \
\"Script\". Aborting instead of quitting with exit code `1`.";

ScriptConfirm::ArgumentCount =
"ScriptConfirm called with `1` arguments; between 1 and 3 arguments are \
expected.";

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Main Definition*)

(* Default message template: *)
ScriptConfirm[ expr_ ] :=
    ScriptConfirm[ expr, $scriptConfirmTemplate ];

(* Default exit code: *)
ScriptConfirm[ expr_, template_ ] :=
    ScriptConfirm[ expr, template, 1 ];

(* Automatic message template: *)
ScriptConfirm[ expr_, Automatic, code_ ] :=
    ScriptConfirm[ expr, $scriptConfirmTemplate, code ];

ScriptConfirm[ expr_, template_, code_ ] :=
    catchTop @ With[ { result = expr },
        If[ successfulQ @ result,
            result,
            scriptFail @ <|
                "ConfirmationType" -> ScriptConfirm,
                "ExitCode"         -> code,
                "Expression"       -> HoldForm @ result,
                "Input"            -> HoldForm @ expr,
                "MessageTemplate"  -> template
            |>
        ]
    ];

(* Invalid arguments: *)
ScriptConfirm[ args___ ] :=
    catchTop @ With[ { len = Length @ HoldComplete @ args },
        messageFailure[ ScriptConfirm::ArgumentCount, len ]
    ];

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*$scriptConfirmTemplate*)
$scriptConfirmTemplate = "ScriptConfirm: `Expression` encountered.";

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*ScriptConfirmAssert*)
ScriptConfirmAssert // Attributes = { HoldAll, SequenceHold };

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Messages*)
ScriptConfirmAssert::Session =
"ScriptConfirmAssert encountered an assertion failure in an evaluation \
environment other than \"Script\". Aborting instead of quitting with exit \
code `1`.";

ScriptConfirmAssert::ArgumentCount =
"ScriptConfirmAssert called with `1` arguments; between 1 and 3 arguments are \
expected.";

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Main Definition*)

(* Default message template: *)
ScriptConfirmAssert[ expr_ ] :=
    ScriptConfirmAssert[ expr, $scriptConfirmAssertTemplate ];

(* Default exit code: *)
ScriptConfirmAssert[ expr_, template_ ] :=
    ScriptConfirmAssert[ expr, template, 1 ];

(* Automatic message template: *)
ScriptConfirmAssert[ expr_, Automatic, code_ ] :=
    ScriptConfirmAssert[ expr, $scriptConfirmAssertTemplate, code ];

ScriptConfirmAssert[ expr_, template_, code_ ] :=
    catchTop @ With[ { result = expr },
        If[ TrueQ @ result,
            Null,
            scriptFail @ <|
                "ConfirmationType" -> ScriptConfirmAssert,
                "ExitCode"         -> code,
                "Expression"       -> HoldForm @ result,
                "Input"            -> HoldForm @ expr,
                "MessageTemplate"  -> template
            |>
        ]
    ];

(* Invalid arguments: *)
ScriptConfirmAssert[ args___ ] :=
    catchTop @ With[ { len = Length @ HoldComplete @ args },
        messageFailure[ ScriptConfirmAssert::ArgumentCount, len ]
    ];

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*$scriptConfirmAssertTemplate*)
$scriptConfirmAssertTemplate = "ScriptConfirmAssert: Assertion `Input` failed.";

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*ScriptConfirmBy*)
ScriptConfirmBy // Attributes = { HoldAll, SequenceHold };

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Messages*)
ScriptConfirmBy::Session =
"ScriptConfirmBy encountered a failure in an evaluation environment other than \
\"Script\". Aborting instead of quitting with exit code `1`.";

ScriptConfirmBy::ArgumentCount =
"ScriptConfirmBy called with `1` arguments; between 2 and 4 arguments are \
expected.";

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Main Definition*)

(* Default message template: *)
ScriptConfirmBy[ expr_, f_ ] :=
    ScriptConfirmBy[ expr, f, $scriptConfirmByTemplate ];

(* Default exit code: *)
ScriptConfirmBy[ expr_, f_, template_ ] :=
    ScriptConfirmBy[ expr, f, template, 1 ];

(* Automatic message template: *)
ScriptConfirmBy[ expr_, f_, Automatic, code_ ] :=
    ScriptConfirmBy[ expr, f, $scriptConfirmByTemplate, code ];

ScriptConfirmBy[ expr_, f_, template_, code_ ] :=
    catchTop @ With[ { result = expr },
        If[ TrueQ @ f @ result,
            result,
            scriptFail @ <|
                "ConfirmationType" -> ScriptConfirmBy,
                "ExitCode"         -> code,
                "Expression"       -> HoldForm @ result,
                "Function"         -> HoldForm @ f,
                "Input"            -> HoldForm @ expr,
                "MessageTemplate"  -> template
            |>
        ]
    ];

(* Invalid arguments: *)
ScriptConfirmBy[ args___ ] :=
    catchTop @ With[ { len = Length @ HoldComplete @ args },
        messageFailure[ ScriptConfirmBy::ArgumentCount, len ]
    ];

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*$scriptConfirmByTemplate*)
$scriptConfirmByTemplate =
    "ScriptConfirmBy: `Function`[`Expression`] did not return True.";

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*ScriptConfirmMatch*)
ScriptConfirmMatch // Attributes = { HoldAll, SequenceHold };

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Messages*)
ScriptConfirmMatch::Session =
"ScriptConfirmMatch encountered a failure in an evaluation environment other than \
\"Script\". Aborting instead of quitting with exit code `1`.";

ScriptConfirmMatch::ArgumentCount =
"ScriptConfirmMatch called with `1` arguments; between 2 and 4 arguments are \
expected.";

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Main Definition*)

(* Default message template: *)
ScriptConfirmMatch[ expr_, patt_ ] :=
    ScriptConfirmMatch[ expr, patt, $scriptConfirmMatchTemplate ];

(* Default exit code: *)
ScriptConfirmMatch[ expr_, patt_, template_ ] :=
    ScriptConfirmMatch[ expr, patt, template, 1 ];

(* Automatic message template: *)
ScriptConfirmMatch[ expr_, patt_, Automatic, code_ ] :=
    ScriptConfirmMatch[ expr, patt, $scriptConfirmMatchTemplate, code ];

ScriptConfirmMatch[ expr_, patt_, template_, code_ ] :=
    catchTop @ With[ { result = expr },
        If[ MatchQ[ result, patt ],
            result,
            scriptFail @ <|
                "ConfirmationType" -> ScriptConfirmMatch,
                "ExitCode"         -> code,
                "Expression"       -> HoldForm @ result,
                "Pattern"          -> HoldForm @ patt,
                "Input"            -> HoldForm @ expr,
                "MessageTemplate"  -> template
            |>
        ]
    ];

(* Invalid arguments: *)
ScriptConfirmMatch[ args___ ] :=
    catchTop @ With[ { len = Length @ HoldComplete @ args },
        messageFailure[ ScriptConfirmMatch::ArgumentCount, len ]
    ];

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*$scriptConfirmMatchTemplate*)
$scriptConfirmMatchTemplate =
    "ScriptConfirmMatch: `Expression` does not match `Pattern`.";

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Common*)

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*scriptFail*)
scriptFail[ as: KeyValuePattern[ "MessageTemplate" -> tmp_ ] ] := (
    ConsoleError @ TemplateApply[
        tmp,
        as,
        InsertionFunction -> toConsoleString
    ];
    scriptExit @ as
);

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*scriptExit*)
(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::SuspiciousSessionSymbol:: *)
scriptExit[ head_Symbol, code_Integer ] :=
    If[ $EvaluationEnvironment === "Script",
        Exit @ code,
        messageFailure[ head::Session, code ];
        Abort[ ]
    ];
(* :!CodeAnalysis::EndBlock:: *)

scriptExit[ as_Association ] :=
    scriptExit[ Lookup[ as, "ConfirmationType" ], Lookup[ as, "ExitCode" ] ];

scriptExit[ head_, _ ] := scriptExit[ head, 1 ];

scriptExit // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Footer*)
End[ ];

EndPackage[ ];