(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Header*)
BeginPackage[ "Wolfram`PacletCICD`" ];

CheckDependencies // ClearAll;

Begin[ "`Private`" ];

$ContextAliases[ "dnc`" ] = "DefinitionNotebookClient`";

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Config*)
$releaseURL = "https://github.com/WolframResearch/PacletCICD/releases/download";

$releasesToInstall = {
    "DefinitionNotebookClient-1.17.2",
    "PacletResource-1.5.1"
};

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*CheckDependencies*)

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Options*)
CheckDependencies // Options = {
    "Message" -> True
};

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Messages*)
CheckDependencies::InvalidPaclet =
"`1` is not a valid paclet specification.";

CheckDependencies::Insufficient =
"The paclet `1` has version `2`, which does not meet the requested \
specification `3`.";

CheckDependencies::Missing =
"Failed to find the following dependencies for `1`:\n`2`";

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Main definition*)
CheckDependencies[ pac_PacletObject, opts: OptionsPattern[ ] ] :=
    catchTop @ Block[ { $dependencyRules },
        Module[ { checked },
            installOwnDependencies[ ];
            $dependencyRules = Internal`Bag[ ];
            checkDependencies @ pac;
            checked = Internal`BagPart[ $dependencyRules, All ];
            withDNCSettings[ { Automatic, "Submit" },
                returnDependencies[ pac, checked, OptionValue[ Message ] ]
            ]
        ]
    ];

CheckDependencies[ id_, opts: OptionsPattern[ ] ] :=
    catchTop @ With[ { pac = PacletObject @ id },
        If[ PacletObjectQ @ pac,
            CheckDependencies[ pac, opts ],
            throwMessageFailure[
                CheckDependencies::InvalidPaclet,
                id
            ]
        ]
    ];

CheckDependencies // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*installOwnDependencies*)
installOwnDependencies[ ] := installOwnDependencies[ ] =
    If[ $VersionNumber < 13.2 && DirectoryQ @ Environment[ "GITHUB_WORKSPACE" ],
        Scan[ PacletInstall @ URLBuild @ { $releaseURL, #1, #1 <> ".paclet" } &,
              $releasesToInstall
        ]
    ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*returnDependencies*)
returnDependencies[ pac_, checked: { Rule[ _, _ ]... }, False ] :=
    Merge[ DeleteCases[ checked, _ -> None ], Identity ];

returnDependencies[ pac_, checked: { Rule[ _, _ ]... }, True ] :=
    Module[ { res, missing, table },
        res = returnDependencies[ pac, checked, False ];

        missing = Cases[
            res,
            Failure[ "CheckDependencies", as_ ] :> as,
            Infinity
        ];

        If[ missing === { }, Throw[ res, $tag ] ];

        table = If[ MatchQ[ dnc`$ConsoleType, "TTY"|"GitHub" ],
                    consoleDepFailTable @ missing,
                    depFailTable @ missing
                ];

        messageFailure[ CheckDependencies::Missing, pac, table ];

        If[ MatchQ[ dnc`$ConsoleType, "GitHub" ],
            dnc`ConsolePrint[
                TemplateApply[
                    CheckDependencies::Missing,
                    { pac[ "Name" ], table }
                ],
                "Level" -> "Error"
            ]
        ];

        res
    ] ~Catch~ $tag;

returnDependencies // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*depFailTable*)
depFailTable[ missing_ ] :=
    TableForm[
        formatRowItems /@ missing,
        TableHeadings -> { None, { "Name", "Available", "Required" } }
    ];

consoleDepFailTable[ missing_ ] :=
    Module[ { rows, columns, padding },

        rows =
            Prepend[
                formatRowItems /@ missing,
                { "Name", "Available", "Required" }
            ];

        columns = Transpose @ rows;
        padding = Max /@ Map[ StringLength, columns, { 2 } ] + 1;
        "\n" <> StringRiffle[
            Map[
                StringJoin[ "\t", # ] &,
                Insert[
                    Transpose @ Apply[
                        StringPadRight,
                        Transpose @ { columns, padding },
                        { 1 }
                    ],
                    Map[
                        ConstantArray[ "-", #1 - 1 ] <> " " &,
                        padding
                    ],
                    2
                ]
            ],
            "\n"
        ] <> "\n"
    ];

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*formatRowItems*)
formatRowItems[ as_Association ] :=
    formatRowItems[
        Lookup[ as, "Name" ],
        Lookup[ as, "Version" ],
        Lookup[ as, "Requested" ]
    ];

formatRowItems[ name_String, ver_String, requested_String ] :=
    { name, ver, requested };

formatRowItems[ name_String, ver_, _Missing ] :=
    formatRowItems[ name, ver, "Any" ];

formatRowItems[ name_String, None, requested_ ] :=
    formatRowItems[
        name,
        Replace[
            Quiet @ PacletObject[ name ][ "Version" ],
            Except[ _String ] -> "None"
        ],
        requested
    ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*$dependencyRules*)
$dependencyRules = Internal`Bag[ ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*checkDependencies*)
checkDependencies[ pac_PacletObject ] :=
    Module[ { dependencies, checked },
        dependencies = pac[ "Dependencies" ];
        checkDependencies[ pac, dependencies ]
    ];

checkDependencies[ pac_, _Missing ] :=
    Internal`StuffBag[ $dependencyRules, pac -> None ];

checkDependencies[ pac_, dependencies_List ] :=
    checkDependencies[ pac, # ] & /@ dependencies;

checkDependencies[ pac_, name_String ] :=
    checkDependency[ pac, name, "*" ];

checkDependencies[ pac_, name_String -> ver_String ] :=
    checkDependency[ pac, name, ver ];

checkDependencies // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*checkDependency*)
checkDependency[ parent_, name_String, spec_String ] :=
    Module[ { installed },
        installed = pacletInstall[ name, spec ];
        checkDependency[ parent, installed, name, spec ]
    ];

checkDependency[ parent_, fail_? FailureQ, name_, spec_ ] :=
    Module[ { res },
        res = dependencyFailure[ parent, fail, name, spec ];
        Internal`StuffBag[ $dependencyRules, parent -> res ];
    ];

checkDependency[ parent_, pac_PacletObject, name_, spec_ ] :=
    Module[ { ver, sel, res },

        ver = pac[ "Version" ];
        sel = selectVersionNumber[ { ver }, spec ];

        res = If[ StringQ @ sel,
                  dependencySuccess[ pac, spec ],
                  dependencyFailure[ parent, pac, name, spec ]
              ];

        Internal`StuffBag[ $dependencyRules, parent -> res ];

        If[ StringQ @ sel, checkDependencies @ pac ]
    ];

checkDependency // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*pacletInstall*)
pacletInstall[ name_String, spec_String ] :=
    If[ StringFreeQ[ spec, Except[ DigitCharacter|"." ] ],
        PacletInstall[ name -> spec ],
        PacletInstall @ name
    ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*selectVersionNumber*)
selectVersionNumber =
    Wolfram`PacletCICD`Private`SelectVersionNumber`selectVersionNumber;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*dependencySuccess*)
dependencySuccess[ pac_, spec_ ] := Enclose[
    Module[ { info },
        info = ConfirmBy[ Association @ pac @ All, AssociationQ ];
        info[ "Requested" ] = toNiceVersionSpec @ spec;
        Success[
            "CheckDependencies",
            DeleteMissing @ KeyTake[
                info,
                { "Name", "Version", "Requested", "Location" }
            ]
        ]
    ],
    throwError[ "Failed to get info from Paclet `1`.", pac ] &
];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*dependencyFailure*)
dependencyFailure[ parent_, pac_, name_, spec_ ] := Enclose[
    Module[ { pName, info },
        pName = Replace[ parent[ "Name" ], Except[ _String ] -> None ];
        info = <| "Name" -> name, "Requested" -> toNiceVersionSpec @ spec |>;
        info[ "Version" ] = If[ PacletObjectQ @ pac, pac[ "Version" ], None ];
        info[ "Location" ] = If[ PacletObjectQ @ pac, pac[ "Location" ], None ];
        info[ "MessageTemplate" ] = dependencyFailureTemplate @ spec;
        info[ "MessageParameters" ] = { name, spec, pName };

        Failure[
            "CheckDependencies",
            DeleteMissing @ KeyTake[
                info,
                {
                    "Name",
                    "Version",
                    "Requested",
                    "Location",
                    "MessageTemplate",
                    "MessageParameters"
                }
            ]
        ]
    ],
    throwError[ "Failed to get info from Paclet `1`.", pac ] &
];

dependencyFailureTemplate[ "*" ] :=
    "Cannot find dependency \"`1`\" for paclet `3`.";

dependencyFailureTemplate[ _ ] :=
    "Cannot find dependency \"`1`\" `2` for paclet `3`.";

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*toNiceVersionSpec*)
toNiceVersionSpec[ "*"   ] := Missing[ ];
toNiceVersionSpec[ spec_ ] := spec;

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Begin SelectVersionNumber Context*)
Begin[ "Wolfram`PacletCICD`Private`SelectVersionNumber`" ];

catchUndefined = Wolfram`PacletCICD`Private`catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*selectVersionNumber*)
(*
    Adapted from the resource function SelectVersionNumber by Bob Sandheinrich
    https://resources.wolframcloud.com/FunctionRepository/resources/SelectVersionNumber/
*)
selectVersionNumber[ available_List, v_ ] :=
    Catch[
        toStr @ selectVersion[ v, ReverseSort[ toInts /@ available ] ],
        $tag
    ];

selectVersionNumber // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*toStr*)
toStr[ l_List ] := StringRiffle[ ToString /@ PadRight[ l, 3 ], "." ];
toStr[ expr_  ] := expr;
toStr // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*selectVersion*)
selectVersion[ _, { } | _Missing ] := Missing[ "NotAvailable" ];

selectVersion[ v_String, available_ ] :=
    selectVersion[ v, PadRight @ available ] /;
        ! SameQ @@ (Length /@ available);

selectVersion[ v_String, available_ ] :=
    multipleSelect[ StringTrim /@ StringSplit[ v, "," ], available ] /;
        StringContainsQ[ v, "," ];

selectVersion[ v_String, available_ ] :=
    tildeSelect[ vTrim @ v, available ] /; StringMatchQ[ v, "~*" ];

selectVersion[ v_String, available_ ] :=
    caretSelect[ vTrim @ v, available ] /; StringMatchQ[ v, "^*" ];

selectVersion[ v_String, available_ ] :=
    wildcardSelect[ v, available ] /; StringContainsQ[ v, "*" ];

selectVersion[ v_String, available_ ] :=
    compareSelect[ vTrim[ v, 2 ], available, ! versionLess[ #1, #2 ] & ] /;
        StringStartsQ[ v, ">=" ];

selectVersion[ v_String, available_ ] :=
    compareSelect[ vTrim @ v, available, versionLess[ #2, #1 ] & ] /;
        StringStartsQ[ v, ">" ];

selectVersion[ v_String, available_ ] :=
    compareSelect[ vTrim[ v, 2 ], available, ! versionLess[ #2, #1 ] & ] /;
        StringStartsQ[ v, "<=" ];

selectVersion[ v_String, available_ ] :=
    compareSelect[ vTrim @ v, available, versionLess[ #1, #2 ] & ] /;
        StringStartsQ[ v, "<" ];

selectVersion[ v_String, available_ ] :=
    compareSelect[ vTrim @ v, available, #1 === #2 & ] /;
        StringStartsQ[ v, "=" ];

selectVersion[ v_? vnumQ, available_ ] :=
    compareSelect[ v, available, #1 === #2 & ];

selectVersion[ v_String, available_ ] := (
    Message[ selectVersionNumber::invspec, v ];
    Throw[ $Failed, $tag ]
);

selectVersion[ v_, available_ ] :=
    selectVersion[ convertSpecification @ v, available ];

selectVersion[ ___ ] := $Failed;

selectVersion // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*toInts*)
toInts[ v_String ] := ToExpression @ StringSplit[ v, "." ] /; vnumQ @ v;
toInts[ v_ ] := (Message[ selectVersionNumber::invver, v ]; $Failed);
toInts // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*multipleSelect*)
multipleSelect[ v_List, available_ ] :=
    Block[ { selectFirst = Select },
        SelectFirst[ Fold[ selectVersion[ #2, #1 ] &, available, v ], ListQ ]
    ];
multipleSelect // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*tildeSelect*)
tildeSelect[ v_, available_ ] :=
    With[ { vi = toInts @ v, n = Length @ First @ available },
        With[
            {
                res =
                    selectFirst[
                        available,
                        Function[
                            And[
                                versionLess[
                                    #1,
                                    MapAt[
                                        #1 + 1 &,
                                        PadRight[ vi, n ],
                                        If[ Length @ vi > 1, 2, 1 ]
                                    ]
                                ],
                                ! versionLess[ #1, PadRight[ vi, n ] ]
                            ]
                        ]
                    ]
            },
            If[ ListQ @ res, res, Missing[ "NotAvailable" ] ]
        ]
    ];
tildeSelect // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*selectFirst*)
selectFirst := SelectFirst;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*vTrim*)
vTrim[ str_, n_: 1 ] := StringTrim @ StringDrop[ str, n ];
vTrim // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*caretSelect*)
caretSelect[ v_, available_ ] :=
    With[ { vi = toInts @ v, n = Length @ First @ available },
        With[
            {
                res =
                    selectFirst[
                        available,
                        Function[
                            And[
                                versionLess[ #, PadRight[ caretBump @ vi, n ] ],
                                ! versionLess[ #, PadRight[ vi, n ] ]
                            ]
                        ]
                    ]
            },
            If[ ListQ @ res, res, Missing[ "NotAvailable" ] ]
        ]
    ];

caretSelect // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*caretBump*)
caretBump[ { z: 0 ..., 0 } ] := { z, 1 };

caretBump[ v_ ] :=
    v /. { z: 0 ..., non_, rest___ } /; non > 0 :> { z, non + 1, rest };

caretBump // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*wildcardSelect*)
wildcardSelect[ v_, available_ ] :=
    With[ { vi = replaceWildcard @ v, n = Length @ First @ available },
        With[
            {
                res =
                    selectFirst[
                        available,
                        MatchQ[ #1, PadRight[ vi, n, _ ] ] &
                    ]
            },
            If[ ListQ @ res, res, Missing[ "NotAvailable" ] ]
        ]
    ];

wildcardSelect // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsubsubsection::Closed:: *)
(*replaceWildcard*)
replaceWildcard[ v_ ] :=
    StringSplit[ v, "." ] /.
        { "*" -> _ } /. { str_String :> ToExpression @ str };

replaceWildcard // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*compareSelect*)
compareSelect[ v_, available_, f_ ] :=
    With[ { vi = toInts @ v, n = Length @ First @ available },
        With[ { res = selectFirst[ available, f[ #, PadRight[ vi, n ] ] & ] },
            If[ ListQ @ res, res, Missing[ "NotAvailable" ] ]
        ]
    ];

compareSelect // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*versionLess*)
versionLess[ v1_, v1_ ] := False;
versionLess[ v1_, v2_ ] := First @ Sort @ { v1, v2 } === v1;
versionLess // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*vnumQ*)
vnumQ[ v_String ] :=
    StringMatchQ[ v, DigitCharacter ~~ (DigitCharacter | ".")... ];
vnumQ // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*convertSpecification*)
convertSpecification[ spec_ ] :=
    With[
        {
            res =
                ReplaceRepeated[
                    spec,
                    {
                        HoldPattern @ GreaterThan[ str_String ] :>
                            "> " <> str,
                        HoldPattern @ GreaterEqualThan[ str_String ] :>
                            ">= " <> str,
                        HoldPattern @ LessThan[ str_String ] :>
                            "< " <> str,
                        HoldPattern @ LessEqualThan[ str_String ] :>
                            "<= " <> str,
                        HoldPattern @ EqualTo[ str_String ] :>
                            "= " <> str,
                        HoldPattern[ And ][ str: _String.. ] :>
                            StringRiffle[ { str }, "," ]
                    }
                ]
        },
        If[ StringQ @ res,
            res,
            Message[ selectVersionNumber::invspec, spec ];
            Throw[ $Failed, $tag ]
        ]
    ];

convertSpecification // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*End SelectVersionNumber Context*)
End[ ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Footer*)
End[ ];

EndPackage[ ];