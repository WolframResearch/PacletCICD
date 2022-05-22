(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Header*)
BeginPackage[ "Wolfram`PacletCICD`" ];

ClearAll[
    BytesToQuantity,
    SecondsToQuantity
];

Begin[ "`Units`Private`" ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*BytesToQuantity*)
(*https://resources.wolframcloud.com/FunctionRepository/resources/BytesToQuantity*)

BytesToQuantity[ b_Integer? Negative ] := -BytesToQuantity[ -b ];
BytesToQuantity[ b_Integer ] := Replace[ b, $btqRules ];

BytesToQuantity[ q_Quantity ] :=
    With[ { b = Check[ UnitConvert[ q, "Bytes" ], $Failed, Quantity::compat ] },
        BytesToQuantity @ QuantityMagnitude @ b /; ! FailureQ @ b
    ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*$btqRules*)
$btqRules = Dispatch @ Flatten @ {
    MapIndexed[
        Function[
            With[ { e = First[ #2 ] },
                n_ /; n < 1000^e :>
                    UnitConvert[ Quantity[ 1.0 * n, "Bytes" ], #1 ]
            ]
        ],
        {
            "Bytes",
            "Kilobytes",
            "Megabytes",
            "Gigabytes",
            "Terabytes",
            "Petabytes",
            "Exabytes",
            "Zettabytes"
        }
    ],
    n_ /; n >= 1000^8 :>
        UnitConvert[ Quantity[ 1.0 * n, "Bytes" ], "Yottabytes" ],
    n_ :> Quantity[ 1.0 * n, "Bytes" ]
};

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*SecondsToQuantity*)
(*https://resources.wolframcloud.com/FunctionRepository/resources/SecondsToQuantity/*)

SecondsToQuantity::compat =
"`1` is not a compatible time unit.";

SecondsToQuantity::mmu =
"The value given for MaxMixedUnits is expected to be a positive integer or Automatic.";

SecondsToQuantity // Attributes = { Listable };

SecondsToQuantity // Options = {
    "MixedUnits"    -> True,
    "MaxMixedUnits" -> Automatic
};

SecondsToQuantity[ q_Quantity, opts: OptionsPattern[ ] ] :=
    With[ { sec = convertSeconds @ q },
        SecondsToQuantity[ sec, opts ] /; sec =!= $fail
    ];

SecondsToQuantity[ sec: Except[ _Quantity ], OptionsPattern[ ] ] :=
    With[
        {
            result = Catch[
                secondsToQuantity[
                    sec,
                    TrueQ @ OptionValue[ "MixedUnits" ],
                    OptionValue[ "MaxMixedUnits" ]
                ],
                $tag
            ]
        },
        result /; QuantityQ @ result
    ];

secondsToQuantity[ sec_? Internal`SyntacticNegativeQ, mixed_, max_ ] :=
    -secondsToQuantity[ -sec, mixed, max ];

secondsToQuantity[ sec_, True, Automatic ] := Replace[ sec, $mixedDispatch ];
secondsToQuantity[ sec_, _   , Automatic ] := Replace[ sec, $dispatch      ];

secondsToQuantity[ sec_, True, max_      ] :=
    Replace[ sec, mixedDispatch @ Min[ $maxMixedUnits, max ] ];

$maxMixedUnits = 3;


convertSeconds[ Quantity[ q_, unit_ ] ] :=
    Module[ { conv },
        conv = compatQuietCheck @ qConvert[ q, unit, "Seconds" ];
        validateConversion[ conv, unit ]
    ];


compatQuietCheck // Attributes = { HoldFirst };
compatQuietCheck[ eval_ ] :=
    Quiet[ Check[ eval, $fail, Quantity::compat ], Quantity::compat ];

validateConversion // ClearAll;
validateConversion[ $fail, unit_ ] := (message[ "compat", unit ]; $fail);
validateConversion[ q: Quantity[ _, "Seconds" ], _ ] := QuantityMagnitude @ q;
validateConversion[ ___ ] := $fail;

message // ClearAll;
message[ tag_, args___ ] :=
    Block[ { PrintTemporary },
        Message[
            MessageName[ SecondsToQuantity, tag ],
            args
        ]
    ];

qConvert // ClearAll;
qConvert[ n_, unit1_, unit2_ ] := UnitConvert[ Quantity[ n, unit1 ], unit2 ];

qConvertMag // ClearAll;
qConvertMag[ n_, unit1_, unit2_ ] := QuantityMagnitude @ qConvert[ n, unit1, unit2 ];

makeThresholds // ClearAll;
makeThresholds[ units_ ] :=
    AssociationMap[ Function @ qConvertMag[ 1.0, #1, "Seconds" ], units ];

makeRules // ClearAll;
makeRules[ thresholds_ ] :=
    Flatten @ {
        If[ FreeQ[ Keys @ thresholds, MixedUnit ],
            Nothing,
            With[ { max = qConvertMag[ 10000.0, "Years", "Seconds" ] },
                x_ /; x > max :> UnitConvert[ Quantity[ x, "Seconds" ], "Years" ]
            ]
        ],
        KeyValueMap[
            x_ /; x >= #2 :> UnitConvert[ Quantity[ x, "Seconds" ], #1 ] &,
            thresholds
        ],
        With[ { pMin = $planckThreshold, min = Min @ thresholds },
            {
                x_ /; 0 < x < pMin :> UnitConvert[ Quantity[ x, "Seconds" ], "PlanckTime" ],
                x_ /; 0 < x < min :> UnitConvert[ Quantity[ x, "Seconds" ], "Yoctoseconds" ]
            }
        ],
        x_ :> Quantity[ x, "Seconds" ]
    };

$unitThresholds = makeThresholds @ {
    "Years",
    "Months",
    "Days",
    "Hours",
    "Minutes",
    "Seconds",
    "Milliseconds",
    "Microseconds",
    "Nanoseconds",
    "Picoseconds",
    "Femtoseconds",
    "Attoseconds",
    "Zeptoseconds"
};

$mixedUnitThresholds = makeThresholds @ {
    MixedUnit @ { "Years", "Months", "Days" },
    MixedUnit @ { "Months", "Days" },
    MixedUnit @ { "Days", "Hours" },
    MixedUnit @ { "Hours", "Minutes", "Seconds" },
    MixedUnit @ { "Minutes", "Seconds" },
    "Seconds",
    "Milliseconds",
    "Microseconds",
    "Nanoseconds",
    "Picoseconds",
    "Femtoseconds",
    "Attoseconds",
    "Zeptoseconds"
};

mixedUnitThresholds[ n_Integer? Positive ] := mixedUnitThresholds[ n ] =
    makeThresholds @ {
        MixedUnit @ Take[ { "Years", "Months", "Days"     }, UpTo @ n ],
        MixedUnit @ Take[ { "Months", "Days"              }, UpTo @ n ],
        MixedUnit @ Take[ { "Days", "Hours"               }, UpTo @ n ],
        MixedUnit @ Take[ { "Hours", "Minutes", "Seconds" }, UpTo @ n ],
        MixedUnit @ Take[ { "Minutes", "Seconds"          }, UpTo @ n ],
        "Seconds",
        "Milliseconds",
        "Microseconds",
        "Nanoseconds",
        "Picoseconds",
        "Femtoseconds",
        "Attoseconds",
        "Zeptoseconds"
    };

mixedUnitThresholds[ ___ ] := (
    Message[ SecondsToQuantity::mmu ];
    Throw[ $fail, $tag ]
);

$planckThreshold =
    QuantityMagnitude @ UnitConvert[
        Quantity[ 1000.0, "PlanckTime" ],
        "Seconds"
    ];

$rules = makeRules @ $unitThresholds;

$mixedRules = makeRules @ $mixedUnitThresholds;

$dispatch := $dispatch = Dispatch @ $rules;

$mixedDispatch := $mixedDispatch = Dispatch @ $mixedRules;

mixedDispatch[ n_ ] := mixedDispatch[ n ] =
    Dispatch @ makeRules @ mixedUnitThresholds @ n;

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Footer*)
End[ ];

EndPackage[ ];