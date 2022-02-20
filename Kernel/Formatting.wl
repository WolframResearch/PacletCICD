(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Header*)
BeginPackage[ "Wolfram`PacletCICD`" ];

FormattingHelper // ClearAll;

Begin[ "`Private`" ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*FormattingHelper*)
FormattingHelper // Attributes = { HoldFirst };

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Workflow*)
FormattingHelper[ workflow: Workflow[ name_String, as_ ], fmt_ ] :=
    Module[ { detail, jobs },
        detail = assocViewer @ as;
        jobs = Lookup[ as, "jobs", <| |> ];
        BoxForm`ArrangeSummaryBox[
            Workflow,
            Unevaluated @ workflow,
            boxIcon @ workflow,
            {
                { BoxForm`SummaryItem @ { "Name: ", name } },
                { BoxForm`SummaryItem @ { "Jobs: ", Length @ jobs } }
            },
            {
                { BoxForm`SummaryItem @ { "Data: ", detail } }
            },
            fmt
        ]
    ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*WorkflowJob*)
FormattingHelper[ job: WorkflowJob[ name_String, as_ ], fmt_ ] :=
    Module[ { detail, steps },
        detail = assocViewer @ as;
        steps = Lookup[ as, "steps", { } ];
        BoxForm`ArrangeSummaryBox[
            WorkflowJob,
            Unevaluated @ job,
            boxIcon @ job,
            {
                { BoxForm`SummaryItem @ { "Name: ", name } },
                { BoxForm`SummaryItem @ { "Steps: ", Length @ steps } }
            },
            {
                { BoxForm`SummaryItem @ { "Data: ", detail } }
            },
            fmt
        ]
    ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*WorkflowStep*)
FormattingHelper[ step: WorkflowStep[ name_String, as_ ], fmt_ ] :=
    Module[ { detail },
        detail = assocViewer @ as;
        BoxForm`ArrangeSummaryBox[
            WorkflowStep,
            Unevaluated @ step,
            actionIcon @ as,
            {
                { BoxForm`SummaryItem @ { "Name: ", name } },
                actionLabel @ as
            },
            {
                { BoxForm`SummaryItem @ { "Data: ", detail } }
            },
            fmt
        ]
    ];

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*actionLabel*)
actionLabel[ KeyValuePattern[ "uses" -> uses_String ] ] :=
    Module[ { lbl, link },
        lbl = StringDelete[ uses, "@" ~~ __ ~~ EndOfString ];
        link = Hyperlink[ lbl, URLBuild @ { "https://github.com", lbl } ];
        { BoxForm`SummaryItem @ { "Action: ", link } }
    ];

actionLabel[ KeyValuePattern[ "run" -> code_String ] ] :=
    If[ StringStartsQ[ code, "wolframscript -code " ],
        actionCodeLabel @ StringTrim[
            StringDelete[ code, StartOfString~~"wolframscript -code " ],
            "'"
        ],
        actionCommandLabel @ StringSplit[ code, "\r\n"|"\n" ]
    ];

actionLabel[ ___ ] := Nothing;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*actionCodeLabel*)
actionCodeLabel[ code_String ] :=
    If[ StringLength @ code < 40,
        { BoxForm`SummaryItem @ {
            "Code: ",
            Style[
                ToExpression[ code, InputForm, HoldForm ],
                ShowStringCharacters -> True
            ]
        } },
        { BoxForm`SummaryItem @ {
            "Code: ",
            Tooltip[
                Style[RawBoxes @ RowBox @ {
                    StringTake[ code, UpTo[ 40 ] ],
                    " \[Ellipsis]"
                }, ShowStringCharacters -> True ],
                code
            ]
        } }
    ];

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*actionCommandLabel*)
actionCommandLabel[ { code_String } ] :=
    If[ StringLength[ code ] < 40,
        { BoxForm`SummaryItem @ { "Command: ", code } },
        { BoxForm`SummaryItem @ {
            "Command: ",
            Tooltip[ StringTake[ code, UpTo[ 40 ] ] <> " \[Ellipsis]", code ]
        } }
    ];

actionCommandLabel @ { code_String, rest__ } := {
    BoxForm`SummaryItem @ {
        "Command: ",
        Tooltip[
            StringTake[ code, UpTo[ 40 ] ] <> " \[Continuation]",
            StringRiffle[ { code, rest }, "\n" ]
        ]
    }
};

actionCommandLabel[ ___ ] := Nothing;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*actionIcon*)
actionIcon[ KeyValuePattern[ "uses" -> uses_String ] ] :=
    Once @ actionIcon @ uses;

actionIcon[ uses_String ] := actionIcon[ uses ] = Enclose[
    Module[ { repo, url, data, avatar },
        repo = StringDelete[ uses, "@" ~~ __ ~~ EndOfString ];
        url = URLBuild @ { "https://api.github.com/repos", repo };
        data = ConfirmBy[ CloudEvaluate @ URLExecute[ url, "RawJSON" ], AssociationQ ];
        avatar = ConfirmBy[ data[ "owner", "avatar_url" ], StringQ ];
        Show[ ConfirmBy[ Import @ avatar, ImageQ ], ImageSize -> 24 ]
    ],
    Show[ $ghIcon, ImageSize -> 24 ] &
];

actionIcon[ KeyValuePattern[ "run" -> _String ] ] := $terminalIcon;

actionIcon[ ___ ] := $defaultIcon;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Other*)
FormattingHelper[ other_, fmt_ ] := $Failed;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Utilities*)

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*assocViewer*)
assocViewer[ as_Association ] :=
    DynamicModule[ { x = False },
        Grid[
            {
                {
                    Opener @ Dynamic @ x,
                    PaneSelector[
                        {
                            False -> elidedAssoc @ as,
                            True ->
                                Grid[
                                    KeyValueMap[ assocViewer, as ],
                                    Alignment -> { Left, Baseline },
                                    Dividers -> {False, {False, {True}, False}},
                                    (* Dividers -> Center, *)
                                    FrameStyle -> GrayLevel[0, 0.1]
                                ]
                        },
                        Dynamic @ x,
                        ImageSize -> Automatic,
                        ContentPadding -> False
                    ]
                }
            },
            Alignment -> { Left, Top }
        ]
    ];

assocViewer[ key_, value_ ] :=
    { Style[ key, "SummaryItemAnnotation" ], assocViewer @ value };

assocViewer[ list_List ] /; StringLength[ ToString @ list ] < 50 :=
    list;

assocViewer[ list_List ] :=
    DynamicModule[ { x = False },
        Grid[
            {
                {
                    Opener @ Dynamic @ x,
                    PaneSelector[
                        {
                            False -> $elidedList,
                            True ->
                                Column[
                                    assocViewer /@ list,
                                    Alignment -> { Left, Top },
                                    Dividers -> {False, {False, {True}, False}},
                                    (* Dividers -> Center, *)
                                    FrameStyle -> GrayLevel[0, 0.1]
                                ]
                        },
                        Dynamic @ x,
                        ImageSize -> Automatic,
                        ContentPadding -> False
                    ]
                }
            },
            Alignment -> { Left, Top }
        ]
    ];

assocViewer[ Null ] := "";
assocViewer[ other_ ] := Style[ other, "SummaryItem" ];


elidedAssoc[ KeyValuePattern[ "name" -> name_String ] ] := elidedAssoc @ name;

elidedAssoc[ name_String ] :=
    Row @ {
        Style[ name, "SummaryItemAnnotation", FontSlant -> Italic ],
        " ",
        $elidedAssoc
    };

elidedAssoc[ name_String ] :=
    Row @ {
        "\[LeftAssociation] ",
        Style[ name, "SummaryItemAnnotation", FontSlant -> Italic ],
        " \[RightAssociation]"
    };

elidedAssoc[ other_ ] := $elidedAssoc;

$elidedAssoc = "\[LeftAssociation]\[Ellipsis]\[RightAssociation]";
$elidedList = "{\[Ellipsis]}";

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*boxIcon*)
boxIcon[ head_ ] := $defaultIcon;

$defaultIcon := $defaultIcon =
    Show[
        Import @ FileNameJoin @ {
            $thisPaclet[ "AssetLocation", "Images" ],
            "Icon.png"
        },
        ImageSize -> $iconSize
    ];

$ghIcon := $ghIcon = BinaryDeserialize @ ByteArray[ "\
OEM6eJzFmM1PE0EUwKUtAfmwhQCLF6g3PGBoKdgao+kHmB6akC4HTYi4tNN2k3aX7G4RPPAHeOHIwci\
JCxqJN+NR9OJNPXAQgkEvRL0AURMg1DfuVNZlZ7v9IDb5pcmb995vdtjpTEjVy/XRHJdGay672+04Z/\
HDdHZ1AAlgGfgA7AMFwj6JLZOcDqt9LXg9wApwqPGV4pDUeKrwOoFHwHEZXj3HpIezgmfeqsKrZ8vqW\
kBeCNitobsI7hkq4R48I7d2DoMUdxvwSZe/DjyrcE67pHZdF8eONgP/kkGPCTLWwKj7aof5d7+9Imj3\
4w7JbSC1EwZ9l3RuH+UZvLo8vCf6AJvB/G1kzKmLeym9fZqcVUrOkNn7YuWDe1B6r5LxbuCIkjNZA/8\
kpfcRcacp41+A9hr424HPFAd2r1DGpqp1a+YwRXGsGOyP4tqU9ZtZwu+k/I2x+6dBfLtWbs0ctg08Rm\
7Mxhn4NykuI/bOwL9Hc1HiXTV0d5m431HG7tbQf5fiwO7HlLH3gOX7mYnbQXoZObD7jsl7sFAD/4JJf\
+xuBX6Y5DwFuivwXiS1tL7Y2UpyFzXxh8BNYE0TOwCeAIhRz0o7ZZ0DQIZRz7ODEntsUVPbA/wicXxn\
lBn1zH9rUId71xn464DnJZxFsKtHVz+vy/EDl5jTZ8d1kzW/YdE/T1m/N5qclyTuYtT7zANg1GjtNT0\
6Lbixw3BfMep5rP2dFGkuSr2rhHuz1LsM473AhqbmNZAH7gGzQGOFftyz1+Jz4DvDC0ofVwV+3Kusuw\
x5n5PA1yr83xh1z57aL2XMo4VR9+NH4DtD7tWU3AaSg3MVXFuqf6FQ+K+wjtCcglI22RHPZ5HcFBazo\
sROcwnE2uO3Qn8HWqKCgqQs4mZ4IS07xqX8SZEzhhQuKqREKccpvCgEbf2s/XZsDH9fgO8QJ/MJNpFB\
OS5Y1882hyXEKaI0LopZtjeYFKeQeywjKqKcEafdYXbY3RfjEryAA5f72W7oEENJnotxApdGOSQopJe\
9n22KCrLCCQkUjbB9s7npKzyfvDYy5LnqD3qDgUBwwOMZ8QaCnqFQ2DPgGw76AwO+CJRFxEQedyqWJU\
/KQvSy5giS+BmUHJXEHH6S83GUQhICPX5Us7n4K5tLgF7WGBZzuErGE2lkxZRyn5MQ61LX889/eOKIS\
879BnZkPNE=" ];

$terminalIcon := With[ { size = $iconSize }, RawBoxes @ GraphicsBox[
    {
        Thickness[ 0.05 ],
        {
            FaceForm @ { RGBColor[ 0.749, 0.749, 0.749 ], Opacity[ 1.0 ] },
            FilledCurveBox[
                {
                    {
                        { 1, 4, 3 },
                        { 0, 1, 0 },
                        { 1, 3, 3 },
                        { 0, 1, 0 },
                        { 1, 3, 3 },
                        { 0, 1, 0 },
                        { 1, 3, 3 },
                        { 0, 1, 0 }
                    }
                },
                {
                    {
                        { 16.0, 17.0 },
                        { 17.104, 17.0 },
                        { 18.0, 16.104 },
                        { 18.0, 15.0 },
                        { 18.0, 5.0 },
                        { 18.0, 3.896 },
                        { 17.104, 3.0 },
                        { 16.0, 3.0 },
                        { 4.0, 3.0 },
                        { 2.896, 3.0 },
                        { 2.0, 3.896 },
                        { 2.0, 5.0 },
                        { 2.0, 15.0 },
                        { 2.0, 16.104 },
                        { 2.896, 17.0 },
                        { 4.0, 17.0 },
                        { 16.0, 17.0 }
                    }
                }
            ]
        },
        {
            FaceForm @ { RGBColor[ 1.0, 1.0, 1.0 ], Opacity[ 1.0 ] },
            FilledCurveBox[
                {
                    {
                        { 0, 2, 0 },
                        { 0, 1, 0 },
                        { 0, 1, 0 },
                        { 0, 1, 0 },
                        { 0, 1, 0 },
                        { 0, 1, 0 },
                        { 0, 1, 0 }
                    }
                },
                {
                    {
                        { 9.0, 9.3159 },
                        { 4.0, 6.1269 },
                        { 4.0, 8.2219 },
                        { 7.0, 10.032 },
                        { 7.0, 10.08 },
                        { 4.0, 11.889 },
                        { 4.0, 13.984 },
                        { 9.0, 10.795 }
                    }
                }
            ],
            FilledCurveBox[
                { { { 0, 2, 0 }, { 0, 1, 0 }, { 0, 1, 0 } } },
                { { { 16.0, 5.0 }, { 10.0, 5.0 }, { 10.0, 6.0 }, { 16.0, 6.0 } } }
            ]
        }
    },
    AspectRatio -> Automatic,
    ImageSize -> size,
    PlotRange -> { { 1.0, 20.0 }, { 1.0, 18.0 } }
] ];

$iconSize = Dynamic @ {
    Automatic,
    (3 CurrentValue[ "FontCapHeight" ]) / AbsoluteCurrentValue @ Magnification
};

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Footer*)
End[ ];

EndPackage[ ];