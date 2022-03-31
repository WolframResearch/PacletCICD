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
    Module[ { repo, data, avatar, img, thumb },
        repo   = StringDelete[ uses, "@" ~~ __ ~~ EndOfString ];
        data   = ConfirmBy[ ghAPI[ "repos", repo ], AssociationQ ];
        avatar = ConfirmBy[ data[ "owner", "avatar_url" ], StringQ ];
        img    = ConfirmBy[ Import @ avatar, ImageQ ];
        thumb  = ImageResize[ img, 48 ];
        Show[ thumb, ImageSize -> 24 ]
    ],
    Show[ $ghIcon, ImageSize -> 24 ] &
];

actionIcon[ KeyValuePattern[ "run" -> _String ] ] := $terminalIcon;

actionIcon[ ___ ] := $defaultIcon;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*PublisherTokenObject*)
FormattingHelper[ pt: PublisherTokenObject[ as_Association ], fmt_ ] := Enclose[
    Module[ { c2cStr, creation, expiration, endpoints, rsb, mainRow, extraRow },

        c2cStr     = Confirm @ tokenC2C @ as;
        creation   = Confirm @ tokenCreationRow @ as;
        expiration = Confirm @ tokenExpirationRow @ as;
        endpoints  = Confirm @ tokenEndpointsRow @ as;
        rsb        = Confirm @ tokenRSBRow @ as;
        mainRow    = Confirm @ tokenMainRow @ as;
        extraRow   = Confirm @ tokenExtraRow @ as;

        BoxForm`ArrangeSummaryBox[
            PublisherTokenObject,
            Unevaluated @ pt,
            $publisherTokenIcon,
            {
                { BoxForm`SummaryItem @ { "Token:", c2cStr } },
                mainRow
            },
            {
                Sequence @@ extraRow,
                creation,
                expiration,
                rsb,
                endpoints
            },
            fmt
        ]
    ],
    ToBoxes[ #, fmt ] &
];

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*tokenMainRow*)
tokenMainRow[ KeyValuePattern[ "Name" -> name_String? StringQ ] ] :=
    { BoxForm`SummaryItem @ { "Name: ", name } };

tokenMainRow[ KeyValuePattern[ "Creator" -> user_String? StringQ ] ] :=
    { BoxForm`SummaryItem @ { "Creator: ", user } };

tokenMainRow[ ___ ] := $Failed;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*tokenExtraRow*)
tokenExtraRow[ KeyValuePattern @ {
    "Creator"     -> user_String? StringQ,
    "PublisherID" -> publisher_String? StringQ,
    "Name"        -> _String? StringQ
} ] :=
    {
        { BoxForm`SummaryItem @ { "Creator: "    , user      } },
        { BoxForm`SummaryItem @ { "PublisherID: ", publisher } }
    };

tokenExtraRow[ KeyValuePattern[ "PublisherID" -> publisher_String? StringQ ] ] :=
    {
        { BoxForm`SummaryItem @ { "PublisherID: ", publisher } }
    };

tokenExtraRow[ ___ ] := $Failed;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*tokenC2C*)
tokenC2C[ as_Association ] :=
    tokenC2C @ catch @ publisherTokenProperty[ as, "TokenString" ];

tokenC2C[ token_String? StringQ ] :=
    tokenC2C[ token, StringSplit[ token, "-" ] ];

(* tokenC2C[ token_, { _, _, id_String, _ } ] :=
    If[ TrueQ[ StringLength @ id > 12 ],
        clickToCopy[
            Style[
                StringJoin[
                    StringDrop[ id, -6 ],
                    ConstantArray[ "\[Bullet]", 6 ]
                ],
                FontFamily -> Dynamic @ CurrentValue @ {
                    StyleHints,
                    "CodeFont"
                }
            ],
            token
        ],
        $Failed
    ]; *)

tokenC2C[ token_, { _, _, id_String, _ } ] :=
    If[ TrueQ[ StringLength @ id > 12 ],
        clickToCopy[
            StringDrop[ id, -6 ] <> ConstantArray[ "\[ThinSpace]\[Bullet]", 6 ],
            token
        ],
        $Failed
    ];

tokenC2C[ ___ ] := $Failed;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*tokenCreationRow*)
tokenCreationRow[ KeyValuePattern[ "CreationDate" -> date_ ] ] :=
    tokenCreationRow @ date;

tokenCreationRow[ date_DateObject? DateObjectQ ] :=
    { BoxForm`SummaryItem @ { "CreationDate: ", date  } };

tokenCreationRow[ ___ ] := $Failed;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*tokenExpirationRow*)
tokenExpirationRow[ KeyValuePattern[ "ExpirationDate" -> date_ ] ] :=
    tokenExpirationRow @ date;

tokenExpirationRow[ None ] := Nothing;

tokenExpirationRow[ date_DateObject? DateObjectQ ] :=
    { BoxForm`SummaryItem @ { "ExpirationDate: ", date  } };

tokenExpirationRow[ ___ ] := $Failed;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*tokenEndpointsRow*)
tokenEndpointsRow[ KeyValuePattern[ "AllowedEndpoints" -> list_ ] ] :=
    tokenEndpointsRow @ list;

tokenEndpointsRow[ list: { ___String? StringQ } ] :=
    { BoxForm`SummaryItem @ { "AllowedEndpoints: ", assocViewer @ list  } };

tokenEndpointsRow[ ___ ] := $Failed;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*tokenRSBRow*)
tokenRSBRow[ KeyValuePattern[ "ResourceSystemBase" -> rsb_String ] ] :=
    tokenRSBRow @ rsb;

tokenRSBRow[ "https://www.wolframcloud.com/obj/resourcesystem/api/1.0" ] :=
    Nothing;

tokenRSBRow[ rsb_String ] :=
    { BoxForm`SummaryItem @ { "ResourceSystemBase: ", rsb  } };

tokenRSBRow[ ___ ] := $Failed;

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

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*Icons*)
$defaultIcon        := wxfResource[ "DefaultIcon"        ];
$ghIcon             := wxfResource[ "GitHubIcon"         ];
$terminalIcon       := wxfResource[ "TerminalIcon"       ];
$publisherTokenIcon := wxfResource[ "PublisherTokenIcon" ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*clickToCopy*)
clickToCopy[ label_, content_String ] :=
    RawBoxes @ clickToCopyBoxes[ ToBoxes @ label, content ];

clickToCopy[ label_, content_ ] :=
    RawBoxes @ clickToCopyBoxes[
        ToBoxes @ label,
        RawBoxes @ MakeBoxes @ content
    ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*clickToCopyBoxes*)
clickToCopyBoxes[ label_, content_ ] :=
    TagBox[
        DynamicModuleBox[
            { $CellContext`boxObj, $CellContext`cellObj },
            TagBox[
                TagBox[
                    ButtonBox[
                        TagBox[
                            label,
                            BoxForm`Undeploy,
                            DefaultBaseStyle -> { Deployed -> False }
                        ],
                        ButtonFunction :>
                            FrontEndExecute @ {
                                CopyToClipboard @ content,
                                NotebookDelete @ $CellContext`cellObj,
                                FrontEnd`AttachCell[
                                    $CellContext`boxObj,
                                    Cell @ BoxData @ TemplateBox[
                                        { "Copied" },
                                        "ClickToCopyTooltip"
                                    ],
                                    { 1, { Center, Bottom } },
                                    { Center, Top },
                                    "ClosingActions" -> {
                                        "ParentChanged",
                                        "MouseExit"
                                    }
                                ]
                            },
                        Evaluator -> None,
                        Appearance -> {
                            "Default" -> None,
                            "Hover" ->
                                FrontEnd`FileName[
                                    { "Typeset", "ClickToCopy" },
                                    "Hover.9.png"
                                ],
                            "Pressed" ->
                                FrontEnd`FileName[
                                    { "Typeset", "ClickToCopy" },
                                    "Pressed.9.png"
                                ]
                        },
                        BaseStyle -> { },
                        DefaultBaseStyle -> { },
                        BaselinePosition -> Baseline,
                        FrameMargins -> 2,
                        Method -> "Preemptive"
                    ],
                    EventHandlerTag @ {
                        "MouseEntered" :> (
                            $CellContext`cellObj =
                                MathLink`CallFrontEnd @ FrontEnd`AttachCell[
                                    $CellContext`boxObj,
                                    Cell @ BoxData @ TemplateBox[
                                        { "Copy" },
                                        "ClickToCopyTooltip"
                                    ],
                                    { 1, { Center, Bottom } },
                                    { Center, Top },
                                    "ClosingActions" -> { "ParentChanged" }
                                ]
                            ),
                        "MouseExited" :> NotebookDelete @ $CellContext`cellObj,
                        PassEventsDown -> True,
                        Method -> "Preemptive",
                        PassEventsUp -> True
                    }
                ],
                MouseAppearanceTag[ "LinkHand" ]
            ],
            Initialization :> ($CellContext`boxObj = EvaluationBox[ ]),
            DynamicModuleValues :> { },
            UnsavedVariables :> { $CellContext`boxObj, $CellContext`cellObj },
            BaseStyle -> { Editable -> False }
        ],
        Deploy,
        DefaultBaseStyle -> "Deploy"
    ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Footer*)
End[ ];

EndPackage[ ];