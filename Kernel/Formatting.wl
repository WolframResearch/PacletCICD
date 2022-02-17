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
            boxIcon @ Workflow,
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
            boxIcon @ WorkflowJob,
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
(*Other*)
FormattingHelper[ other_, fmt_ ] := MakeBoxes[ other, fmt ];

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
        ImageSize -> 25
    ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Footer*)
End[ ];

EndPackage[ ];