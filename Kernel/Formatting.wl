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
(*WorkflowJob*)
FormattingHelper[ job: WorkflowJob[ name_String, as_ ], fmt_ ] :=
    Module[ { detail },
        detail = assocViewer @ as;
        BoxForm`ArrangeSummaryBox[
            WorkflowJob,
            Unevaluated @ job,
            boxIcon @ WorkflowJob,
            {
                { BoxForm`SummaryItem @ { "Name: ", name } },
                { BoxForm`SummaryItem @ { "Data: ", detail } }
            },
            { },
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
                            False -> $elidedAssoc,
                            True ->
                                Grid[
                                    KeyValueMap[ assocViewer, as ],
                                    Alignment -> { Left, Baseline }
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
                                    Alignment -> { Left, Top }
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

assocViewer[ other_ ] := Style[ other, "SummaryItem" ];

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