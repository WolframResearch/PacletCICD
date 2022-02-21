(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Header*)
BeginPackage[ "Wolfram`PacletCICD`" ];

WorkflowValue // ClearAll;

Begin[ "`Private`" ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*WorkflowValue*)

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Options*)
WorkflowValue // Options = { };

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Argument Patterns*)

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Main Definition*)
WorkflowValue[ name_String ] := catchTop @ WorkflowValue[ name, $defaultScope ];
WorkflowValue[ name_, scope_ ] := catchTop @ getWorkflowValue[ name, scope ];
WorkflowValue // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*UpValues*)

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*Set*)
WorkflowValue /:
HoldPattern[ Set ][ WorkflowValue[ name_String ], value_ ] :=
    catchTop @ With[ { scope = $defaultScope },
        Set[ WorkflowValue[ name, scope ], value ]
    ];

WorkflowValue /:
HoldPattern[ Set ][ WorkflowValue[ name_String, scope_ ], value_ ] :=
    catchTop @ setWFValue[ name, scope, value ];

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*SetDelayed*)
WorkflowValue /:
HoldPattern[ SetDelayed ][ WorkflowValue[ name_String ], value_ ] :=
    catchTop @ With[ { scope = $defaultScope },
        SetDelayed[ WorkflowValue[ name, scope ], value ]
    ];

WorkflowValue /:
HoldPattern[ SetDelayed ][ WorkflowValue[ name_String, scope_ ], value_ ] :=
    catchTop @ setDelayedWFValue[ name, scope, value ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*$defaultScope*)
$defaultScope = "Job";

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*getWorkflowValue*)
getWorkflowValue[ name_, "Step" ] :=
    Module[ { value },
        value = Environment[ "PACLET_CICD_WF_VALUE_" <> wfHashString @ name ];
        If[ StringQ @ value,
            BinaryDeserialize @ BaseDecode @ value,
            Missing[ "WorkflowValueNotFound", name ]
        ]
    ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*setWorkflowValue*)
setWFValue[ name_, scope_ ] := Failure[ "NotImplemented", <| |> ];
(* TODO *)
setWFValue // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*setDelayedWFValue*)
setDelayedWFValue[ name_, scope_ ] := Failure[ "NotImplemented", <| |> ];
(* TODO *)
setDelayedWFValue // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*wfHashString*)
wfHashString[ name_ ] := ToUpperCase @ Hash[ name, Automatic, "HexString" ];
wfHashString // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Footer*)
End[ ];

EndPackage[ ];