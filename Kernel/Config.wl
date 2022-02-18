(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Header*)
BeginPackage[ "Wolfram`PacletCICD`" ];

Begin[ "`Private`" ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Config*)
$thisRepository    := "rhennigan/PacletCICD";
$thisPacletName    := "Wolfram/PacletCICD";

$thisPaclet        := $thisPaclet        = PacletObject[ $thisPacletName ];
$thisPacletDir     := $thisPacletDir     = $thisPaclet[ "Location"       ];
$thisPacletVersion := $thisPacletVersion = $thisPaclet[ "Version"        ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Debug*)
Wolfram`PacletCICD`$Debug = True;

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Footer*)
End[ ];

EndPackage[ ];