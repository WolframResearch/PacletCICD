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

$tokenVersion = 1;

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Debug*)
Wolfram`PacletCICD`$Debug = False;

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Footer*)
End[ ];

EndPackage[ ];