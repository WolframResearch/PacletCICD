#!/usr/bin/env wolframscript
(* ::Package:: *)

(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::SuspiciousSessionSymbol:: *)

print[ a___ ] := WriteString[ "stdout", a, "\n" ];

print[ "::group::Installing dependencies..." ];

pacletInstall[ url_ ] :=
    Module[ { installed },
        print[ "Installing paclet from: ", url ];
        installed = PacletInstall[ url, ForceVersionInstall -> True ];
        If[ PacletObjectQ @ installed,
            print[ "Paclet installed to: ", installed[ "Location" ] ],
            print[ "::error:: Paclet installation failed: ", installed ];
            Exit[ 1 ]
        ]
    ];

pacletInstall[ "https://www.wolframcloud.com/obj/rhennigan/Paclets/DefinitionNotebookClient.paclet" ];
pacletInstall[ "https://www.wolframcloud.com/obj/rhennigan/Paclets/PacletResource.paclet"           ];

print[ "::endgroup::" ];

(* :!CodeAnalysis::EndBlock:: *)