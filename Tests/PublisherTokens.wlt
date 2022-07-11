(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Initialization*)
VerificationTest[
    PacletObjectQ @ PacletObject @ File[
        $pacletDir =
            Module[ { root, mx },
                root = DirectoryName[ $TestFileName, 2 ];
                mx = FileNameJoin @ { root, "MXBuild", "Wolfram__PacletCICD" };
                If[ DirectoryQ @ mx, mx, root ]
            ]
    ],
    TestID -> "Initialize-PacletObject@@Tests/PublisherTokens.wlt:4,1-14,2"
]

(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::SuspiciousSessionSymbol:: *)
VerificationTest[
    Echo[ $TestFileName, "TestFileName" ];
    PacletDirectoryLoad @ Echo[ $pacletDir, "PacletDirectory" ],
    { ___, $pacletDir, ___ },
    SameTest -> MatchQ,
    TestID   -> "Initialize-PacletDirectoryLoad@@Tests/PublisherTokens.wlt:18,1-24,2"
]
(* :!CodeAnalysis::EndBlock:: *)

VerificationTest[
    Needs[ "Wolfram`PacletCICD`" ],
    Null,
    TestID -> "Initialize-Paclet-Needs@@Tests/PublisherTokens.wlt:27,1-31,2"
]

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Context Check*)
VerificationTest[
    Context @ CreatePublisherToken,
    "Wolfram`PacletCICD`",
    TestID -> "CreatePublisherToken-Context@@Tests/PublisherTokens.wlt:36,1-40,2"
]

VerificationTest[
    Context @ DeletePublisherToken,
    "Wolfram`PacletCICD`",
    TestID -> "DeletePublisherToken-Context@@Tests/PublisherTokens.wlt:42,1-46,2"
]

VerificationTest[
    Context @ PublisherTokenObject,
    "Wolfram`PacletCICD`",
    TestID -> "PublisherTokenObject-Context@@Tests/PublisherTokens.wlt:48,1-52,2"
]

VerificationTest[
    Context @ PublisherTokens,
    "Wolfram`PacletCICD`",
    TestID -> "PublisherTokens-Context@@Tests/PublisherTokens.wlt:54,1-58,2"
]

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Setup*)
VerificationTest[
    Needs[ "ResourceSystemClient`" -> None ],
    Null,
    TestID -> "Needs-ResourceSystemClient@@Tests/PublisherTokens.wlt:63,1-67,2"
]

VerificationTest[
    withoutToken // ClearAll;
    withoutToken // Attributes = { HoldFirst };
    withoutToken[ eval_ ] :=
        Block[ { ResourceSystemClient`$PublisherToken = None }, eval ],
    Null,
    TestID -> "WithoutToken-Definition@@Tests/PublisherTokens.wlt:69,1-76,2"
]

VerificationTest[
    With[ { rsb = Environment[ "RESOURCE_SYSTEM_BASE" ] },
        If[ StringQ @ rsb, $ResourceSystemBase = rsb, $ResourceSystemBase ]
    ],
    _String? StringQ,
    SameTest -> MatchQ,
    TestID   -> "ResourceSystemBase@@Tests/PublisherTokens.wlt:78,1-85,2"
]

VerificationTest[
    With[
        {
            user = Environment[ "TEST_CLOUD_ACCOUNT_USER" ],
            pass = Environment[ "TEST_CLOUD_ACCOUNT_PASSWORD" ]
        },
        Switch[ { StringQ @ user, StringQ @ pass },
                { True, True  }, CloudConnect[ user, pass ],
                { True, False }, CloudConnect[ user ],
                _              , CloudConnect[ ]
        ]
    ],
    _String? StringQ,
    SameTest -> MatchQ,
    TestID   -> "CloudConnect@@Tests/PublisherTokens.wlt:87,1-102,2"
]

VerificationTest[
    With[ { publisher = Environment[ "TEST_PUBLISHER_ID" ] },
        $PublisherID = If[ StringQ @ publisher,
                           publisher,
                           ResourceSystemClient`GetDefaultPublisherID[ ]
                       ]
    ],
    _String? StringQ,
    SameTest -> MatchQ,
    TestID   -> "PublisherID@@Tests/PublisherTokens.wlt:104,1-114,2"
]

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*CreatePublisherToken*)
VerificationTest[
    token1 = withoutToken @ CreatePublisherToken[ ],
    _PublisherTokenObject,
    SameTest -> MatchQ,
    TestID   -> "CreatePublisherToken-Anonymous@@Tests/PublisherTokens.wlt:119,1-124,2"
]

VerificationTest[
    token2 = withoutToken @
        CreatePublisherToken[ $deleteMeName = CreateUUID[ "DeleteMe" ] ],
    _PublisherTokenObject,
    SameTest -> MatchQ,
    TestID   -> "CreatePublisherToken-Named@@Tests/PublisherTokens.wlt:126,1-132,2"
]

VerificationTest[
    token3 =
        withoutToken @ CreatePublisherToken[
            "AllowedEndpoints" -> { "CheckPublisherToken", "DefaultPublisher" }
        ],
    _PublisherTokenObject,
    SameTest -> MatchQ,
    TestID   -> "CreatePublisherToken-AllowedEndpoints@@Tests/PublisherTokens.wlt:134,1-142,2"
]

VerificationTest[
    $expiration24 = Now + Quantity[ 24, "Hours" ],
    _DateObject? DateObjectQ,
    SameTest -> MatchQ,
    TestID   -> "Define-ExpirationDate@@Tests/PublisherTokens.wlt:144,1-149,2"
]

VerificationTest[
    token4 = withoutToken @ CreatePublisherToken[ ExpirationDate -> $expiration24 ],
    _PublisherTokenObject,
    SameTest -> MatchQ,
    TestID   -> "CreatePublisherToken-ExpirationDate@@Tests/PublisherTokens.wlt:151,1-156,2"
]

VerificationTest[
    $expirationNow = Now + Quantity[ 5, "Seconds" ],
    _DateObject? DateObjectQ,
    SameTest -> MatchQ,
    TestID   -> "Define-ExpirationDate-Now@@Tests/PublisherTokens.wlt:158,1-163,2"
]

VerificationTest[
    token5 = withoutToken @ CreatePublisherToken[ ExpirationDate -> $expirationNow ],
    _PublisherTokenObject,
    SameTest -> MatchQ,
    TestID   -> "CreatePublisherToken-ExpirationDate-Now@@Tests/PublisherTokens.wlt:165,1-170,2"
]

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*PublisherTokenObject*)

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Properties*)
VerificationTest[
    token1 @ All,
    _Association? AssociationQ,
    SameTest -> MatchQ,
    TestID   -> "PublisherTokenObject-All@@Tests/PublisherTokens.wlt:179,1-184,2"
]

VerificationTest[
    token3[ "AllowedEndpoints" ],
    { "CheckPublisherToken", "DefaultPublisher" },
    TestID -> "PublisherTokenObject-AllowedEndpoints@@Tests/PublisherTokens.wlt:186,1-190,2"
]

VerificationTest[
    token3[ "AllowedURLs" ],
    {
        URL @ URLBuild @ { $ResourceSystemBase, "CheckPublisherToken" },
        URL @ URLBuild @ { $ResourceSystemBase, "DefaultPublisher" }
    },
    TestID -> "PublisherTokenObject-AllowedURLs@@Tests/PublisherTokens.wlt:192,1-199,2"
]

VerificationTest[
    token3[ "CreationDate" ],
    _DateObject? DateObjectQ,
    SameTest -> MatchQ,
    TestID   -> "PublisherTokenObject-CreationDate-1@@Tests/PublisherTokens.wlt:201,1-206,2"
]

VerificationTest[
    Equal[ token3[ "CreationDate" ][ "TimeZone" ], 0 ],
    True,
    TestID -> "PublisherTokenObject-CreationDate-2@@Tests/PublisherTokens.wlt:208,1-212,2"
]

VerificationTest[
    Now - token3[ "CreationDate" ],
    t_ /; Less[ Quantity[ -5, "Seconds" ], t, Quantity[ 5, "Minutes" ] ],
    SameTest -> MatchQ,
    TestID   -> "PublisherTokenObject-CreationDate-3@@Tests/PublisherTokens.wlt:214,1-219,2"
]

VerificationTest[
    token1[ "Creator" ],
    $CloudUserID,
    TestID -> "PublisherTokenObject-Creator@@Tests/PublisherTokens.wlt:221,1-225,2"
]

VerificationTest[
    token1[ "CreatorUUID" ],
    $CloudUserUUID,
    TestID -> "PublisherTokenObject-CreatorUUID@@Tests/PublisherTokens.wlt:227,1-231,2"
]

VerificationTest[
    token1[ "Dataset" ],
    _Dataset,
    SameTest -> MatchQ,
    TestID   -> "PublisherTokenObject-Dataset-1@@Tests/PublisherTokens.wlt:233,1-238,2"
]

VerificationTest[
    Normal[ token1[ "Dataset" ], Dataset ],
    Normal[ Dataset @ token1 @ All, Dataset ],
    TestID -> "PublisherTokenObject-Dataset-2@@Tests/PublisherTokens.wlt:240,1-244,2"
]

VerificationTest[
    Equal[ token4[ "ExpirationDate" ], token4 @ ExpirationDate, $expiration24 ],
    True,
    TestID -> "PublisherTokenObject-ExpirationDate@@Tests/PublisherTokens.wlt:246,1-250,2"
]

VerificationTest[
    token1[ "Name" ],
    None,
    TestID -> "PublisherTokenObject-Name-Anonymous@@Tests/PublisherTokens.wlt:252,1-256,2"
]

VerificationTest[
    token2[ "Name" ],
    $deleteMeName,
    TestID -> "PublisherTokenObject-Name-Named@@Tests/PublisherTokens.wlt:258,1-262,2"
]

VerificationTest[
    token1[ "Properties" ],
    {
        OrderlessPatternSequence[
            "AllowedEndpoints",
            "AllowedURLs",
            "ClickToCopy",
            "CreationDate",
            "Creator",
            "CreatorUUID",
            "Dataset",
            "ExpirationDate",
            "Name",
            "Properties",
            "PublisherID",
            "PublisherUUID",
            "ResourceSystemBase",
            "TokenByteArray",
            "TokenString",
            "Version",
            ___
        ]
    },
    SameTest -> MatchQ,
    TestID   -> "PublisherTokenObject-Properties@@Tests/PublisherTokens.wlt:264,1-289,2"
]

VerificationTest[
    token1[ "PublisherID" ] === token1 @ PublisherID === $PublisherID,
    True,
    TestID -> "PublisherTokenObject-PublisherID@@Tests/PublisherTokens.wlt:291,1-295,2"
]

VerificationTest[
    token1[ "PublisherUUID" ],
    withoutToken @ ResourceSystemClient`ResourceSystemExecute[
        "PublisherInformation",
        { "PublisherID" -> $PublisherID }
    ][
        "AccountInformation",
        "UUID"
    ],
    TestID -> "PublisherTokenObject-PublisherUUID@@Tests/PublisherTokens.wlt:297,1-307,2"
]

VerificationTest[
    SameQ[
        token1[ "ResourceSystemBase" ],
        token1 @ ResourceSystemBase,
        $ResourceSystemBase
    ],
    True,
    TestID -> "PublisherTokenObject-ResourceSystemBase@@Tests/PublisherTokens.wlt:309,1-317,2"
]

VerificationTest[
    $tokenByteArray = token3[ "TokenByteArray" ],
    _ByteArray? ByteArrayQ,
    SameTest -> MatchQ,
    TestID   -> "PublisherTokenObject-TokenByteArray@@Tests/PublisherTokens.wlt:319,1-324,2"
]

VerificationTest[
    $tokenString = token3[ "TokenString" ],
    _String? StringQ,
    SameTest -> MatchQ,
    TestID   -> "PublisherTokenObject-TokenString@@Tests/PublisherTokens.wlt:326,1-331,2"
]

VerificationTest[
    token1[ "Version" ],
    (_Integer | _Real)? Positive,
    SameTest -> MatchQ,
    TestID   -> "PublisherTokenObject-Version@@Tests/PublisherTokens.wlt:333,1-338,2"
]

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Retrieval*)
VerificationTest[
    withoutToken @ PublisherTokenObject @ $deleteMeName,
    token2,
    TestID -> "PublisherTokenObject-Get-Name@@Tests/PublisherTokens.wlt:343,1-347,2"
]

VerificationTest[
    withoutToken @ PublisherTokenObject @ $tokenString,
    token3,
    TestID -> "PublisherTokenObject-Get-TokenString@@Tests/PublisherTokens.wlt:349,1-353,2"
]

VerificationTest[
    While[ $expirationNow + Quantity[ 1, "Seconds" ] < Now, Pause[ 1 ] ],
    Null,
    TimeConstraint -> 20,
    TestID         -> "PublisherTokenObject-Expiration-Wait@@Tests/PublisherTokens.wlt:355,1-360,2"
]

VerificationTest[
    Pause[ 10 ];
    withoutToken @ Quiet @ FailureQ @ PublisherTokenObject @ token5[ "TokenString" ],
    True,
    TestID -> "PublisherTokenObject-Expired@@Tests/PublisherTokens.wlt:362,1-367,2"
]

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*StageSubmissionFile*)
VerificationTest[
    token6 =
        withoutToken @ CreatePublisherToken[
            "AllowedEndpoints" -> {
                "CheckPublisherToken",
                "StageSubmissionFile"
            }
        ],
    _PublisherTokenObject,
    SameTest -> MatchQ,
    TestID   -> "StageSubmissionFile-Token@@Tests/PublisherTokens.wlt:372,1-383,2"
]

VerificationTest[
    $stagingName = CreateUUID[ "Test-" ],
    _String? StringQ,
    SameTest -> MatchQ,
    TestID   -> "Define-Staging-Name@@Tests/PublisherTokens.wlt:385,1-390,2"
]

VerificationTest[
    $localNB = File @ FileNameJoin @ {
        $pacletDir,
        "Documentation",
        "English",
        "Guides",
        "Workflows.nb"
    },
    _File? FileExistsQ,
    SameTest -> MatchQ,
    TestID   -> "StageSubmissionFile-Notebook-File@@Tests/PublisherTokens.wlt:392,1-403,2"
]

VerificationTest[
    stagedNB =
        ResourceSystemClient`ResourceSystemExecute[
            "StageSubmissionFile",
            {
                "Name"       -> $stagingName,
                "Key"        -> "Test",
                "Initialize" -> True,
                "Body"       -> <| "SubmissionFile" -> $localNB |>
            },
            "PublisherToken" -> token6[ "TokenString" ]
        ],
    _CloudObject,
    SameTest -> MatchQ,
    TestID   -> "StageSubmissionFile-NB@@Tests/PublisherTokens.wlt:405,1-420,2"
]

VerificationTest[
    Information[ stagedNB, "MIMEType" ],
    "application/vnd.wolfram.notebook",
    TestID -> "StageSubmissionFile-NB-MIMEType-1@@Tests/PublisherTokens.wlt:422,1-426,2"
]

VerificationTest[
    stagedWL =
        ResourceSystemClient`ResourceSystemExecute[
            "StageSubmissionFile",
            {
                "Name"       -> $stagingName,
                "Key"        -> "Test",
                "Initialize" -> False,
                "Body"       -> <|
                    "SubmissionFile" -> File @ FileNameJoin @ {
                        $pacletDir,
                        "PacletInfo.wl"
                    }
                |>
            },
            "PublisherToken" -> token6[ "TokenString" ]
        ],
    _CloudObject,
    SameTest -> MatchQ,
    TestID   -> "StageSubmissionFile-WL@@Tests/PublisherTokens.wlt:428,1-448,2"
]

VerificationTest[
    Information[ stagedWL, "MIMEType" ],
    "application/vnd.wolfram.wl",
    TestID -> "StageSubmissionFile-WL-MIMEType@@Tests/PublisherTokens.wlt:450,1-454,2"
]

VerificationTest[
    Information[ stagedNB, "MIMEType" ],
    "application/vnd.wolfram.notebook",
    TestID -> "StageSubmissionFile-NB-MIMEType-2@@Tests/PublisherTokens.wlt:456,1-460,2"
]

VerificationTest[
    ResourceSystemClient`ResourceSystemExecute[
        "StageSubmissionFile",
        {
            "Name"           -> $stagingName,
            "Key"            -> "Test",
            "Initialize"     -> True,
            "SubmissionFile" -> None
        },
        "PublisherToken" -> token6[ "TokenString" ]
    ],
    _CloudObject,
    SameTest -> MatchQ,
    TestID   -> "StageSubmissionFile-Clear-1@@Tests/PublisherTokens.wlt:462,1-476,2"
]

VerificationTest[
    FileExistsQ @ stagedWL,
    False,
    TestID -> "StageSubmissionFile-Clear-2@@Tests/PublisherTokens.wlt:478,1-482,2"
]

VerificationTest[
    FileExistsQ @ stagedNB,
    False,
    TestID -> "StageSubmissionFile-Clear-3@@Tests/PublisherTokens.wlt:484,1-488,2"
]

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*DeletePublisherToken*)
VerificationTest[
    withoutToken @ DeletePublisherToken @ token1,
    Success[ "TokenDeleted", _ ],
    SameTest -> MatchQ,
    TestID   -> "DeletePublisherToken-1@@Tests/PublisherTokens.wlt:493,1-498,2"
]

VerificationTest[
    withoutToken @ DeleteObject @ token2,
    Success[ "TokenDeleted", _ ],
    SameTest -> MatchQ,
    TestID   -> "DeletePublisherToken-DeleteObject@@Tests/PublisherTokens.wlt:500,1-505,2"
]

VerificationTest[
    withoutToken @ DeletePublisherToken @ { token3, token4, token6 },
    { Success[ "TokenDeleted", _ ].. },
    SameTest -> MatchQ,
    TestID   -> "DeletePublisherToken-List@@Tests/PublisherTokens.wlt:507,1-512,2"
]

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Cleanup*)
VerificationTest[
    If[ Environment[ "TEST_CLOUD_ACCOUNT_USER" ] === $CloudUserID,
        CloudDisconnect[ ]
    ],
    Null,
    TestID -> "CloudDisconnect@@Tests/PublisherTokens.wlt:517,1-523,2"
]