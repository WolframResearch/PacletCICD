(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Initialization*)
VerificationTest[
    PacletObjectQ @ PacletObject @ File[
        $pacletDir = DirectoryName[ $TestFileName, 2 ]
    ],
    TestID -> "Initialize-Paclet-Directory@@Tests/PublisherTokens.wlt:4,1-9,2"
]

VerificationTest[
    PacletDirectoryLoad @ $pacletDir,
    { ___, $pacletDir, ___ },
    SameTest -> MatchQ,
    TestID -> "Initialize-Paclet-Load@@Tests/PublisherTokens.wlt:11,1-16,2"
]

VerificationTest[
    Needs[ "Wolfram`PacletCICD`" ],
    Null,
    TestID -> "Initialize-Paclet-Needs@@Tests/PublisherTokens.wlt:18,1-22,2"
]

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Context Check*)
VerificationTest[
    Context @ CreatePublisherToken,
    "Wolfram`PacletCICD`",
    TestID -> "CreatePublisherToken-Context@@Tests/PublisherTokens.wlt:27,1-31,2"
]

VerificationTest[
    Context @ DeletePublisherToken,
    "Wolfram`PacletCICD`",
    TestID -> "DeletePublisherToken-Context@@Tests/PublisherTokens.wlt:33,1-37,2"
]

VerificationTest[
    Context @ PublisherTokenObject,
    "Wolfram`PacletCICD`",
    TestID -> "PublisherTokenObject-Context@@Tests/PublisherTokens.wlt:39,1-43,2"
]

VerificationTest[
    Context @ PublisherTokens,
    "Wolfram`PacletCICD`",
    TestID -> "PublisherTokens-Context@@Tests/PublisherTokens.wlt:45,1-49,2"
]

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Setup*)
VerificationTest[
    Needs[ "ResourceSystemClient`" -> None ],
    Null,
    TestID -> "Needs-ResourceSystemClient@@Tests/PublisherTokens.wlt:54,1-58,2"
]

VerificationTest[
    withoutToken // ClearAll;
    withoutToken // Attributes = { HoldFirst };
    withoutToken[ eval_ ] :=
        Block[ { ResourceSystemClient`$PublisherToken = None }, eval ],
    Null,
    TestID -> "WithoutToken-Definition@@Tests/PublisherTokens.wlt:60,1-67,2"
]

VerificationTest[
    With[ { rsb = Environment[ "RESOURCE_SYSTEM_BASE" ] },
        If[ StringQ @ rsb, $ResourceSystemBase = rsb, $ResourceSystemBase ]
    ],
    _String? StringQ,
    SameTest -> MatchQ,
    TestID   -> "ResourceSystemBase@@Tests/PublisherTokens.wlt:69,1-76,2"
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
    TestID   -> "CloudConnect@@Tests/PublisherTokens.wlt:78,1-93,2"
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
    TestID   -> "PublisherID@@Tests/PublisherTokens.wlt:95,1-105,2"
]

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*CreatePublisherToken*)
VerificationTest[
    token1 = withoutToken @ CreatePublisherToken[ ],
    _PublisherTokenObject,
    SameTest -> MatchQ,
    TestID   -> "CreatePublisherToken-Anonymous@@Tests/PublisherTokens.wlt:110,1-115,2"
]

VerificationTest[
    token2 = withoutToken @
        CreatePublisherToken[ $deleteMeName = CreateUUID[ "DeleteMe" ] ],
    _PublisherTokenObject,
    SameTest -> MatchQ,
    TestID   -> "CreatePublisherToken-Named@@Tests/PublisherTokens.wlt:117,1-123,2"
]

VerificationTest[
    token3 =
        withoutToken @ CreatePublisherToken[
            "AllowedEndpoints" -> { "CheckPublisherToken", "DefaultPublisher" }
        ],
    _PublisherTokenObject,
    SameTest -> MatchQ,
    TestID   -> "CreatePublisherToken-AllowedEndpoints@@Tests/PublisherTokens.wlt:125,1-133,2"
]

VerificationTest[
    $expiration24 = Now + Quantity[ 24, "Hours" ],
    _DateObject? DateObjectQ,
    SameTest -> MatchQ,
    TestID   -> "Define-ExpirationDate@@Tests/PublisherTokens.wlt:135,1-140,2"
]

VerificationTest[
    token4 = withoutToken @ CreatePublisherToken[ ExpirationDate -> $expiration24 ],
    _PublisherTokenObject,
    SameTest -> MatchQ,
    TestID   -> "CreatePublisherToken-ExpirationDate@@Tests/PublisherTokens.wlt:142,1-147,2"
]

VerificationTest[
    $expirationNow = Now + Quantity[ 5, "Seconds" ],
    _DateObject? DateObjectQ,
    SameTest -> MatchQ,
    TestID   -> "Define-ExpirationDate-Now@@Tests/PublisherTokens.wlt:149,1-154,2"
]

VerificationTest[
    token5 = withoutToken @ CreatePublisherToken[ ExpirationDate -> $expirationNow ],
    _PublisherTokenObject,
    SameTest -> MatchQ,
    TestID   -> "CreatePublisherToken-ExpirationDate-Now@@Tests/PublisherTokens.wlt:156,1-161,2"
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
    TestID   -> "PublisherTokenObject-All@@Tests/PublisherTokens.wlt:170,1-175,2"
]

VerificationTest[
    token3[ "AllowedEndpoints" ],
    { "CheckPublisherToken", "DefaultPublisher" },
    TestID -> "PublisherTokenObject-AllowedEndpoints@@Tests/PublisherTokens.wlt:177,1-181,2"
]

VerificationTest[
    token3[ "AllowedURLs" ],
    {
        URL @ URLBuild @ { $ResourceSystemBase, "CheckPublisherToken" },
        URL @ URLBuild @ { $ResourceSystemBase, "DefaultPublisher" }
    },
    TestID -> "PublisherTokenObject-AllowedURLs@@Tests/PublisherTokens.wlt:183,1-190,2"
]

VerificationTest[
    token3[ "CreationDate" ],
    _DateObject? DateObjectQ,
    SameTest -> MatchQ,
    TestID   -> "PublisherTokenObject-CreationDate-1@@Tests/PublisherTokens.wlt:192,1-197,2"
]

VerificationTest[
    Equal[ token3[ "CreationDate" ][ "TimeZone" ], 0 ],
    True,
    TestID -> "PublisherTokenObject-CreationDate-2@@Tests/PublisherTokens.wlt:199,1-203,2"
]

VerificationTest[
    Now - token3[ "CreationDate" ],
    t_ /; Less[ Quantity[ -5, "Seconds" ], t, Quantity[ 5, "Minutes" ] ],
    SameTest -> MatchQ,
    TestID   -> "PublisherTokenObject-CreationDate-3@@Tests/PublisherTokens.wlt:205,1-210,2"
]

VerificationTest[
    token1[ "Creator" ],
    $CloudUserID,
    TestID -> "PublisherTokenObject-Creator@@Tests/PublisherTokens.wlt:212,1-216,2"
]

VerificationTest[
    token1[ "CreatorUUID" ],
    $CloudUserUUID,
    TestID -> "PublisherTokenObject-CreatorUUID@@Tests/PublisherTokens.wlt:218,1-222,2"
]

VerificationTest[
    token1[ "Dataset" ],
    _Dataset,
    SameTest -> MatchQ,
    TestID   -> "PublisherTokenObject-Dataset-1@@Tests/PublisherTokens.wlt:224,1-229,2"
]

VerificationTest[
    Normal[ token1[ "Dataset" ], Dataset ],
    Normal[ Dataset @ token1 @ All, Dataset ],
    TestID -> "PublisherTokenObject-Dataset-2@@Tests/PublisherTokens.wlt:231,1-235,2"
]

VerificationTest[
    Equal[ token4[ "ExpirationDate" ], token4 @ ExpirationDate, $expiration24 ],
    True,
    TestID -> "PublisherTokenObject-ExpirationDate@@Tests/PublisherTokens.wlt:237,1-241,2"
]

VerificationTest[
    token1[ "Name" ],
    None,
    TestID -> "PublisherTokenObject-Name-Anonymous@@Tests/PublisherTokens.wlt:243,1-247,2"
]

VerificationTest[
    token2[ "Name" ],
    $deleteMeName,
    TestID -> "PublisherTokenObject-Name-Named@@Tests/PublisherTokens.wlt:249,1-253,2"
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
    TestID   -> "PublisherTokenObject-Properties@@Tests/PublisherTokens.wlt:255,1-280,2"
]

VerificationTest[
    token1[ "PublisherID" ] === token1 @ PublisherID === $PublisherID,
    True,
    TestID -> "PublisherTokenObject-PublisherID@@Tests/PublisherTokens.wlt:282,1-286,2"
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
    TestID -> "PublisherTokenObject-PublisherUUID@@Tests/PublisherTokens.wlt:288,1-298,2"
]

VerificationTest[
    SameQ[
        token1[ "ResourceSystemBase" ],
        token1 @ ResourceSystemBase,
        $ResourceSystemBase
    ],
    True,
    TestID -> "PublisherTokenObject-ResourceSystemBase@@Tests/PublisherTokens.wlt:300,1-308,2"
]

VerificationTest[
    $tokenByteArray = token3[ "TokenByteArray" ],
    _ByteArray? ByteArrayQ,
    SameTest -> MatchQ,
    TestID   -> "PublisherTokenObject-TokenByteArray@@Tests/PublisherTokens.wlt:310,1-315,2"
]

VerificationTest[
    $tokenString = token3[ "TokenString" ],
    _String? StringQ,
    SameTest -> MatchQ,
    TestID   -> "PublisherTokenObject-TokenString@@Tests/PublisherTokens.wlt:317,1-322,2"
]

VerificationTest[
    token1[ "Version" ],
    (_Integer | _Real)? Positive,
    SameTest -> MatchQ,
    TestID   -> "PublisherTokenObject-Version@@Tests/PublisherTokens.wlt:324,1-329,2"
]

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Retrieval*)
VerificationTest[
    withoutToken @ PublisherTokenObject @ $deleteMeName,
    token2,
    TestID -> "PublisherTokenObject-Get-Name@@Tests/PublisherTokens.wlt:334,1-338,2"
]

VerificationTest[
    withoutToken @ PublisherTokenObject @ $tokenString,
    token3,
    TestID -> "PublisherTokenObject-Get-TokenString@@Tests/PublisherTokens.wlt:340,1-344,2"
]

VerificationTest[
    While[ $expirationNow + Quantity[ 1, "Seconds" ] < Now, Pause[ 1 ] ],
    Null,
    TimeConstraint -> 20,
    TestID         -> "PublisherTokenObject-Expiration-Wait@@Tests/PublisherTokens.wlt:346,1-351,2"
]

VerificationTest[
    Pause[ 3 ];
    withoutToken @ Quiet @ FailureQ @ PublisherTokenObject @ token5[ "TokenString" ],
    True,
    TestID -> "PublisherTokenObject-Expired@@Tests/PublisherTokens.wlt:353,1-358,2"
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
    TestID   -> "StageSubmissionFile-Token@@Tests/PublisherTokens.wlt:363,1-374,2"
]

VerificationTest[
    $stagingName = CreateUUID[ "Test-" ],
    _String? StringQ,
    SameTest -> MatchQ,
    TestID   -> "Define-Staging-Name@@Tests/PublisherTokens.wlt:376,1-381,2"
]

VerificationTest[
    stagedNB =
        ResourceSystemClient`ResourceSystemExecute[
            "StageSubmissionFile",
            {
                "Name"       -> $stagingName,
                "Key"        -> "Test",
                "Initialize" -> True,
                "Body"       -> <|
                    "SubmissionFile" -> File @ FileNameJoin @ {
                        $pacletDir,
                        "ResourceDefinition.nb"
                    }
                |>
            },
            "PublisherToken" -> token6[ "TokenString" ]
        ],
    _CloudObject,
    SameTest -> MatchQ,
    TestID   -> "StageSubmissionFile-NB@@Tests/PublisherTokens.wlt:383,1-403,2"
]

VerificationTest[
    Information[ stagedNB, "MIMEType" ],
    "application/vnd.wolfram.notebook",
    TestID -> "StageSubmissionFile-NB-MIMEType-1@@Tests/PublisherTokens.wlt:405,1-409,2"
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
    TestID   -> "StageSubmissionFile-WL@@Tests/PublisherTokens.wlt:411,1-431,2"
]

VerificationTest[
    Information[ stagedWL, "MIMEType" ],
    "application/vnd.wolfram.wl",
    TestID -> "StageSubmissionFile-WL-MIMEType@@Tests/PublisherTokens.wlt:433,1-437,2"
]

VerificationTest[
    Information[ stagedNB, "MIMEType" ],
    "application/vnd.wolfram.notebook",
    TestID -> "StageSubmissionFile-NB-MIMEType-2@@Tests/PublisherTokens.wlt:439,1-443,2"
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
    TestID   -> "StageSubmissionFile-Clear-1@@Tests/PublisherTokens.wlt:445,1-459,2"
]

VerificationTest[
    FileExistsQ @ stagedWL,
    False,
    TestID -> "StageSubmissionFile-Clear-2@@Tests/PublisherTokens.wlt:461,1-465,2"
]

VerificationTest[
    FileExistsQ @ stagedNB,
    False,
    TestID -> "StageSubmissionFile-Clear-3@@Tests/PublisherTokens.wlt:467,1-471,2"
]

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*DeletePublisherToken*)
VerificationTest[
    withoutToken @ DeletePublisherToken @ token1,
    Success[ "TokenDeleted", _ ],
    SameTest -> MatchQ,
    TestID   -> "DeletePublisherToken-1@@Tests/PublisherTokens.wlt:476,1-481,2"
]

VerificationTest[
    withoutToken @ DeleteObject @ token2,
    Success[ "TokenDeleted", _ ],
    SameTest -> MatchQ,
    TestID   -> "DeletePublisherToken-DeleteObject@@Tests/PublisherTokens.wlt:483,1-488,2"
]

VerificationTest[
    withoutToken @ DeletePublisherToken @ { token3, token4, token6 },
    { Success[ "TokenDeleted", _ ].. },
    SameTest -> MatchQ,
    TestID   -> "DeletePublisherToken-List@@Tests/PublisherTokens.wlt:490,1-495,2"
]

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Cleanup*)
VerificationTest[
    If[ Environment[ "TEST_CLOUD_ACCOUNT_USER" ] === $CloudUserID,
        CloudDisconnect[ ]
    ],
    Null,
    TestID -> "CloudDisconnect@@Tests/PublisherTokens.wlt:500,1-506,2"
]