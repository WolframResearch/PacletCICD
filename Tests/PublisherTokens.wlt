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
    Less[
        Quantity[ 0, "Minutes" ],
        Echo[ Now, "Now" ] - Echo[ token3[ "CreationDate" ], "CreationDate" ],
        Quantity[ 30, "Minutes" ]
    ],
    True,
    TestID -> "PublisherTokenObject-CreationDate-3@@Tests/PublisherTokens.wlt:205,1-213,2"
]

VerificationTest[
    token1[ "Creator" ],
    $CloudUserID,
    TestID -> "PublisherTokenObject-Creator@@Tests/PublisherTokens.wlt:215,1-219,2"
]

VerificationTest[
    token1[ "CreatorUUID" ],
    $CloudUserUUID,
    TestID -> "PublisherTokenObject-CreatorUUID@@Tests/PublisherTokens.wlt:221,1-225,2"
]

VerificationTest[
    token1[ "Dataset" ],
    _Dataset,
    SameTest -> MatchQ,
    TestID   -> "PublisherTokenObject-Dataset-1@@Tests/PublisherTokens.wlt:227,1-232,2"
]

VerificationTest[
    Normal[ token1[ "Dataset" ], Dataset ],
    Normal[ Dataset @ token1 @ All, Dataset ],
    TestID -> "PublisherTokenObject-Dataset-2@@Tests/PublisherTokens.wlt:234,1-238,2"
]

VerificationTest[
    Equal[ token4[ "ExpirationDate" ], token4 @ ExpirationDate, $expiration24 ],
    True,
    TestID -> "PublisherTokenObject-ExpirationDate@@Tests/PublisherTokens.wlt:240,1-244,2"
]

VerificationTest[
    token1[ "Name" ],
    None,
    TestID -> "PublisherTokenObject-Name-Anonymous@@Tests/PublisherTokens.wlt:246,1-250,2"
]

VerificationTest[
    token2[ "Name" ],
    $deleteMeName,
    TestID -> "PublisherTokenObject-Name-Named@@Tests/PublisherTokens.wlt:252,1-256,2"
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
    TestID   -> "PublisherTokenObject-Properties@@Tests/PublisherTokens.wlt:258,1-283,2"
]

VerificationTest[
    token1[ "PublisherID" ] === token1 @ PublisherID === $PublisherID,
    True,
    TestID -> "PublisherTokenObject-PublisherID@@Tests/PublisherTokens.wlt:285,1-289,2"
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
    TestID -> "PublisherTokenObject-PublisherUUID@@Tests/PublisherTokens.wlt:291,1-301,2"
]

VerificationTest[
    SameQ[
        token1[ "ResourceSystemBase" ],
        token1 @ ResourceSystemBase,
        $ResourceSystemBase
    ],
    True,
    TestID -> "PublisherTokenObject-ResourceSystemBase@@Tests/PublisherTokens.wlt:303,1-311,2"
]

VerificationTest[
    $tokenByteArray = token3[ "TokenByteArray" ],
    _ByteArray? ByteArrayQ,
    SameTest -> MatchQ,
    TestID   -> "PublisherTokenObject-TokenByteArray@@Tests/PublisherTokens.wlt:313,1-318,2"
]

VerificationTest[
    $tokenString = token3[ "TokenString" ],
    _String? StringQ,
    SameTest -> MatchQ,
    TestID   -> "PublisherTokenObject-TokenString@@Tests/PublisherTokens.wlt:320,1-325,2"
]

VerificationTest[
    token1[ "Version" ],
    (_Integer | _Real)? Positive,
    SameTest -> MatchQ,
    TestID   -> "PublisherTokenObject-Version@@Tests/PublisherTokens.wlt:327,1-332,2"
]

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Retrieval*)
VerificationTest[
    withoutToken @ PublisherTokenObject @ $deleteMeName,
    token2,
    TestID -> "PublisherTokenObject-Get-Name@@Tests/PublisherTokens.wlt:337,1-341,2"
]

VerificationTest[
    withoutToken @ PublisherTokenObject @ $tokenString,
    token3,
    TestID -> "PublisherTokenObject-Get-TokenString@@Tests/PublisherTokens.wlt:343,1-347,2"
]

VerificationTest[
    While[ $expirationNow + Quantity[ 1, "Seconds" ] < Now, Pause[ 1 ] ],
    Null,
    TimeConstraint -> 20,
    TestID         -> "PublisherTokenObject-Expiration-Wait@@Tests/PublisherTokens.wlt:349,1-354,2"
]

VerificationTest[
    Pause[ 3 ];
    withoutToken @ Quiet @ FailureQ @ PublisherTokenObject @ token5[ "TokenString" ],
    True,
    TestID -> "PublisherTokenObject-Expired@@Tests/PublisherTokens.wlt:356,1-361,2"
]

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*DeletePublisherToken*)
VerificationTest[
    withoutToken @ DeletePublisherToken @ token1,
    Success[ "TokenDeleted", _ ],
    SameTest -> MatchQ,
    TestID   -> "DeletePublisherToken-1@@Tests/PublisherTokens.wlt:366,1-371,2"
]

VerificationTest[
    withoutToken @ DeleteObject @ token2,
    Success[ "TokenDeleted", _ ],
    SameTest -> MatchQ,
    TestID   -> "DeletePublisherToken-DeleteObject@@Tests/PublisherTokens.wlt:373,1-378,2"
]

VerificationTest[
    withoutToken @ DeletePublisherToken @ { token3, token4 },
    { Success[ "TokenDeleted", _ ].. },
    SameTest -> MatchQ,
    TestID   -> "DeletePublisherToken-List@@Tests/PublisherTokens.wlt:380,1-385,2"
]

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Cleanup*)
VerificationTest[
    If[ Environment[ "TEST_CLOUD_ACCOUNT_USER" ] === $CloudUserID,
        CloudDisconnect[ ]
    ],
    Null,
    TestID -> "CloudDisconnect@@Tests/PublisherTokens.wlt:390,1-396,2"
]