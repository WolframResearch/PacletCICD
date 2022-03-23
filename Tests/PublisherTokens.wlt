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
    With[ { rsb = Environment[ "RESOURCE_SYSTEM_BASE" ] },
        If[ StringQ @ rsb, $ResourceSystemBase = rsb, $ResourceSystemBase ]
    ],
    _String? StringQ,
    SameTest -> MatchQ,
    TestID   -> "ResourceSystemBase@@Tests/PublisherTokens.wlt:60,1-67,2"
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
    TestID   -> "CloudConnect@@Tests/PublisherTokens.wlt:69,1-84,2"
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
    TestID   -> "PublisherID@@Tests/PublisherTokens.wlt:86,1-96,2"
]

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*CreatePublisherToken*)
VerificationTest[
    token1 = CreatePublisherToken[ ],
    _PublisherTokenObject,
    SameTest -> MatchQ,
    TestID   -> "CreatePublisherToken-Anonymous@@Tests/PublisherTokens.wlt:101,1-106,2"
]

VerificationTest[
    token2 = CreatePublisherToken[ $deleteMeName = CreateUUID[ "DeleteMe" ] ],
    _PublisherTokenObject,
    SameTest -> MatchQ,
    TestID   -> "CreatePublisherToken-Named@@Tests/PublisherTokens.wlt:108,1-113,2"
]

VerificationTest[
    token3 =
        CreatePublisherToken[
            "AllowedEndpoints" -> { "CheckPublisherToken", "DefaultPublisher" }
        ],
    _PublisherTokenObject,
    SameTest -> MatchQ,
    TestID   -> "CreatePublisherToken-AllowedEndpoints@@Tests/PublisherTokens.wlt:115,1-123,2"
]

VerificationTest[
    $expiration24 = Now + Quantity[ 24, "Hours" ],
    _DateObject? DateObjectQ,
    SameTest -> MatchQ,
    TestID   -> "Define-ExpirationDate@@Tests/PublisherTokens.wlt:125,1-130,2"
]

VerificationTest[
    token4 = CreatePublisherToken[ ExpirationDate -> $expiration24 ],
    _PublisherTokenObject,
    SameTest -> MatchQ,
    TestID   -> "CreatePublisherToken-ExpirationDate@@Tests/PublisherTokens.wlt:132,1-137,2"
]

VerificationTest[
    $expirationNow = Now + Quantity[ 5, "Seconds" ],
    _DateObject? DateObjectQ,
    SameTest -> MatchQ,
    TestID   -> "Define-ExpirationDate-Now@@Tests/PublisherTokens.wlt:139,1-144,2"
]

VerificationTest[
    token5 = CreatePublisherToken[ ExpirationDate -> $expirationNow ],
    _PublisherTokenObject,
    SameTest -> MatchQ,
    TestID   -> "CreatePublisherToken-ExpirationDate-Now@@Tests/PublisherTokens.wlt:146,1-151,2"
]

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*PublisherTokenObject*)

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Properties*)
VerificationTest[
    token1[ All ],
    _Association? AssociationQ,
    SameTest -> MatchQ,
    TestID   -> "PublisherTokenObject-All@@Tests/PublisherTokens.wlt:160,1-165,2"
]

VerificationTest[
    token3[ "AllowedEndpoints" ],
    { "CheckPublisherToken", "DefaultPublisher" },
    TestID -> "PublisherTokenObject-AllowedEndpoints@@Tests/PublisherTokens.wlt:167,1-171,2"
]

VerificationTest[
    token3[ "AllowedURLs" ],
    {
        URL @ URLBuild[ { $ResourceSystemBase, "CheckPublisherToken" } ],
        URL @ URLBuild[ { $ResourceSystemBase, "DefaultPublisher"    } ]
    },
    TestID -> "PublisherTokenObject-AllowedURLs@@Tests/PublisherTokens.wlt:173,1-180,2"
]

VerificationTest[
    token3[ "CreationDate" ],
    _DateObject? DateObjectQ,
    TestID -> "PublisherTokenObject-CreationDate-1@@Tests/PublisherTokens.wlt:182,1-186,2"
]

VerificationTest[
    token3[ "CreationDate" ][ "TimeZone" ] == 0,
    True,
    TestID -> "PublisherTokenObject-CreationDate-2@@Tests/PublisherTokens.wlt:188,1-192,2"
]

VerificationTest[
    Less[ Quantity[ 0, "Minutes" ],
          Now - token3[ "CreationDate" ],
          Quantity[ 30, "Minutes" ]
    ],
    True,
    TestID -> "PublisherTokenObject-CreationDate-3@@Tests/PublisherTokens.wlt:194,1-201,2"
]

VerificationTest[
    token1[ "Creator" ],
    $CloudUserID,
    TestID -> "PublisherTokenObject-Creator@@Tests/PublisherTokens.wlt:203,1-207,2"
]

VerificationTest[
    token1[ "CreatorUUID" ],
    $CloudUserUUID,
    TestID -> "PublisherTokenObject-CreatorUUID@@Tests/PublisherTokens.wlt:209,1-213,2"
]

VerificationTest[
    token1[ "Dataset" ],
    _Dataset,
    TestID -> "PublisherTokenObject-Dataset-1@@Tests/PublisherTokens.wlt:215,1-219,2"
]

VerificationTest[
    Normal[ token1[ "Dataset" ], Dataset ],
    Normal[ Dataset @ token1 @ All, Dataset ],
    TestID -> "PublisherTokenObject-Dataset-2@@Tests/PublisherTokens.wlt:221,1-225,2"
]

VerificationTest[
    token4[ "ExpirationDate" ] == token4[ ExpirationDate ] == $expiration24,
    True,
    TestID -> "PublisherTokenObject-ExpirationDate@@Tests/PublisherTokens.wlt:227,1-231,2"
]

VerificationTest[
    token1[ "Name" ],
    None,
    TestID -> "PublisherTokenObject-Name-Anonymous@@Tests/PublisherTokens.wlt:233,1-237,2"
]

VerificationTest[
    token2[ "Name" ],
    $deleteMeName,
    TestID -> "PublisherTokenObject-Name-Named@@Tests/PublisherTokens.wlt:239,1-243,2"
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
    TestID   -> "PublisherTokenObject-Properties@@Tests/PublisherTokens.wlt:245,1-270,2"
]

VerificationTest[
    token1[ "PublisherID" ] === token1[ PublisherID ] === $PublisherID,
    True,
    TestID -> "PublisherTokenObject-PublisherID@@Tests/PublisherTokens.wlt:272,1-276,2"
]

VerificationTest[
    token1[ "PublisherUUID" ],
    ResourceSystemClient`ResourceSystemExecute[
        "PublisherInformation",
        { "PublisherID" -> $PublisherID }
    ][
        "AccountInformation",
        "UUID"
    ],
    TestID -> "PublisherTokenObject-PublisherUUID@@Tests/PublisherTokens.wlt:278,1-288,2"
]

VerificationTest[
    SameQ[ token1[ "ResourceSystemBase" ],
           token1[ ResourceSystemBase ],
           $ResourceSystemBase
    ],
    True,
    TestID -> "PublisherTokenObject-ResourceSystemBase@@Tests/PublisherTokens.wlt:290,1-297,2"
]

VerificationTest[
    $tokenByteArray = token3[ "TokenByteArray" ],
    _ByteArray? ByteArrayQ,
    SameTest -> MatchQ,
    TestID   -> "PublisherTokenObject-TokenByteArray@@Tests/PublisherTokens.wlt:299,1-304,2"
]

VerificationTest[
    $tokenString = token3[ "TokenString" ],
    _String? StringQ,
    SameTest -> MatchQ,
    TestID   -> "PublisherTokenObject-TokenString@@Tests/PublisherTokens.wlt:306,1-311,2"
]

VerificationTest[
    token1[ "Version" ],
    (_Integer|_Real)? Positive,
    SameTest -> MatchQ,
    TestID   -> "PublisherTokenObject-Version@@Tests/PublisherTokens.wlt:313,1-318,2"
]

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Retrieval*)
VerificationTest[
    PublisherTokenObject[ $deleteMeName ],
    token2,
    TestID -> "PublisherTokenObject-Get-Name@@Tests/PublisherTokens.wlt:323,1-327,2"
]

VerificationTest[
    PublisherTokenObject[ $tokenString ],
    token3,
    TestID -> "PublisherTokenObject-Get-TokenString@@Tests/PublisherTokens.wlt:329,1-333,2"
]

VerificationTest[
    While[ $expiration24 < Now, Pause[ 1 ] ],
    Null,
    TimeConstraint -> 6,
    TestID         -> "PublisherTokenObject-Expiration-Wait@@Tests/PublisherTokens.wlt:335,1-340,2"
]

VerificationTest[
    Quiet @ FailureQ @ PublisherTokenObject @ token5[ "TokenString" ],
    True,
    TestID -> "PublisherTokenObject-Expired@@Tests/PublisherTokens.wlt:342,1-346,2"
]

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*DeletePublisherToken*)
VerificationTest[
    DeletePublisherToken @ token1,
    Success[ "TokenDeleted", _ ],
    SameTest -> MatchQ,
    TestID   -> "DeletePublisherToken-1@@Tests/PublisherTokens.wlt:351,1-356,2"
]

VerificationTest[
    DeleteObject @ token2,
    Success[ "TokenDeleted", _ ],
    SameTest -> MatchQ,
    TestID   -> "DeletePublisherToken-DeleteObject@@Tests/PublisherTokens.wlt:358,1-363,2"
]

VerificationTest[
    DeletePublisherToken @ { token3, token4 },
    { Success[ "TokenDeleted", _ ].. },
    SameTest -> MatchQ,
    TestID   -> "DeletePublisherToken-List@@Tests/PublisherTokens.wlt:365,1-370,2"
]