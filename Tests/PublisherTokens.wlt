(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Initialization*)
VerificationTest[
    PacletObjectQ @ PacletObject @ File[
        $pacletDir = DirectoryName[ $TestFileName, 2 ]
    ],
    TestID -> "Initialize-Paclet-Directory"
]

VerificationTest[
    PacletDirectoryLoad @ $pacletDir,
    { ___, $pacletDir, ___ },
    SameTest -> MatchQ,
    TestID -> "Initialize-Paclet-Load"
]

VerificationTest[
    Needs[ "Wolfram`PacletCICD`" ],
    Null,
    TestID -> "Initialize-Paclet-Needs"
]

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Context Check*)
VerificationTest[
    Context @ CreatePublisherToken,
    "Wolfram`PacletCICD`",
    TestID -> "CreatePublisherToken-Context"
]

VerificationTest[
    Context @ DeletePublisherToken,
    "Wolfram`PacletCICD`",
    TestID -> "DeletePublisherToken-Context"
]

VerificationTest[
    Context @ PublisherTokenObject,
    "Wolfram`PacletCICD`",
    TestID -> "PublisherTokenObject-Context"
]

VerificationTest[
    Context @ PublisherTokens,
    "Wolfram`PacletCICD`",
    TestID -> "PublisherTokens-Context"
]

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Setup*)
VerificationTest[
    Needs[ "ResourceSystemClient`" -> None ],
    Null,
    TestID -> "Needs-ResourceSystemClient"
]

VerificationTest[
    With[ { rsb = Environment[ "RESOURCE_SYSTEM_BASE" ] },
        If[ StringQ @ rsb, $ResourceSystemBase = rsb, $ResourceSystemBase ]
    ],
    _String? StringQ,
    SameTest -> MatchQ,
    TestID   -> "ResourceSystemBase"
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
    TestID   -> "CloudConnect"
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
    TestID   -> "PublisherID"
]

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*CreatePublisherToken*)
VerificationTest[
    token1 = CreatePublisherToken[ ],
    _PublisherTokenObject,
    SameTest -> MatchQ,
    TestID   -> "CreatePublisherToken-Anonymous"
]

VerificationTest[
    token2 = CreatePublisherToken[ $deleteMeName = CreateUUID[ "DeleteMe" ] ],
    _PublisherTokenObject,
    SameTest -> MatchQ,
    TestID   -> "CreatePublisherToken-Named"
]

VerificationTest[
    token3 =
        CreatePublisherToken[
            "AllowedEndpoints" -> { "CheckPublisherToken", "DefaultPublisher" }
        ],
    _PublisherTokenObject,
    SameTest -> MatchQ,
    TestID   -> "CreatePublisherToken-AllowedEndpoints"
]

VerificationTest[
    $expiration24 = Now + Quantity[ 24, "Hours" ],
    _DateObject? DateObjectQ,
    SameTest -> MatchQ,
    TestID   -> "Define-ExpirationDate"
]

VerificationTest[
    token4 = CreatePublisherToken[ ExpirationDate -> $expiration24 ],
    _PublisherTokenObject,
    SameTest -> MatchQ,
    TestID   -> "CreatePublisherToken-ExpirationDate"
]

VerificationTest[
    $expirationNow = Now + Quantity[ 5, "Seconds" ],
    _DateObject? DateObjectQ,
    SameTest -> MatchQ,
    TestID   -> "Define-ExpirationDate-Now"
]

VerificationTest[
    token5 = CreatePublisherToken[ ExpirationDate -> $expirationNow ],
    _PublisherTokenObject,
    SameTest -> MatchQ,
    TestID   -> "CreatePublisherToken-ExpirationDate-Now"
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
    TestID   -> "PublisherTokenObject-All"
]

VerificationTest[
    token3[ "AllowedEndpoints" ],
    { "CheckPublisherToken", "DefaultPublisher" },
    TestID -> "PublisherTokenObject-AllowedEndpoints"
]

VerificationTest[
    token3[ "AllowedURLs" ],
    {
        URL @ URLBuild[ { $ResourceSystemBase, "CheckPublisherToken" } ],
        URL @ URLBuild[ { $ResourceSystemBase, "DefaultPublisher"    } ]
    },
    TestID -> "PublisherTokenObject-AllowedURLs"
]

VerificationTest[
    token3[ "CreationDate" ],
    _DateObject? DateObjectQ,
    TestID -> "PublisherTokenObject-CreationDate-1"
]

VerificationTest[
    token3[ "CreationDate" ][ "TimeZone" ] == 0,
    True,
    TestID -> "PublisherTokenObject-CreationDate-2"
]

VerificationTest[
    Less[ Quantity[ 0, "Minutes" ],
          Now - token3[ "CreationDate" ],
          Quantity[ 30, "Minutes" ]
    ],
    True,
    TestID -> "PublisherTokenObject-CreationDate-3"
]

VerificationTest[
    token1[ "Creator" ],
    $CloudUserID,
    TestID -> "PublisherTokenObject-Creator"
]

VerificationTest[
    token1[ "CreatorUUID" ],
    $CloudUserUUID,
    TestID -> "PublisherTokenObject-CreatorUUID"
]

VerificationTest[
    token1[ "Dataset" ],
    _Dataset,
    TestID -> "PublisherTokenObject-Dataset-1"
]

VerificationTest[
    Normal[ token1[ "Dataset" ], Dataset ],
    Normal[ Dataset @ token1 @ All, Dataset ],
    TestID -> "PublisherTokenObject-Dataset-2"
]

VerificationTest[
    token4[ "ExpirationDate" ] == token4[ ExpirationDate ] == $expiration24,
    True,
    TestID -> "PublisherTokenObject-ExpirationDate"
]

VerificationTest[
    token1[ "Name" ],
    None,
    TestID -> "PublisherTokenObject-Name-Anonymous"
]

VerificationTest[
    token2[ "Name" ],
    $deleteMeName,
    TestID -> "PublisherTokenObject-Name-Named"
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
    TestID   -> "PublisherTokenObject-Properties"
]

VerificationTest[
    token1[ "PublisherID" ] === token1[ PublisherID ] === $PublisherID,
    True,
    TestID -> "PublisherTokenObject-PublisherID"
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
    TestID -> "PublisherTokenObject-PublisherUUID"
]

VerificationTest[
    SameQ[ token1[ "ResourceSystemBase" ],
           token1[ ResourceSystemBase ],
           $ResourceSystemBase
    ],
    True,
    TestID -> "PublisherTokenObject-ResourceSystemBase"
]

VerificationTest[
    $tokenByteArray = token3[ "TokenByteArray" ],
    _ByteArray? ByteArrayQ,
    SameTest -> MatchQ,
    TestID   -> "PublisherTokenObject-TokenByteArray"
]

VerificationTest[
    $tokenString = token3[ "TokenString" ],
    _String? StringQ,
    SameTest -> MatchQ,
    TestID   -> "PublisherTokenObject-TokenString"
]

VerificationTest[
    token1[ "Version" ],
    (_Integer|_Real)? Positive,
    SameTest -> MatchQ,
    TestID   -> "PublisherTokenObject-Version"
]

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Retrieval*)
VerificationTest[
    PublisherTokenObject[ $deleteMeName ],
    token2,
    TestID -> "PublisherTokenObject-Get-Name"
]

VerificationTest[
    PublisherTokenObject[ $tokenString ],
    token3,
    TestID -> "PublisherTokenObject-Get-TokenString"
]

VerificationTest[
    While[ $expiration24 < Now, Pause[ 1 ] ],
    Null,
    TimeConstraint -> 6,
    TestID -> "PublisherTokenObject-Expiration-Wait"
]

VerificationTest[
    Quiet @ FailureQ @ PublisherTokenObject @ token5[ "TokenString" ],
    True,
    TestID -> "PublisherTokenObject-Expired"
]

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*DeletePublisherToken*)
VerificationTest[
    DeletePublisherToken @ token1,
    Success[ "TokenDeleted", _ ],
    SameTest -> MatchQ,
    TestID   -> "DeletePublisherToken-1"
]

VerificationTest[
    DeleteObject @ token2,
    Success[ "TokenDeleted", _ ],
    SameTest -> MatchQ,
    TestID   -> "DeletePublisherToken-DeleteObject"
]

VerificationTest[
    DeletePublisherToken @ { token3, token4 },
    { Success[ "TokenDeleted", _ ].. },
    SameTest -> MatchQ,
    TestID   -> "DeletePublisherToken-List"
]