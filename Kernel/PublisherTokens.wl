(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Header*)
BeginPackage[ "Wolfram`PacletCICD`" ];

ClearAll[
    CreatePublisherToken,
    DeletePublisherToken,
    PublisherTokenObject,
    PublisherTokens
];

Begin[ "`Private`" ];

$ContextAliases[ "rsc`" ] = "ResourceSystemClient`";
$ContextAliases[ "sp`"  ] = "System`Private`";

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*CreatePublisherToken*)
CreatePublisherToken::Authentication =
"CreatePublisherToken requires cloud authentication. \
Please CloudConnect and try again.";

CreatePublisherToken::ExpiredDate =
"The value for ExpirationDate cannot be a past date.";

CreatePublisherToken::InvalidExpirationDate =
"The value `1` is not a valid ExpirationDate specification.";

CreatePublisherToken::InvalidResourceSystemBase =
"The value `1` is not a valid ResourceSystemBase specification.";

CreatePublisherToken::NoDefaultPublisher =
"Cannot determine a value for PublisherID automatically.";

CreatePublisherToken::InvalidName =
"The value `1` is not a valid token name.";

CreatePublisherToken::InvalidPublisherID =
"The value `1` is not a valid PublisherID specification.";

CreatePublisherToken::ResourceSystemInformation =
"Failed to retrieve information from the resource system at `1`. \
Please check your ResourceSystemBase and try again.";

CreatePublisherToken::TokensUnsupported =
"The resource system at `1` does not support publisher tokens.";

CreatePublisherToken::InvalidEndpoint =
"The value `1` is not a valid endpoint specification.";

CreatePublisherToken::InvalidEndpoints =
"The endpoints `1` are not valid for the resource system at `2`. \
Supported endpoints are `3`.";

CreatePublisherToken::ServerError =
"The resource system server was unable to fulfill the request.";

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Options*)
CreatePublisherToken // Options = {
    ExpirationDate     -> None,
    PublisherID        -> Automatic,
    ResourceSystemBase -> Automatic,
    "AllowedEndpoints" -> All
};

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Main definition*)
CreatePublisherToken[ opts: OptionsPattern[ ] ] :=
    catchTop @ CreatePublisherToken[ Automatic, opts ];

CreatePublisherToken[ name_, opts: OptionsPattern[ ] ] :=
    catchTop[
        If[ ! TrueQ @ $CloudConnected,
            throwMessageFailure[ CreatePublisherToken::Authentication ]
        ];
        Needs[ "ResourceSystemClient`" -> None ];
        createPublisherToken[
            name,
            OptionValue @ ExpirationDate,
            OptionValue @ PublisherID,
            OptionValue @ ResourceSystemBase,
            OptionValue @ AllowedEndpoints
        ]
    ];

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*createPublisherToken*)
createPublisherToken[ name_, expiration_, publisher_, rsBase_, endpoints_ ] :=
    Block[
        {
            $ResourceSystemBase = toTokenResourceSystemBase @ rsBase,
            $PublisherID        = toTokenPublisherID @ publisher
        },
        createPublisherToken @ DeleteMissing @ <|
            "Name"               -> toTokenName @ name,
            "ExpirationDate"     -> toTokenExpirationDate @ expiration,
            "PublisherID"        -> $PublisherID,
            "ResourceSystemBase" -> $ResourceSystemBase,
            "AllowedEndpoints"   -> toTokenAllowedEndpoints @ endpoints
        |>
    ];

createPublisherToken[ as_Association ] := Enclose[
    Module[ { params, info },
        params = <| "ContentFormat" -> "Compressed", "Data" -> Compress @ as |>;
        info = rsc`ResourceSystemExecute[ "CreatePublisherToken", params ];
        If[ AssociationQ @ info,
            PublisherTokenObject @ info,
            info
        ]
    ],
    throwMessageFailure[ CreatePublisherToken::ServerError ] &
];

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*toTokenResourceSystemBase*)
toTokenResourceSystemBase[ HoldPattern @ $ResourceSystemBase ] :=
    toTokenResourceSystemBase @ $defaultResourceSystemBase;

toTokenResourceSystemBase[ Automatic ] :=
    With[ { rsb = $ResourceSystemBase },
        toTokenResourceSystemBase @ rsb /; rsb =!= Automatic
    ];

toTokenResourceSystemBase[ (URL|CloudObject)[ url_, ___ ] ] :=
    toTokenResourceSystemBase @ url;

toTokenResourceSystemBase[ url_String? resourceSystemBaseQ ] :=
    toTokenResourceSystemBase[ url ] = url;

toTokenResourceSystemBase[ other_ ] :=
    throwMessageFailure[
        CreatePublisherToken::InvalidResourceSystemBase,
        other
    ];

toTokenResourceSystemBase // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*$defaultResourceSystemBase*)
$defaultResourceSystemBase = Enclose[
    URLBuild @ {
        ConfirmBy[ $CloudBase, StringQ ],
        "obj",
        "resourcesystem",
        "api",
        "1.0"
    }
];

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*resourceSystemBaseQ*)
resourceSystemBaseQ[ base_String? StringQ ] := Enclose[
    Module[ { url, resp, body },
        url  = ConfirmBy[ URLBuild @ { base, "TestSystem" }, StringQ ];
        resp = Confirm @ URLRead[ url, TimeConstraint -> 5 ];
        ConfirmAssert[ resp[ "StatusCode" ] === 200 ];
        body = ConfirmBy[ resp[ "Body" ], StringQ ];
        ConfirmAssert @ StringContainsQ[ body, "Success" ];
        resourceSystemBaseQ[ base ] = True
    ],
    False &
];

resourceSystemBaseQ // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*toTokenName*)
toTokenName[ name_String? StringQ ] := name;
toTokenName[ Automatic|None ] := Missing[ ];

toTokenName[ other_ ] :=
    throwMessageFailure[ CreatePublisherToken::InvalidName, other ];

toTokenName // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*toTokenExpirationDate*)
toTokenExpirationDate[ do_DateObject? DateObjectQ ] :=
    Module[ { diff },
        diff = do - Now;
        If[ TrueQ @ Positive @ diff,
            do,
            throwMessageFailure[ CreatePublisherToken::ExpiredDate, do ]
        ]
    ];

toTokenExpirationDate[ None|InfiniteFuture|Automatic ] := None;

toTokenExpirationDate[ other_ ] :=
    With[ { do = DateObject @ other },
        If[ DateObjectQ @ do,
            toTokenExpirationDate @ do,
            throwMessageFailure[
                CreatePublisherToken::InvalidExpirationDate,
                other
            ]
        ]
    ];

toTokenExpirationDate // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*toTokenPublisherID*)
toTokenPublisherID[ Automatic ] :=
    Module[ { pub },
        If[ StringQ @ $PublisherID,
            $PublisherID,
            pub = Catch @ ResourceSystemClient`GetDefaultPublisherID[ ];
            If[ StringQ @ pub,
                pub,
                throwMessageFailure[ CreatePublisherToken::NoDefaultPublisher ]
            ]
        ]
    ];

toTokenPublisherID[ publisher_String? StringQ ] := publisher;

toTokenPublisherID[ other_ ] :=
    throwMessageFailure[
        CreatePublisherToken::InvalidPublisherID,
        other
    ];

toTokenPublisherID // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*toTokenAllowedEndpoints*)
toTokenAllowedEndpoints[ expr_ ] /; ListQ @ tokenEnabledEndpoints[ ] :=
    toTokenAllowedEndpoints0 @ expr;

toTokenAllowedEndpoints // catchUndefined;

toTokenAllowedEndpoints0[ Automatic ] := Automatic;
toTokenAllowedEndpoints0[ All       ] := All;
toTokenAllowedEndpoints0[ None      ] := None;
toTokenAllowedEndpoints0[ ep_String ] := toTokenAllowedEndpoints0 @ { ep };
toTokenAllowedEndpoints0[ eps_List  ] := checkAllowedEndpoints @ eps;

toTokenAllowedEndpoints0 // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*checkAllowedEndpoints*)
checkAllowedEndpoints[ eps: { ___String? StringQ } ] :=
    Module[ { available, invalid },
        available = tokenEnabledEndpoints[ ];
        invalid   = Complement[ eps, available ];

        If[ invalid === { },
            Union @ eps,
            throwMessageFailure[
                CreatePublisherToken::InvalidEndpoints,
                invalid,
                $ResourceSystemBase,
                available
            ]
        ]
    ];

checkAllowedEndpoints[ { ___, inv: Except[ _String? StringQ ], ___ } ] :=
    throwMessageFailure[ CreatePublisherToken::InvalidEndpoint, inv ];

checkAllowedEndpoints // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*tokenEnabledEndpoints*)
tokenEnabledEndpoints[ ] := tokenEnabledEndpoints @ $ResourceSystemBase;

tokenEnabledEndpoints[ rsb_ ] := Enclose[
    Module[ { info, eps },
        Needs[ "ResourceSystemClient`" -> None ];
        info = ConfirmBy[ rsc`ResourceSystemInformation @ rsb, AssociationQ ];
        eps  = Lookup[ info, "TokenEnabledEndpoints", { } ];
        If[ eps === { },
            throwMessageFailure[
                CreatePublisherToken::TokensUnsupported,
                rsb
            ],
            tokenEnabledEndpoints[ rsb ] = eps
        ]
    ],
    throwMessageFailure[
        CreatePublisherToken::ResourceSystemInformation,
        rsb
    ] &
];

tokenEnabledEndpoints // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*DeletePublisherToken*)
DeletePublisherToken::InternalError =
"An unexpected error occurred.";

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Main definition*)
DeletePublisherToken[ token_PublisherTokenObject? publisherTokenObjectQ ] :=
    catchTop @ Enclose[
        Failure[ "NotImplemented", <| |> ],
        throwMessageFailure[ DeletePublisherToken::InternalError, # ] &
    ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*PublisherTokens*)
PublisherTokens[ ___ ] := Failure[ "NotImplemented", <| |> ];

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*PublisherTokenObject*)
PublisherTokenObject::InvalidToken =
"Invalid token specification: `1`";

PublisherTokenObject::InvalidTokenRule =
"Invalid token value for key `1`: `2`";

PublisherTokenObject::InvalidTokenProperty =
"Invalid token property: `1`";

PublisherTokenObject::Undefined =
"Unhandled arguments for `1` in `2`.";

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Main definition*)
PublisherTokenObject[ as0_Association ]? sp`HoldNotValidQ :=
    With[ { as = catchTop @ validateTokenInfo @ as0 },
        sp`HoldSetValid @ PublisherTokenObject @ as /; AssociationQ @ as
    ];

PublisherTokenObject[ name_String ]

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*UpValues*)

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*DeleteObject*)
PublisherTokenObject /:
    DeleteObject[ token_PublisherTokenObject? publisherTokenObjectQ ] :=
        DeletePublisherToken @ token;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*$PublisherToken*)
PublisherTokenObject /:
    HoldPattern @ Set[ rsc`$PublisherToken, pt_PublisherTokenObject ] :=
        catchTop @ Module[ { bytes },
            If[ ! publisherTokenObjectQ @ pt,
                throwMessageFailure[
                    PublisherTokenObject::InvalidToken,
                    hideTokenString @ pt
                ]
            ];
            bytes = publisherTokenProperty[ pt, "ByteArray" ];
            Needs[ "ResourceSystemClient`" -> None ];
            rsc`$PublisherToken = bytes
        ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Error Cases*)

(* Invalid token data *)
e: PublisherTokenObject[ as_ ]? sp`HoldNotValidQ :=
    throwMessageFailure[
        PublisherTokenObject::InvalidToken,
        hideTokenString @ as,
        hideTokenString @ HoldForm @ e
    ];

(* Unexpected arguments: *)
e: PublisherTokenObject[ ___ ]? sp`HoldNotValidQ :=
    throwMessageFailure[
        PublisherTokenObject::Undefined,
        PublisherTokenObject,
        hideTokenString @ HoldForm @ e
    ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Formatting*)
PublisherTokenObject /: MakeBoxes[
    token_PublisherTokenObject? publisherTokenObjectQ,
    fmt_
] :=
    With[ { boxes = FormattingHelper[ token, fmt ] },
        boxes /; MatchQ[ boxes, _InterpretationBox ]
    ];

PublisherTokenObject /: MakeBoxes[
    token_PublisherTokenObject,
    fmt_
] /; ! TrueQ @ $formattingTag :=
    Block[ { $formattingTag = True },
        hideTokenString @ Unevaluated @ MakeBoxes[ token, fmt ]
    ];

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Properties*)
(token_PublisherTokenObject? publisherTokenObjectQ)[ prop_ ] :=
    catchTop @ publisherTokenProperty[ token, prop ];

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*publisherTokenProperty*)
publisherTokenProperty[ HoldPattern @ PublisherTokenObject[ as_ ], prop_ ] :=
    publisherTokenProperty[ as, prop ];

publisherTokenProperty[ as_Association, "All" ] :=
    as;

publisherTokenProperty[ as_Association, "Properties" ] :=
    Union[ Keys @ as, $extraTokenProperties ];

publisherTokenProperty[ as_Association, "Dataset" ] :=
    Dataset @ as;

publisherTokenProperty[ as_Association, "ByteArray" ] :=
    With[ { bytes = publisherTokenProperty[ as, "TokenByteArray" ] },
        bytes /; ByteArrayQ @ bytes
    ];

publisherTokenProperty[
    as_Association,
    "User"|"UserID"|"CloudUserID"|"WolframID"
] := publisherTokenProperty[ as, "Creator" ];

publisherTokenProperty[
    as_Association,
    "UserUUID"|"CloudUserUUID"|"WolframUUID"
] := publisherTokenProperty[ as, "CreatorUUID" ];

publisherTokenProperty[ as_Association, "TokenString"|"String" ] :=
    With[ { ba = publisherTokenProperty[ as, "TokenByteArray" ] },
        ByteArrayToString @ ba /; ByteArrayQ @ ba
    ];

publisherTokenProperty[ as_Association, "ClickToCopy" ] :=
    With[ { str = publisherTokenProperty[ as, "TokenString" ] },
        LoadSubPackage[ "Formatting" ];
        tokenC2C @ str /; tokenStringQ @ str
    ];

publisherTokenProperty[ as_Association, "AllowedURLs"|"URLs" ] :=
    tokenURLs @ as;

publisherTokenProperty[ as_Association, sym_Symbol ] :=
    With[ { prop = SymbolName @ Unevaluated @ sym },
        publisherTokenProperty[ as, prop ]
    ];

publisherTokenProperty[ as_Association, p_List ] :=
    AssociationMap[ catch @ publisherTokenProperty[ as, # ] &, p ];

publisherTokenProperty[ KeyValuePattern[ p_String -> v_ ], p_String ] :=
    v;

publisherTokenProperty[ as_Association, other_ ] :=
    throwMessageFailure[
        PublisherTokenObject::InvalidTokenProperty,
        hideTokenString @ other
    ];

publisherTokenProperty // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*tokenURLs*)
tokenURLs[ as_Association ] := Enclose[
    Module[ { rsb, names },
        rsb   = ConfirmBy[ as[ "ResourceSystemBase" ], StringQ ];
        names = ConfirmMatch[ as[ "AllowedEndpoints" ], { ___? StringQ } ];
        URL @ URLBuild[ { rsb, # } ] & /@ names
    ],
    throwError[
        "An internal error ocurred.",
        hideTokenString @ HoldForm @ tokenURLs @ as
    ] &
];

tokenURLs // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*$extraTokenProperties*)
$extraTokenProperties = {
    "Dataset",
    "Properties",
    "TokenString",
    "ClickToCopy"
};

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Dependencies*)

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*publisherTokenObjectQ*)
publisherTokenObjectQ // Attributes = { HoldFirst };
publisherTokenObjectQ[ t_PublisherTokenObject ] := sp`HoldValidQ @ t;
publisherTokenObjectQ[ ___                    ] := False;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*validateTokenInfo*)
validateTokenInfo[ as: KeyValuePattern[
    "TokenString" -> str_String? tokenStringQ
] ] :=
    validateTokenInfo @ Append[
        KeyDrop[ as, "TokenString" ],
        "TokenByteArray" -> StringToByteArray @ str
    ];

validateTokenInfo[ info_Association? validTokenInfoQ ] :=
    sortTokenInfo @ KeyDrop[ info, $clientDroppedTokenKeys ];

validateTokenInfo[ other_ ] :=
    throwMessageFailure[
        PublisherTokenObject::InvalidToken,
        hideTokenString @ other
    ];

validateTokenInfo // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*sortTokenInfo*)
sortTokenInfo[ info_Association ] :=
    Join[ KeyTake[ info, $clientExpectedTokenKeys ], info ];

sortTokenInfo // catchUndefined;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*validTokenInfoQ*)
validTokenInfoQ[ info_Association? AssociationQ ] :=
    TrueQ @ And[
        ContainsAll[ Keys @ info, $clientExpectedTokenKeys ],
        AllTrue[
            Normal[ KeyTake[ info, $clientExpectedTokenKeys ], Association ],
            validTokenRuleQ
        ]
    ];

validTokenInfoQ[ ___ ] := False;

(* ::**********************************************************************:: *)
(* ::Subsubsubsection::Closed:: *)
(*validTokenRuleQ*)
validTokenRuleQ[ Rule[ key_, value_ ] ] := validTokenRuleQ[ key, value ];

validTokenRuleQ[ "Creator"           , _String? StringQ         ] := True;
validTokenRuleQ[ "PublisherID"       , _String? StringQ         ] := True;
validTokenRuleQ[ "CreationDate"      , _DateObject? DateObjectQ ] := True;
validTokenRuleQ[ "ExpirationDate"    , _DateObject? DateObjectQ ] := True;
validTokenRuleQ[ "ExpirationDate"    , None                     ] := True;
validTokenRuleQ[ "AllowedEndpoints"  , { ___String? StringQ }   ] := True;
validTokenRuleQ[ "ResourceSystemBase", _String? StringQ         ] := True;
validTokenRuleQ[ "Version"           , _Integer? IntegerQ       ] := True;
validTokenRuleQ[ "Name"              , None | _String? StringQ  ] := True;

validTokenRuleQ[ "TokenByteArray", bytes_ByteArray? ByteArrayQ ] :=
    tokenStringQ @ ByteArrayToString @ bytes;

validTokenRuleQ[ key_, value_ ] :=
    throwMessageFailure[
        PublisherTokenObject::InvalidTokenRule,
        hideTokenString @ key,
        hideTokenString @ value
    ];

validTokenRuleQ[ ___ ] := False;

(* ::**********************************************************************:: *)
(* ::Subsubsubsection::Closed:: *)
(*tokenStringQ*)
tokenStringQ[ str_String? StringQ ] :=
    MatchQ[ StringSplit[ str, "-" ],
            { _? b62UUIDQ, _? b62UUIDQ, _? b62UUIDQ, _? numberStringQ }
    ];

tokenStringQ[ ___ ] := False;

(* ::**********************************************************************:: *)
(* ::Subsubsubsection::Closed:: *)
(*b62UUIDQ*)
b62UUIDQ[ s_String ] :=
    And[ StringLength @ s <= 22,
         StringMatchQ[ s, RegularExpression[ "[a-zA-Z0-9]+" ] ]
    ];

b62UUIDQ[ ___ ] := False;

(* ::**********************************************************************:: *)
(* ::Subsubsubsection::Closed:: *)
(*numberStringQ*)
numberStringQ[ s_String? StringQ ] := StringMatchQ[ s, $numberString ];
numberStringQ[ ___ ] := False;

$intString    = DigitCharacter..;
$realString   = $intString ~~ "." ~~ ($intString | "");
$numberString = $intString | $realString;

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*$clientExpectedTokenKeys*)
$clientExpectedTokenKeys = {
    "Name",
    "Creator",
    "PublisherID",
    "CreationDate",
    "ExpirationDate",
    "AllowedEndpoints",
    "ResourceSystemBase",
    "Version",
    "TokenByteArray"
};

(* ::**********************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*$clientDroppedTokenKeys*)
$clientDroppedTokenKeys = {
    "TokenID",
    "TokenString"
};

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Utilities*)

(* ::**********************************************************************:: *)
(* ::Subsection::Closed:: *)
(*hideTokenString*)
hideTokenString[ expr_ ] :=
    With[ { str = $redactedTokenString },
        ReplaceAll[
            Unevaluated @ expr,
            token_String? tokenStringQ :> str
        ]
    ];

$redactedTokenString = "\[LeftSkeleton]RedactedTokenString\[RightSkeleton]";

(* ::**********************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Footer*)
End[ ];

EndPackage[ ];