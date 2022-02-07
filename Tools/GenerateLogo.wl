$pacDir = DirectoryName[ $InputFileName, 2 ];

$icon = BinaryDeserialize @ ByteArray[ "\
OEM6eJxLYynmcC9KLMjITC5O4ypm8cksLkljLOYMAQpk56UWFxcJgsFE+zQmuCyHW2Jyqlt+US5cjLm\
YI8jdyTk/J7+oiAEMPtgXzZ4FArfsi+TAYJ09UCu7f0FicmZJJVwV0Ahut8ycnNQU59KistSDDMyMLM\
wMTAwMjFB0UJmZkZUJolzAAUgccF9o54DEd+C67gqiE3bM0wPRD6qmOCHzG2ZKWjig6x9w3zhwmYNdz\
RDTLwNxlaI9mM9x/bE9Er+BK1zbAVn9EgMTB3T9A+4bhXXHwa7pOSTqBXa1SJ43mH590c0BSb5BpM8Z\
6lsHbPJg/UT7Rj9Igzs+/r190fczkxhMTZ/aF8mc+5PFJ3CBSimNYckHXZSUI8dqAw77HfdsUOIGyof\
FHYr+weAbWH7xA+eLtGUuIPqLxh5wXD1YXGCAHDcKMf0iyHGDon8w+MYHOW4WzIwEu7LB/aEC2DeBEW\
A+ohT45w3JL5BSAkU/6fkGg0GZbxpeTzQEp6wzZ8wckPgXzrxxRE5ZD6pCbFFS2jp3Awd0/QPuG1hKK\
bDVckNOaQlpz5xQUtI6d3DZ5mC82QibPFj/gPsGVsLC4gZeQkPDHsaHxw1UPTzukPWDXB5UmpNazO1Y\
XJCaXBKUWJKZX8zpWFqSnwtkJsPlOT1zE9NTgzOrUg8qM0ID1c8BRiOUBeTkAw3JSwcqY2KCqoMBmHo\
EHwDQG+1Z" ];

$textCurves =
    First @ First @ ImportString[
        ExportString[
            Style[
                "CI/CD",
                FontColor  -> White,
                FontFamily -> "Source Sans Pro",
                FontWeight -> "Heavy",
                FontSize   -> 22
            ],
            "PDF"
        ],
        { "PDF", "PageGraphics" },
        "TextOutlines" -> True
    ];

$logoGFX =
    Framed[
        Show[
            $icon,
            Epilog ->
                Inset[
                    Graphics[
                        {
                            EdgeForm @ {
                                Thickness[ 0.05 ],
                                RGBColor[ "#ab3626" ]
                            },
                            $textCurves,
                            EdgeForm @ None,
                            $textCurves
                        },
                        PlotRangePadding -> Scaled[ 0.1 ],
                        ImageSize -> 65
                    ],
                    Scaled @ { 0.5, 0.2 }
                ],
            PlotRangePadding -> {
                { Automatic      , Automatic },
                { Scaled[ 0.125 ], Automatic }
            }
        ],
        Background     -> RGBColor[ "#ab3626" ],
        FrameStyle     -> None,
        RoundingRadius -> 8,
        FrameMargins   -> { { 4, 4 }, { 4, 4 } }
    ];

$pacletLogo =
    Rasterize[
        $logoGFX,
        ImageResolution -> 144,
        ImageSize       -> 50,
        Background      -> None
    ];

$logo =
    Rasterize[
        $logoGFX,
        ImageResolution -> 144,
        RasterSize      -> 128,
        Background      -> None
    ];

Export[
    FileNameJoin @ { $pacDir, "Images", "Icon.png" },
    $logo
];

Export[
    FileNameJoin @ { $pacDir, "Images", "PacletIcon.png" },
    $pacletLogo,
    "PNG",
    ImageSize -> 50
]

