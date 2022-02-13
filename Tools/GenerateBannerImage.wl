$pacDir = DirectoryName[ $InputFileName, 2 ];

$icon = Graphics[
    {
        Thickness[ 0.016667 ],
        {
            FaceForm @ { RGBColor[ 1.0, 0.41569, 0.058824 ], Opacity[ 1.0 ] },
            FilledCurve[
                { { { 0, 2, 0 }, { 0, 1, 0 }, { 0, 1, 0 }, { 0, 1, 0 } } },
                {
                    {
                        { 4.0, 30.63 },
                        { 4.0, 43.68 },
                        { 15.31, 37.16 },
                        { 15.31, 24.1 },
                        { 4.0, 30.63 }
                    }
                }
            ]
        },
        {
            FaceForm @ { RGBColor[ 1.0, 0.41569, 0.058824 ], Opacity[ 1.0 ] },
            FilledCurve[
                { { { 0, 2, 0 }, { 0, 1, 0 }, { 0, 1, 0 }, { 0, 1, 0 } } },
                {
                    {
                        { 42.43, 7.14 },
                        { 31.13, 0.62 },
                        { 31.13, 13.67 },
                        { 42.43, 20.19 },
                        { 42.43, 7.14 }
                    }
                }
            ]
        },
        {
            FaceForm @ { RGBColor[ 1.0, 0.41569, 0.058824 ], Opacity[ 1.0 ] },
            FilledCurve[
                { { { 0, 2, 0 }, { 0, 1, 0 }, { 0, 1, 0 }, { 0, 1, 0 } } },
                {
                    {
                        { 43.56, 52.17 },
                        { 54.86, 45.64 },
                        { 43.56, 39.11 },
                        { 32.26, 45.64 },
                        { 43.56, 52.17 }
                    }
                }
            ]
        },
        {
            FaceForm @ { RGBColor[ 0.98039, 0.66274, 0.25098 ], Opacity[ 1.0 ] },
            FilledCurve[
                { { { 0, 2, 0 }, { 0, 1, 0 }, { 0, 1, 0 }, { 0, 1, 0 } } },
                {
                    {
                        { 4.0, 14.97 },
                        { 4.0, 28.02 },
                        { 28.87, 13.67 },
                        { 28.87, 0.62 },
                        { 4.0, 14.97 }
                    }
                }
            ]
        },
        {
            FaceForm @ { RGBColor[ 0.98039, 0.66274, 0.25098 ], Opacity[ 1.0 ] },
            FilledCurve[
                { { { 0, 2, 0 }, { 0, 1, 0 }, { 0, 1, 0 }, { 0, 1, 0 } } },
                {
                    {
                        { 30.0, 60.0 },
                        { 41.3, 53.47 },
                        { 16.44, 39.11 },
                        { 5.14, 45.64 },
                        { 30.0, 60.0 }
                    }
                }
            ]
        },
        {
            FaceForm @ { RGBColor[ 0.98039, 0.66274, 0.25098 ], Opacity[ 1.0 ] },
            FilledCurve[
                { { { 0, 2, 0 }, { 0, 1, 0 }, { 0, 1, 0 }, { 0, 1, 0 } } },
                {
                    {
                        { 56.0, 14.97 },
                        { 44.7, 8.44 },
                        { 44.69, 37.16 },
                        { 55.99, 43.68 },
                        { 56.0, 14.97 }
                    }
                }
            ]
        },
        {
            FaceForm @ { RGBColor[ 0.67059, 0.21176, 0.14902 ], Opacity[ 1.0 ] },
            FilledCurve[
                { { { 0, 2, 0 }, { 0, 1, 0 }, { 0, 1, 0 }, { 0, 1, 0 } } },
                {
                    {
                        { 17.57, 22.8 },
                        { 17.57, 35.85 },
                        { 28.87, 29.33 },
                        { 28.87, 16.28 },
                        { 17.57, 22.8 }
                    }
                }
            ]
        },
        {
            FaceForm @ { RGBColor[ 0.67059, 0.21176, 0.14902 ], Opacity[ 1.0 ] },
            FilledCurve[
                { { { 0, 2, 0 }, { 0, 1, 0 }, { 0, 1, 0 }, { 0, 1, 0 } } },
                {
                    {
                        { 30.0, 44.33 },
                        { 41.3, 37.8 },
                        { 30.0, 31.28 },
                        { 18.7, 37.8 },
                        { 30.0, 44.33 }
                    }
                }
            ]
        },
        {
            FaceForm @ { RGBColor[ 0.67059, 0.21176, 0.14902 ], Opacity[ 1.0 ] },
            FilledCurve[
                { { { 0, 2, 0 }, { 0, 1, 0 }, { 0, 1, 0 }, { 0, 1, 0 } } },
                {
                    {
                        { 42.43, 22.8 },
                        { 31.13, 16.28 },
                        { 31.13, 29.33 },
                        { 42.43, 35.85 },
                        { 42.43, 22.8 }
                    }
                }
            ]
        }
    },
    AspectRatio -> Automatic,
    ImageSize   -> { 60.0, 60.0 },
    PlotRange   -> { { 0.0, 60.0 }, { 0.0, 60.0 } }
];

$banner =
    Framed[
        Grid[
            {
                {
                    Graphics[
                        First @ $icon,
                        PlotRange        -> { { 0, 60 }, { 0, 60 } },
                        ImageSize        -> 50,
                        BaselinePosition -> Scaled[ 0.4 ]
                    ],
                    Column @ {
                        Style[
                            "Paclet CI/CD",
                            FontColor  -> RGBColor[ "#ff6a0f" ],
                            FontFamily -> "Source Sans Pro",
                            FontWeight -> "Heavy",
                            FontSize   -> 40
                        ],
                        Style[
                            "CI/CD utilities for Wolfram Language Paclets",
                            FontSize   -> 12,
                            FontColor  -> GrayLevel[ 0.5 ],
                            FontFamily -> "Source Sans Pro"
                        ]
                    }
                }
            },
            Alignment -> { Automatic, Baseline },
            Spacings  -> 0.5
        ],
        Background     -> White,
        FrameStyle     -> None,
        RoundingRadius -> 12,
        FrameMargins   -> { { 8, 12 }, { 6, 8 } }
    ];

$bannerImage =
    Rasterize[
        $banner,
        ImageResolution -> 144,
        RasterSize      -> { Automatic, 250 },
        Background      -> None
    ];

Export[
    FileNameJoin @ { $pacDir, "Images", "Banner.png" },
    $bannerImage,
    "PNG",
    ImageSize -> { Automatic, 250 }
]
