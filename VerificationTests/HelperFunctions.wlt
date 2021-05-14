BeginTestSection["HelperFunctions"]

BeginTestSection["Initialization"]

EndTestSection[]

BeginTestSection["DualTuples"]

VerificationTest[
    DualNumbers`Private`dualTuplesPositions /@ Range[0, 4]
    ,
    {{}, {{{1, 2}}}, {{{1, 2}, {2, 1}}, {{1, 1}, {2, 2}}}, {{{1, 2}, {2, 
    1}, {3, 1}}, {{1, 1}, {2, 2}, {3, 1}}, {{1, 1}, {2, 1}, {3, 2}}}, {{{
    1, 2}, {2, 1}, {3, 1}, {4, 1}}, {{1, 1}, {2, 2}, {3, 1}, {4, 1}}, {{1,
     1}, {2, 1}, {3, 2}, {4, 1}}, {{1, 1}, {2, 1}, {3, 1}, {4, 2}}}}
    ,
    TestID -> "f9ea9141-1da4-4de8-bebf-9937fb358343"
]

VerificationTest[
    DualNumbers`Private`dualTuplesPositions[0, 1]
    ,
    {}
    ,
    TestID -> "3f70d915-a97f-4ab5-bda9-568b3b77dc83"
]

VerificationTest[
    DualNumbers`Private`dualTuplesPositions[4] === (DualNumbers`Private`dualTuplesPositions[
    4, #1]&) /@ Range[4]
    ,
    True
    ,
    TestID -> "56dbc595-4041-4ea7-a9f7-973136b359e4"
]

VerificationTest[
    Reverse[DualNumbers`Private`dualTuplesPositions[4]] === (DualNumbers`Private`dualTuplesPositions[
    4, -#1]&) /@ Range[4]
    ,
    True
    ,
    TestID -> "5a073783-ff22-48ad-9727-72957ab829f8"
]

VerificationTest[
    dualListPacked = Dual[Array[a, 10], Array[b, 10]]; dualList = UnpackDualArray[
    dualListPacked]; {AssociationMap[DualTuples[Take[dualList, #1]]&, Range[
    0, 4]], AssociationMap[DualTuples[Take[dualListPacked, #1]]&, Range[0,
     4]]}
    ,
    {Association[0 -> {}, 1 -> {{b[1]}}, 2 -> {{b[1], a[2]}, {a[1], b[2]}
    }, 3 -> {{b[1], a[2], a[3]}, {a[1], b[2], a[3]}, {a[1], a[2], b[3]}},
     4 -> {{b[1], a[2], a[3], a[4]}, {a[1], b[2], a[3], a[4]}, {a[1], a[2
    ], b[3], a[4]}, {a[1], a[2], a[3], b[4]}}], Association[0 -> {}, 1 ->
     {{b[1]}}, 2 -> {{b[1], a[2]}, {a[1], b[2]}}, 3 -> {{b[1], a[2], a[3]
    }, {a[1], b[2], a[3]}, {a[1], a[2], b[3]}}, 4 -> {{b[1], a[2], a[3], 
    a[4]}, {a[1], b[2], a[3], a[4]}, {a[1], a[2], b[3], a[4]}, {a[1], a[2
    ], a[3], b[4]}}]}
    ,
    TestID -> "0073410b-f0f3-47cd-9b3f-d0463b39086a"
]

VerificationTest[
    {AssociationMap[DualTuplesReduce[Take[dualList, #1], f]&, Range[0, 4]
    ], AssociationMap[DualTuplesReduce[Take[dualListPacked, #1], f]&, Range[
    0, 4]]}
    ,
    {Association[0 -> {}, 1 -> {f[b[1]]}, 2 -> {f[b[1], a[2]], f[a[1], b[
    2]]}, 3 -> {f[b[1], a[2], a[3]], f[a[1], b[2], a[3]], f[a[1], a[2], b[
    3]]}, 4 -> {f[b[1], a[2], a[3], a[4]], f[a[1], b[2], a[3], a[4]], f[a[
    1], a[2], b[3], a[4]], f[a[1], a[2], a[3], b[4]]}], Association[0 -> 
    {}, 1 -> {f[b[1]]}, 2 -> {f[b[1], a[2]], f[a[1], b[2]]}, 3 -> {f[b[1],
     a[2], a[3]], f[a[1], b[2], a[3]], f[a[1], a[2], b[3]]}, 4 -> {f[b[1],
     a[2], a[3], a[4]], f[a[1], b[2], a[3], a[4]], f[a[1], a[2], b[3], a[
    4]], f[a[1], a[2], a[3], b[4]]}]}
    ,
    TestID -> "9808cb57-a24f-43d7-bb90-86469ce74498"
]

VerificationTest[
    {AssociationMap[DualTuplesReduce[Take[dualList, #1], f, g]&, Range[0,
     2]], AssociationMap[DualTuplesReduce[Take[dualListPacked, #1], f, g]
    &, Range[0, 2]]}
    ,
    {Association[0 -> g[], 1 -> g[f[b[1]]], 2 -> g[g[f[b[1], a[2]]], f[a[
    1], b[2]]]], Association[0 -> g[], 1 -> g[f[b[1]]], 2 -> g[g[f[b[1], 
    a[2]]], f[a[1], b[2]]]]}
    ,
    TestID -> "1ce565c0-3d88-470d-80b3-25b3090bb4fd"
]

VerificationTest[
    {AssociationMap[DualTuplesReduce[Take[dualList, #1], Times, Plus]&, Range[
    0, 4]], AssociationMap[DualTuplesReduce[Take[dualListPacked, #1], Times,
     Plus]&, Range[0, 4]]}
    ,
    {Association[0 -> 0, 1 -> b[1], 2 -> a[2] * b[1] + a[1] * b[2], 3 -> 
    a[2] * a[3] * b[1] + a[1] * a[3] * b[2] + a[1] * a[2] * b[3], 4 -> a[
    2] * a[3] * a[4] * b[1] + a[1] * a[3] * a[4] * b[2] + a[1] * a[2] * a[
    4] * b[3] + a[1] * a[2] * a[3] * b[4]], Association[0 -> 0, 1 -> b[1],
     2 -> a[2] * b[1] + a[1] * b[2], 3 -> a[2] * a[3] * b[1] + a[1] * a[3
    ] * b[2] + a[1] * a[2] * b[3], 4 -> a[2] * a[3] * a[4] * b[1] + a[1] 
    * a[3] * a[4] * b[2] + a[1] * a[2] * a[4] * b[3] + a[1] * a[2] * a[3]
     * b[4]]}
    ,
    TestID -> "5d96e02e-4125-4d95-8827-294d67b93017"
]

EndTestSection[]

BeginTestSection["DualApply"]

VerificationTest[
    {DualApply[f, Dual[a, b]], DualApply[{f, g}, Dual[a, b]], DualApply[{
    {f[##1], g[##1]}&}, Dual[a, b]]}
    ,
    {Dual[f[a], f[b]], Dual[f[a], g[b]], Dual[f[a, b], g[a, b]]}
    ,
    TestID -> "b00344b3-e86e-4693-99cb-f765409ac1d2"
]

VerificationTest[
    {DualApply[f, a], DualApply[{f, g}, a], DualApply[{{f[##1], g[##1]}&},
     a]}
    ,
    {Dual[f[a], f[0]], Dual[f[a], g[0]], Dual[f[a, 0], g[a, 0]]}
    ,
    TestID -> "90de102c-7eea-4479-b782-cfe46dcfa4fd"
]

VerificationTest[
    {DualApply[f][Dual[a, b]], DualApply[{f, g}][Dual[a, b]], DualApply[{
    {f[##1], g[##1]}&}][Dual[a, b]]}
    ,
    {Dual[f[a], f[b]], Dual[f[a], g[b]], Dual[f[a, b], g[a, b]]}
    ,
    TestID -> "3c3b5616-b9c4-41e2-901a-c4babd095bf3"
]

VerificationTest[
    DualApply[{f}, Dual[a, b]]
    ,
    DualApply[{f}, Dual[a, b]]
    ,
    {DualApply::resultlength}
    ,
    TestID -> "b3c5b856-bf34-4958-b832-6dc081c7c66a"
]

VerificationTest[
    {DualApply[f, Dual[Array[a, {3, 1}], Array[b, {3, 1}]]], DualApply[f,
     Dual[Array[a, {3, 1}], Array[b, {3, 1}]], {0}], DualApply[f, Dual[Array[
    a, {3, 1}], Array[b, {3, 1}]], {1}], DualApply[f, Dual[Array[a, {3, 1
    }], Array[b, {3, 1}]], {2}]}
    ,
    {Dual[f[{{a[1, 1]}, {a[2, 1]}, {a[3, 1]}}], f[{{b[1, 1]}, {b[2, 1]}, 
    {b[3, 1]}}]], Dual[f[{{a[1, 1]}, {a[2, 1]}, {a[3, 1]}}], f[{{b[1, 1]},
     {b[2, 1]}, {b[3, 1]}}]], Dual[{f[{a[1, 1]}], f[{a[2, 1]}], f[{a[3, 1
    ]}]}, {f[{b[1, 1]}], f[{b[2, 1]}], f[{b[3, 1]}]}], Dual[{{f[a[1, 1]]},
     {f[a[2, 1]]}, {f[a[3, 1]]}}, {{f[b[1, 1]]}, {f[b[2, 1]]}, {f[b[3, 1]
    ]}}]}
    ,
    TestID -> "43a922d9-9916-42f4-8784-e60e5516ea21"
]

VerificationTest[
    {DualApply[{f, g}, Dual[Array[a, {3, 1}], Array[b, {3, 1}]]], DualApply[
    {f, g}, Dual[Array[a, {3, 1}], Array[b, {3, 1}]], {0}], DualApply[{f,
     g}, Dual[Array[a, {3, 1}], Array[b, {3, 1}]], {1}], DualApply[{f, g},
     Dual[Array[a, {3, 1}], Array[b, {3, 1}]], {2}]}
    ,
    {Dual[f[{{a[1, 1]}, {a[2, 1]}, {a[3, 1]}}], g[{{b[1, 1]}, {b[2, 1]}, 
    {b[3, 1]}}]], Dual[f[{{a[1, 1]}, {a[2, 1]}, {a[3, 1]}}], g[{{b[1, 1]},
     {b[2, 1]}, {b[3, 1]}}]], Dual[{f[{a[1, 1]}], f[{a[2, 1]}], f[{a[3, 1
    ]}]}, {g[{b[1, 1]}], g[{b[2, 1]}], g[{b[3, 1]}]}], Dual[{{f[a[1, 1]]},
     {f[a[2, 1]]}, {f[a[3, 1]]}}, {{g[b[1, 1]]}, {g[b[2, 1]]}, {g[b[3, 1]
    ]}}]}
    ,
    TestID -> "f4746d8a-5c01-4db7-87c0-432919b4a310"
]

VerificationTest[
    {DualApply[{{f[##1], g[##1]}&}, Dual[Array[a, {3, 1}], Array[b, {3, 1
    }]]], DualApply[{{f[##1], g[##1]}&}, Dual[Array[a, {3, 1}], Array[b, 
    {3, 1}]], 0], DualApply[{{f[##1], g[##1]}&}, Dual[Array[a, {3, 1}], Array[
    b, {3, 1}]], 1], DualApply[{{f[##1], g[##1]}&}, Dual[Array[a, {3, 1}],
     Array[b, {3, 1}]], 2]}
    ,
    {Dual[f[{{a[1, 1]}, {a[2, 1]}, {a[3, 1]}}, {{b[1, 1]}, {b[2, 1]}, {b[
    3, 1]}}], g[{{a[1, 1]}, {a[2, 1]}, {a[3, 1]}}, {{b[1, 1]}, {b[2, 1]},
     {b[3, 1]}}]], Dual[f[{{a[1, 1]}, {a[2, 1]}, {a[3, 1]}}, {{b[1, 1]}, 
    {b[2, 1]}, {b[3, 1]}}], g[{{a[1, 1]}, {a[2, 1]}, {a[3, 1]}}, {{b[1, 1
    ]}, {b[2, 1]}, {b[3, 1]}}]], Dual[{f[{a[1, 1]}, {b[1, 1]}], f[{a[2, 1
    ]}, {b[2, 1]}], f[{a[3, 1]}, {b[3, 1]}]}, {g[{a[1, 1]}, {b[1, 1]}], g[
    {a[2, 1]}, {b[2, 1]}], g[{a[3, 1]}, {b[3, 1]}]}], Dual[{{f[a[1, 1], b[
    1, 1]], f[a[2, 1], b[2, 1]], f[a[3, 1], b[3, 1]]}}, {{g[a[1, 1], b[1,
     1]], g[a[2, 1], b[2, 1]], g[a[3, 1], b[3, 1]]}}]}
    ,
    TestID -> "e308c611-9731-4a4e-af39-899410dfbe4a"
]

VerificationTest[
    DualApply[{f}, Dual[Array[a, {3, 1}], Array[b, {3, 1}]], 2]
    ,
    DualApply[{f}, Dual[{{a[1, 1]}, {a[2, 1]}, {a[3, 1]}}, {{b[1, 1]}, {b[
    2, 1]}, {b[3, 1]}}], 2]
    ,
    {DualApply::resultlength}
    ,
    TestID -> "ea63f1ae-5b4b-46ab-aa88-6499e19f3394"
]

VerificationTest[
    DualApply[f, Dual[a, b], {1}]
    ,
    DualApply[f, Dual[a, b], {1}]
    ,
    {DualApply::arraySpec}
    ,
    TestID -> "7ace9783-e0c8-48f4-8f0a-78c127cc8dff"
]

VerificationTest[
    DualApply[{{f[##1], g[##1]}&}, Dual[Array[a, {3, 1}], Array[b, {3, 1}
    ]], {2}]
    ,
    DualApply[{{f[##1], g[##1]}&}, Dual[{{a[1, 1]}, {a[2, 1]}, {a[3, 1]}},
     {{b[1, 1]}, {b[2, 1]}, {b[3, 1]}}], {2}]
    ,
    {DualApply::lvlSpec}
    ,
    TestID -> "1c6582f3-56c4-48fb-a278-c591976a4769"
]

EndTestSection[]

BeginTestSection["Equation solving"]

VerificationTest[
    {FindDualSolution[x == Dual[1, b], x -> 1], FindDualSolution[x == Dual[
    1, b], {x -> 1}]}
    ,
    {{{x -> Dual[1, b]}}, {{x -> Dual[1, b]}}}
    ,
    TestID -> "0f9b56f2-43b3-456c-b3ea-2a9d7be84422"
]

VerificationTest[
    {FindDualSolution[x^2 == Dual[2, b], x -> Sqrt[2]], FindDualSolution[
    x^2 == Dual[2, b], x -> -Sqrt[2]], FindDualSolution[Exp[x] == Dual[2,
     b], x -> Log[2]], FindDualSolution[Log[x] == Dual[2, b], x -> Exp[2]
    ]}
    ,
    {{{x -> Dual[Sqrt[2], b / (2 * Sqrt[2])]}}, {{x -> Dual[-Sqrt[2], -(b
     / (2 * Sqrt[2]))]}}, {{x -> Dual[Log[2], b / 2]}}, {{x -> Dual[E^2, 
    b * E^2]}}}
    ,
    TestID -> "14a0b9f2-a16c-4408-b192-9603298cef51"
]

VerificationTest[
    Quiet[{FindDualSolution[x^2 == Dual[2., b], x -> Sqrt[2.]], FindDualSolution[
    Exp[x] == Dual[2., b], x -> Log[2.]], FindDualSolution[Log[x] == Dual[
    2., b], x -> Exp[2.]]}]
    ,
    {{{x -> Dual[1.4142135623730951, 0.35355339059327373 * b]}}, {{x -> Dual[
    0.6931471805599453, 0.5 * b]}}, {{x -> Dual[7.38905609893065, 7.3890560989306495
     * b]}}}
    ,
    TestID -> "983bc19e-7dd2-4dc9-8db0-c552e0a86084"
]

VerificationTest[
    Quiet[{FindDualSolution[x^2 == Dual[2., 1.], x -> Sqrt[2.]], FindDualSolution[
    Exp[x] == Dual[2., 1.], x -> Log[2.]], FindDualSolution[Log[x] == Dual[
    2., 1.], x -> Exp[2.]]}]
    ,
    {{{x -> Dual[1.4142135623730951, 0.35355339059327373]}}, {{x -> Dual[
    0.6931471805599453, 0.5]}}, {{x -> Dual[7.38905609893065, 7.3890560989306495
    ]}}}
    ,
    TestID -> "bed46822-1c97-4906-a345-a6fab85f044e"
]

VerificationTest[
    Assuming[a > 0, {FindDualSolution[x^2 == Dual[a, b], x -> Sqrt[a]], FindDualSolution[
    Exp[x] == Dual[a, b], x -> Log[a]], FindDualSolution[Log[x] == Dual[a,
     b], x -> Exp[a]]}]
    ,
    {{{x -> Dual[Sqrt[a], b / (2 * Sqrt[a])]}}, {{x -> Dual[Log[a], b / a
    ]}}, {{x -> Dual[E^a, b * E^a]}}}
    ,
    TestID -> "615e90c9-7e54-4264-ba16-f6a0c3b9af67"
]

VerificationTest[
    FindDualSolution[x == Dual[1, b], x -> 2]
    ,
    {{x -> Dual[2, b]}}
    ,
    {FindDualSolution::nonsol}
    ,
    TestID -> "0e508e37-8eb8-41bb-94b6-7c9bb6447dca"
]

VerificationTest[
    standardSol = FindRoot[{Exp[x - 2] == y, y^2 == x}, {{x, 1}, {y, 1}}]; equations = {
    {Exp[x - 2] == y, y^2 == x}, {Exp[x - 2] == y, Dual[1, 1] * y^2 == x},
     {Exp[Dual[1, 1] * x - 2] == y, y^2 == x}, {Exp[x - Dual[2, 1]] == y,
     y^2 == x}, {Exp[x - 2] == y, y^Dual[2, 1] == x}, {Exp[x - Dual[2, 1]
    ] == y, y^Dual[2, 1] == x}}; sols1 = (FindDualSolution[#1, standardSol
    ]&) /@ equations
    ,
    {{{x -> Dual[_?NumericQ, _?NumericQ], y -> Dual[_?NumericQ, _?NumericQ
    ]}}..}
    ,
    SameTest -> MatchQ
    ,
    TestID -> "315543b7-13e2-4547-b678-71c4e5e7f356"
]

VerificationTest[
    {DualFindRoot[x == Dual[2, 1], {x, 1}], DualFindRoot[x^2 == Dual[2, 1
    ], {x, 1}], DualFindRoot[Exp[x] == Dual[2, 1], {x, 1}], DualFindRoot[
    Log[x] == Dual[2, 1], {x, 1}]}
    ,
    {{x -> Dual[2., 1]}, {x -> Dual[1.4142135623730951, 0.35355339059327373
    ]}, {x -> Dual[0.6931471805599453, 0.5]}, {x -> Dual[7.389056098930651,
     7.389056098930651]}}
    ,
    TestID -> "214622c5-e7e7-4b46-be87-a53671012ca2"
]

VerificationTest[
    sols2 = (DualFindRoot[#1, {{x, 1}, {y, 1}}]&) /@ equations
    ,
    {{x -> Dual[0.019026016103714054, 0.], y -> Dual[0.13793482556524314,
     0.]}, {x -> Dual[0.019026016103714054, 0.019778633294869334], y -> Dual[
    0.13793482556524314, 0.0027281623334467118]}, {x -> Dual[0.019026016103714054,
     0.0007526171911552778], y -> Dual[0.13793482556524314, 0.0027281623334467118
    ]}, {x -> Dual[0.019026016103714054, -0.03955726658973867], y -> Dual[
    0.13793482556524314, -0.14339115023213655]}, {x -> Dual[0.019026016103714054,
     -0.039180957994161034], y -> Dual[0.13793482556524314, -0.005404418606403721
    ]}, {x -> Dual[0.019026016103714054, -0.0787382245838997], y -> Dual[
    0.13793482556524314, -0.14879556883854028]}}
    ,
    TestID -> "a96358af-f5b7-45f7-a063-1ab112e5fdb5"
]

VerificationTest[
    sols1[[All, 1]] == sols2
    ,
    True
    ,
    TestID -> "5d4de4af-4f1b-4f84-af8e-252c585b2b1f"
]

EndTestSection[]

BeginTestSection["DualFindMinimum & DualFindMaximum"]

VerificationTest[
    {DualFindMinimum[Dual[1, 1] * x * Cos[x], {x, 2}], DualFindMinimum[x 
    * Cos[Dual[1, 1] * x], {x, 2}]}
    ,
    {{Dual[-3.2883713955908966, -3.2883713955908966], {x -> Dual[3.425618459492147,
     -1.0418883362877956*^-11]}}, {Dual[-3.2883713955908966, 3.2883713955908966
    ], {x -> Dual[3.425618459492147, -3.425618459492147]}}}
    ,
    TestID -> "f33d863f-ebff-4abb-b0ad-f9bad0e75aa4"
]

VerificationTest[
    {DualFindMaximum[(-Dual[1, 1]) * x * Cos[x], {x, 2}], DualFindMaximum[
    (-x) * Cos[Dual[1, 1] * x], {x, 2}]}
    ,
    {{Dual[3.2883713955908966, 3.2883713955908966], {x -> Dual[3.425618459492147,
     -1.0418883362877956*^-11]}}, {Dual[3.2883713955908966, -3.2883713955908966
    ], {x -> Dual[3.425618459492147, -3.425618459492147]}}}
    ,
    TestID -> "29d62661-bf9e-4ce0-ad29-d4fda175f628"
]

VerificationTest[
    {DualFindMinimum[Sin[x] * Sin[Dual[2, 1] * y], {{x, 2}, {y, 2}}], DualFindMinimum[
    Sin[Dual[1, 1] * x] * Sin[2 * y], {{x, 2}, {y, 2}}], DualFindMinimum[
    Dual[0, 1] + Sin[1 * x] * Sin[2 * y], {{x, 2}, {y, 2}}]}
    ,
    {{Dual[-1., -6.0641305537977*^-18], {x -> Dual[1.5707963225561392, 2.5704377344461602*^-26
    ], y -> Dual[2.356194488451062, -1.1780972433548897]}}, {Dual[-1., -1.7967064353811206*^-17
    ], {x -> Dual[1.5707963225561392, -1.5707963183173819], y -> Dual[2.356194488451062,
     3.1285739149167003*^-26]}}, {Dual[-1., 1.], {x -> Dual[1.5707963225561392,
     0.], y -> Dual[2.356194488451062, 0.]}}}
    ,
    TestID -> "66b29d4b-b1b6-41b7-9dfe-d4a5a528e986"
]

VerificationTest[
    {DualFindMaximum[(-Sin[x]) * Sin[Dual[2, 1] * y], {{x, 2}, {y, 2}}], 
    DualFindMaximum[(-Sin[Dual[1, 1] * x]) * Sin[2 * y], {{x, 2}, {y, 2}}
    ], DualFindMaximum[-Dual[0, 1] - Sin[1 * x] * Sin[2 * y], {{x, 2}, {y,
     2}}]}
    ,
    {{Dual[1., 6.0641305537977*^-18], {x -> Dual[1.5707963225561392, 2.5704377344461602*^-26
    ], y -> Dual[2.356194488451062, -1.1780972433548897]}}, {Dual[1., 1.7967064353811206*^-17
    ], {x -> Dual[1.5707963225561392, -1.5707963183173819], y -> Dual[2.356194488451062,
     3.1285739149167003*^-26]}}, {Dual[1., -1.], {x -> Dual[1.5707963225561392,
     0.], y -> Dual[2.356194488451062, 0.]}}}
    ,
    TestID -> "aaff126c-2c10-4635-ac9f-3a544d2bdbaa"
]

EndTestSection[]

BeginTestSection["AddDualHandling"]

VerificationTest[
    AddDualHandling[fun, funPrime]; {fun[a], fun[Dual[a, b]], fun[a1, a2],
     fun[Dual[a1, b1], a2], fun[a1, Dual[a2, b2]], fun[Dual[a1, b1], Dual[
    a2, b2]]}
    ,
    {fun[a], Dual[fun[a], b * funPrime[a]], fun[a1, a2], fun[Dual[a1, b1],
     a2], fun[a1, Dual[a2, b2]], fun[Dual[a1, b1], Dual[a2, b2]]}
    ,
    TestID -> "24e3267b-6b21-49c9-808a-d31aa5871741"
]

VerificationTest[
    AddDualHandling[fun, Array[funPrime, 2]]; {fun[a], fun[Dual[a, b]], fun[
    a1, a2], fun[Dual[a1, b1], a2], fun[a1, Dual[a2, b2]], fun[Dual[a1, b1
    ], Dual[a2, b2]]}
    ,
    {fun[a], Dual[fun[a], b * funPrime[a]], fun[a1, a2], Dual[fun[a1, a2],
     b1 * funPrime[1][a1, a2]], Dual[fun[a1, a2], b2 * funPrime[2][a1, a2
    ]], Dual[fun[a1, a2], b1 * funPrime[1][a1, a2] + b2 * funPrime[2][a1,
     a2]]}
    ,
    TestID -> "1f79cfd6-96c2-402d-8855-17a3eeaaa516"
]

VerificationTest[
    AddDualHandling[fun2, 1]; {fun2[a], fun2[Dual[a, b]], fun2[a1, a2], fun2[
    Dual[a1, b1], a2], fun2[a1, Dual[a2, b2]], fun2[Dual[a1, b1], Dual[a2,
     b2]]}
    ,
    {fun2[a], Dual[fun2[a], b * Derivative[1][fun2][a]], fun2[a1, a2], fun2[
    Dual[a1, b1], a2], fun2[a1, Dual[a2, b2]], fun2[Dual[a1, b1], Dual[a2,
     b2]]}
    ,
    TestID -> "be5d31f6-1da0-4fe7-829e-1db0a1f63116"
]

VerificationTest[
    AddDualHandling[fun2, 2]; {fun2[a], fun2[Dual[a, b]], fun2[a1, a2], fun2[
    Dual[a1, b1], a2], fun2[a1, Dual[a2, b2]], fun2[Dual[a1, b1], Dual[a2,
     b2]]}
    ,
    {fun2[a], Dual[fun2[a], b * Derivative[1][fun2][a]], fun2[a1, a2], Dual[
    fun2[a1, a2], b1 * Derivative[1, 0][fun2][a1, a2]], Dual[fun2[a1, a2],
     b2 * Derivative[0, 1][fun2][a1, a2]], Dual[fun2[a1, a2], b2 * Derivative[
    0, 1][fun2][a1, a2] + b1 * Derivative[1, 0][fun2][a1, a2]]}
    ,
    TestID -> "786f0819-448c-4d99-945a-8c13eabc89f9"
]

EndTestSection[]

BeginTestSection["End"]

EndTestSection[]

EndTestSection[]