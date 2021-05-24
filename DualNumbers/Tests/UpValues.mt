BeginTestSection["UpValues"]

BeginTestSection["Initialization"]

EndTestSection[]

BeginTestSection["Plus, Times, Power"]

VerificationTest[
    {Dual[1, 2] + 3, Dual[1, 2] + Dual[3, 4], Dual[{1}, {2}] + 3, Dual[{1
        }, {2}] + Dual[{3}, {4}], Dual[a1, b1] + Dual[a2, b2]}
    ,
    {Dual[4, 2], Dual[4, 6], Dual[{4}, {2}], Dual[{4}, {6}], Dual[a1 + a2,
         b1 + b2]}
    ,
    TestID -> "1c34a7ec-32a6-4988-8dcf-b4667c102eb3"
]

VerificationTest[
    {Dual[1, 2] * 3, Dual[1, 2] * Dual[3, 4], Dual[{1}, {2}] * 3, Dual[{1
        }, {2}] * Dual[{3}, {4}], Dual[a1, b1] * Dual[a2, b2]}
    ,
    {Dual[3, 6], Dual[3, 10], Dual[{3}, {6}], Dual[{3}, {10}], Dual[a1 * 
        a2, a2 * b1 + a1 * b2]}
    ,
    TestID -> "04375380-f83b-4293-83ee-f30c1500d280"
]

VerificationTest[
    Times @@ Flatten[{Dual[a, b], Array[c, 25]}]
    ,
    Dual[_, _]
    ,
    TestID -> "864a7296-c903-4630-9081-b86bd3fcad01"
    ,
    TimeConstraint -> 2
    ,
    SameTest -> MatchQ
]

VerificationTest[
    Plus @@ Flatten[{Dual[a, b], Array[c, 25]}]
    ,
    c[1] + c[2] + c[3] + c[4] + c[5] + c[6] + c[7] + c[8] + c[9] + c[10] +
         c[11] + c[12] + c[13] + c[14] + c[15] + c[16] + c[17] + c[18] + c[19
        ] + c[20] + c[21] + c[22] + c[23] + c[24] + c[25] + Dual[a, b]
    ,
    TestID -> "48907ddd-0a07-4876-b48e-1aac0c03cbd0"
    ,
    TimeConstraint -> 2
]

VerificationTest[
    0^Dual[0, 1]
    ,
    Dual[Indeterminate, Indeterminate]
    ,
    {Power::indet, Power::indet}
    ,
    TestID -> "ef3b9e7c-1901-4391-ad48-aca96b7c5c76"
]

VerificationTest[
    Dual[0, 1]^0
    ,
    Dual[Indeterminate, Indeterminate]
    ,
    {Power::indet, Power::infy, Infinity::indet}
    ,
    TestID -> "71c987d7-b03d-40fe-ac61-606e190a8ed2"
]

VerificationTest[
    Dual[0, 1]^Dual[0, 1]
    ,
    Dual[Indeterminate, Indeterminate]
    ,
    {Power::indet, Power::infy, Infinity::indet, Power::indet}
    ,
    TestID -> "e7ca5c2c-9988-4b1a-aeed-ea6353b16463"
]

VerificationTest[
    testValuesSym = Join[{\[FormalX][1], \[FormalX][2]}, UnpackDualArray[
        Dual[Array[\[FormalA], {10}], Array[\[FormalB], {10}]]]]; testValuesNum =
        BlockRandom[
            SeedRandom[1];
            UnpackDualArray[Dual[RandomReal[{-2, 2}, 50], RandomReal[{-2,
                 2}, 50]]]
        ]; {Plus @@ testValuesSym, Plus @@ testValuesNum, DualApply[Expand
            ][Times @@ testValuesSym[[1 ;; 4]]], Times @@ testValuesNum}
    ,
    {Dual[\[FormalA][1] + \[FormalA][2] + \[FormalA][3] + \[FormalA][4] +
         \[FormalA][5] + \[FormalA][6] + \[FormalA][7] + \[FormalA][8] + \[FormalA][
        9] + \[FormalA][10] + \[FormalX][1] + \[FormalX][2], \[FormalB][1] + 
        \[FormalB][2] + \[FormalB][3] + \[FormalB][4] + \[FormalB][5] + \[FormalB][
        6] + \[FormalB][7] + \[FormalB][8] + \[FormalB][9] + \[FormalB][10]],
         Dual[-8.42866398437041, -5.969132875714167], Dual[\[FormalA][1] * \[FormalA][
        2] * \[FormalX][1] * \[FormalX][2], \[FormalA][2] * \[FormalB][1] * \[FormalX][
        1] * \[FormalX][2] + \[FormalA][1] * \[FormalB][2] * \[FormalX][1] * 
        \[FormalX][2]], Dual[-4.023358697120797*^-9, 5.5480408466810513*^-8]}
    ,
    TestID -> "b1dc5aa4-58a9-4f26-8a74-f453ada0ee3f"
]

VerificationTest[
    {Fold[Plus, testValuesNum] == Plus @@ testValuesNum, Fold[Times, testValuesNum
        ] == Times @@ testValuesNum, NonStandard[Fold[Times, testValuesNum]] 
        == Total[Apply[Times, DualTuples[testValuesNum], {1}]]}
    ,
    {True, True, True}
    ,
    TestID -> "26171df0-0069-4981-8d67-5e2e2efd3d0e"
]

VerificationTest[
    largeTestArray = RandomSample[Join[UnpackDualArray[Dual @@ RandomReal[
        {0.99, 1.01}, {2, 10000}]], RandomReal[{0.99, 1.01}, {10000}]]]; {TimeConstrained[
        Abs[Plus @@ largeTestArray - Fold[Plus, largeTestArray]] < 10^(-8), 1
        ], TimeConstrained[Abs[Times @@ largeTestArray - Fold[Times, largeTestArray
        ]] < 10^(-10), 1]}
    ,
    {True, True}
    ,
    TestID -> "1f06c788-1bed-49b6-b76f-c018c57a1320"
]

VerificationTest[
    {Plus @@ RandomSample[Flatten[{Dual[1., 2.], RandomReal[1, 10000]}]],
         Times @@ RandomSample[Flatten[{Dual[1., 2.], RandomReal[{0.99, 1.01},
         10000]}]]}
    ,
    {Dual[_?NumericQ, _?NumericQ]..}
    ,
    SameTest -> MatchQ
    ,
    TimeConstraint -> 2
    ,
    TestID -> "d58e656f-2418-4036-bc3a-887fb3715006"
]

EndTestSection[]

BeginTestSection["Boolean functions"]

VerificationTest[
    {Dual[1, 2] == 1, Dual[1, 2] == 1., Dual[1, 2] != 2, Dual[1, 2] != 2.
        }
    ,
    {True, True, True, True}
    ,
    TestID -> "cf03268d-d4a0-472a-a103-704bc90b7cf7"
]

VerificationTest[
    {Dual[1, 2] == Dual[1, 2], Dual[1, 2] == Dual[1, 3], Dual[1, 2] == Dual[
        1., 3], Dual[1, 2] != Dual[2, 3], Dual[1, 2] != Dual[2., 3], Dual[1, 
        2] != Dual[1, 3]}
    ,
    {True, True, True, True, True, False}
    ,
    TestID -> "70c3255b-71b0-4199-919e-3787e79f4444"
]

VerificationTest[
    {Dual[1, 2] < 2, Dual[1, 2] <= 2, Dual[1, 2] < Dual[2, 2], Dual[1, 2]
         <= Dual[2, 2]}
    ,
    {True, True, True, True}
    ,
    TestID -> "4d77ee91-a268-43fb-aedf-e9242a446a4e"
]

VerificationTest[
    {Dual[{{2, 1, 5}, {5, 0, 5}, {5, 3, 2}}, {{4, 5, 1}, {0, 3, 1}, {3, 1,
         0}}] == Dual[{{2., 1., 5.}, {5., 0., 5.}, {5., 3., 2.}}, {{4., 5., 1.
        }, {0., 3., 1.}, {3., 1., 0.}}], Dual[{{2, 1, 5}, {5, 0, 5}, {5, 3, 2
        }}, {{1, 0, 0}, {0, 1, 0}, {0, 0, 1}}] == Dual[{{2., 1., 5.}, {5., 0.,
         5.}, {5., 3., 2.}}, {{4., 5., 1.}, {0., 3., 1.}, {3., 1., 0.}}]}
    ,
    {True, True}
    ,
    TestID -> "e35ba256-da43-4227-81bc-1f038b49366d"
]

EndTestSection[]

BeginTestSection["Arrays"]

BeginTestSection["MatrixPower"]

VerificationTest[
    angle = Pi / 7; mat = RotationMatrix[angle]; {MatrixPower[N[mat], 1000
        ], MatrixPower[N[mat, 20], 1000], MatrixPower[N[mat, 50], 1000]}
    ,
    ConstantArray[RotationMatrix[1000 * angle], 3]
    ,
    SameTest -> (Max[Abs[#1 - #2]] < 10^(-10)&)
    ,
    TimeConstraint -> 2
    ,
    TestID -> "7175ef12-fab3-41c8-9090-8d7b0294c2db"
]

EndTestSection[]

BeginTestSection["Fold"]

VerificationTest[
    {DualApply[Expand][Fold[Plus, Dual[Array[\[FormalA], 5], Array[\[FormalB],
         5]]]], DualApply[Expand][Fold[Times, Dual[Array[\[FormalA], 5], Array[
        \[FormalB], 5]]]], DualApply[Expand][Fold[Plus, \[FormalX], Dual[Array[
        \[FormalA], 5], Array[\[FormalB], 5]]]], DualApply[Expand][Fold[Times,
         \[FormalX], Dual[Array[\[FormalA], 5], Array[\[FormalB], 5]]]]}
    ,
    {Dual[\[FormalA][1] + \[FormalA][2] + \[FormalA][3] + \[FormalA][4] +
         \[FormalA][5], \[FormalB][1] + \[FormalB][2] + \[FormalB][3] + \[FormalB][
        4] + \[FormalB][5]], Dual[\[FormalA][1] * \[FormalA][2] * \[FormalA][
        3] * \[FormalA][4] * \[FormalA][5], \[FormalA][2] * \[FormalA][3] * \[FormalA][
        4] * \[FormalA][5] * \[FormalB][1] + \[FormalA][1] * \[FormalA][3] * 
        \[FormalA][4] * \[FormalA][5] * \[FormalB][2] + \[FormalA][1] * \[FormalA][
        2] * \[FormalA][4] * \[FormalA][5] * \[FormalB][3] + \[FormalA][1] * 
        \[FormalA][2] * \[FormalA][3] * \[FormalA][5] * \[FormalB][4] + \[FormalA][
        1] * \[FormalA][2] * \[FormalA][3] * \[FormalA][4] * \[FormalB][5]], 
        Dual[\[FormalX] + \[FormalA][1] + \[FormalA][2] + \[FormalA][3] + \[FormalA][
        4] + \[FormalA][5], \[FormalB][1] + \[FormalB][2] + \[FormalB][3] + \[FormalB][
        4] + \[FormalB][5]], Dual[\[FormalX] * \[FormalA][1] * \[FormalA][2] 
        * \[FormalA][3] * \[FormalA][4] * \[FormalA][5], \[FormalX] * \[FormalA][
        2] * \[FormalA][3] * \[FormalA][4] * \[FormalA][5] * \[FormalB][1] + 
        \[FormalX] * \[FormalA][1] * \[FormalA][3] * \[FormalA][4] * \[FormalA][
        5] * \[FormalB][2] + \[FormalX] * \[FormalA][1] * \[FormalA][2] * \[FormalA][
        4] * \[FormalA][5] * \[FormalB][3] + \[FormalX] * \[FormalA][1] * \[FormalA][
        2] * \[FormalA][3] * \[FormalA][5] * \[FormalB][4] + \[FormalX] * \[FormalA][
        1] * \[FormalA][2] * \[FormalA][3] * \[FormalA][4] * \[FormalB][5]]}
    ,
    TestID -> "9113ab76-b211-4f2b-b03b-085cd044cdb3"
]

VerificationTest[
    {DualApply[Expand][FoldList[Plus, Dual[Array[\[FormalA], 3], Array[\[FormalB],
         3]]]], DualApply[Expand][FoldList[Times, Dual[Array[\[FormalA], 3], 
        Array[\[FormalB], 3]]]], DualApply[Expand][FoldList[Plus, \[FormalX],
         Dual[Array[\[FormalA], 3], Array[\[FormalB], 3]]]], DualApply[Expand
        ][FoldList[Times, \[FormalX], Dual[Array[\[FormalA], 3], Array[\[FormalB],
         3]]]]}
    ,
    {Dual[{\[FormalA][1], \[FormalA][1] + \[FormalA][2], \[FormalA][1] + 
        \[FormalA][2] + \[FormalA][3]}, {\[FormalB][1], \[FormalB][1] + \[FormalB][
        2], \[FormalB][1] + \[FormalB][2] + \[FormalB][3]}], Dual[{\[FormalA][
        1], \[FormalA][1] * \[FormalA][2], \[FormalA][1] * \[FormalA][2] * \[FormalA][
        3]}, {\[FormalB][1], \[FormalA][2] * \[FormalB][1] + \[FormalA][1] * 
        \[FormalB][2], \[FormalA][2] * \[FormalA][3] * \[FormalB][1] + \[FormalA][
        1] * \[FormalA][3] * \[FormalB][2] + \[FormalA][1] * \[FormalA][2] * 
        \[FormalB][3]}], Dual[{\[FormalX], \[FormalX] + \[FormalA][1], \[FormalX]
         + \[FormalA][1] + \[FormalA][2], \[FormalX] + \[FormalA][1] + \[FormalA][
        2] + \[FormalA][3]}, {0, \[FormalB][1], \[FormalB][1] + \[FormalB][2],
         \[FormalB][1] + \[FormalB][2] + \[FormalB][3]}], Dual[{\[FormalX], \[FormalX]
         * \[FormalA][1], \[FormalX] * \[FormalA][1] * \[FormalA][2], \[FormalX]
         * \[FormalA][1] * \[FormalA][2] * \[FormalA][3]}, {0, \[FormalX] * \[FormalB][
        1], \[FormalX] * \[FormalA][2] * \[FormalB][1] + \[FormalX] * \[FormalA][
        1] * \[FormalB][2], \[FormalX] * \[FormalA][2] * \[FormalA][3] * \[FormalB][
        1] + \[FormalX] * \[FormalA][1] * \[FormalA][3] * \[FormalB][2] + \[FormalX]
         * \[FormalA][1] * \[FormalA][2] * \[FormalB][3]}]}
    ,
    TestID -> "4335f9a5-34b4-4e16-a2a0-85b44a4495bf"
]

VerificationTest[
    Fold[Times, Dual[1, 2]]
    ,
    2
    ,
    {Dual::arrayOp}
    ,
    TestID -> "365ecee2-bd31-40d4-87d2-f63e4feab3df"
]

VerificationTest[
    Fold[Times, 1, Dual[1, 2]]
    ,
    2
    ,
    {Dual::arrayOp}
    ,
    TestID -> "122151f9-4073-4704-8520-10568d9ea09e"
]

VerificationTest[
    FoldList[Times, Dual[1, 2]]
    ,
    Dual[1, 2]
    ,
    {Dual::arrayOp}
    ,
    TestID -> "c4f5161c-13e4-44fb-ac74-15322b838465"
]

VerificationTest[
    FoldList[Times, 1, Dual[1, 2]]
    ,
    Dual[1, 1, 2]
    ,
    {Dual::arrayOp, Dual::argt}
    ,
    TestID -> "a1372ded-f932-44b8-bb40-1e9d41c179e4"
]

VerificationTest[
    {Fold[Times, Dual[1, 2], Range[5]], FoldList[Times, Dual[1, 2], Range[
        5]]}
    ,
    {Dual[120, 240], {Dual[1, 2], Dual[1, 2], Dual[2, 4], Dual[6, 12], Dual[
        24, 48], Dual[120, 240]}}
    ,
    TestID -> "7bb97bb8-1617-4e4f-9035-0802f2dda2ec"
]

EndTestSection[]

BeginTestSection["Dot"]

VerificationTest[
    {Dot[Dual[a, b]], Dot[Dual[{1, 2}, {3, 4}]]}
    ,
    {Dual[a, b], Dual[{1, 2}, {3, 4}]}
    ,
    TestID -> "6aec3a2d-1c2c-40c1-bb64-1f551439b190"
]

VerificationTest[
    {Dual[{}, {}] . {}, Dual[{a1}, {b1}] . {a2}, Dual[Array[a1, 2], Array[
        b1, 2]] . Array[a2, 2], Dual[Array[a1, {2, 2}], Array[b1, {2, 2}]] . 
        Array[a2, {2, 2}]}
    ,
    {Dual[0, 0], Dual[a1 * a2, a2 * b1], Dual[a1[1] * a2[1] + a1[2] * a2[
        2], a2[1] * b1[1] + a2[2] * b1[2]], Dual[{{a1[1, 1] * a2[1, 1] + a1[1,
         2] * a2[2, 1], a1[1, 1] * a2[1, 2] + a1[1, 2] * a2[2, 2]}, {a1[2, 1]
         * a2[1, 1] + a1[2, 2] * a2[2, 1], a1[2, 1] * a2[1, 2] + a1[2, 2] * a2[
        2, 2]}}, {{a2[1, 1] * b1[1, 1] + a2[2, 1] * b1[1, 2], a2[1, 2] * b1[1,
         1] + a2[2, 2] * b1[1, 2]}, {a2[1, 1] * b1[2, 1] + a2[2, 1] * b1[2, 2
        ], a2[1, 2] * b1[2, 1] + a2[2, 2] * b1[2, 2]}}]}
    ,
    TestID -> "13b47a3e-c1a1-48e9-859f-5be87a636b0a"
]

VerificationTest[
    {{} . Dual[{}, {}], {a2} . Dual[{a1}, {b1}], Array[a2, 2] . Dual[Array[
        a1, 2], Array[b1, 2]], Array[a2, {2, 2}] . Dual[Array[a1, {2, 2}], Array[
        b1, {2, 2}]]}
    ,
    {Dual[0, 0], Dual[a1 * a2, a2 * b1], Dual[a1[1] * a2[1] + a1[2] * a2[
        2], a2[1] * b1[1] + a2[2] * b1[2]], Dual[{{a1[1, 1] * a2[1, 1] + a1[2,
         1] * a2[1, 2], a1[1, 2] * a2[1, 1] + a1[2, 2] * a2[1, 2]}, {a1[1, 1]
         * a2[2, 1] + a1[2, 1] * a2[2, 2], a1[1, 2] * a2[2, 1] + a1[2, 2] * a2[
        2, 2]}}, {{a2[1, 1] * b1[1, 1] + a2[1, 2] * b1[2, 1], a2[1, 1] * b1[1,
         2] + a2[1, 2] * b1[2, 2]}, {a2[2, 1] * b1[1, 1] + a2[2, 2] * b1[2, 1
        ], a2[2, 1] * b1[1, 2] + a2[2, 2] * b1[2, 2]}}]}
    ,
    TestID -> "c686d623-4510-4a1b-8183-f763bfb2527a"
]

VerificationTest[
    {Dual[{}, {}] . Dual[{}, {}], Dual[{a1}, {b1}] . Dual[{a2}, {b2}], Dual[
        Array[a1, 2], Array[b1, 2]] . Dual[Array[a2, 2], Array[b2, 2]]}
    ,
    {Dual[0, 0], Dual[a1 * a2, a2 * b1 + a1 * b2], Dual[a1[1] * a2[1] + a1[
        2] * a2[2], a2[1] * b1[1] + a2[2] * b1[2] + a1[1] * b2[1] + a1[2] * b2[
        2]]}
    ,
    TestID -> "46b86cb7-cffd-4708-a130-6e39d76cc346"
]

VerificationTest[
    {Dual[1, 2] . {1, 2}, {1, 2} . Dual[1, 2], Dual[1, 2] . Dual[1, 2], Dual[
        {1}, {2}] . 3, 3 . Dual[{1}, {2}], Dual[a, b] . x . y . z, Dot @@ Flatten[
        {Dual[a, b], Array[c, 20]}]}
    ,
    {Dual[_, _]..}
    ,
    TestID -> "c5b5445e-2a00-4eef-b536-58381696d908"
    ,
    TimeConstraint -> 2
    ,
    SameTest -> MatchQ
]

VerificationTest[
    With[{manyDuals = RandomSample[Join[PackDualArray /@ Apply[Dual, RandomReal[
        1, {1000, 2, 3, 3}], {1}], RandomReal[1, {1000, 3, 3}]]]},
        Through[{DualArrayQ, Dimensions}[Dot @@ manyDuals]]
]
    ,
    {True, {3, 3}}
    ,
    TimeConstraint -> 5
    ,
    TestID -> "a69d21a2-abad-4299-8cd2-a73d6e46f276"
]

VerificationTest[
    With[{manyDuals = RandomSample[Join[PackDualArray /@ Apply[Dual, RandomInteger[
        10, {20, 2, 5, 5}], {1}], RandomInteger[10, {20, 5, 5}]]]},
        Dot @@ manyDuals === Fold[Dot, manyDuals]
]
    ,
    True
    ,
    TestID -> "1cf05627-d07a-45b7-affb-a6e048698d41"
    ,
    TimeConstraint -> 5
]

EndTestSection[]

BeginTestSection["Matrix inversion"]

VerificationTest[
    {mat1, mat2, mat3, mat4} = RandomReal[1, {4, 4, 4}]; dualMat = Dual[mat1,
         mat2]; dualMat2 = Dual[mat3, mat4]; DualArrayQ[Inverse[dualMat]]
    ,
    True
    ,
    TestID -> "db5df0c2-00c8-422c-951b-e18e958fbdb4"
]

VerificationTest[
    Chop[Inverse[dualMat] . dualMat - ToDual[IdentityMatrix[4]]]
    ,
    Dual[{{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}, {{0, 0,
         0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}]
    ,
    TestID -> "cc778c12-5aca-4293-845b-9158062f2915"
]

VerificationTest[
    Standard[Inverse[dualMat]] == Inverse[mat1]
    ,
    True
    ,
    TestID -> "98dd14f7-8390-4bc1-a1ba-3686d60fb76e"
]

VerificationTest[
    ls = LinearSolve[dualMat]
    ,
    _DualLinearSolveFunction
    ,
    SameTest -> MatchQ
    ,
    TestID -> "f0c5370a-9ceb-4698-880e-560d77126aa2"
]

VerificationTest[
    Chop[{ls[mat3] - Inverse[dualMat] . mat3, ls[dualMat2] - Inverse[dualMat
        ] . dualMat2}]
    ,
    {Dual[{{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}, {{0, 
        0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}], Dual[{{0, 0, 0,
         0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}, {{0, 0, 0, 0}, {0, 0,
         0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}]}
    ,
    TestID -> "6898df53-1b08-4aca-8714-4e5d92bd169f"
]

VerificationTest[
    Chop[{LinearSolve[dualMat, mat3] - Inverse[dualMat] . mat3, LinearSolve[
        dualMat, dualMat2] - Inverse[dualMat] . dualMat2}]
    ,
    {Dual[{{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}, {{0, 
        0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}], Dual[{{0, 0, 0,
         0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}, {{0, 0, 0, 0}, {0, 0,
         0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}]}
    ,
    TestID -> "4b556395-dd19-4c8d-879a-dc8df626b152"
]

VerificationTest[
    ls2 = LinearSolve[dualMat, Method -> "CofactorExpansion"]; Chop[{ls2[
        mat3] - ls[mat3], ls2[dualMat2] - ls[dualMat2], LinearSolve[dualMat, 
        mat3, Method -> "CofactorExpansion"] - ls[mat3], LinearSolve[dualMat,
         dualMat2, Method -> "CofactorExpansion"] - ls[dualMat2]}]
    ,
    {Dual[{{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}, {{0, 
        0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}], Dual[{{0, 0, 0,
         0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}, {{0, 0, 0, 0}, {0, 0,
         0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}], Dual[{{0, 0, 0, 0}, {0, 0, 0, 0
        }, {0, 0, 0, 0}, {0, 0, 0, 0}}, {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0,
         0}, {0, 0, 0, 0}}], Dual[{{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, 
        {0, 0, 0, 0}}, {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0
        }}]}
    ,
    TestID -> "dfc397bc-0e22-432e-8e81-481e2a5c43c2"
]

VerificationTest[
    With[{n = 3000},
        MatchQ[HoldPattern[Dual[__?VectorQ]]] /@ {LinearSolve[Dual[RandomReal[
            1, n * {1, 1}], RandomReal[1, n * {1, 1}]], RandomReal[1, n]], LinearSolve[
            Dual[RandomReal[1, n * {1, 1}], RandomReal[1, n * {1, 1}]], Dual[RandomReal[
            1, n], RandomReal[1, n]]]}
]
    ,
    {True, True}
    ,
    TimeConstraint -> 5
    ,
    TestID -> "2411cdd8-ce1c-4cb3-aad6-66dec0cdb102"
]

EndTestSection[]

BeginTestSection["Selecting"]

VerificationTest[
    arr = Dual[Array[a, {3, 2}], Array[b, {3, 2}]]; {arr[[{1, 2}]], Take[
        arr, 2], Drop[arr, 2], Extract[arr, {{1}, {2}}], Extract[arr, {{1, 1},
         {2, 2}}]}
    ,
    {Dual[{{a[1, 1], a[1, 2]}, {a[2, 1], a[2, 2]}}, {{b[1, 1], b[1, 2]}, 
        {b[2, 1], b[2, 2]}}], Dual[{{a[1, 1], a[1, 2]}, {a[2, 1], a[2, 2]}}, 
        {{b[1, 1], b[1, 2]}, {b[2, 1], b[2, 2]}}], Dual[{{a[3, 1], a[3, 2]}},
         {{b[3, 1], b[3, 2]}}], Dual[{{a[1, 1], a[1, 2]}, {a[2, 1], a[2, 2]}},
         {{b[1, 1], b[1, 2]}, {b[2, 1], b[2, 2]}}], Dual[{a[1, 1], a[2, 2]}, 
        {b[1, 1], b[2, 2]}]}
    ,
    TestID -> "56931f4e-6eaf-494c-a315-14a7d8c0c2d7"
]

VerificationTest[
    Through[{First, Rest, Most, Last}[arr]]
    ,
    {Dual[{a[1, 1], a[1, 2]}, {b[1, 1], b[1, 2]}], Dual[{{a[2, 1], a[2, 2
        ]}, {a[3, 1], a[3, 2]}}, {{b[2, 1], b[2, 2]}, {b[3, 1], b[3, 2]}}], Dual[
        {{a[1, 1], a[1, 2]}, {a[2, 1], a[2, 2]}}, {{b[1, 1], b[1, 2]}, {b[2, 
        1], b[2, 2]}}], Dual[{a[3, 1], a[3, 2]}, {b[3, 1], b[3, 2]}]}
    ,
    TestID -> "3ea728f1-5171-4f9b-96c7-8e0e046b6361"
]

VerificationTest[
    {Join[arr[[{1}]], arr[[2 ;; All]]] === arr, Join[arr[[All, {1}]], arr[[
        All, 2 ;; All]], 2] === arr}
    ,
    {True, True}
    ,
    TestID -> "083afab6-6a66-4298-abd7-5aaf57544739"
]

VerificationTest[
    Through[{Transpose, Mean, Total, Flatten}[arr]]
    ,
    {Dual[{{a[1, 1], a[2, 1], a[3, 1]}, {a[1, 2], a[2, 2], a[3, 2]}}, {{b[
        1, 1], b[2, 1], b[3, 1]}, {b[1, 2], b[2, 2], b[3, 2]}}], Dual[{(1 / 3
        ) * (a[1, 1] + a[2, 1] + a[3, 1]), (1 / 3) * (a[1, 2] + a[2, 2] + a[3,
         2])}, {(1 / 3) * (b[1, 1] + b[2, 1] + b[3, 1]), (1 / 3) * (b[1, 2] +
         b[2, 2] + b[3, 2])}], Dual[{a[1, 1] + a[2, 1] + a[3, 1], a[1, 2] + a[
        2, 2] + a[3, 2]}, {b[1, 1] + b[2, 1] + b[3, 1], b[1, 2] + b[2, 2] + b[
        3, 2]}], Dual[{a[1, 1], a[1, 2], a[2, 1], a[2, 2], a[3, 1], a[3, 2]},
         {b[1, 1], b[1, 2], b[2, 1], b[2, 2], b[3, 1], b[3, 2]}]}
    ,
    TestID -> "2e63fc1c-c822-4ca4-94e0-2dfd7937906f"
]

VerificationTest[
    {GroupBy[arr, First], GroupBy[arr, EvenQ[#1[[1, 1]]]&], GroupBy[arr, 
        First -> Sin], GroupBy[arr, First -> Sin, Total[#1, 2]&]}
    ,
    {Association[a[1, 1] -> Dual[{{a[1, 1], a[1, 2]}}, {{b[1, 1], b[1, 2]
        }}], a[2, 1] -> Dual[{{a[2, 1], a[2, 2]}}, {{b[2, 1], b[2, 2]}}], a[3,
         1] -> Dual[{{a[3, 1], a[3, 2]}}, {{b[3, 1], b[3, 2]}}]], Association[
        False -> Dual[{{a[1, 1], a[1, 2]}, {a[3, 1], a[3, 2]}}, {{b[1, 1], b[
        1, 2]}, {b[3, 1], b[3, 2]}}], True -> Dual[{{a[2, 1], a[2, 2]}}, {{b[
        2, 1], b[2, 2]}}]], Association[a[1, 1] -> Dual[{{Sin[a[1, 1]], Sin[a[
        1, 2]]}}, {{b[1, 1] * Cos[a[1, 1]], b[1, 2] * Cos[a[1, 2]]}}], a[2, 1
        ] -> Dual[{{Sin[a[2, 1]], Sin[a[2, 2]]}}, {{b[2, 1] * Cos[a[2, 1]], b[
        2, 2] * Cos[a[2, 2]]}}], a[3, 1] -> Dual[{{Sin[a[3, 1]], Sin[a[3, 2]]
        }}, {{b[3, 1] * Cos[a[3, 1]], b[3, 2] * Cos[a[3, 2]]}}]], Association[
        a[1, 1] -> Dual[Sin[a[1, 1]] + Sin[a[1, 2]], b[1, 1] * Cos[a[1, 1]] +
         b[1, 2] * Cos[a[1, 2]]], a[2, 1] -> Dual[Sin[a[2, 1]] + Sin[a[2, 2]],
         b[2, 1] * Cos[a[2, 1]] + b[2, 2] * Cos[a[2, 2]]], a[3, 1] -> Dual[Sin[
        a[3, 1]] + Sin[a[3, 2]], b[3, 1] * Cos[a[3, 1]] + b[3, 2] * Cos[a[3, 
        2]]]]}
    ,
    TestID -> "e4067233-cde4-458e-988c-82e17b75cb70"
]

VerificationTest[
    GroupBy[Dual[1, 2], f]
    ,
    GroupBy[Dual[1, 2], f]
    ,
    {Dual::arrayOp, GroupBy::list1}
    ,
    TestID -> "5ac38c7c-bda0-4821-9361-a5629eb09265"
]

VerificationTest[
    GroupBy[Dual[1, 2], f, g]
    ,
    GroupBy[Dual[1, 2], f, g]
    ,
    {Dual::arrayOp, GroupBy::list1}
    ,
    TestID -> "ad7e9d62-cc10-4fb1-82cf-5db2d37450b8"
]

VerificationTest[
    GroupBy[Dual[1, 2], {f}]
    ,
    GroupBy[Dual[1, 2], {f}]
    ,
    {Dual::arrayOp, GroupBy::list1}
    ,
    TestID -> "3cc02ffc-c1a5-41a1-bbfd-631cd3f546f0"
]

VerificationTest[
    GroupBy[Dual[{1}, {2}], {f}]
    ,
    GroupBy[Dual[{1}, {2}], {f}]
    ,
    {Dual::groupbyfun, GroupBy::list1}
    ,
    TestID -> "03f34999-0633-4cd6-93d8-580c9dedb0a1"
]

VerificationTest[
    GroupBy[Dual[{1}, {2}], {f}, g]
    ,
    GroupBy[Dual[{1}, {2}], {f}, g]
    ,
    {Dual::groupbyfun, GroupBy::list1}
    ,
    TestID -> "ab3ce908-750e-4e32-b377-a022621be77e"
]

VerificationTest[
    Module[{res},
        On["Packing"];
        res = {Through[(Map /@ {First, Last, Most, Rest})[arr]], GroupBy[
            arr, First -> Rest]};
        Off["Packing"];
        res
]
    ,
    {{Dual[{a[1, 1], a[2, 1], a[3, 1]}, {b[1, 1], b[2, 1], b[3, 1]}], Dual[
        {a[1, 2], a[2, 2], a[3, 2]}, {b[1, 2], b[2, 2], b[3, 2]}], Dual[{{a[1,
         1]}, {a[2, 1]}, {a[3, 1]}}, {{b[1, 1]}, {b[2, 1]}, {b[3, 1]}}], Dual[
        {{a[1, 2]}, {a[2, 2]}, {a[3, 2]}}, {{b[1, 2]}, {b[2, 2]}, {b[3, 2]}}]
        }, Association[a[1, 1] -> Dual[{{a[1, 2]}}, {{b[1, 2]}}], a[2, 1] -> 
        Dual[{{a[2, 2]}}, {{b[2, 2]}}], a[3, 1] -> Dual[{{a[3, 2]}}, {{b[3, 2
        ]}}]]}
    ,
    TestID -> "8c51c238-f95d-4a48-87ab-c2af2b64984c"
]

VerificationTest[
    {Select[Dual[{}, {}], EvenQ], Select[Dual[{1, 2}, {a, b}], False&], Select[
        Dual[{1, 2}, {a, b}], True&], Select[Dual[{1, 2}, {a, b}], EvenQ], Select[
        UnpackDualArray[Dual[{1, 2}, {a, b}]], EvenQ]}
    ,
    {Dual[{}, {}], Dual[{}, {}], Dual[{1, 2}, {a, b}], Dual[{2}, {b}], {Dual[
        2, b]}}
    ,
    TestID -> "5a4fbafa-117c-41e4-821a-aa4e30b7a195"
]

VerificationTest[
    Select[Dual[1, 2], EvenQ]
    ,
    Dual[2, 0]
    ,
    {Dual::arrayOp}
    ,
    TestID -> "3af53e2f-f4df-466b-a0fe-8cc3800c82ad"
]

VerificationTest[
    {Pick[Dual[{}, {}], {}], Pick[{}, Dual[{}, {}]], Pick[Dual[{}, {}], Dual[
        {}, {}]], Pick[Dual[{a1, a2}, {b1, b2}], {True, False}], Pick[{a1, a2
        }, Dual[{0, 1}, {b1, b2}], 1], Pick[Dual[{a1, a2}, {b1, b2}], Dual[{0,
         1}, {5, 6}], 1]}
    ,
    {Dual[{}, {}], {}, Dual[{}, {}], Dual[{a1}, {b1}], {a2}, Dual[{a2}, {
        b2}]}
    ,
    TestID -> "b4e494d1-0f16-40bb-a66b-e1ad69a55efc"
]

VerificationTest[
    Pick[Dual[1, 2], {}]
    ,
    Pick[Dual[1, 2], {}]
    ,
    {Dual::arrayOp, Pick::incomp}
    ,
    TestID -> "2ff42615-1afa-4625-8307-ea5a628bf23d"
]

VerificationTest[
    Pick[{}, Dual[1, 2]]
    ,
    Pick[{}, Dual[1, 2]]
    ,
    {Dual::arrayOp, Pick::incomp}
    ,
    TestID -> "4781a4f6-e1eb-47e0-a94d-9ae6f6d84d4d"
]

VerificationTest[
    {Position[Dual[{}, {}], 1], Position[Dual[{1}, {2}], 1]}
    ,
    {{}, {{1}}}
    ,
    TestID -> "4f21f8bb-a42e-4795-af1e-9f9730e8c620"
]

EndTestSection[]

BeginTestSection["Modifying"]

VerificationTest[
    {Prepend[Dual[{1}, {2}], 3], Append[Dual[{1}, {2}], 3], Prepend[Dual[
        {1}, {2}], Dual[3, 4]], Append[Dual[{1}, {2}], Dual[3, 4]]}
    ,
    {Dual[{3, 1}, {0, 2}], Dual[{1, 3}, {2, 0}], Dual[{3, 1}, {4, 2}], Dual[
        {1, 3}, {2, 4}]}
    ,
    TestID -> "b210c438-fa7e-49d6-a6f6-66f1348e392e"
]

VerificationTest[
    {Prepend[3][Dual[{1}, {2}]], Append[3][Dual[{1}, {2}]], Prepend[Dual[
        3, 4]][Dual[{1}, {2}]], Append[Dual[3, 4]][Dual[{1}, {2}]]}
    ,
    {Dual[{3, 1}, {0, 2}], Dual[{1, 3}, {2, 0}], Dual[{3, 1}, {4, 2}], Dual[
        {1, 3}, {2, 4}]}
    ,
    TestID -> "a9b10011-0cb8-4681-b886-06d154177eaa"
]

VerificationTest[
    Prepend[Dual[1, 2], 3]
    ,
    Dual[Dual[3, 0], 1, 2]
    ,
    {Dual::arrayOp, Dual::argt}
    ,
    TestID -> "cdf03783-a870-4aee-8577-d636f5641f8b"
]

VerificationTest[
    Append[Dual[1, 2], 3]
    ,
    Dual[1, 2, Dual[3, 0]]
    ,
    {Dual::arrayOp, Dual::argt}
    ,
    TestID -> "fd694949-0637-46fa-89ed-99a846b88d27"
]

EndTestSection[]

EndTestSection[]

BeginTestSection["End"]

EndTestSection[]

EndTestSection[]