(* Wolfram Language Package *)

BeginPackage["DualNumbers`", {"GeneralUtilities`", "Developer`"}]

(* This list is just to fix syntax highlighting in my edition *)
Dual;
DualArrayQ;
DualLinearSolveFunction;
DualTuples;

Begin["`Private`"]

(* This comes up frequently enough to warrant a dedicated function *)
listPosition[list_, patt_, lvl : _ : {1}, n : _ : DirectedInfinity[1]] := Position[list, patt, lvl, n, Heads -> False];

(* Plus UpValue for long sums. The /; True condition makes sure this one gets priority whenever it matches *)
Dual /: (plus : Plus[
    _Dual,
    Longest @ Repeated[_, {10, DirectedInfinity[1]}]
]) /; True := With[{
    reap = Reap[
        Replace[Unevaluated[plus],
            Dual[a_, b_] :> (Sow[b, "dual"]; a),
            {1}
        ],
        "dual"
    ]
},
    Dual[
        reap[[1]],
        Total[reap[[2]], 2]
    ]
];

(* And one that's faster for short ones *)
Dual /: Dual[a1_, b1_] + Dual[a2_, b2_] := Dual[a1 + a2, b1 + b2];
Dual /: (c : standardPatt) + Dual[a_, b_] := Dual[c + a, b];

Unprotect[foldTimesQ];
foldTimesQ = True;
Protect[foldTimesQ];
Block[{Times}, (* Get rid of the Flat attribute to make sure Times can only match with all its arguments *)
    SetAttributes[Times, Orderless];
    (* Times UpValue for many arguments. *) 
    Dual /: (times : Times[
        _Dual,
        Repeated[_, {10, DirectedInfinity[1]}]
    ]) /; foldTimesQ := Block[{
        foldTimesQ  = False,
        try
    },  (* The Fold cuts down on pattern matching overhead *)
        try = Fold[Times, Unevaluated[times]];
        try /; Head[try] === Dual
    ];
];
(* And one that's faster for short ones *)
Dual /: Dual[a1_, b1_] * Dual[a2_, b2_] := Dual[a1 * a2, b1 * a2 + a1 * b2];
Dual /: (c : standardPatt) * Dual[a_, b_] := Dual[c * a, c * b];

(* Divide and Subtract are generally faster than the - and / infix operators. That's why they get dedicated rules *)
Dual /: HoldPattern @ Subtract[Dual[a1_, b1_], Dual[a2_, b2_]] := Dual[Subtract[a1, a2], Subtract[b1, b2]];

Dual /: HoldPattern @ Divide[Dual[a1_, b1_], Dual[a2_, b2_]] := Dual[
    Divide[a1, a2],
    Subtract[Divide[b1, a2], Divide[a1 * b2, a2^2]]
];

(* Special rules for power; the general rule is covered elsewhere. *)
Dual /: Power[Dual[a_, b_], 1] := Dual[a, b];
Dual /: Power[Dual[Except[0 | 0.], _], 0] := 1;
Dual /: Power[Dual[a_, b_], -1] := Dual[Divide[1, a], -Divide[b, a^2]];

(* (* This definition can be helpful for calculating very large Powers. *)
Dual /: Power[d_Dual, n_Integer?Positive] := Fold[
    Function[If[#2 === 1, d * #1, #1] * #1],
    d,
    Rest[IntegerDigits[n, 2]]
];
*)
(* Set upvalues for most built-in numeric functions where possible (e.g., Exp, Log, Sin, etc.) *)
KeyValueMap[
    Function[{fun, derriv},
        Dual /: HoldPattern[fun[Dual[a_, b_]]] := Dual[fun[a], derriv[a] * b]
    ],
    KeyDrop[{Power, Times, Plus}] @ Select[
        AssociationMap[
            Derivative[1],
            Symbol /@ Select[
                Names["System`*"],
                MemberQ[Attributes[#], NumericFunction]&
            ]
        ],
        MatchQ[derivativePatt]
    ]
];

(* Set upvalues for some 2-argument functions, including Power *)
KeyValueMap[
    Function[{fun, derriv},
        With[{d1 = derriv[[1]], d2 = derriv[[2]]},
            Dual /: HoldPattern[fun[Dual[a1_, b1_], Dual[a2_, b2_]]] := Dual[
                fun[a1, a2],
                d1[a1, a2] * b1 + d2[a1, a2] * b2
            ];
            Dual /: HoldPattern[fun[Dual[a_, b_], c_]] := Dual[fun[a, c], d1[a, c] * b];
            Dual /: HoldPattern[fun[c_, Dual[a_, b_]]] := Dual[fun[c, a], d2[c, a] * b]
        ]
    ],
    Quiet[
        AssociationMap[
            Function[f,
                Derivative[##][f]& @@@ IdentityMatrix[2]
            ],
            {Power, Log, Mod, Binomial, Gamma}
        ],
        {FromPackedArray::punpack1} (* generated by IdentityMatrix[2] when packing messages are on *)
    ]
];

(* Special cases *)
Dual /: Abs[Dual[a_, b_]] := Dual[Abs[a], b * Sign[a]];
Dual /: Sign[Dual[a_, b_]] := Sign[a];

(* Special cases for Clip *)
Dual /: Clip[Dual[a_, b_], {xmin : standardPatt, xmax : standardPatt}] := Dual[
    Clip[a, {xmin, xmax}],
    b * Piecewise[{{1, xmin<= a <= xmax}}, 0]
];
Dual /: Clip[Dual[a_, b_], {xmin : standardPatt, xmax : standardPatt}, {ymin : standardPatt, ymax : standardPatt}] := Dual[
    Clip[a, {xmin, xmax}, {ymin, ymax}],
    b * Piecewise[{{1, xmin<= a <= xmax}}, 0]
];
With[{
    clipDerivatives3arg1 = Piecewise[{{1, #[[2]] <= #[[1]] <= #[[3]]}}, 0]&,
    clipDerivatives3arg2 = Piecewise[{{1, #[[1]] < #[[2]]}}, 0]&,
    clipDerivatives3arg3 = Piecewise[{{1, #[[1]] > #[[3]] && #[[1]] >= #[[2]]}}, 0]&,
    clipDerivatives5arg1 = Piecewise[{{1, #[[2]] <= #[[1]] <= #[[3]]}}, 0]&,
    (*
    clipDerivatives5arg2 = 0&,
    clipDerivatives5arg3 = 0&,
    *)
    clipDerivatives5arg4 = Piecewise[{{1, #[[1]] < #[[2]]}}, 0]&,
    clipDerivatives5arg5 = Piecewise[{{1, #[[1]] > #[[3]] && #[[1]] >= #[[2]]}}, 0]&
},
    Dual /: Clip[Dual[a_, b_], {xmin_, xmax_}] /; NoneTrue[{xmin, xmax}, DualArrayQ] := With[{
        stdargs = {a, std @ xmin, std @ xmax}
    },
        Dual[
            Clip[#[[1]], #[[{2, 3}]]]& @ stdargs,
            Plus[
                b * clipDerivatives3arg1 @ stdargs,
                If[ DualQ[xmin], nonstd[xmin] * clipDerivatives3arg2 @ stdargs, 0],
                If[ DualQ[xmax], nonstd[xmax] * clipDerivatives3arg3 @ stdargs, 0]
            ]
        ]
    ];
    Dual /: Clip[
        Dual[a_, b_],
        {xmin_, xmax_},
        {ymin_, ymax_}
    ] /; NoneTrue[{xmin, xmax, ymin, ymax}, DualArrayQ] := With[{
        stdargs = {a, std @ xmin, std @ xmax, std @ ymin, std @ ymax}
    },
        Dual[
            Clip[#[[1]], #[[{2, 3}]], #[[{4, 5}]]]& @ stdargs,
            Plus[
                b * clipDerivatives5arg1 @ stdargs,
                (* these are always 0 anyway *)
                (*
                If[ DualQ[xmin], nonstd[xmin] * clipDerivatives5arg2 @@ stdargs, 0],
                If[ DualQ[xmax], nonstd[xmax] * clipDerivatives5arg3 @@ stdargs, 0],
                *)
                If[ DualQ[ymin], nonstd[ymin] * clipDerivatives5arg4 @ stdargs, 0],
                If[ DualQ[ymax], nonstd[ymax] * clipDerivatives5arg5 @ stdargs, 0]
            ]
        ]
    ]
];


(* Array operations *)

Dual::norm = "Encountered array of depth `1`. Cannot compute norms of dual arrays of depth > 1.";
Dual::infnorm = "Infinite norms can only be computed for numeric dual vectors.";
Dual /: Norm[Dual[a_?VectorQ, b_]?DualArrayQ, p : Except[DirectedInfinity[1]] : 2] := Dual[
    Norm[a, p],
    Times[
        Total[Abs[a]^p]^Subtract[Divide[1, p], 1],
        b . (Abs[a]^Subtract[p, 1] * Sign[a])
    ]
];
Dual /: Norm[Dual[a : Except[_?VectorQ], b_]?DualArrayQ, ___] /; (Message[Dual::norm, ArrayDepth[a]]; False) := Undefined;
Dual /: Norm[Dual[a_?VectorQ, b_]?DualArrayQ, DirectedInfinity[1]] := With[{
    absA = Abs[a]
},
    With[{
        max = Max[absA]
    },
        With[{
            pos = listPosition[absA, _?(EqualTo[max])]
        },
            Dual[
                max,
                Mean[Sign[Extract[a, pos]] * Extract[b, pos]]
            ]
        ] /; Head[max] =!= Max
    ]
];
Dual /: Norm[Dual[a_?VectorQ, b_]?DualArrayQ, DirectedInfinity[1]] /; (Message[Dual::infnorm]; False):= Undefined;
Dual /: Norm[Dual[a_, b_]?DualScalarQ, ___] := Abs[Dual[a, b]];

Unprotect[foldDotQ];
foldDotQ = True;
Protect[foldDotQ];

Block[{Dot}, (* Block Dot to temporarily get rid of the Flat attribute *)
    (* Dot UpValues for few arguments *)
    Dual /: Dot[d_Dual] := d;
    Dual /: Dot[
        Dual[a1_, b1_],
        Dual[a2_, b2_]
    ] := Dual[a1.a2, a1.b2 + b1.a2];
    Dual /: Dot[c : standardPatt, Dual[a_, b_]] := Dual[c.a, c.b];
    Dual /: Dot[Dual[a_, b_], c : standardPatt] := Dual[a.c, b.c];
    (* Dot UpValue for many arguments *)
    Dual /: (
        dot : Dot[___, _Dual, ___]
    ) /; foldDotQ && Length[Unevaluated[dot]] > 2 := Block[{
        foldDotQ = False,
        try
    },  (* Folding cuts down on pattern matching overhead *)
        try = Fold[Dot, Unevaluated[dot]];
        try /; Head[try] === Dual
    ];
];
Dual /: MatrixPower[
    d_Dual?SquareMatrixQ,
    n_Integer?Positive
] := Fold[
    Function[
        If[#2 === 1, d.#1, #1] . #1
    ],
    d,
    Rest[IntegerDigits[n, 2]]
];
Dual /: MatrixPower[
    d_Dual?SquareMatrixQ,
    n_Integer?Negative
] := Inverse @ MatrixPower[d, -n];

Dual /: Inverse[
    Dual[a_, b_]?SquareMatrixQ
] := With[{inv = Inverse[a]},
    Dual[
        inv,
        - Dot[inv, b, inv]
    ] /; MatrixQ[inv]
];

DualLinearSolveFunction[ls_LinearSolveFunction, b_?SquareMatrixQ][m_?ArrayQ] := With[{
    inv = ls[m]
},
    Dual[
        inv,
        -ls[b. inv]
    ] /; ArrayQ[inv]
];
DualLinearSolveFunction[ls_LinearSolveFunction, b_?SquareMatrixQ][
    Dual[a2_?ArrayQ, b2_?ArrayQ]?DualArrayQ
] := With[{
    inv = ls[a2]
},
    Dual[
        inv,
        -ls[b.inv] + ls[b2]
    ] /; ArrayQ[inv]
];

Dual /: LinearSolve[
    Dual[a_, b_]?SquareMatrixQ,
    opts : OptionsPattern[]
] := With[{
    ls = LinearSolve[a, opts]
},
    DualLinearSolveFunction[ls, b] /; Head[ls] === LinearSolveFunction
];

Dual /: LinearSolve[
    Dual[a_, b_]?SquareMatrixQ,
    x : (_?ArrayQ | _Dual?DualArrayQ),
    opts : OptionsPattern[]
] := With[{
    ls = LinearSolve[a, opts]
},
    DualLinearSolveFunction[ls, b][x] /; Head[ls] === LinearSolveFunction
];

Scan[
    Function[fun,
        Dual /: HoldPattern[fun[Dual[a_, _]]] := fun[a];
    ],
    {
        NumericQ, NumberQ, IntegerQ, Positive, Negative, NonPositive, NonNegative,
        EvenQ, OddQ, PrimeQ, AlgebraicIntegerQ, PossibleZeroQ
    }
];

(* Elementary support for manipulating Dual arrays *)
Dual::arrayOp = "Warning: operation `1` attempted on a Dual number that's not an array.";
Scan[
    Function[fun,
        Dual /: HoldPattern[fun[Dual[a_, b_]?DualArrayQ, rest___]] := Dual[fun[a, rest], fun[b, rest]];
        Dual /: HoldPattern[fun[Dual[_, _], ___]] /; (Message[Dual::arrayOp, fun]; False):= Undefined
    ],
    {
        Total, Mean, Transpose, Flatten,
        Part, Extract, Take, Drop,
        First, Last, Rest, Most
    }
];

Dual::join = "Warning: Join attempted, but it did not produce a valid DualArray.";
Dual /: Join[arrays : Longest[__Dual?DualArrayQ], n_Integer] := With[{
    a = Standard[{arrays}],
    b = NonStandard[{arrays}]
},
    With[{
        try = Dual[Join[Sequence @@ a, n], Join[Sequence @@ b, n]]
    },
        If[ DualArrayQ[try]
            ,
            try
            ,
            Message[Dual::join];
            Inactive[Join][arrays] (* Stop the evaluation chain to prevent Join's Flat attribute from trying other combinations *)
        ]
    ]
];
Dual /: Join[arrays : Longest[__Dual?DualArrayQ]] := With[{
    a = Standard[{arrays}],
    b = NonStandard[{arrays}]
},
    With[{
        try = Dual[Join[Sequence @@ a], Join[Sequence @@ b]]
    },
        If[ DualArrayQ[try]
            ,
            try
            ,
            Message[Dual::join];
            Inactive[Join][arrays] (* Stop the evaluation chain to prevent Join's Flat attribute from trying other combinations *)
        ]
    ]
];
Dual /: Join[___, _Dual, ___] /; (Message[Dual::arrayOp, Join]; False) := Undefined; 

(* Sort and Ordering functions *)
MapThread[
    Function[{orderer, sorter, insertPt},
        Dual /: orderer[Dual[a_, b_]?DualArrayQ, rest___] := orderer[a, rest];
        Dual /: sorter[Dual[a_, b_]?DualArrayQ, rest___] := With[{
            perm = orderer @@ Insert[{a, rest}, All, insertPt]
        },
            Dual[a[[perm]], b[[perm]]]
        ];
        Dual /: orderer[_Dual, ___] /; (Message[Dual::arrayOp, orderer]; False) := Undefined;
        Dual /: sorter[_Dual, ___] /; (Message[Dual::arrayOp, sorter]; False) := Undefined;
    ],
    {
        {Ordering,  OrderingBy},
        {Sort,      SortBy},
        {2,         3}
    }
];

(* Append and Prepend *)
Scan[
    Function[{pender},
        Dual /: pender[Dual[a1_, b1_]?DualArrayQ, Dual[a2_, b2_]] := Dual[pender[a1, a2], pender[b1, b2]];
        Dual /: pender[d_Dual, a2 : standardPatt] := pender[d, ToDual[a2, 0]];
        Dual /: pender[_Dual, ___] /; (Message[Dual::arrayOp, pender]; False) := Undefined;
    ],
    {Append, Prepend}
];

Dual /: Select[Dual[a_, b_]?DualArrayQ, selFun_, n : _ : DirectedInfinity[1]] := With[{
    pos = listPosition[a, _?selFun, {1}, n]
},
    Dual[Extract[a, pos], Extract[b, pos]]
];
Dual /: Select[fun_][d_Dual] := Select[d, fun];
Dual /: Select[_Dual, ___] /; (Message[Dual::arrayOp, Select]; False) := Undefined;

Dual /: Position[Dual[a_, _]?DualArrayQ, rest___] := Position[a, rest];
Dual /: Position[_Dual, ___] /; (Message[Dual::arrayOp, Position]; False) := Undefined;

Dual /: Pick[list_Dual?DualArrayQ, sel_, patt : _ : True] /; Length[list] === Length[sel] := With[{
    pos = listPosition[sel, patt]
},
    Extract[list, pos]
];
Dual /: Pick[list_, sel_Dual?DualArrayQ, patt : _ : True] /; Length[list] === Length[sel] := With[{
    pos = listPosition[sel, patt]
},
    Extract[list, pos]
];
Dual /: Pick[_Dual, ___] /; (Message[Dual::arrayOp, Pick]; False) := Undefined;
Dual /: Pick[_, _Dual, ___] /; (Message[Dual::arrayOp, Pick]; False) := Undefined;

groupDual[
    d_Dual,
    (fun1 : Except[_Rule | _List]) -> (fun2 : Except[_Rule | _List])
] := Map[Map[fun2], groupDual[d, fun1]];
groupDual[Dual[a_, b_], fun : Except[_Rule | _List]] := With[{
    posAssoc = PositionIndex[fun /@ a]
},
    Map[
        Dual[
            Developer`ToPackedArray @ Part[a, #],
            Developer`ToPackedArray @ Part[b, #]
        ]&,
        posAssoc
    ]
];

Dual::groupbyfun = "Function spec `1` is currently not supported for GroupBy.";
Dual /: GroupBy[d_Dual?DualArrayQ, fun : Except[_List]] := With[{
    assoc = groupDual[d, fun]
},
    assoc /; AssociationQ[assoc]
];
Dual /: GroupBy[d_Dual?DualArrayQ, fun : Except[_List], reducer_] := With[{
    assoc = groupDual[d, fun]
},
    Map[reducer, assoc] /; AssociationQ[assoc]
];
Dual /: GroupBy[fun_][d_Dual] := GroupBy[d, fun];
Dual /: GroupBy[Dual[a_, b_]?DualArrayQ, fun_, ___] /; (Message[Dual::groupbyfun, Short[fun]]; False) := Undefined;
Dual /: GroupBy[_Dual?DualScalarQ, ___] /; (Message[Dual::arrayOp, GroupBy]; False) := Undefined;

(* Short-circuit definitions for Map to prevent uncessary unpacking *)
Dual /: Map[Identity, d_Dual?DualArrayQ] := d;
Scan[
    Function[specialCase,
        Dual /: Map[specialCase, Dual[a_, b_]?DualArrayQ, rest___] := Dual[
            Map[specialCase, a, rest],
            Map[specialCase, b, rest]
        ];
    ],
    {
        First, Last, Most, Rest, Transpose, Mean, Total,
        Flatten
    }
];
(* General definitions for mapping functions *)
Scan[
    Function[mapper,
        Dual /: mapper[fun_, dualArr_Dual?DualArrayQ, rest___] := PackDualArray[
            mapper[fun, UnpackDualArray[dualArr], rest]
        ];
        Dual /: mapper[fun_][d_Dual] := mapper[fun, d];
        Dual /: mapper[_, _Dual, ___] /; (Message[Dual::arrayOp, mapper]; False) := Undefined;
    ],
    {Map, MapIndexed, Apply}
];

Dual /: FoldList[args__, dualArr_Dual?DualArrayQ] := PackDualArray[
    FoldList[args, UnpackDualArray[dualArr]]
];
Dual /: Fold[args__, dualArr_Dual?DualArrayQ] := ( 
    Fold[args, UnpackDualArray[dualArr]]
);
Scan[
    Function[folder,
        Dual /: folder[fun_][d_Dual] := folder[fun, d];
        Dual /: folder[fun_][x_, d_Dual] := folder[fun, x, d];
        Dual /: folder[__, _Dual] /; (Message[Dual::arrayOp, folder]; False) := Undefined;
    ],
    {Fold, FoldList}
];

Scan[
    Function[fun,
        Dual /: HoldPattern[fun[Dual[a_, b_]?DualArrayQ, rest___]] := fun[a, rest]
    ],
    {
        MatrixQ, VectorQ, SquareMatrixQ, Dimensions, Length, ArrayDepth
    }
];
Scan[
    Function[fun,
        Dual /: HoldPattern[fun[Dual[_, _], ___]] := False;
    ],
    {MatrixQ, VectorQ, SquareMatrixQ}
];
Dual /: HoldPattern[Length[Dual[_, _]]] := 0;
Dual /: HoldPattern[Dimensions[Dual[_, _]]] := {};
Dual /: HoldPattern[ArrayDepth[Dual[_, _]]] := 0;

Scan[ (* Make sure comparing functions throw away the infinitesimal parts of dual numbers *)
    Function[fun,
        Dual /: HoldPattern[fun[first___, d : Dual[_?NumericQ, _], rest___]] := With[{
            test = fun @@ std[{first, d, rest}]
        },
            test /; BooleanQ[test]
        ]
    ],
    {Equal, Unequal, Greater, GreaterEqual, Less, LessEqual}
];

End[] (* End Private Context *)

EndPackage[]