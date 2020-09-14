(* Wolfram Language Package *)

BeginPackage["DualNumbers`", {"GeneralUtilities`", "Developer`"}]
ClearAll["DualNumbers`*", "DualNumbers`*`*"];

(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[Dual, "Dual[a$, b$] represents a dual number with standard part a$ and infinitesimal part b$."];
GeneralUtilities`SetUsage[Standard,
    "Standard[d$] extracts the standard part of a dual number d$ (i.e., the first argument)."
];
GeneralUtilities`SetUsage[StandardAll,
    "StandardAll[expr$] replaces all dual numbers in expr$ with their standard parts."
];
GeneralUtilities`SetUsage[NonStandard,
    "NonStandard[d$] extracts the non-standard part of a dual number d$ (i.e., the second argument)."
];
GeneralUtilities`SetUsage[DualExpand,
    "DualExpand[expr$, eps$] replaces each dual number Dual[a$, b$] with a$ + b$ * eps$."
];
GeneralUtilities`SetUsage[DualFactor,
    "DualFactor[expr$, eps$] replaces eps$ with Dual[0, 1] in expr$."
];
GeneralUtilities`SetUsage[DualSimplify,
    "DualSimplify[expr$, eps$] expands expr$ around eps$ = 0, keeping only the 0th and 1st order terms."
];
GeneralUtilities`SetUsage[\[Epsilon], "\[Epsilon] is an inactive form of Dual[0, 1] that can be used for algebraic manipulation."];
GeneralUtilities`SetUsage[DualQ, "DualQ[expr$] tests if expr$ is a dual number."];
GeneralUtilities`SetUsage[DualScalarQ, "DualQ[expr$] tests if expr$ is a dual number but not a dual array."];
GeneralUtilities`SetUsage[DualArrayQ, "DualArrayQ[expr$] tests if expr$ is an array of dual numbers."];
GeneralUtilities`SetUsage[StandardQ, "StandardQ[expr$] tests if expr$ has a head different from Dual."];
GeneralUtilities`SetUsage[DualFindRoot,
    "DualFindRoot works like FindRoot, but allows for Dual numbers in the equations."
];
GeneralUtilities`SetUsage[DualFindMinimum,
    "DualFindMinimum works like FindMinimum, but allows for Dual numbers in the objective function.
DualFindMinimum does not support constraints on the independent variables."
];
GeneralUtilities`SetUsage[DualFindMaximum,
    "DualFindMaximum works like FindMaximum, but allows for Dual numbers in the objective function.
DualFindMaximum does not support constraints on the independent variables."
];
GeneralUtilities`SetUsage[FindDualSolution,
    "FindDualSolution[eqs$, sol$] finds a Dual-valued solution to eqs$ where sol$ is the standard-valued solution."
];
GeneralUtilities`SetUsage[DualLinearSolveFunction,
    "DualLinearSolveFunction[ls$, b$] is produced from LinearSolve[Dual[a$, b$]]. A DualLinearSolveFunction can be applied to Dual arrays."
];
GeneralUtilities`SetUsage[PackDualArray,
    "PackDualArray[array$] converts an array of numbers (possibly duals) to the form Dual[std$, nonstd$]."
];
GeneralUtilities`SetUsage[UnpackDualArray,
    "UnpackDualArray[dualArray$] reverses to operation of PackDualArray and creates an array of dual scalars."
];
GeneralUtilities`SetUsage[AddDualHandling,
    "AddDualHandling[f$, {f$1, $$, f$n}] specifies derivatives for f$ to use with Dual numbers when called with n$ arguments.
AddDualHandling[f$, n$] uses Derivative to infer derivatives of f$ for when f$ is called with $n arguments.
AddDualHandling[f$, {n$1, n$2, $$}] uses Derivative to infer derivatives of f$ for when f$ is called with n$1, n$2, $$ arguments."
];

Begin["`Private`"] (* Begin Private Context *) 

(* 
    Code inspired by the following post on Mathematica StackExchange:
    https://mathematica.stackexchange.com/a/13926/43522
*)

derivativePatt = Except[Function[D[__]], _Function];
arrayPattern = _List | _SparseArray | _QuantityArray;

dualPatt = Dual[_, _];
DualQ[expr : dualPatt] := DualScalarQ[expr] || DualArrayQ[expr];
DualQ[_] := False;

DualScalarQ[Dual[Except[_?ArrayQ], Except[_?ArrayQ]]] := True;
DualScalarQ[_] := False;

DualArrayQ[Dual[a_?ArrayQ, b_?ArrayQ]] /; Dimensions[a] === Dimensions[b] := True;
DualArrayQ[_] := False;

StandardQ[_Dual] := False;
StandardQ[_] := True;
standardPatt = Except[_Dual];

Dual[] := Dual[0, 1];

Dual[a_SparseArray?ArrayQ] := Dual[a, SparseArray[{}, Dimensions[a], 1]]
Dual[a_?ArrayQ] := Dual[a, ConstantArray[1, Dimensions[a]]];
Dual[a_] := Dual[a, 1];

(* Messages to warn when invalid Dual arrays have been constructed *)
Dual::array = "Mismatching dimensions `1` and `2` found for the standard and non-standard parts of `3`";
Dual[a : arrayPattern, b_] /; And[
    !DualArrayQ[Unevaluated @ Dual[a, b]],
    (
        Message[Dual::array, Dimensions[a], Dimensions[b], Short[Inactive[Dual][a, b]]];
        False
    )
] := Undefined;
Dual[a : Except[arrayPattern], b : arrayPattern] /; And[
    !DualArrayQ[Unevaluated @ Dual[a, b]],
    (
        Message[Dual::array, Dimensions[a], Dimensions[b], Short[Inactive[Dual][a, b]]];
        False
    )
] := Undefined;

PackDualArray::arrayQ = "`1` is not an array.";
PackDualArray[array_?ArrayQ] := Dual[
    Developer`ToPackedArray @ Standard[array],
    Developer`ToPackedArray @ NonStandard[array]
];
PackDualArray[other_] := (
    Message[PackDualArray::arrayQ, Short[other]];
    other
);

With[{
    testArray = Developer`ToPackedArray[{0}]
},
    packingMessagesEnabledQ[] := TrueQ @ Quiet @ Check[ (* test if packing messages are on (i.e., from On["Packing"]) *)
        Developer`FromPackedArray @ testArray,
        True,
        {FromPackedArray::punpack}
    ]
];

UnpackDualArray::unpack = "Unpacking Dual array with dimensions `1`.";
UnpackDualArray::badarray = "Cannot unpack expression `1`."
UnpackDualArray[Dual[a_, b_]?DualArrayQ] := (
    If[ packingMessagesEnabledQ[],
        Quiet[
            Message[UnpackDualArray::unpack, Dimensions[a]],
            {FromPackedArray::unpack, FromPackedArray::punpack1}
        ]
    ];
    MapThread[
        Dual,
        {a, b},
        ArrayDepth[a]
    ]
);
UnpackDualArray[other_] := (
    Message[UnpackDualArray::badarray, Short[other]];
    other
);

SetAttributes[Standard, Listable];
Standard[Dual[a_, _]] := a;
Standard[x_?NumericQ] := x;

SetAttributes[NonStandard, Listable];
NonStandard[Dual[_, b_]] := b;
NonStandard[_?NumericQ] := 0;

StandardAll[expr_] := ReplaceRepeated[expr, Dual[a_, _] :> a];

DualExpand[expr_, eps : _ : \[Epsilon]] := ReplaceRepeated[
    expr,
    Dual[a_, b_] :> a + b * eps
];
DualFactor[expr_, eps : _ : \[Epsilon]] := ReplaceRepeated[expr, eps :> Dual[0, 1]];

DualSimplify[expr_, eps : _ : \[Epsilon]] := Normal @ Series[expr, {eps, 0, 1}];

SetAttributes[std, Listable];
std[Dual[a_, _]] := a;
std[x_] := x;

SetAttributes[nonstd, Listable];
nonstd[Dual[_, b_]] := b;
nonstd[x_] := 0;

Dual[Dual[a_, b_], c_] ^:= Dual[a, b + c];
Dual[a_, Dual[b_, _]] ^:= Dual[a, b];

Derivative[1, 0][Dual] = 1&;
Derivative[0, 1][Dual] = Dual[0, 1]&;

Dual[a_, 0] := a;
Dual /: (c : standardPatt) + Dual[a_, b_] := Dual[c + a, b];
Dual /: Dual[a1_, b1_] + Dual[a2_, b2_] := Dual[a1 + a2, b1 + b2];
Dual /: (c : standardPatt) * Dual[a_, b_] := Dual[c * a, c * b];
Dual /: Dual[a1_, b1_] * Dual[a2_, b2_] := Dual[a1 * a2, b1 * a2 + a1 * b2];

Dual /: HoldPattern @ Subtract[Dual[a1_, b1_], Dual[a2_, b2_]] := Dual[Subtract[a1, a2], Subtract[b1, b2]];

Dual /: HoldPattern @ Divide[Dual[a1_, b1_], Dual[a2_, b2_]] := Dual[
    Divide[a1, a2],
    Subtract[Divide[b1, a2], Divide[a1 * b2, a2^2]]
];

Dual /: Power[Dual[a_, b_], 1] := Dual[a, b];
Dual /: Power[Dual[Except[0 | 0.], _], 0] := 1;
Dual /: Power[Dual[a_, b_], -1] := Dual[Divide[1, a], -Divide[b, a^2]];

(* (* This definition can be helpful for calculating very large Powers *)
Dual /: Power[d_Dual, n_Integer?Positive] := Fold[
    Function[If[#2 === 1, d * #1, #1] * #1],
    d,
    Rest[IntegerDigits[n, 2]]
];
*)


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
    Dual /: Clip[Dual[a_, b_], {xmin_, xmax_}] := With[{
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
    Dual /: Clip[Dual[a_, b_], {xmin_, xmax_}, {ymin_, ymax_}] := With[{
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
            pos = Position[absA, _?(EqualTo[max]), {1}, Heads -> False]
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

Dual /: Dot[c_?ArrayQ, Dual[a_, b_]?DualArrayQ] := Dual[c.a, c.b]
Dual /: Dot[Dual[a_, b_]?DualArrayQ, c_?ArrayQ] := Dual[a.c, b.c]

Dual /: Dot[
    Dual[a1_, b1_]?DualArrayQ,
    Dual[a2_, b2_]?DualArrayQ
] := Dual[a1.a2, a1.b2 + b1.a2];

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
        Total, Mean, Transpose, 
        Part, Extract, Take, Drop,
        First, Last, Rest, Most
    }
];

Dual::join = "Warning: Join attempted, but it did not produce a valid DualArray.";
Dual /: Join[arrays__Dual?DualArrayQ, n_Integer] := With[{
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
Dual /: Join[arrays__Dual?DualArrayQ] := With[{
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

Dual /: Select[Dual[a_, b_]?DualArrayQ, selFun_, n : _ : DirectedInfinity[1]] := With[{
    pos = Position[a, _?selFun, {1}, n, Heads -> False]
},
    Dual[Extract[a, pos], Extract[b, pos]]
];
Dual /: Select[_Dual, ___] /; (Message[Dual::arrayOp, Select]; False) := Undefined;

Dual /: Position[Dual[a_, _]?DualArrayQ, rest___] := Position[a, rest];
Dual /: Position[_Dual, ___] /; (Message[Dual::arrayOp, Position]; False) := Undefined;

Dual::maprepack = "Failed to re-pack after mapping `f`";
Scan[
    Function[mapper,
        Dual /: mapper[fun_, dualArr_Dual?DualArrayQ, rest___] := With[{
            try = PackDualArray @ mapper[fun, UnpackDualArray[dualArr], rest]
        },
            try /; Replace[DualArrayQ[try], False :> (Message[Dual::maprepack, fun]; False)]
        ];
        Dual /: mapper[_, _Dual, ___] /; (Message[Dual::arrayOp, mapper]; False) := Undefined;
    ],
    {Map, MapIndexed}
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

(* Set upvalues for most built-in numeric functions where possible *)
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

(* Set upvalues for some 2-argument functions *)
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

AddDualHandling[f_, n_Integer?Positive] := AddDualHandling[f, Derivative[##][f]& @@@ IdentityMatrix[n]];
AddDualHandling[f_, nList : {__Integer?Positive}] := Scan[AddDualHandling[f, #]&, nList];
AddDualHandling[f_, derivatives_List] := With[{n = Length[derivatives]},
    Dual /: f[first___, d_Dual, rest___] := With[{
        args = {first, d, rest}
    },
        With[{
            dualPos = Flatten @ Position[args, dualPatt, {1}, Heads -> False],
            inputs = std[args]
        },
            With[{dlist = derivatives[[dualPos]]},
                Dual[
                    f @@ inputs,
                    Dot[
                        Through[dlist[Sequence @@ inputs]],
                        args[[dualPos, 2]]
                    ]
                ] /; ListQ[dlist]
            ]
        ] /; Length[args] === n
    ]
];

(* Helper functions for equation solving with Dual numbers *)

equationNormalForm[eq : Except[_List]] := equationNormalForm[{eq}]
equationNormalForm[eqs : {___, Except[_Equal], ___}] := equationNormalForm @ Replace[eqs, f : Except[_Equal]:> f == 0, {1}];
equationNormalForm[eqs : {HoldPattern[Equal[_, _]]..}] := Flatten @ Map[Thread, eqs];
equationNormalForm[_] := $Failed

firstSol[{el_, ___}] := el;
firstSol[other_] := other;

FindDualSolution::nonsol = "Warning: solution `1` could not be verified to solve the standard parts of the provided equations.";

FindDualSolution[eqs_, sol : {__Rule}] := Module[{
    equations = equationNormalForm[eqs],
    vars = Keys[sol],
    dualRules,
    nonstdSol
},
    If[ FailureQ[equations],
        Return[$Failed]
    ];
    dualRules = Thread[vars -> (Values[sol] + Map[Dual[0, #]&, vars])];
    equations = DualFactor[Subtract @@@ equations] /. dualRules;
    If[ !MatchQ[equations, {Dual[_?(EqualTo[0]), _]..}],
        Message[FindDualSolution::nonsol, Short @ sol]
    ];
    equations = Function[NonStandard[#] == 0] /@ equations;
    nonstdSol = Solve[equations, vars];
    If[ MatchQ[nonstdSol, {{__Rule}..}],
        Map[
            Thread[vars -> MapThread[Dual, {Lookup[sol, vars], Lookup[#, vars, 0]}]]&,
            nonstdSol
        ],
        $Failed
    ]
];

DualFindRoot[eq_, spec : {_, __?NumericQ}, rest___] := DualFindRoot[eq, {spec}, rest];
DualFindRoot[eqs_, spec : {{_, __?NumericQ}..}, rest___] := Module[{
    equations = equationNormalForm[eqs],
    stdEqs, stdSol
},
    If[ FailureQ[equations],
        Return[$Failed]
    ];
    stdEqs = Subtract @@@ equations;
    stdEqs = StandardAll[DualFactor[stdEqs]];
    stdSol = FindRoot[stdEqs, spec, rest];
    If[ !MatchQ[stdSol, {(_ -> _?NumericQ)..}],
        Return[$Failed]
    ];
    Quiet[firstSol @ FindDualSolution[equations, stdSol], {FindDualSolution::nonsol}]
];

KeyValueMap[
    Function[{existingFun, newFun},
        newFun[eq_, spec : {_, __?NumericQ}, rest___] := newFun[eq, {spec}, rest];
        newFun[fun : Except[_List], spec : {{_, __?NumericQ}..}, rest___] := Module[{
            stdfun, stdSol,
            vars = spec[[All, 1]],
            dualSol
        },
            stdfun = StandardAll[DualFactor[fun]];
            stdSol = existingFun[stdfun, spec, rest];
            If[ !MatchQ[stdSol, {_?NumericQ, {(_ -> _?NumericQ)..}}],
                Return[$Failed]
            ];
            dualSol = Quiet[
                firstSol @ FindDualSolution[
                    D[fun, {vars}],
                    Last @ stdSol
                ],
                {FindDualSolution::nonsol}
            ];
            If[ MatchQ[dualSol, {__Rule}],
                {fun /. dualSol, dualSol},
                dualSol
            ]
        ];
    ],
    <|
        FindMinimum -> DualFindMinimum,
        FindMaximum -> DualFindMaximum
    |>
];

End[] (* End Private Context *)

EndPackage[]