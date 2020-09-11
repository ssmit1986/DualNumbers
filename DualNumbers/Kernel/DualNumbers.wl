(* Wolfram Language Package *)

BeginPackage["DualNumbers`", {"GeneralUtilities`"}]
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
GeneralUtilities`SetUsage[DualEpsilon, "DualEpsilon = Dual[0, 1]."];
GeneralUtilities`SetUsage[InactiveEpsilon, "InactiveEpsilon is an inactive form of Dual[0, 1] that can be used for algebraic manipulation."];
GeneralUtilities`SetUsage[DualQ, "DualQ[expr$] tests if expr$ is a dual number."];
GeneralUtilities`SetUsage[DualArrayQ, "DualArrayQ[expr$] tests if expr$ is an array of dual numbers."];
GeneralUtilities`SetUsage[DualSquareMatrixQ, "DualSquareMatrixQ[expr$] tests if expr$ is a square matrix of dual numbers."]
GeneralUtilities`SetUsage[ScalarQ, "ScalarQ[expr$] = !DualQ[expr$]"];
GeneralUtilities`SetUsage[DualFindRoot,
    "DualFindRoot works like FindRoot, but allows for Dual numbers in the equations."
];
GeneralUtilities`SetUsage[DualFindMinimum,
    "DualFindMinimum works like DualFindMinimum, but allows for Dual numbers in the objective function.\nDualFindMinimum does not support constraints on the independent variables."
];
GeneralUtilities`SetUsage[FindDualSolution,
    "FindDualSolution[eqs$, sol$] finds a Dual-valued solution to eqs$ where sol$ is the standard-valued solution."
];
GeneralUtilities`SetUsage[DualLinearSolveFunction,
    "DualLinearSolveFunction[ls$, b$] is produced from LinearSolve[Dual[a$, b$]]. A DualLinearSolveFunction can be applied to Dual arrays."
];

Begin["`Private`"] (* Begin Private Context *) 

(* 
    Code inspired by the following post on Mathematica StackExchange:
    https://mathematica.stackexchange.com/a/13926/43522
*)

derivativePatt = Except[Function[D[__]], _Function];

Dual /: DualQ[Dual[_, _]] := True;
DualQ[_] := False;

Dual /: DualArrayQ[Dual[a_?ArrayQ, b_?ArrayQ]] /; Dimensions[a] === Dimensions[b] := True;
DualArrayQ[_] := False;

Dual /: DualSquareMatrixQ[Dual[a_?SquareMatrixQ, b_?SquareMatrixQ]] /; Dimensions[a] === Dimensions[b] := True;
DualSquareMatrixQ[_] := False;

Dual /: ScalarQ[Dual[_, _]] := False;
ScalarQ[_] := True;
scalarPatt = Except[_Dual];

Dual[] = DualEpsilon = Dual[0, 1];
InactiveEpsilon = Inactive[Dual][0, 1];

Dual[a_SparseArray?ArrayQ] := Dual[a, SparseArray[{}, Dimensions[a], 1]]
Dual[a_?ArrayQ] := Dual[a, ConstantArray[1, Dimensions[a]]];
Dual[a_] := Dual[a, 1];

SetAttributes[Standard, Listable];
Dual /: Standard[Dual[a_, _]] := a;
Standard[x_?NumericQ] := x;

SetAttributes[NonStandard, Listable];
Dual /: NonStandard[Dual[_, b_]] := b;
NonStandard[_?NumericQ] := 0;

StandardAll[expr_] := ReplaceRepeated[expr, Dual[a_, _] :> a];

DualExpand[expr_, eps : _ : InactiveEpsilon] := ReplaceRepeated[
    expr,
    Dual[a_, b_] :> a + b * eps
];
DualFactor[expr_, eps : _ : InactiveEpsilon] := ReplaceRepeated[expr, eps :> Dual[0, 1]];

DualSimplify[expr_, eps : _ : InactiveEpsilon] := Normal @ Series[expr, {eps, 0, 1}];

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

Dual /: Dual[a_, 0] := a;
Dual /: (c : scalarPatt) + Dual[a_, b_] := Dual[c + a, b];
Dual /: Dual[a1_, b1_] + Dual[a2_, b2_] := Dual[a1 + a2, b1 + b2];
Dual /: (c : scalarPatt) * Dual[a_, b_] := Dual[c * a, c * b];
Dual /: Dual[a1_, b1_] * Dual[a2_, b2_] := Dual[a1 * a2, b1 * a2 + a1 * b2];

Dual /: HoldPattern @ Subtract[Dual[a1_, b1_], Dual[a2_, b2_]] := Dual[Subtract[a1, a2], Subtract[b1, b2]];

Dual /: HoldPattern @ Divide[Dual[a1_, b1_], Dual[a2_, b2_]] := Dual[
    Divide[a1, a2],
    Divide[Subtract[b1 * a2, a1 * b2] , a2^2]
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
Dual /: Clip[Dual[a_, b_], {xmin : scalarPatt, xmax : scalarPatt}] := Dual[
    Clip[a, {xmin, xmax}],
    b * Piecewise[{{1, xmin<= a <= xmax}}, 0]
];
Dual /: Clip[Dual[a_, b_], {xmin : scalarPatt, xmax : scalarPatt}, {ymin : scalarPatt, ymax : scalarPatt}] := Dual[
    Clip[a, {xmin, xmax}, {ymin, ymax}],
    b * Piecewise[{{1, xmin<= a <= xmax}}, 0]
];
With[{
    clipDerivatives3arg1 = Piecewise[{{1, #[[2]] <= #[[1]] <= #[[3]]}}, 0]&,
    clipDerivatives3arg2 = Piecewise[{{1, #[[1]] < #[[2]]}}, 0]&,
    clipDerivatives3arg3 = Piecewise[{{1, #[[1]] > #[[3]] && #[[1]] >= #[[2]]}}, 0]&,
    clipDerivatives5arg1 = Piecewise[{{1, #[[2]] <= #[[1]] <= #[[3]]}}, 0]&,
    clipDerivatives5arg2 = 0&,
    clipDerivatives5arg3 = 0&,
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
Dual /: Dot[c_?ArrayQ, Dual[a_, b_]?DualArrayQ] := Dual[c.a, c.b]
Dual /: Dot[Dual[a_, b_]?DualArrayQ, c_?ArrayQ] := Dual[a.c, b.c]

Dual /: Dot[
    Dual[a1_, b1_]?DualArrayQ,
    Dual[a2_, b2_]?DualArrayQ
] := Dual[a1.a2, a1.b2 + b1.a2];

Dual /: MatrixPower[
    d_Dual?DualSquareMatrixQ,
    n_Integer?Positive
] := Fold[
    Function[
        If[#2 === 1, d.#1, #1] . #1
    ],
    d,
    Rest[IntegerDigits[n, 2]]
];
Dual /: MatrixPower[
    d_Dual?DualSquareMatrixQ,
    n_Integer?Negative
] := Inverse @ MatrixPower[d, -n];

Dual /: Inverse[
    Dual[a_, b_]?DualSquareMatrixQ
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
    Dual[a_, b_]?DualSquareMatrixQ,
    opts : OptionsPattern[]
] := With[{
    ls = LinearSolve[a, opts]
},
    DualLinearSolveFunction[ls, b] /; Head[ls] === LinearSolveFunction
];

Dual /: LinearSolve[
    Dual[a_, b_]?DualSquareMatrixQ,
    x : (_?ArrayQ | _?DualArrayQ),
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
        NumericQ, NumberQ, IntegerQ, Positive, Negative, NonPositive, NonNegative, PossibleZeroQ,
        Re, Im, EvenQ, OddQ, PrimeQ, AlgebraicIntegerQ
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
        Total, Mean, Transpose, Part, Take, Drop
    }
];

Scan[
    Function[fun,
        Dual /: HoldPattern[fun[Dual[a_, b_]?DualArrayQ, rest___]] := fun[a, rest];
        Dual /: HoldPattern[fun[Dual[_, _], ___]] /; (Message[Dual::arrayOp, fun]; False):= Undefined
    ],
    {
        MatrixQ, VectorQ, ArrayQ, Dimensions, Length
    }
];

(* Set upvalues for most built-in numeric functions where possible *)
KeyValueMap[
    Function[{fun, derriv},
        Dual /: HoldPattern[fun[Dual[a_, b_]]] := Dual[fun[a], derriv[a] * b]
    ],
    KeyDrop[{Plus, Times, Power, Divide, Subtract, Abs, Sign, Mod, Binomial}] @ Select[
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
    AssociationMap[
        Function[f,
            Derivative[##][f]& @@@ IdentityMatrix[2]
        ],
        {Power, Mod, Binomial, Gamma}
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

Dual /: f_Symbol[first___, d_Dual, rest___] /; MemberQ[Attributes[f], NumericFunction] := With[{
    args = {first, d, rest}
}, With[{
    dualPos = Flatten @ Position[args, _Dual?DualQ, {1}, Heads -> False],
    inputs = std[args]
}, With[{
    derrivs = Derivative[##][f]& @@@ IdentityMatrix[Length[args]][[dualPos]]
},
    Dual[
        f @@ inputs,
        Dot[
            Function[# @@ inputs] /@ derrivs,
            args[[dualPos, 2]]
        ]
    ] /; MatchQ[derrivs, {derivativePatt..}]
]]];

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

DualFindMinimum[eq_, spec : {_, __?NumericQ}, rest___] := DualFindMinimum[eq, {spec}, rest];
DualFindMinimum[fun : Except[_List], spec : {{_, __?NumericQ}..}, rest___] := Module[{
    stdfun, stdSol,
    vars = spec[[All, 1]],
    dualSol
},
    stdfun = StandardAll[DualFactor[fun]];
    stdSol = FindMinimum[stdfun, spec, rest];
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

End[] (* End Private Context *)

EndPackage[]