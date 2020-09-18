(* Wolfram Language Package *)

BeginPackage["DualNumbers`", {"GeneralUtilities`", "Developer`"}]

Dual;
StandardAll;
DualExpand;
DualFactor;
DualSimplify;
DualTuples;
AddDualHandling;
DualApply;
FindDualSolution;
DualFindRoot;
DualFindMinimum;
DualFindMaximum;

Begin["`Private`"] (* Begin Private Context *) 

(* Manipulating expressions with dual numbers *)
StandardAll[expr_] := ReplaceRepeated[expr, Dual[a_, _] :> a];

DualExpand[expr_, eps : _ : \[Epsilon]] := ReplaceRepeated[
    expr,
    Dual[a_, b_] :> a + b * eps
];
DualFactor[expr_, eps : _ : \[Epsilon]] := ReplaceRepeated[expr, eps :> Dual[0, 1]];

DualSimplify[expr_, eps : _ : \[Epsilon]] := Normal @ Series[expr, {eps, 0, 1}];

DualTuples[{}] := {};
DualTuples[{Dual[a_, b_]}] := {{b}};
DualTuples[{Dual[a1_, b1_], Dual[a2_, b2_]}] := {{b1, a2}, {a1, b2}};
DualTuples[dList : {__Dual}] := Map[
    Extract[dList, #]&,
    dualTuplesPositions[Length[dList]]
];
With[{
    cf = Compile[{
        {n, _Integer}
    },
        Table[
            If[ k == 1, j, If[i == j, 2, 1]],
            {i, n}, {j, n}, {k, 2}
        ]
    ]
},
    dualTuplesPositions[0] := {};
    dualTuplesPositions[n_Integer] := cf[n]
];

(* Set UpValues for custom functions to be used with Dual *)
AddDualHandling[f_, n_Integer?Positive] := AddDualHandling[f, Derivative[##][f]& @@@ IdentityMatrix[n]];
AddDualHandling[f_, nList : {__Integer?Positive}] := Scan[AddDualHandling[f, #]&, nList];
AddDualHandling[f_, derivatives_List] := With[{n = Length[derivatives]},
    Dual /: f[first___, d_Dual, rest___] := With[{
        args = {first, d, rest}
    },
        With[{
            dualPos = Flatten @ listPosition[args, dualPatt],
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

(* Modify standard and nonstandard parts directly *)
DualApply::resultlength = "Function `1` did not return a list of length 2."
DualApply[{funa_, funb_}, Dual[a_, b_]] := Dual[funa[a], funb[b]];
DualApply[fun : Except[_List], Dual[a_, b_]] := Dual[fun[a], fun[b]];
DualApply[{funAll_}, Dual[a_, b_]] := With[{
    try = funAll[a, b]
},
    Dual @@ try /; Replace[
        MatchQ[try, {_, _}],
        False :> (Message[DualApply::resultlength, Short[funAll]]; False)
    ]
];
DualApply[f_, other : standardPatt] := DualApply[f, ToDual[other, 0]];
DualApply[fun_][d_Dual] := DualApply[fun, d];

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