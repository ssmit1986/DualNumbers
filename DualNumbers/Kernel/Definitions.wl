(* Wolfram Language Package *)

BeginPackage["DualNumbers`", {"GeneralUtilities`", "Developer`"}]

(* Exported symbols added here with SymbolName::usage *)
GeneralUtilities`SetUsage[Dual, "Dual[a$, b$] represents a dual number with standard part a$ and infinitesimal part b$.
Dual[array$1, array$2] represents an array of dual numbers. The arrays should have the same shape (i.e., Dimensions[array$1] === Dimensions[array$2])
Dual[a$] uses ToDual[a, 0] to construct a dual quantity."
];
GeneralUtilities`SetUsage[ToDual, "ToDual[expr$, const$] constructs a dual scalar or array with constant nonstandard part. \
The default value for const$ is 0."
];
GeneralUtilities`SetUsage[Standard,
    "Standard[d$] extracts the standard part of a dual number d$ (i.e., the first argument).
Symbolic quantities are assumed to have zero nonstandard parts. Threads over lists."
];
GeneralUtilities`SetUsage[NonStandard,
    "NonStandard[d$] extracts the nonstandard part of a dual number d$ (i.e., the second argument).
Symbolic quantities are assumed to have zero nonstandard parts. Threads over lists."
];
GeneralUtilities`SetUsage[StandardAll,
    "StandardAll[expr$] replaces all dual numbers in expr$ with their standard parts."
];
GeneralUtilities`SetUsage[DualExpand,
    "DualExpand[expr$] replaces each dual number Dual[a$, b$] with a$ + b$ \[Epsilon].
DualExpand[expr$, eps$] uses eps$ instead."
];
GeneralUtilities`SetUsage[DualFactor,
    "DualFactor[expr$] replaces \[Epsilon] with Dual[0, 1] in expr$.
DualFactor[expr$, eps$] uses eps$ instead.
"
];
GeneralUtilities`SetUsage[DualSimplify,
    "DualSimplify[expr$] expands expr$ around \[Epsilon] = 0, keeping only the 0th and 1st order terms.
DualSimplify[expr$, eps$] uses eps$ as symbol for the dual unit.
"
];
GeneralUtilities`SetUsage[\[Epsilon], "\[Epsilon] is an inactive form of Dual[0, 1] that can be used for algebraic manipulation."];
GeneralUtilities`SetUsage[DualQ, "DualQ[expr$] tests if expr$ is a dual number."];
GeneralUtilities`SetUsage[DualScalarQ, "DualQ[expr$] tests if expr$ is a dual number but not a dual array."];
GeneralUtilities`SetUsage[DualArrayQ, "DualArrayQ[expr$] tests if expr$ is a valid packed array of dual numbers."];
GeneralUtilities`SetUsage[UnpackedDualArrayQ, "UnpackedDualArrayQ[expr$] tests if expr$ is an ordinary array where only Dual occurs as a head at the deepest level."];
GeneralUtilities`SetUsage[DualFreeArrayQ,
    "DualFreeArrayQ[expr$] tests if expr$ is an ordinary array that has no dual numbers at the deepest level."
]
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
    "UnpackDualArray[dualArray$] reverses to operation of PackDualArray and creates an array of dual scalars.
Produces a message if packing messages have been turned on with On[\"Packing\"]."
];
GeneralUtilities`SetUsage[AddDualHandling,
    "AddDualHandling[f$, {f$1, $$, f$n}] specifies derivatives for f$ to use with Dual numbers when called with n$ arguments.
AddDualHandling[f$, n$] uses Derivative to infer derivatives of f$ for when f$ is called with $n arguments.
AddDualHandling[f$, {n$1, n$2, $$}] uses Derivative to infer derivatives of f$ for when f$ is called with n$1, n$2, $$ arguments."
];
GeneralUtilities`SetUsage[DualApply,
    "DualApply[{f$a, f$b}, Dual[a$, b$]] returns Dual[f$a[a$], f$b[b$]].
DualApply[{f$All}, Dual[a$, b$]] returns Dual[f$All[a$, b$][[1]], f$All[a$, b$][[2]]]. f$All should return a List of length 2.
DualApply[f$, Dual[a$, b$]] returns Dual[f$[a$], f$[b$]].
DualApply[fspec$, Dual[a$, b$], lvlSpec$] maps the function(s) to deeper levels of a$ and b$.
DualApply[f$] is the operator form of DualApply.
DualApply[f$, x$] will use ToDual[x$, 0] to cast standard quantities x$ to duals."
];
GeneralUtilities`SetUsage[DualTuples,
    "DualTuples[{Dual[a$1, b$1], Dual[a$2, b$2], $$, Dual[a$n, b$n]}] finds all ways to pick n$ -1 a$'s and one b$ \
from the list of dual numbers and returns the length-n$ list: 
{
    {b$1, a$2, a$3, $$, a$n},
    {a$1, b$2, a$3, $$, a$n},
    $$,
    {a$1, a$2, a$3, $$, b$n}
}
DualTuples[list$, i$] gives element i$ of DualTuples[list$]."
];
GeneralUtilities`SetUsage[DualTuplesReduce,
    "DualTuplesReduce[list$, f$] applies f$ to the elements of DualTuples[list$] and is effectively equal to f$ @@@ DualTuples[list$].
DualTuplesReduce[list$, f$, g$] folds g$ over DualTuplesReduce[list$, f$]."
];


Begin["`Private`"] (* Begin Private Context *) 

(* 
    Code inspired by the following post on Mathematica StackExchange:
    https://mathematica.stackexchange.com/a/13926/43522
*)
Protect[\[Epsilon]];

derivativePatt = Except[Function[D[__]], _Function];
arrayPattern = _List | _SparseArray;

numericArrayQ = Function[ArrayQ[#, _, NumericQ]];

(* To avoid ?Dual from dumping all definitions in the summary box *)
SetAttributes[Dual, ReadProtected];

(* Boolean functions to test validity of Dual objects *)
dualPatt = _Dual;
DualQ[expr_Dual] := DualScalarQ[expr] || DualArrayQ[expr];
DualQ[_] := False;

DualScalarQ[Dual[___, arrayPattern, ___]] := False;
DualScalarQ[Dual[a_, b_]] := NoneTrue[{a, b}, ArrayQ];
DualScalarQ[_] := False;

DualArrayQ[Dual[a_?DualFreeArrayQ, b_?DualFreeArrayQ]] /; Dimensions[a] === Dimensions[b] := True;
DualArrayQ[_] := False;

UnpackedDualArrayQ[a_?Developer`PackedArrayQ] := False;
UnpackedDualArrayQ[a_?ArrayQ] := FreeQ[a, Except[_Dual], {ArrayDepth[a]}, Heads -> False];
UnpackedDualArrayQ[_] := False;

DualFreeArrayQ[a_?Developer`PackedArrayQ] := True
DualFreeArrayQ[a_?ArrayQ] := FreeQ[a, _Dual, {ArrayDepth[a]}, Heads -> False];
DualFreeArrayQ[_] := False;

StandardQ[_Dual] := False;
StandardQ[_] := True;
standardPatt = Except[_Dual];

(* Accessing standard and nonstandard parts *)
SetAttributes[Standard, Listable];
Standard[Dual[a_, _]] := a;
Standard[x_] := x;

SetAttributes[NonStandard, Listable];
NonStandard[Dual[_, b_]] := b;
NonStandard[_] := 0;

SetAttributes[std, Listable];
std[Dual[a_, _]] := a;
std[x_] := x;

SetAttributes[nonstd, Listable];
nonstd[Dual[_, b_]] := b;
nonstd[x_] := 0;

(* Constructors *)
Dual[] := Dual[0, 1];
Dual[a_] := ToDual[a, 0];

ToDual::cons = "Cannot construct a dual quantity from arguments `1`";
(* Take a standard quantities and give it a constant nonstandard part *)
ToDual[d_Dual, ___] := d;
ToDual[a_SparseArray?DualFreeArrayQ, const : Except[_?ArrayQ] : 0] := Dual[a, SparseArray[{}, Dimensions[a], const]];
ToDual[a_?DualFreeArrayQ, const : Except[_?ArrayQ] : 0] := Dual[a, ConstantArray[const, Dimensions[a]]];
ToDual[a_?ArrayQ /; !DualFreeArrayQ[a], const : Except[_?ArrayQ] : 0] := PackDualArray @ Map[ToDual[#, const]&, a, {ArrayDepth[a]}];
ToDual[a_?DualFreeArrayQ, b_?DualFreeArrayQ] := Dual[a, b];
ToDual[a : Except[_?ArrayQ], arr_SparseArray?DualFreeArrayQ] := Dual[SparseArray[{}, Dimensions[arr], a], arr];
ToDual[a : Except[_?ArrayQ], arr_?DualFreeArrayQ] := Dual[ConstantArray[a, Dimensions[arr]], arr];
ToDual[a_, const : Except[_?ArrayQ] : 0] := Dual[a, const];
ToDual[args__] /; (Message[ToDual::cons, Short /@ {args}]; False) := Undefined

(* Messages to warn when invalid Dual arrays have been constructed *)
Dual::array = "Bad packed dual array found. Dimensions `1` and `2` found for the standard and nonstandard parts of `3`";
Dual[a : arrayPattern, b_] /; And[
    !DualArrayQ[Unevaluated @ Dual[a, b]],
    (
        Message[Dual::array, Dimensions[a], Dimensions[b], Short[Inactive[Dual][a, b]]];
        False
    )
] := Undefined;
Dual[a : Except[arrayPattern], b : arrayPattern] /; (
    Message[Dual::array, Dimensions[a], Dimensions[b], Short[Inactive[Dual][a, b]]];
    False
) := Undefined;
Dual[a_, b_, c__] /; (
    Message[Dual::argt, Dual, Length[{a, b, c}], 1, 2];
    False
) := Undefined;

(* Packing and unpacking dual arrays *)
PackDualArray::arrayQ = "`1` is not an array.";
PackDualArray[Dual[a_, b_]] := Dual[Developer`ToPackedArray[a], Developer`ToPackedArray[b]];
PackDualArray[array_?UnpackedDualArrayQ] := With[{
    depth = ArrayDepth[array]
},
    Dual[
        Developer`ToPackedArray @ Part[array, Sequence @@ ConstantArray[All, depth], 1],
        Developer`ToPackedArray @ Part[array, Sequence @@ ConstantArray[All, depth], 2]
    ]
];
(* Backup definition in case normal numbers are mixed in *)
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
UnpackDualArray::notArray = "Cannot unpack dual scalar `1`.";
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
UnpackDualArray[d_Dual?DualScalarQ] := (Message[UnpackDualArray::notArray, Short[d]]; d);
UnpackDualArray[other_] := (
    Message[UnpackDualArray::badarray, Short[other]];
    other
);

(* Basic properties of dual numbers *)
Dual[Dual[a1_, b1_], Dual[a2_, _]] := Dual[a1, a2 + b1];
Dual[Dual[a_, b_], c_] := Dual[a, b + c];
Dual[a_, Dual[b_, _]] := Dual[a, b];

(* I found that making Dual randomly disappear is more trouble than it's worth. Enable this at your own peril. *)
(* Dual[a_, 0] := a; *)

(* This makes sure that D[expr, var] works for expressions involving dual numbers *)
Derivative[1, 0][Dual] = 1&;
Derivative[0, 1][Dual] = Dual[0, 1]&;

(* SyntaxInformation *)
Scan[
    Function[
        SyntaxInformation[#] = {"ArgumentsPattern" -> {_}}
    ],
    {
        Standard, NonStandard, StandardAll, DualSimplify, DualQ, DualScalarQ,
        DualArrayQ, UnpackedDualArrayQ, DualFreeArrayQ, StandardQ,
        PackDualArray, UnpackDualArray
    }
];

Scan[
    Function[
        SyntaxInformation[#] = {"ArgumentsPattern" -> {__}}
    ],
    {
       ToDual, DualExpand, DualFactor, DualFindMinimum, FindDualSolution,
       DualFindMaximum, DualApply, DualTuples
    }
];

Scan[
    Function[
        SyntaxInformation[#] = {"ArgumentsPattern" -> {_, _}}
    ],
    {DualLinearSolveFunction, AddDualHandling}
];

End[] (* End Private Context *)

EndPackage[]