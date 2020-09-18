# Dual numbers and automatic differentiation for Mathematica

This Wolfram Language (WL) Paclet provides an implementations of dual numbers and adds support for a number of built-in symbols to work with them. This should allow the user to perform automatic differentiation of many programs written in WL simply by calling them with dual numbers instead of an ordinary ones. 

## Installation instructions

To automatically download and install the currently released version of the paclet, simply run:

```
PacletInstall["https://github.com/ssmit1986/DualNumbers/releases/download/1.0/DualNumbers-1.0.paclet"]
```

To install the latest version of the package, download the latest paclet file from the GitHub [paclet archive](https://github.com/ssmit1986/DualNumbers/tree/main/Paclet%20archives) and run:

```
PacletInstall["/path/to/DualNumbers-x.y.z.paclet"]
```

The easiest way to get the full path to the file is with the `Insert > File Path...` option from 
the menu bar. After installation, you can load the package with:

```
<<DualNumbers`
```

Alternatively, if you want to edit the code for your own purposes, you can also load the package 
from the source code by cloning the GitHub repository and then using:

```
PacletDirectoryLoad["/path/to/DualNumbers/"] (* Same directory as the one containing this README file *)
<<DualNumbers`
```

**Note on the Paclet version:** the paclet is configured to work with Mathematica version 12.1 and 
higher because that is what it has been tested on. Most of the code in this repository should work with 
older versions of Mathematica if you `Get` the DualNumbers.wl file directly.

## Introduction

This package is developed primarily for automatic (or algorithmic) differentiation (abbreviated as AD) 
in Wolfram Language. AD is the middle ground between symbolic differentiation 
(which is handled by `D` and `Derivative`) and numerical differentiation 
(which can be done with [ND](http://reference.wolfram.com/language/NumericalCalculus/ref/ND.html)). 
The main idea behind AD is that most programs take many small differentiable steps even if it's 
not possible to find a general symbolic derivative the program as a whole. So to find the derivative 
of a program, all you need to do is keep track of the derivative at each step of the computation.

One way to accomplish this task of keeping track of derivatives, is to implement dual numbers. 
Dual numbers are somewhat similar to complex numbers in that they can be written in the form:

```
d == a + b ϵ
```

where `ϵ`, (like the imaginary unit `i`) is a new number not found on the real line. It is defined by the property that `ϵ^2 == 0`, but `ϵ != 0`.
You can think of `ϵ` as the algebraic version of an infinitesimal. So a dual number can be considered to be a tuple of 2 numbers (`a` and `b`) that
in this package will be represented as: 

```
Dual[a, b]
```

The first argument `a` will be called the *standard* part while the second argument `b` will be called the *nonstandard* part (this terminology has been borrowed from [https://en.wikipedia.org/wiki/Nonstandard_analysis](nonstandard analysis)). 
You can add and multiply duals much like normal numbers. The standard part behaves exactly as real numbers would and will never be influenced by the nonstandard part. For this reason, you can think of the standard part as the most important part of a dual number. The nonstandard part, on the other hand, keeps track of the derivative and can be thought of as a very tiny perturbation on the standard part. You can see this by expanding a general function as a Taylor series in `ϵ`:

```
In[]:= Normal @ Series[f[a + b ϵ], {ϵ, 0, 4}] /. Power[ϵ, _?(GreaterEqualThan[2])] -> 0

Out[]= f[a] + b ϵ f'[a]
```

Since `ϵ^2 == 0`, the series has only two terms and produces a new dual number `Dual[f[a], b f'[a]]`. So if you call the function as:

```
f[Dual[a, 1]]
```

you get:

```
Dual[f[a], f'[a]]]
```

Of course, you need to make sure that all operations performed in the computation of `f` can deal with dual numbers to get this result, 
and that is precisely what this package tries to achieve.

## Example

Here's a simple example of a programmatic function that Mathematica cannot provide a simple derivative for:

```
In[]:= f[a_] := Module[{x = 1., y, i = 0},
    While[ Not[(y = Cos[a x]) == x],
        x = y;
        i++
    ];
    x
];
Derivative[1] @ f

Out[]= 0&
```

The derivative returned by `Derivative` is incorrect because While resists symbolic differentiation. Of course, in this case it would,
in principle, be possible to find a derivative for the fixed point with some mathematical insight, but that would difficult to spot programmatically.
The function can be easily differentiated using dual numbers:

```
<<DualNumbers`;
In[]:= d = f[Dual[0.5, 1.]]

Out[]= Dual[0.900367, -0.321771]
```

You can extract the returned value and derivative with `Standard` and `NonStandard`:

```
In[]:= Standard[d]
NonStandard[d]

Out[]= 0.900367

Out[]= -0.321771
```

Check that the nonstandard part really gives the right derivative with a simple differential quotient:

```
In[]:= With[{h = 0.001, a = 0.5}, (f[a + h] - f[a - h])/(2 h)]

Out[]= -0.321771
```


It's possible to evaluate `f[Dual[0.5, 1]]` because `Dual` has definitions to evaluate comparison functions like `Equal` (so the `While` gate evaluates) and elementary functions like `Cos` and `Sin` (for both numerical and symbolic dual numbers):

```
In[]:= Dual[0.5, 1] == 0.5
Dual[0.5, 1] == Dual[0.5, 2]

Out[]= True

Out[]= True
```

```
In[]:= Cos[Dual[Pi/2, 1]]
Cos[Dual[a, b]]

Out[]= Dual[0, -1]

Out[] = Dual[Cos[a], -b Sin[a]]
```

Note that `Equal` only cares about the standard part of dual numbers. In addition, if the comparison does not yield `True` or `False`, the equation is left alone:

```
In[]:= Dual[x, 1] == Dual[y, 2]

Out[]= Dual[x, 1] == Dual[y, 2]
```

Because the termination of the `While` loop is independent of the nonstandard part, it's even possible to call `f` with a symbolic nonstandard part:

```
In[]:= f[Dual[0.5, b]]

Out[]= Dual[0.900367, -0.321771 b]
```

This can be particularly useful for functions with more than one argument.

## Functions with more than one argument

When you have a function that takes multiple arguments and outputs a single number, you can use dual numbers to calculate [directional derivatives](https://en.wikipedia.org/wiki/Directional_derivative). Here is a simple example using a function for which we can also find a symbolic derivative (for comparison):

```
In[]:= g[x_, y_] := Sin[x * y]/(x^2 + y^2);
d = g[Dual[0.5, b1], Dual[2., b2]] // Simplify

Out[]= Dual[0.197993, 0.207673 b1 - 0.122782 b2]
``` 

The gradient of `g` at `{x -> 0.5, y -> 2.}` is:

```
In[]:= grad = D[g[x, y], {{x, y}}] /. {x -> 0.5, y -> 2.}

Out[]= {0.207673, -0.122782}
```

Note how the coefficients of `b1` and `b2` correspond to the components of the gradient. In other words, the nonstandard part of `d` satisfies:

```
In[]:= NonStandard[d] == {b1, b2} . grad

Out[]= True
```

So if you want to calculate the full gradient of `g` with dual numbers, you can either use symbolic nonstandard parts for the input arguments and then collect the coefficients afterwards or you can invoke `g` twice to obtain the coordinates independently:

```
In[]:= {g[Dual[0.5, 1.], 2.], g[0.5, Dual[2., 1.]]}

Out[]= {Dual[0.197993, 0.207673], Dual[0.197993, -0.122782]}
```

In many situations, directional derivatives are quite powerful by themselves and it's not always necessary to compute all components of the gradient.

## Dual arrays

In many programs it's not enough to deal with only scalar values, which is why this package also features *packed dual arrays* to facilitate efficient array operations.

As a simple example, suppose you have a list of dual numbers and you want to compute its square norm:

```
In[]:= dvec = Dual[#, 1.] & /@ RandomReal[1, 5]
dvec . dvec

Out[]= {Dual[0.704343, 1.], Dual[0.384163, 1.], Dual[0.189591, 1.], Dual[0.266149, 1.], Dual[0.528375, 1.]}
Out[]= Dual[1.02964, 4.14524]
``` 

This works just fine, but because `dvec` is not a list of normal numbers, the `Dot` operation doesn't evaluate as fast as it can be:

```
In[]:= dvec = Dual[#, 1.] & /@ RandomReal[1, 10^3];
dvec . dvec // RepeatedTiming

Out[]= {0.011, Dual[335.28, 992.399]}
```

You can use `PackDualArray` to create a more efficient representation of `dvec`, which looks like `Dual[aArray, bArray]`:

```
In[33]:= dvecPacked = PackDualArray[dvec];
dvecPacked // Short

Out[]= Dual[{0.104868,<<998>>,0.981509},{1.,<<998>>,1.}]
```

Many vector operations support this packed format (see [Features section](#features)) and are also significantly faster this way:

```
In[]:= dvecPacked . dvecPacked // RepeatedTiming

Out[]= {0.000079, Dual[335.28, 992.399]}
```

You can convert back to the `dvecPacked` back to `dvec` if necessary with `UnpackDualArray`:

```
In[]:= UnpackDualArray[dvecPacked] === dvec

Out[]= True
```

You can test if a dual array is in packed form with `DualArrayQ` or in unpacked form with `UnpackedDualArrayQ`:

```
In[]:= DualArrayQ[dvecPacked]
UnpackedDualArrayQ[dvec]

Out[]= True

Out[]= True
```

You can define packed dual arrays directly without `PackDualArray`. Any dual with two arrays (satisfying [ArrayQ](https://reference.wolfram.com/language/ref/ArrayQ.html)) of the same dimensions is a valid dual array:

```
In[]:= myArray = Dual[Range[10], ConstantArray[1, 10]]
DualArrayQ[myArray]

Out[]= Dual[{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}, {1, 1, 1, 1, 1, 1, 1, 1, 1, 1}]

Out[]= True
```

## Features

* Calculate derivatives of programs by passing dual numbers as arguments. The standard part of the returned result is the function value and the nonstandard part gives you the exact (directional) derivative.

* You can define packed dual arrays and efficiently perform structural operations on them.


* Built-in functions with support for dual numbers and dual arrays:

    * Elementary arithmetic: `Plus`, `Times`, `Power`, `Subtract`, `Divide`.
    * Many elementary functions: `Exp`, `Log`, `Sin`, `Cos`, `Abs`, `Sign`, `Clip`, `Gamma`, etc.
    * Boolean functions: `Equal`, `Unequal`, `Greater`, `Less`, `NumericQ`, `Positive`, etc.
    * Mathematical array operations: `Dot`, `Transpose`, `MatrixPower`, `Norm`, `Inverse`, `LinearSolve`, `Total`, `Mean`.
    * Accessing and modifying arrays: `Part`, `Take`, `Drop`, `Extract`, `First`, `Most`, `Last`, `Rest`, `Join`, `Append`, `Prepend`.
    * Structural array manipulation: `Flatten`, `Map`, `Apply`, `MapIndexed`, `Select`, `GroupBy`, `Pick` (level 1 only), `Position`,
`Fold`, `FoldList`.
    * Sorting: `Sort`, `SortBy`, `Ordering`, `OrderingBy`.
    * Array identification: `Length`, `Dimensions`, `ArrayDepth`, `MatrixQ`, `VectorQ`, `SquareMatrixQ`.


* Helper functions. Use `?` for more information (e.g., `?ToDual`):
    * `ToDual`: construct dual numbers from scalars or arrays.
    * `Standard`, `NonStandard`: Extract the first/second argument of a dual quantity.
    * `DualQ`, `DualScalarQ`, `DualArrayQ`, `UnpackedDualArrayQ`, `DualFreeArrayQ`: testing different types of dual expressions.
    * `DualApply`: apply functions directly to the standard and nonstandard parts of a dual quantity.
    * `AddDualHandling`: specify derivatives for custom functions to be used with dual numbers.
    * `DualFindRoot`, `FindDualSolution`, `DualFindMinimum`, `DualFindMaximum`: solve equations and optimization problems involving dual numbers.
    * `PackDualArray`, `UnpackDualArray`: convert dual arrays between the packed form `Dual[_List, _List]` and the unpacked form (i.e., a normal array with dual numbers at the deepest level).
    * `DualExpand`, `DualFactor`, `DualSimplify`: convert back and forth between the programmatic form `Dual[_, _]` and the algebraic form `a + b ϵ`.
	* `DualTuples`: For a list of dual numbers, find all ways to pick the nonstandard part from one dual number and the standard part from the other ones.

## Know issues and limitations

* Because `Plus`, `Times` and `Power` have the `Listable` attribute, it's not possible to correctly add (multiply, etc.) a packed dual array to a normal array:

```
In[]:= Dual[{1, 2}, {3, 4}] + {5, 6}
Out[]= {Dual[{6, 7}, {3, 4}], Dual[{7, 8}, {3, 4}]} (* Should be Dual[{6, 8}, {3, 4}] *)
```

There is no good way around this because the `Listable` attribute always takes precedence over any `UpValue` (see, e.g., [this discussion](https://mathematica.stackexchange.com/questions/19067/apply-upvalues-before-listability)).
The best way around this, is to cast the normal array to a dual array:

```
In[]:= Dual[{1, 2}, {3, 4}] + Dual @ {5, 6}
Out[]= Dual[{6, 8}, {3, 4}]
```

* `Pick` only works on level 1 with packed dual arrays because it can return ragged arrays if used at deeper levels. 
Use `UnpackDualArray` if you want to use `Pick` at level 2 or deeper.

## Sources:
* [StackExchange post](https://mathematica.stackexchange.com/a/13926/43522) that provided inspiration
* [Dual number - Wikipedia](https://en.wikipedia.org/wiki/Dual_number)
* [Automatic differentiation - Wikipedia](https://en.wikipedia.org/wiki/Automatic_differentiation)

## Version history

* 2020-09-14
    * Release of V1.0 . This release is still subject to change because no stable version has been reached yet.
* 2020-09-17
    * Beta release of V1.0.