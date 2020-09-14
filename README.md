# Dual numbers and automatic differentiation for Mathematica

This Wolfram Language (WL) Paclet provides an implementations of dual numbers and adds support for a number of built-in symbols to work with them. This should allow the user to perform automatic differentiation of many programs written in WL simply by calling them with dual numbers instead of an ordinary ones. 

## Installation instructions

To install the package, download the paclet file from the [GitHub repository](https://github.com/ssmit1986/DualNumbers) and run:

    PacletInstall["/path/to/DualNumbers-1.0.paclet"]
    <<DualNumbers`

The easiest way to get the full path to the file is with the `Insert > File Path...` option from the menu bar.

Alternatively, if you want to edit the code for your own purposes, you can also load the package from the source code by cloning the GitHub repository and then using:

    PacletDirectoryLoad["/path/to/DualNumbers/"] (* Same directory as the one containing this README file *)
    <<DualNumbers`

**Note on the Paclet version:** the paclet is configured to work with Mathematica version 12.1 and higher because that is what it has been tested on. Most of the code in this repository should work with older versions of Mathematica if you `Get` the DualNumbers.wl file directly.

## Introduction

## Features

* Calculate derivatives of programs by passing dual numbers as arguments. The standard part of the returned result is the function value and the non-standard part gives you the exact (directional) derivative.

* You can define packed dual arrays and efficiently perform structural operations on them.


* Built-in functions with support for dual numbers and dual arrays:

    * Elementary arithmetic: `Plus`, `Times`, `Power`, `Subtract`, `Divide`.
    * Many elementary functions: `Exp`, `Log`, `Abs`, `Sign`, `Clip`, `Gamma`, etc.
    * Boolean functions: `Equal`, `Unequal`, `Greater`, `Less`, `NumericQ`, `Positive`, etc.
    * Mathematical array operations: `Dot`, `Transpose`, `MatrixPower`, `Norm`, `Inverse`, `LinearSolve`, `Total`, `Mean`.
    * Accessing arrays: `Part`, `Take`, `Drop`, `Extract`, `First`, `Most`, `Last`, `Rest`.
    * Structural array manipulation: `Flatten`, `Map`, `Apply`, `MapIndexed`, `Join`, `Select`, `Pick`, `Position`.
    * Array identification: `Length`, `Dimensions`, `ArrayDepth`, `MatrixQ`, `VectorQ`, `SquareMatrixQ`.


* Helper functions:
    * `ToDual`: construct dual numbers from scalars or arrays.
    * `Standard`, `NonStandard`: Extract the first/second argument of a dual quantity.
    * `DualApply`: apply functions directly to the standard and non-standard parts of a dual quantity.
    * `AddDualHandling`: specify derivatives for custom functions to be used with dual numbers.
    * `DualFindRoot`, `FindDualSolution`, `DualFindMinimum`, `DualFindMaximum`: solve equations and optimization problems involving dual numbers.
    * `PackDualArray`, `UnpackDualArray`: convert dual arrays between the packed form `Dual[_List, _List]` and the unpacked form (i.e., a normal array with dual numbers at the deepest level).
    * `DualExpand`, `DualFactor`, `DualSimplify`: convert back and forth between the programmatic form `Dual[_, _]` and the algebraic form `a + b * eps`.

## Sources:
* [StackExchange post](https://mathematica.stackexchange.com/a/13926/43522) that provided inspiration.
* [Dual number - Wikipedia](https://en.wikipedia.org/wiki/Dual_number)
* [Automatic differentiation - Wikipedia](https://en.wikipedia.org/wiki/Automatic_differentiation)

## Version history

* 2020-09-14
    * Release of V1.0.