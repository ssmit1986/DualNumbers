# Dual numbers and automatic differentiation for Mathematica

This Wolfram Language (WL) Paclet provides an implementations of dual numbers and adds support for a number of built-in symbols to work with them. This should allow the user to perform automatic differentiation of many programs written in WL simply by calling them with dual numbers instead of an ordinary ones. 

## Installation instructions

To install the package, simply download the paclet from the [GitHub repository](https://github.com/ssmit1986/DualNumbers) and run:

    PacletInstall["/path/to/DualNumbers-1.0.paclet"]
    <<DualNumbers`

The easiest way to get the full path to the file is with the `Insert > File Path...` option from the menu bar.

Alternatively, if you want to edit the code for your own purposes, you can also load the package from the source code by cloning the GitHub repository and then using:

    PacletDirectoryLoad["/path/to/DualNumbers/"] (* Same directory as the one containing this README file *)
    <<DualNumbers`

## Introduction

## Features


## Sources:
* [StackExchange post](https://mathematica.stackexchange.com/a/13926/43522) that provided inspiration.
* [Dual number - Wikipedia](https://en.wikipedia.org/wiki/Dual_number)
* [Automatic differentiation - Wikipedia](https://en.wikipedia.org/wiki/Automatic_differentiation)