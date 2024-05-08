(* ::Package:: *)

(* :Title: Backend *)
(* :Context: Backend` *)
(* :Author:  *)
(* :Summary: Questo package contiene le funzioni per la generazione di espressioni, punti e sistemi di equazioni. *)


BeginPackage["Backend`"]

GenerateEquation::usage = "GenerateEquation[grade] genera un'equazione di grado grade";
GeneratePointsOnLineOrParabola::usage = "GeneratePointsOnLineOrParabola[nPoints] genera nPoints punti su una retta o una parabola";
GenerateVandermondeMatrix::usage = "GenerateVandermondeMatrix[points] genera la matrice di Vandermonde a partire da una lista di punti";
x::usage = "x rappresenta la variabile indipendente";
Begin["Private`"]

(* Set seed for random number generator *)
SeedRandom[1234];


GenerateEquation[1] := Module[{a, b},
    a = RandomChoice[{1, -1}];
    b = RandomInteger[{-5, 5}];
    expr = a*x + b;
    Return[expr];
]

GenerateEquation[2] := Module[{a, b, c},
    a = RandomChoice[{1, -1}];
    b = RandomInteger[{-5, 5}];
    c = RandomInteger[{-10, 10}];
    expr = a*x^2 + b*x + c;
    Return[expr];
]

GenerateEquation[_] := (Print["Grade must be 1 or 2"]; Return[])


GeneratePointsOnLineOrParabola[nPoints_] := Module[{points},
    xList = RandomInteger[{-10, 10}, nPoints];

    While[Length[xList] != Length[DeleteDuplicates[xList] ],
        xList = RandomInteger[{-10, 10}, nPoints];
    ];
   
    points = {#, expr /. x -> #} & /@ xList;    
    Return[points];
]

GenerateVandermondeMatrix[points_] := Module[{coefficentMatrix, rhsVector},
    (* generate matrix *)
    coefficentMatrix = Table[points[[i, 1]]^j, {i, 1, Length[points], 1}, {j, 0, Length[points]-1, 1}];
    
    (* generate right hand side *)
    rhsVector = Table[points[[i, 2]], {i, 1, Length[points], 1}];

    Return[{coefficentMatrix, rhsVector}];
]

End[]
EndPackage[]