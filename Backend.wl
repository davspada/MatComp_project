(* ::Package:: *)

(* Package Backend *)

(* Titolo e contesto del package *)
(* :Title: Backend *)
(* :Context: Backend` *)
(* :Summary: Questo package contiene le funzioni per la generazione di espressioni, punti e sistemi di equazioni. *)
(* :Copyright: Leonardo Dess\[IGrave], Emanuele Grasso, Luca Polese, Davide Spada - 2023 *)
(* :Package Version: 1.0.0*)
(* :Mathematica Version: 14 *)
(* :History: \\ *)
(* :Keywords: funzioni, punti, parabole *)
(* :Sources: \\ *)
(* :Discussion: \\ *)

BeginPackage["Backend`"];

(* Dichiarazione delle funzioni esportate *)
(* Queste funzioni necessitano di essere dichiarate per permettere
   la comunicazione con il package principale, rendendo le funzioni
   locali si incapperebbe in errori di comunicazione che sfociano
   in grafica mal renderizzata e popup di messaggi di errore *)
myGenerateEquation::usage = "myGenerateEquation[grade] genera un'equazione di grado grade";
myGeneratePointsOnLineOrParabola::usage = "myGeneratePointsOnLineOrParabola[nPoints] genera nPoints punti su una retta o una parabola";
myGenerateVandermondeMatrix::usage = "myGenerateVandermondeMatrix[points] genera la matrice di Vandermonde a partire da una lista di punti";

Begin["Private`"];

(* Funzione per generare un'equazione di primo grado *)
myGenerateEquation[1, seed_] := Module[{a, b},
    SeedRandom[seed];	
    a = RandomInteger[{-5, 5}]; (* Genera un numero intero tra -5 e 5 *)
    b = RandomInteger[{-10, 10}]; (* Genera un numero intero tra -10 e 10 *)
    Return[{a*x + b, {a, b}}]; (* Restituisce l'equazione e i coefficienti a e b *)
]

(* Funzione per generare un'equazione di secondo grado *)
myGenerateEquation[2, seed_] := Module[{a, b, c},
    SeedRandom[seed];
    a = RandomChoice[{1, -1}]; (* Sceglie casualmente 1 o -1 *)
    b = RandomInteger[{-5, 5}]; (* Genera un numero intero tra -5 e 5 *)
    c = RandomInteger[{-10, 10}]; (* Genera un numero intero tra -10 e 10 *)
    Return[{a*x^2 + b*x + c, {a, b, c}}]; (* Restituisce l'equazione e i coefficienti a, b e c *)
]

(* Funzione per gestire l'errore se il grado dell'equazione non \[EGrave] 1 o 2 
   Questa funzione \[EGrave] commentata in quanto rappresenta una funzione da utilizzare 
   per eventuali estensioni del package *)
(* myGenerateEquation[_] := Module[{}, Print["Grade must be 1 or 2"]; Return[]] *)

(* Funzione per generare punti su una retta o una parabola *)
myGeneratePointsOnLineOrParabola[nPoints_, expr_] := Module[{points, xList},
    xList = RandomInteger[{-10, 10}, nPoints]; (* Genera una lista di numeri casuali per le ascisse *)

    (* Assicura che non ci siano duplicati nelle ascisse *)
    While[Length[xList] != Length[DeleteDuplicates[xList]],
        xList = RandomInteger[{-10, 10}, nPoints];
    ];
   
    points = {#, expr /. x -> #} & /@ xList; (* Calcola le ordinate corrispondenti utilizzando l'equazione generata *)
    Return[points]; (* Restituisce i punti generati *)
]

(* Funzione per generare la matrice di Vandermonde a partire dai punti *)
myGenerateVandermondeMatrix[points_] := Module[{coefficentMatrix, rhsVector, myPow},
    (* Definisce una funzione per elevare un numero a una potenza *)
    myPow[0, 0] = 1;
    myPow[a_, b_] := a^b;
    
    (* Genera la matrice di Vandermonde *)
    coefficentMatrix = Table[myPow[points[[i, 1]], j], {i, 1, Length[points], 1}, {j, 0, Length[points]-1, 1}];
    
    (* Genera il vettore del lato destro *)
    rhsVector = Table[points[[i, 2]], {i, 1, Length[points], 1}];

    Return[{coefficentMatrix, rhsVector}]; (* Restituisce la matrice di Vandermonde e il vettore del lato destro *)
]

End[];

EndPackage[];


