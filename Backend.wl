(* Package Backend *)

(* Titolo e contesto del package *)
(* :Title: Backend *)
(* :Context: Backend` *)
(* :Summary: Questo package contiene le funzioni per la generazione di espressioni, punti e sistemi di equazioni. *)

BeginPackage["Backend`"];

(* Dichiarazione delle funzioni esportate *)
myGenerateEquation::usage = "myGenerateEquation[grade] genera un'equazione di grado grade";
myGeneratePointsOnLineOrParabola::usage = "myGeneratePointsOnLineOrParabola[nPoints] genera nPoints punti su una retta o una parabola";
myGenerateVandermondeMatrix::usage = "myGenerateVandermondeMatrix[points] genera la matrice di Vandermonde a partire da una lista di punti";
x::usage = "x rappresenta la variabile indipendente";

Begin["Private`"];

(* Set seed for random number generator *)
expr = None;

(* Funzione per impostare il seed del generatore di numeri casuali *)
setSeed[seed_] := Module[{},
    Print[seed]; (* Stampa il seed per debug *)
    SeedRandom[seed];
]

(* Funzione per generare un'equazione di primo grado *)
myGenerateEquation[1, seed_] := Module[{a, b},
    SeedRandom[seed];	
    a = RandomChoice[{1, -1}]; (* Sceglie casualmente 1 o -1 *)
    b = RandomInteger[{-5, 5}]; (* Genera un numero intero tra -5 e 5 *)
    expr = a*x + b; (* Costruisce l'equazione di primo grado ax + b *)
    Return[{expr, a, b}]; (* Restituisce l'equazione e i coefficienti a e b *)
]

(* Funzione per generare un'equazione di secondo grado *)
myGenerateEquation[2, seed_] := Module[{a, b, c},
    SeedRandom[seed];
    a = RandomChoice[{1, -1}]; (* Sceglie casualmente 1 o -1 *)
    b = RandomInteger[{-5, 5}]; (* Genera un numero intero tra -5 e 5 *)
    c = RandomInteger[{-10, 10}]; (* Genera un numero intero tra -10 e 10 *)
    expr = a*x^2 + b*x + c; (* Costruisce l'equazione di secondo grado ax^2 + bx + c *)
    Return[{expr, a, b, c}]; (* Restituisce l'equazione e i coefficienti a, b e c *)
]

(* Funzione per gestire l'errore se il grado dell'equazione non è 1 o 2 *)
myGenerateEquation[_] := (Print["Grade must be 1 or 2"]; Return[])

(* Funzione per generare punti su una retta o una parabola *)
myGeneratePointsOnLineOrParabola[nPoints_] := Module[{points, xList},
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