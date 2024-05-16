(* ::Package:: *)

(* :Title: Guess the function*)
(* :Context: Gioco matematico che .. *)
(* :Author: Leonardi Dess\[IGrave], Emanuele Grasso, Luca Polese, Davide Spada *)
(* :Summary: Package per giocare con Mathematica ... *)
(* :Copyright: Alma Mater Studiorum - Universit\[AGrave] di Bologna 2023 *)
(* :Package Version: 0.0.1*)
(* :Mathematica Version: 14*)
(* :History: \\ *)
(* :Keywords: funzioni, punti, parabole*)
(* :Sources: \\*)
(* :Discussion: \\*)



(* ::Text:: *)
(*Dichiarazione di inizio del package*)


BeginPackage["Package`"];

SetDirectory[NotebookDirectory[]];

Get["Backend.wl"];

GuessTheFunctionGUI::usage = "GuessTheFunctionGUI[] permette di creare l'interfaccia grafica con cui l'utente interagisce per poter avviare il gioco";

CreateDynamicWindow::usage = "CreateDynamicWindow[]";

CreateInfoWindow::usage = "CreateInfoWindow[]"


Begin["`Private`"];

myCounterErrori = 0;
seed = 0;

SetDirectory[NotebookDirectory[]];


(* ::Text:: *)
(*Codice per creare un interfaccia grafica esterna*)


CheckInput[realA_, realB_, realC_, a_, b_, c_] := Module[{message = "", condition},
	condition = {realA, realB, realC} === {IntegerPart@a, IntegerPart@b, IntegerPart@c};
	If[condition, 
		{message="Bravo";
		myCounterErrori=0;}, 
		{message = "Hai sbagliato";
		myCounterErrori++;}];
	Return[message];
]

myCheckMatrix[matrix_, constantVector_, points_] := Module[{message = "", coefficentMatrix, rhsVector, solution, tmpSol},
	{coefficentMatrix, rhsVector} = Backend`myGenerateVandermondeMatrix[points];
	solution = LinearSolve[coefficentMatrix, rhsVector];
	tmpSol = LinearSolve[matrix, Transpose[constantVector][[1]]];
	Print[coefficentMatrix];
	Print[matrix];
	Print[rhsVector];
	Print[Transpose[constantVector][[1]]];
	If[solution === tmpSol,
		message = "Matrice corretta",
		message = "Matrice sbagliata"
	];
	Print[solution];
	Print[tmpSol];
	Return[message]
]

myGeneratePointDisplay[points_] := Module[{},
	Return[
		Column[
			Table[
				StringForm["\!\(\*SubscriptBox[\(p\), \(``\)]\) = (``, ``)",i, points[[i, 1]], points[[i, 2]]],
				{i, Length[points]}]
			]
		]
]



(* ::Text:: *)
(*Codice per creare un'interfaccia grafica che permette di eseguire l'esercizio per trovare la parabola passante per 3 punti*)


GuessTheFunctionGUI[2] := CreateDialog[
	DynamicModule[{
		a, b, c, 
		realA, realB, realC,
		x, 
		message, message2, message3, 
		expr, 
		coefficentMatrix,
		constantVector
		},
		myCounterErrori = 0;
		{expr, realA, realB, realC} = Backend`myGenerateEquation[2, seed];
		points = Backend`myGeneratePointsOnLineOrParabola[3];
		message = "";
		message2 = "";
		message3 = "";
		dims = {3, 3};
		Style[
		Pane[
			Column[{
				TextCell["Guess the function", "Title"],
				TextCell["In questo esercizio dovrai trovare la funzione della parabola che passa per i seguenti tre punti:", TextAlignment->Left],
				TextCell[TraditionalForm@myGeneratePointDisplay[points], "Output"],
				TextCell["Nella seguente sezione \[EGrave] possibile inserire i 3 coefficenti dell'equazione di secondo grado.\nCliccando sul bottone \[EGrave] possibile visualizzare la parabola nell'asse cartesiano:"],
				EventHandler[
					Row[
						{
							InputField[Dynamic[a], Number, FieldHint->"a", FieldSize->2],
							DisplayForm[ToExpression["x^2"]],
							DisplayForm[" + "],
							InputField[Dynamic[b], Number, FieldHint->"b", FieldSize->2],
							DisplayForm[ToExpression["x"]],
							DisplayForm[" + "],
							InputField[Dynamic[c], Number, FieldHint->"c", FieldSize->2],
							Spacer[30],
							Button["Inserisci funzione nel grafico", 
								{
									If[
										Head[a] === Integer && Head[b] === Integer && Head[c] === Integer,
										{
											fun = a*x^2 + b*x + c,
											{expr, realA, realB, realC} = Backend`myGenerateEquation[2, seed];
											message = CheckInput[realA, realB, realC, a,  b,  c];
											message3 = "";
										},
										message3 = "Attenzione! Campi vuoti o numeri non interi inseriti!";
									]
								}
							]
						}
					], {{"KeyDown", "."} :> Null},
						PassEventsDown -> False
				],
				TextForm@Dynamic@Style[message3, FontColor -> Red],
				TextCell["Grafico dell'equazione inserita in input:", "Subsection"],
				Framed@Dynamic@Show[
					callouts = Callout[#, "(" <> ToString[#[[1]]] <> ", " <> ToString[#[[2]]] <> ")", Background->LightBlue, Frame->True, RoundingRadius->5,FrameMargins->5] &/@ points;
					ListPlot[callouts, ImageSize->Medium, ImageMargins->20, PlotRange->{{-10,10},Automatic}],
					Plot[fun,{x,-10,10}]
				],
				TextForm[Dynamic@message],
				Dynamic@DisplayForm["Numero errori: " <> ToString[myCounterErrori]],
				Dynamic@DisplayForm@If[myCounterErrori >= 3, Column[{
					coefficentMatrix = ConstantArray[Null, dims];
					constantVector =  ConstantArray[Null, {3, 1}];
					Row[{
						MatrixForm[
						    Table[With[{i = i, j = j},
						      InputField[Dynamic@coefficentMatrix[[i, j]], Number, FieldSize->2]],
						     {i, 3}, {j, 3}]
						    ],
						 Style[DisplayForm[" \[Times] "], FontSize->16],
						 Style[MatrixForm[{"c", "b", "a"}], FontSize->16],
						 Style[DisplayForm[" = "], FontSize->16],
						 MatrixForm[
						    Table[With[{i = i, j = j},
						      InputField[Dynamic@constantVector[[i, j]], Number, FieldSize->2]],
						     {i, 3}, {j, 1}]
						    ],
						Spacer[20],
					    Button["Controlla", 
							{
								If[
									AllTrue[Flatten[coefficentMatrix], Head[#] === Integer &] && AllTrue[Flatten[constantVector], Head[#] === Integer &],
									message2 = myCheckMatrix[coefficentMatrix, constantVector, points],
									message2 = "Attenzione! Campi vuoti o numeri non interi inseriti!"
								]
							}
						]
				    }],
					TextForm@Dynamic@Style[message2, FontColor -> Red]
				    (*Dynamic@MatrixForm[coefficentMatrix],
				    Dynamic@MatrixForm[constantVector]*)
				    }, Center],
					""
				]
		   }, Alignment->Center
		   ],
		   Alignment->{Center},
	       ImageSizeAction->"Scrollable",
	       ImageSize->Full,
	       ImageMargins->20
	], FontSize->16
	]],
	WindowTitle -> "Plot area",
	WindowSize -> {Scaled[1],Scaled[1]},
	WindowElements->{"VerticalScrollBar", "StatusArea"}
];



(* ::Text:: *)
(*Codice per creare un'interfaccia grafica che permette di eseguire l'esercizio per trovare la retta passante per 2 punti*)


GuessTheFunctionGUI[1] := CreateDialog[
	DynamicModule[{
	m, q,
	realM, realQ,
	x, 
	message, message2, message3,
	expr, 
	coefficentMatrix,
	constantVector
	},
	myCounterErrori = 0;
	{expr, realM, realQ} = Backend`myGenerateEquation[1, seed];
	points = Backend`myGeneratePointsOnLineOrParabola[2];
	message = "";
	message2 = "";
	message3= "";
	dims = {2, 2};
	Style[
		Pane[
			Column[{
				TextCell["Guess the function", "Title"],
				TextCell["In questo esercizio dovrai trovare la funzione della retta che passa per i seguenti due punti:", TextAlignment->Left],
				TextCell[TraditionalForm@myGeneratePointDisplay[points], "InlineFormula"],
				TextCell["Nella seguente sezione \[EGrave] possibile inserire i 2 coefficenti dell'equazione di primo grado.\nCliccando sul bottone \[EGrave] possibile visualizzare la parabola nell'asse cartesiano:"],
				EventHandler[
					Row[
						{
							InputField[Dynamic[m], Number, FieldHint->"m", FieldSize->2],
							DisplayForm[ToExpression["x"]],
							DisplayForm[" + "],
							InputField[Dynamic[q], Number, FieldHint->"q", FieldSize->2],
							Spacer[30],
							Button["Inserisci funzione nel grafico", 
								{
									If[
										m === Null || Head[m] =!= Integer || q === Null || Head[q] =!= Integer,
										message3 = "Attenzione! Campi vuoti o numeri non interi inseriti!",
										{
											fun = m*x + q;
											message = CheckInput[0, realM, realQ, 0,  m,  q];
											message3 = "";
										}
									]
								}
							]
						}
					], {{"KeyDown", "."} :> Null},
						PassEventsDown -> False
				],
				TextForm@Dynamic@Style[message3, FontColor -> Red],
				TextCell["Grafico dell'equazione inserita in input:", "Subsection"],
				Framed@Dynamic@Show[
					callouts = Callout[#, "(" <> ToString[#[[1]]] <> ", " <> ToString[#[[2]]] <> ")", Background->LightBlue, Frame->True, RoundingRadius->5,FrameMargins->5] &/@ points;
					ListPlot[callouts, ImageSize->Medium, ImageMargins->20, PlotRange->{{-10,10},Automatic}],
					Plot[fun,{x,-10,10}]
				],
				Dynamic@TextForm[message],
				Dynamic@DisplayForm["Numero errori: " <> ToString[myCounterErrori]],
				Dynamic@DisplayForm@If[myCounterErrori >= 3, Column[{
					coefficentMatrix = ConstantArray[Null, dims];
					constantVector =  ConstantArray[Null, {2, 1}];
					Row[{
						MatrixForm[
						    Table[With[{i = i, j = j},
						      InputField[Dynamic@coefficentMatrix[[i, j]], Number, FieldSize->2]],
						     {i, 2}, {j, 2}]
						    ],
						  Style[DisplayForm[" \[Times] "], FontSize->16],
						 Style[MatrixForm[{"q", "m"}], FontSize->16],
						 Style[DisplayForm[" = "], FontSize->16],
						 MatrixForm[
						    Table[With[{i = i, j = j},
						      InputField[Dynamic@constantVector[[i, j]], Number, FieldSize->2]],
						     {i, 2}, {j, 1}]
						    ],
						Spacer[20],
					    Button["Controlla", 
							{
								If[
									AllTrue[Flatten[coefficentMatrix], Head[#] === Integer &] && AllTrue[Flatten[constantVector], Head[#] === Integer &],
									message2 = myCheckMatrix[coefficentMatrix, constantVector, points],
									message2 = "Attenzione! Campi vuoti o numeri non interi inseriti!"
								]
							}
						]
				    }],
					TextForm@Dynamic@Style[message2, FontColor -> Red]
				    (*Dynamic@MatrixForm[coefficentMatrix],
				    Dynamic@MatrixForm[constantVector]*)
				    }, Center],
					""
				]
		   }, Alignment->Center
		   ],
		   Alignment->{Center},
	       ImageSizeAction->"Scrollable",
	       ImageSize->Full,
	       ImageMargins->20
	      ], FontSize->16]
	  ],
	WindowTitle -> "Plot area",
	WindowSize -> {Scaled[1],Scaled[1]},
	WindowElements->{"VerticalScrollBar", "StatusArea"}
];

(* ::Text:: *)
(*Codice per creare un'interfaccia grafica che permetta di scegliere la modalit\[AGrave] di gioco*)


CreateDynamicWindow[] :=
  DynamicModule[{
    seedMessage,
    infoWindow
    },
  
    seedMessage = "";
    infoText = "Un seed \[EGrave] un numero di partenza utilizzato dagli algoritmi che generano numeri casuali. Impostare un seed garantisce che l'algoritmo generi la stessa sequenza di esercizi ogni volta che viene eseguito con lo stesso seed. Questo \[EGrave] essenziale per la riproducibilit\[AGrave] e la coerenza dei risultati degli esercizi.";
    
    CreateDialog[
		DialogNotebook[
			Pane[
				Column[{
					Row[{
						EventHandler[ 
							Tooltip[Style["\:2139", FontSize -> 16], "Perch\[EAcute] inserire un seed?"], {"MouseClicked" :> CreateInfoWindow[infoText]}],
						Spacer[1],
						TextCell["Inserire Seed:"],
						Spacer[5],
						EventHandler[
							InputField[Dynamic[seed], Number, ContinuousAction->True, FieldSize->Tiny],
							 {{"KeyDown", "."} :> Null},
							PassEventsDown -> False]
							}],
					Spacer[20],
					Row[{
						Button["Retta", {If[
							Head[seed] === Integer,
							{
								Dynamic@Backend`setSeed[seed];
								GuessTheFunctionGUI[1];
								DialogReturn[];
								seedMessage = "";
							},
							seedMessage = "Attenzione!\nIl seed non pu\[OGrave] essere vuoto\no un numero non intero!";
							]}], 
						Spacer[20],
						Button["Parabola", {If[
							Head[seed] === Integer,
							{
								Dynamic@Backend`setSeed[seed];
								GuessTheFunctionGUI[2];
								DialogReturn[];
								seedMessage = "";
							},
							seedMessage = "Attenzione!\nIl seed non pu\[OGrave] essere vuoto\no un numero non intero!";
							]}]
						}], Spacer[20], TextForm@Dynamic@Style[seedMessage, FontColor -> Red]
					}, Center],
			    Alignment->{Center},
				ImageSizeAction->"Scrollable",
				ImageSize->Full,
				ImageMargins->20
			]
	   ], 
	   WindowSize-> {All, All}, 
	   WindowTitle-> "Scegli esercizio"
	]
  ]


(* ::Text:: *)
(*Codice che gestisce l'apertura di una finestra di informazione per l'utente.*)


CreateInfoWindow[infoText_] :=
  CreateDialog[Column[{TextCell[infoText, "Text", FontSize -> 12], Spacer[20], Button[
    "Close", DialogReturn[]]}], WindowSize -> {400, 150
    }, WindowTitle -> "Perch\[EAcute] inserire un seed?"]


(* ::Text:: *)
(*Dichiarazione di fine del package*)


End[];

EndPackage[];
