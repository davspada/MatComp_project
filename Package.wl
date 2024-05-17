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

GuessTheFunctionGUI::usage = "GuessTheFunctionGUI[]";

CreateDynamicWindow::usage = "CreateDynamicWindow[] permette di creare l'interfaccia grafica con cui l'utente interagisce per poter avviare il programma";

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
	If[
		AllTrue[Flatten[matrix], Head[#] === Integer || Head[#] =!= Null &] && AllTrue[Flatten[constantVector], Head[#] === Integer || Head[#] =!= Null &],
		message = "",
		{message = "Attenzione! Campi vuoti o numeri non interi inseriti!", Return[message]}
	];
	
	{coefficentMatrix, rhsVector} = Backend`myGenerateVandermondeMatrix[points];
	solution = LinearSolve[coefficentMatrix, rhsVector];
	tmpSol = LinearSolve[matrix, Transpose[constantVector][[1]]];
	
	If[solution === tmpSol,
		message = "Congratulazioni, la matrice \[EGrave] corretta, ora risolvila ed inserisci i coefficienti in alto.",
		message = "\|01f6ab Matrice sbagliata \|01f6ab"
	];
	
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
		
		If[Head[seed] === Integer, 
			{expr, realA, realB, realC} = Backend`myGenerateEquation[2, seed],
			(*Genera pseudorandomicamente un numero intero da 0 a 9999*)
			{seed = RandomInteger[9999], {expr, realA, realB, realC} = Backend`myGenerateEquation[2, seed]}
		];
			
		points = Backend`myGeneratePointsOnLineOrParabola[3];
		message = "";
		message2 = "";
		message3 = "";
		dims = {3, 3};
		coefficentMatrix = ConstantArray[Null, dims];
		constantVector =  ConstantArray[Null, {3, 1}];
		Style[
		Pane[
			Column[{
				Row[{
					TextCell["Guess the function", "Title"],
					Spacer[20],
					TextCell[StringForm["Seed: ``", Dynamic@seed], FontSize->10]
				}],
				
				TextCell["In questo esercizio dovrai trovare la funzione della parabola che passa per i seguenti tre punti:"],
				Spacer[1],
				TextCell[TraditionalForm@myGeneratePointDisplay[points], "Abstract"],
				Spacer[1],
				TextCell["Nella seguente sezione \[EGrave] possibile inserire i 3 coefficenti dell'equazione di secondo grado."],
				TextCell["Cliccando sul bottone \[EGrave] possibile visualizzare la parabola nell'asse cartesiano:"],
				Spacer[10],
				EventHandler[
					Row[
						{
							DisplayForm["y ="],
							Spacer[10],
							InputField[Dynamic[a], Number, FieldHint->"a", FieldSize->3, Alignment->Center],
							DisplayForm[ToExpression["x^2"]],
							DisplayForm[" + "],
							InputField[Dynamic[b], Number, FieldHint->"b", FieldSize->3, Alignment->Center],
							DisplayForm["x  +  "],
							InputField[Dynamic[c], Number, FieldHint->"c", FieldSize->3, Alignment->Center],
							Spacer[30],
							Button[TextCell[" Inserisci funzione nel grafico ", FontSize->16], 
								{
									If[
										Head[a] === Integer && Head[b] === Integer && Head[c] === Integer,
										{
											fun = a*x^2 + b*x + c,
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
				Spacer[20],
				Dynamic@DisplayForm@If[myCounterErrori >= 3, Column[{
					TextCell["Completa e risolvi la matrice di Vandermonde per trovare i coefficienti:", "Subsubsection"],
					EventHandler[
						Row[{
							MatrixForm[
							    Table[With[{i = i, j = j},
							      InputField[Dynamic@coefficentMatrix[[i, j]], Number, FieldSize->2, Alignment->Center, FieldHint->ToString[StringForm["\*SubsuperscriptBox[x, ``, ``]", i, j-1]]]],
							     {i, 3}, {j, 3}]
							    ],
							 Style[DisplayForm[" \[Times] "], FontSize->16],
							 Style[MatrixForm[{"c", "b", "a"}], FontSize->16],
							 Style[DisplayForm[" = "], FontSize->16],
							 MatrixForm[
							    Table[With[{i = i, j = j},
							      InputField[Dynamic@constantVector[[i, j]], Number, FieldSize->2, Alignment->Center, FieldHint->ToString[StringForm["\*SubscriptBox[y, ``]", i]]]],
							     {i, 3}, {j, 1}]
							    ],
							Spacer[20],
						    Button["Controlla", {message2 = myCheckMatrix[coefficentMatrix, constantVector, points]}]
					    }], {{"KeyDown", "."} :> Null},
							PassEventsDown -> False
					],
				    Spacer[10],
					If[StringContainsQ[message2, "Congratulazioni"],
						TextForm@Dynamic@Style[message2, FontColor->RGBColor[0, 0.741, 0]],
						TextForm@Dynamic@Style[message2, FontColor->Red]]
				    (*Dynamic@MatrixForm[coefficentMatrix],
				    Dynamic@MatrixForm[constantVector]*)
				    }, Alignment->Center],
					""
				]
		   }, Alignment->Center
		   ],
		   Alignment->Center,
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
	
	If[Head[seed] === Integer, 
			{expr, realM, realQ} = Backend`myGenerateEquation[1, seed],
			(*Genera pseudorandomicamente un numero intero da 0 a 9999*)
			{seed = RandomInteger[9999], {expr, realM, realQ} = Backend`myGenerateEquation[1, seed]}
	];
	
	points = Backend`myGeneratePointsOnLineOrParabola[2];
	message = "";
	message2 = "";
	message3= "";
	dims = {2, 2};
	coefficentMatrix = ConstantArray[Null, dims];
	constantVector =  ConstantArray[Null, {2, 1}];
	
	Style[
		Pane[
			Column[{
				Row[{
					TextCell["Guess the function", "Title"],
					Spacer[20],
					TextCell[StringForm["Seed: ``", Dynamic@seed], FontSize->10]
				}],
				TextCell["In questo esercizio dovrai trovare la funzione della retta che passa per i seguenti due punti:", TextAlignment->Left],
				Spacer[1],
				TextCell[TraditionalForm@myGeneratePointDisplay[points], "Abstract"],
				Spacer[1],
				TextCell["Nella seguente sezione \[EGrave] possibile inserire i 2 coefficenti dell'equazione di primo grado."],
				TextCell["Cliccando sul bottone \[EGrave] possibile visualizzare la parabola nell'asse cartesiano:"],
				Spacer[10],
				EventHandler[
					Row[
						{
							DisplayForm["y ="],
							Spacer[10],
							InputField[Dynamic[m], Number, FieldHint->"m", FieldSize->3, Alignment->Center],
							DisplayForm["x  +  "],
							InputField[Dynamic[q], Number, FieldHint->"q", FieldSize->3, Alignment->Center],
							Spacer[30],
							Button[TextCell[" Inserisci funzione nel grafico ", FontSize->16], 
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
				Spacer[20],
				Dynamic@DisplayForm@If[myCounterErrori >= 3, Column[{
					TextCell["Completa e risolvi la matrice di Vandermonde per trovare i coefficienti:", "Subsubsection"],
					
					EventHandler[
						Row[{
							MatrixForm[
							    Table[With[{i = i, j = j},
							      InputField[Dynamic@coefficentMatrix[[i, j]], Number, FieldSize->2, Alignment->Center, FieldHint->ToString[StringForm["\*SubsuperscriptBox[x, ``, ``]", i, j-1]]]],
							     {i, 2}, {j, 2}]
							    ],
							 Style[DisplayForm[" \[Times] "], FontSize->16],
							 Style[MatrixForm[{"q", "m"}], FontSize->16],
							 Style[DisplayForm[" = "], FontSize->16],
							 MatrixForm[
							    Table[With[{i = i, j = j},
							      InputField[Dynamic@constantVector[[i, j]], Number, FieldSize->2, Alignment->Center, FieldHint->ToString[StringForm["\*SubscriptBox[y, ``]", i]]]],
							     {i, 2}, {j, 1}]
							    ],
							Spacer[20],
						    Button["Controlla", {message2 = myCheckMatrix[coefficentMatrix, constantVector, points]}]
					    }], {{"KeyDown", "."} :> Null},
							PassEventsDown -> False
					],
					Spacer[10],
					If[StringContainsQ[message2, "Congratulazioni"],
						TextForm@Dynamic@Style[message2, FontColor->RGBColor[0, 0.741, 0]],
						TextForm@Dynamic@Style[message2, FontColor->Red]]
				    (*Dynamic@MatrixForm[coefficentMatrix],
				    Dynamic@MatrixForm[constantVector]*)
				    }, Alignment->Center],
					""
				]
		   }, Alignment->Center
		   ],
		   Alignment->Center,
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
						Button["Retta", {If[Head[seed] === Integer || Head[seed] === Null,
										{
											GuessTheFunctionGUI[1];
											DialogReturn[];
											seedMessage = "";
										},
										seedMessage = "\:26a0\:fe0f Attenzione \:26a0\:fe0f\nIl seed pu\[OGrave] essere o un intero\n o al pi\[UGrave] lasciato vuoto."
									]}],
						Spacer[20],
						Button["Parabola", {If[Head[seed] === Integer || Head[seed] === Null,
										{
											GuessTheFunctionGUI[2];
											DialogReturn[];
											seedMessage = "";
										},
										seedMessage = "\:26a0\:fe0f Attenzione \:26a0\:fe0f\nIl seed pu\[OGrave] essere o un intero\n o al pi\[UGrave] lasciato vuoto."
									]}]
						}], 
					Spacer[10],
					TextForm@Dynamic@Style[seedMessage, FontColor->RGBColor[0.9, 0.3, 0], TextAlignment->Center, Bold]
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
