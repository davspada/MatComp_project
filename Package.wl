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
	(* Print[coefficentMatrix];*);
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

GuessTheFunctionGUI[2] := DynamicModule[{
		a, b, c, 
		x, 
		message, message2, 
		expr, 
		coefficentMatrix,
		constantVector
		},
	myCounterErrori = 0;
	{expr, realA, realB, realC} = Backend`myGenerateEquation[2, seed];
	points = Backend`myGeneratePointsOnLineOrParabola[3];
	message := "";
	message2 := "";
	
	dims = {3, 3};
	
	fieldH = Array ["x" <> ToString@# &, dims[[1]]];

	CreateDialog[
		DialogNotebook[
			Pane[
				Column[{
					TextCell["Inserire i coefficenti dell'equazione di secondo grado nelle celle:", "Subsection"],
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
								Button["Inserisci funzione nel grafico", {
									fun = a*x^2 + b*x + c;
									message = CheckInput[realA, realB, realC, a,  b,  c];
								}]
							}
						], {{"KeyDown", "."} :> Null},
							PassEventsDown -> False
					],
					TextCell["Grafico dell'equazione inserita in input:", "Subsection"],
					Framed@Dynamic@Show[
						callouts = Callout[#, "(" <> ToString[#[[1]]] <> ", " <> ToString[#[[2]]] <> ")", Background->LightBlue, Frame->True, RoundingRadius->5,FrameMargins->5] &/@ points;
						ListPlot[callouts, ImageSize->Large, ImageMargins->20, PlotRange->{{-10,10},Automatic}],
						Plot[fun,{x,-10,10}]
					],
					DisplayForm[Dynamic@message],
					Dynamic@DisplayForm["Numero errori: " <> ToString[myCounterErrori]],
					Dynamic@DisplayForm@If[myCounterErrori >= 3, Column[{
						coefficentMatrix = ConstantArray[Null, dims];
						constantVector =  ConstantArray[Null, {3, 1}];
						Row[{
							Framed@Grid[
							    Table[With[{i = i, j = j},
							      InputField[Dynamic@coefficentMatrix[[i, j]], Number, FieldSize->2]],
							     {i, 3}, {j, 3}]
							    ],
							 DisplayForm[" * "],
							 MatrixForm[{"c","b","a"}],
							 DisplayForm[" = "],
							 Framed@Grid[
							    Table[With[{i = i, j = j},
							      InputField[Dynamic@constantVector[[i, j]], Number, FieldSize->2]],
							     {i, 3}, {j, 1}]
							    ],
						    Button["Controlla", {message2 = myCheckMatrix[coefficentMatrix, constantVector, points];}]
					    }],
					    DisplayForm[Dynamic@message2],
					    Dynamic@MatrixForm[coefficentMatrix],
					    Dynamic@MatrixForm[constantVector]
					    }],
						""
					]
			   }, Alignment->Center
			   ],
			   Alignment->{Center},
		       ImageSizeAction->"Scrollable",
		       ImageSize->Full,
		       ImageMargins->20
		      ] 
	      ],
	      WindowTitle -> "Plot area",
	      WindowElements->{"VerticalScrollBar", "HorizontalScrollBar", "StatusArea"}
	  ]
];

GuessTheFunctionGUI[1] := DynamicModule[{
		m, q,
		x, 
		message, message2, 
		expr, 
		coefficentMatrix,
		constantVector
		},
	myCounterErrori = 0;
	{expr, realM, realQ} = Backend`myGenerateEquation[1, seed];
	points = Backend`myGeneratePointsOnLineOrParabola[2];
	message := "";
	message2 := "";
	
	dims = {2, 2};
	
	fieldH = Array ["x" <> ToString@# &, dims[[1]]];

	CreateDialog[
		DialogNotebook[
			Pane[
				Column[{
					TextCell["Inserire i coefficenti dell'equazione di secondo grado nelle celle:", "Subsection"],
					EventHandler[
						Row[
							{
								InputField[Dynamic[m], Number, FieldHint->"m", FieldSize->2],
								DisplayForm[ToExpression["x"]],
								DisplayForm[" + "],
								InputField[Dynamic[q], Number, FieldHint->"q", FieldSize->2],
								Spacer[30],
								Button["Inserisci funzione nel grafico", {
									fun = m*x + q;
									message = CheckInput[0, realM, realQ, 0,  m,  q];
								}]
							}
						], {{"KeyDown", "."} :> Null},
							PassEventsDown -> False
					],
					TextCell["Grafico dell'equazione inserita in input:", "Subsection"],
					Framed@Dynamic@Show[
						callouts = Callout[#, "(" <> ToString[#[[1]]] <> ", " <> ToString[#[[2]]] <> ")", Background->LightBlue, Frame->True, RoundingRadius->5,FrameMargins->5] &/@ points;
						ListPlot[callouts, ImageSize->Large, ImageMargins->20, PlotRange->{{-10,10},Automatic}],
						Plot[fun,{x,-10,10}]
					],
					DisplayForm[Dynamic@message],
					Dynamic@DisplayForm["Numero errori: " <> ToString[myCounterErrori]],
					Dynamic@DisplayForm@If[myCounterErrori >= 3, Column[{
						coefficentMatrix = ConstantArray[Null, dims];
						constantVector =  ConstantArray[Null, {2, 1}];
						Row[{
							Framed@Grid[
							    Table[With[{i = i, j = j},
							      InputField[Dynamic@coefficentMatrix[[i, j]], Number, FieldSize->2]],
							     {i, 2}, {j, 2}]
							    ],
							 DisplayForm[" * "],
							 MatrixForm[{"q","m"}],
							 DisplayForm[" = "],
							 Framed@Grid[
							    Table[With[{i = i, j = j},
							      InputField[Dynamic@constantVector[[i, j]], Number, FieldSize->2]],
							     {i, 2}, {j, 1}]
							    ],
						    Button["Controlla", {message2 = myCheckMatrix[coefficentMatrix, constantVector, points];}]
					    }],
					    DisplayForm[Dynamic@message2],
					    Dynamic@MatrixForm[coefficentMatrix],
					    Dynamic@MatrixForm[constantVector]
					    }],
						""
					]
			   }, Alignment->Center
			   ],
			   Alignment->{Center},
		       ImageSizeAction->"Scrollable",
		       ImageSize->Full,
		       ImageMargins->20
		      ] 
	      ],
	      WindowTitle -> "Plot area",
	      WindowElements->{"VerticalScrollBar", "HorizontalScrollBar", "StatusArea"}
	  ]
];


(* ::Text:: *)
(*Codice per creare un'interfaccia grafica che permetta di scegliere la modalit\[AGrave] di gioco*)


CreateDynamicWindow[] :=
  DynamicModule[{
	buttonText = "", 
    infoWindow
    },
  
    infoTitle = "How to play";
    infoText = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.";
    CreateDialog[
		Column[{
			EventHandler[
				Tooltip[Style["\:2139", FontSize -> 16], "Click for info"], {"MouseClicked" :> CreateInfoWindow[infoTitle, infoText]}], 
			Row[{
				TextCell["Inserire Seed"],
				EventHandler[
					InputField[Dynamic[seed], Number, ContinuousAction->True],
					 {{"KeyDown", "."} :> Null},
					PassEventsDown -> False]
					}],
			Row[{
				Button["Retta", {Dynamic@Backend`setSeed[seed]; GuessTheFunctionGUI[1]}], 
				Button["Parabola", {Dynamic@Backend`setSeed[seed]; GuessTheFunctionGUI[2]}]
				}, Frame -> True], 
			Dynamic@TextCell[buttonText, "Text", FontSize ->12]}, Center, Frame -> All], 
       WindowSize-> {400, 400}, 
       WindowTitle-> "Dynamic Window"];
    displayText[] := buttonText;
  ]


(* ::Text:: *)
(*Codice che gestisce l'apertura di una finestra di informazione per l'utente.*)


CreateInfoWindow[infoTitle_, infoText_] :=
  CreateDialog[Column[{TextCell[infoText, "Text", FontSize -> 12], Button[
    "Close", NotebookDelete[SelectedNotebook[]]]}], WindowSize -> {400, 400
    }, WindowTitle -> infoTitle]


(* ::Text:: *)
(*Dichiarazione di fine del package*)


End[];

EndPackage[];
