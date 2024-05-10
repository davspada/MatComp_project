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

SetDirectory[NotebookDirectory[]];


(* ::Text:: *)
(*Codice per creare un interfaccia grafica esterna*)


(*
GuessTheFunctionGUI[] := DynamicModule[{x},
  Column[{ InputField[Dynamic[a], Number],
    TextCell["x^2"],
    InputField[Dynamic[b], Integer],
    TextCell["x"],
    InputField[Dynamic[c], Integer],
    funzione[x] = a*x^2 + b*x + c
    Dynamic@Plot[funzione, {x, -10, 10}, PlotStyle -> Blue, 
      AxesLabel -> {"x", "y"}, PlotLabel -> "Plot della funzione", ImageSize -> Medium],
    Dynamic@DisplayForm[ToExpression[func]]}
    ]
  ];
 *)
 (*
GuessTheFunctionGUI[] := DynamicModule[{f="x^2", fun={2x}, x},
	Column[{
		Row[
			{
				InputField[Dynamic[f], String, FieldHint->"sas"], 
				Button["Plot", {fun = Dynamic@ToExpression[f],
								Dynamic@Plot[fun,{x,-5,5}],
								Print[Dynamic[fun]]
								}]
			}
		],
		Dynamic@Plot[fun,{x,-5,5}]
      }]
];
*)
CheckInput[realA_, realB_, realC_, a_, b_, c_] := Module[{message = ""},
	condition = MatchQ[{realA, realB, realC}, {IntegerPart@a, IntegerPart@b, IntegerPart@c}];
	If[condition, 
		{message="Bravo";
		myCounterErrori=0;}, 
		{message = "Hai sbagliato";
		myCounterErrori++;}];
	Return[message];
]

myCheckMatrix[matrix_, points_] := Module[{message, coefficentMatrix, rhsVector},
	message = "";
	{coefficentMatrix, rhsVector} = Backend`myGenerateVandermondeMatrix[points];
	Print[coefficentMatrix];
	If[coefficentMatrix === matrix,
		message = "Matrice corretta",
		message = "Matrice sbagliata"
	];
	Return[message]
]

GuessTheFunctionGUI[] := DynamicModule[{a, b, c, x, message, message2, expr, fieldH},
	{expr, realA, realB, realC} = Backend`myGenerateEquation[2];
	Print[expr];
	points = Backend`myGeneratePointsOnLineOrParabola[3];
	message := "";
	message2 := "";
	dims = {3, 3};
	mat = ConstantArray[Null, dims];
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
						], {"KeyDown", "."} :> Null,
							PassEventsDown -> False
					],
					TextCell["Grafico dell'equazione inserita in input:", "Subsection"],
					Framed@Dynamic@Show[
						ListPlot[points, ImageSize->Large],
						Plot[fun,{x,-10,10}]
					],
					Dynamic@DisplayForm[message],
					Dynamic@DisplayForm["Numero errori: " <> ToString[myCounterErrori]],
					Dynamic@DisplayForm@If[myCounterErrori >= 3, Column[{
						Row[{
							Framed@Grid@Array[InputField[Dynamic@mat[[##]], 
						                   FieldSize -> 2,
						                   FieldHint -> fieldH[[#2]]] &, 
						                   dims],
						    Button["Controlla", {message2 = myCheckMatrix[mat, points]}]
					    }],
					    Dynamic@DisplayForm[message2],
					    Dynamic@MatrixForm[mat]
					    }],
						{}
					]
			   }, Alignment->Center
			   ],
			   Alignment->{Center},
		       ImageSizeAction->"Scrollable",
		       ImageSize->Full
		      ] 
	      ],
	      WindowSize -> {Scaled[0.9],Scaled[0.9]},
	      WindowTitle -> "Plot area",
	      WindowElements->{"VerticalScrollBar", "HorizontalScrollBar", "StatusArea"}
	  ]
];


(* ::Text:: *)
(*Codice per creare un'interfaccia grafica che permetta di scegliere la modalit\[AGrave] di gioco*)


CreateDynamicWindow[] :=
  DynamicModule[
    {buttonText = "", infoWindow}
    ,
    infoTitle = "How to play";
    infoText = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat."
      ;
    CreateDialog[Column[{EventHandler[Tooltip[Style["\:2139", FontSize -> 
      16], "Click for info"], {"MouseClicked" :> CreateInfoWindow[infoTitle,
       infoText]}], Row[{Button["Easy", buttonText = "Easy"], Button["Medium",
       buttonText = "Medium"], Button["Difficult", buttonText = "Difficult"
      ]}, Frame -> True], Dynamic @ TextCell[buttonText, "Text", FontSize ->
       12]}, Center, Frame -> All], WindowSize -> {400, 400}, WindowTitle ->
       "Dynamic Window"];
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
