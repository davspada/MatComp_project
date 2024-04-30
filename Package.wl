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

GuessTheFunctionGUI::usage = "GuessTheFunctionGUI[] permette di creare l'interfaccia grafica con cui l'utente interagisce per poter avviare il gioco";
CreateDynamicWindow::usage = "CreateDynamicWindow[]";
CreateInfoWindow::usage = "CreateInfoWindow[]"

Begin["`Private`"];
SetDirectory[NotebookDirectory[]];


(* ::Text:: *)
(*Codice per creare un interfaccia grafica esterna*)


GuessTheFunctionGUI[] := DynamicModule[{f=x^2,g=Sin[x],a,b},
	DialogInput[
		DialogNotebook[
			Column[{
				"Enter a function to plot:",
				InputField[Dynamic[f],String,FieldSize->20],
				"Enter a function for the points:",
				InputField[Dynamic[g],String,FieldSize->20],
				"Enter the range for x:",
				InputField[Dynamic[a],Number,FieldSize->5],
				InputField[Dynamic[b],Number,FieldSize->5],
				Dynamic[
					Show[
						Plot[ToExpression[f],{x,a,b}],
						Plot[ToExpression[g],{x,a,b},PlotStyle->Red],
						ImageSize->Medium
					]
				]
			}],
			WindowTitle -> "Interactive Plot", WindowSize -> {500, 500}, 
			WindowMargins -> {{Automatic, 0}, {0, Automatic}}, 
			WindowElements -> {"VerticalScrollBar", "HorizontalScrollBar", "StatusArea"}
		]
	]
]


(* ::Text:: *)
(*Codice per creare un'interfaccia grafica che permetta di scegliere la modalit\[AGrave] di gioco*)


CreateDynamicWindow[] :=
  DynamicModule[{buttonText = "", infoWindow},
    infoTitle = "How to play";
    infoText = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.";
    CreateDialog[
      Column[{
        EventHandler[
          Tooltip[Style["\:2139", FontSize -> 16], "Click for info"],
          {"MouseClicked" :> CreateInfoWindow[infoTitle,infoText]}
        ],
        Row[{
          Button["Easy", buttonText = "Easy"],
          Button["Medium", buttonText = "Medium"],
          Button["Difficult", buttonText = "Difficult"]
        },Frame->True],
        Dynamic@TextCell[buttonText, "Text", FontSize->12]
      },
      Center, Frame->All],
      WindowSize -> {400, 400},
      WindowTitle -> "Dynamic Window"
    ];
    displayText[] := buttonText;
  ]


(* ::Text:: *)
(*Codice che gestisce l'apertura di una finestra di informazione per l'utente.*)


CreateInfoWindow[infoTitle_, infoText_] :=
  CreateDialog[
    Column[{
      TextCell[infoText, "Text", FontSize->12],
      Button["Close", NotebookDelete[SelectedNotebook[]]]
    }],
    WindowSize -> {400, 400},
    WindowTitle -> infoTitle
  ]


(* ::Text:: *)
(*Dichiarazione di fine del package*)


End[];
EndPackage[];
