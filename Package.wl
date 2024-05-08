(* ::Package:: *)


<<<<<<< Updated upstream
=======

(* ::Text:: *)
(*Dichiarazione di inizio del package*)


BeginPackage["Package`"];

GuessTheFunctionGUI::usage = "GuessTheFunctionGUI[] permette di creare l'interfaccia grafica con cui l'utente interagisce per poter avviare il gioco";
CreateDynamicWindow::usage = "CreateDynamicWindow[]";
CreateInfoWindow::usage = "CreateInfoWindow[]"

Get["Backend.wl"];

Begin["`Private`"];
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
GuessTheFunctionGUI[] := DynamicModule[{a=0, b=0, c=0, x},
	CreateDialog[
		Column[{
			Row[
				{
					InputField[Dynamic[a], Number, FieldHint->"", FieldSize->2],
					DisplayForm[ToExpression["x^2"]],
					DisplayForm[" + "],
					InputField[Dynamic[b], Number, FieldHint->"", FieldSize->2],
					DisplayForm[ToExpression["x"]],
					DisplayForm[" + "],
					InputField[Dynamic[c], Number, FieldHint->"", FieldSize->2],
					Button["Plot", {fun = a*x^2 + b*x + c}]
				}
			],
			Dynamic@Plot[fun,{x,-10,10}, AspectRatio->1, PlotRange->{-10,10}]
	      }],
	      WindowSize -> {400, 400},
	      WindowTitle -> "PORCODIO"
	  ]
];





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
>>>>>>> Stashed changes
