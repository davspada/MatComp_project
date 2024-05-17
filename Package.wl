(* ::Package:: *)

(* :Title: Guess the function *)
(* :Context: Gioco matematico che ... *)
(* :Author: Leonardi Dess\[IGrave], Emanuele Grasso, Luca Polese, Davide Spada *)
(* :Summary: Package per giocare con Mathematica *)
(* :Copyright: Alma Mater Studiorum - Universit\[AGrave] di Bologna 2023 *)
(* :Package Version: 0.0.1 *)
(* :Mathematica Version: 14 *)
(* :History: \\ *)
(* :Keywords: funzioni, punti, parabole *)
(* :Sources: \\ *)
(* :Discussion: \\ *)

BeginPackage["Package`"];

(* Imposta la directory di lavoro al percorso del notebook corrente *)
SetDirectory[NotebookDirectory[]];

(* Carica un file di supporto chiamato "Backend.wl" *)
Get["Backend.wl"];

(* Definisce l'uso delle funzioni definite nel pacchetto *)
GuessTheFunctionGUI::usage = "GuessTheFunctionGUI[] Genera l'interfaccia per l'esercizio";
CreateDynamicWindow::usage = "CreateDynamicWindow[] permette di creare l'interfaccia grafica con cui l'utente interagisce per poter avviare il programma";
CreateInfoWindow::usage = "CreateInfoWindow[] permette di generare la finestra di info per settare il seed per la generazione randomica dei valori";

Begin["`Private`"];

(* Inizializza le variabili *)
myCounterErrori = 0;
seed = 0;

(* Imposta la directory di lavoro al percorso del notebook corrente *)
SetDirectory[NotebookDirectory[]];

(* Dichiarazione di una funzione per controllare l'input *)
CheckInput[realA_, realB_, realC_, a_, b_, c_] := Module[{message = "", condition},
	(*Controlla che i punti generati dal kernel siano uguali a quelli dati in input dall'utente*)
    condition = {realA, realB, realC} === {IntegerPart@a, IntegerPart@b, IntegerPart@c};
    If[condition, 
        {message="Bravo"; myCounterErrori=0;}, 
        {message = "Hai sbagliato"; myCounterErrori++;}];
    Return[message];
]

(* Dichiarazione di una funzione per controllare una matrice *)
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

(* Dichiarazione di una funzione per generare una visualizzazione dei punti nella schermata principale*)
myGeneratePointDisplay[points_] := Module[{},
    Return[
        Column[
            Table[
                StringForm["\!\(\*SubscriptBox[\(p\), \(``\)]\) = (``, ``)", i, points[[i, 1]], points[[i, 2]]],
                {i, Length[points]}]
            ]
        ]
]



(* ::Text:: *)
(*Codice per creare un'interfaccia grafica che permette di eseguire l'esercizio per trovare la parabola passante per 3 punti*)


GuessTheFunctionGUI[2] := CreateDialog[(* Definisce una finestra di dialogo per l'interfaccia grafica del gioco *)
    DynamicModule[{
        a, b, c, (* Variabili per i coefficienti dell'equazione *)
        realA, realB, realC, (* Coefficienti reali dell'equazione *)
        x, (* Variabile indipendente *)
        message, message2, message3, (* Messaggi di feedback *)
        expr, (* Espressione dell'equazione *)
        coefficentMatrix, (* Matrice dei coefficienti per il sistema lineare *)
        constantVector (* Vettore dei termini noti per il sistema lineare *)
    },
        myCounterErrori = 0; (* Inizializza il contatore degli errori *)

        (* Genera l'equazione e i coefficienti reali *)
        If[Head[seed] === Integer, 
            {expr, realA, realB, realC} = Backend`myGenerateEquation[2, seed],
            (* Genera pseudorandomicamente un numero intero da 0 a 9999 *)
            {seed = RandomInteger[9999], {expr, realA, realB, realC} = Backend`myGenerateEquation[2, seed]}
        ];
            
			
      points = Backend`myGeneratePointsOnLineOrParabola[3]; (* Genera tre punti casuali *)
        message = ""; (* Inizializza il messaggio di feedback *)
        message2 = ""; (* Inizializza un altro messaggio di feedback *)
        message3 = ""; (* Inizializza un altro messaggio di feedback *)
        dims = {3, 3}; (* Dimensioni della matrice dei coefficienti *)
        coefficentMatrix = ConstantArray[Null, dims]; (* Inizializza la matrice dei coefficienti *)
        constantVector =  ConstantArray[Null, {3, 1}]; (* Inizializza il vettore dei termini noti *)

        (* Crea la grafica dell'interfaccia utente *)
        Style[Pane[
            Column[{
                (* Intestazione del gioco *)
                Row[{
                    TextCell["Guess the function", "Title"],
                    Spacer[20],
                    TextCell[StringForm["Seed: ``", Dynamic@seed], FontSize->10] (* Visualizza il seed attuale *)
                }],
                (* Descrizione del gioco e dei punti *)
                TextCell["In questo esercizio dovrai trovare la funzione della parabola che passa per i seguenti tre punti:"],
                Spacer[1],
                TextCell[TraditionalForm@myGeneratePointDisplay[points], "Abstract"],
                Spacer[1],
                (* Inserimento dei coefficienti dell'equazione *)
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
                                            fun = a*x^2 + b*x + c, (* Definisce la funzione inserita dall'utente con i coefficienti*)
                                            message = CheckInput[realA, realB, realC, a,  b,  c]; (* Controlla se la funzione è corretta *)
                                            message3 = ""; (* Azzera eventuali messaggi precedenti *)
                                        },
                                        message3 = "Attenzione! Campi vuoti o numeri non interi inseriti!"; (* Visualizza un messaggio di errore *)
                                    ]
                                }
                            ]
                        }
                    ], {{"KeyDown", "."} :> Null}, PassEventsDown -> False
                ],
                TextForm@Dynamic@Style[message3, FontColor -> Red], (* Visualizza eventuali messaggi di errore *)
                (* Grafico dell'equazione inserita *)
                TextCell["Grafico dell'equazione inserita in input:", "Subsection"],
                Framed@Dynamic@Show[
                    callouts = Callout[#, "(" <> ToString[#[[1]]] <> ", " <> ToString[#[[2]]] <> ")", Background->LightBlue, Frame->True, RoundingRadius->5,FrameMargins->5] &/@ points;
                    ListPlot[callouts, ImageSize->Medium, ImageMargins->20, PlotRange->{{-10,10},Automatic}],
                    Plot[fun,{x,-10,10}]
                ],
                TextForm[Dynamic@message], (* Visualizza messaggi di feedback *)
                Dynamic@DisplayForm["Numero errori: " <> ToString[myCounterErrori]], (* Visualizza il numero di errori *)
                Spacer[20],
                Dynamic@DisplayForm@If[myCounterErrori >= 3, Column[{(* Visualizza la matrice dei coefficienti e il vettore dei termini noti *)
                    TextCell["Completa e risolvi la matrice di Vandermonde per trovare i coefficienti:", "Subsubsection"],
					(*l'event handler impedisce all'utente di inserire un punto nell'input field*)
                    EventHandler[
                        Row[{
							(*display e formattazione della matrice delle x e degli input field relativi ad essa*)
                            MatrixForm[
								(*crea la tabella i,j*)
                                Table[With[{i = i, j = j},
								(*input field relativi alla matrice i,j*)
                                  InputField[Dynamic@coefficentMatrix[[i, j]], Number, FieldSize->2, Alignment->Center, FieldHint->ToString[StringForm["\*SubsuperscriptBox[x, ``, ``]", i, j-1]]]],
                                 {i, 3}, {j, 3}]
                                ],
                             Style[DisplayForm[" \[Times] "], FontSize->16],
                             Style[MatrixForm[{"c", "b", "a"}], FontSize->16],
                             Style[DisplayForm[" = "], FontSize->16],
                             (*vettore dei termini noti*)
							 MatrixForm[
                                Table[With[{i = i, j = j},
                                  InputField[Dynamic@constantVector[[i, j]], Number, FieldSize->2, Alignment->Center, FieldHint->ToString[StringForm["\*SubscriptBox[y, ``]", i]]]],
                                 {i, 3}, {j, 1}]
                                ],
                            Spacer[20],
							(*premendo il bottone chiama la funzione per il controllo dei coefficienti della matrice*)
                            Button["Controlla", {message2 = myCheckMatrix[coefficentMatrix, constantVector, points]}]
							(*event handler che non permette di inserire "."*)
					    }], {{"KeyDown", "."} :> Null},
							PassEventsDown -> False
					],
				    Spacer[10],
					If[StringContainsQ[message2, "Congratulazioni"],
						TextForm@Dynamic@Style[message2, FontColor->RGBColor[0, 0.741, 0]],
						TextForm@Dynamic@Style[message2, FontColor->Red]]
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


GuessTheFunctionGUI[1] := CreateDialog[(* Definisce una finestra di dialogo per l'interfaccia grafica del gioco *)
    DynamicModule[{
        m, q, (* Variabili per i coefficienti dell'equazione *)
        realM, realQ, (* Coefficienti reali dell'equazione *)
        x, (* Variabile indipendente *)
        message, message2, message3, (* Messaggi di feedback *)
        expr, (* Espressione dell'equazione *)
        coefficentMatrix, (* Matrice dei coefficienti per il sistema lineare *)
        constantVector (* Vettore dei termini noti per il sistema lineare *)
    },
        myCounterErrori = 0; (* Inizializza il contatore degli errori *)

        (* Genera l'equazione e i coefficienti reali *)
        If[Head[seed] === Integer, 
            {expr, realM, realQ} = Backend`myGenerateEquation[1, seed],
            (* Genera pseudorandomicamente un numero intero da 0 a 9999 *)
            {seed = RandomInteger[9999], {expr, realM, realQ} = Backend`myGenerateEquation[1, seed]}
        ];
        
        points = Backend`myGeneratePointsOnLineOrParabola[2]; (* Genera due punti casuali *)
        message = ""; (* Inizializza il messaggio di feedback *)
        message2 = ""; (* Inizializza un altro messaggio di feedback *)
        message3= ""; (* Inizializza un altro messaggio di feedback *)
        dims = {2, 2}; (* Dimensioni della matrice dei coefficienti *)
        coefficentMatrix = ConstantArray[Null, dims]; (* Inizializza la matrice dei coefficienti *)
        constantVector =  ConstantArray[Null, {2, 1}]; (* Inizializza il vettore dei termini noti *)

        (* Crea la grafica dell'interfaccia utente *)
        Style[Pane[
            Column[{
                (* Intestazione del gioco *)
                Row[{
                    TextCell["Guess the function", "Title"],
                    Spacer[20],
                    TextCell[StringForm["Seed: ``", Dynamic@seed], FontSize->10] (* Visualizza il seed attuale *)
                }],
                (* Descrizione del gioco e dei punti *)
                TextCell["In questo esercizio dovrai trovare la funzione della retta che passa per i seguenti due punti:", TextAlignment->Left],
                Spacer[1],
                TextCell[TraditionalForm@myGeneratePointDisplay[points], "Abstract"],
                Spacer[1],
                (* Inserimento dei coefficienti dell'equazione *)
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
                                        message3 = "Attenzione! Campi vuoti o numeri non interi inseriti!", (* Visualizza un messaggio di errore *)
                                        {
                                            fun = m*x + q; (* Definisce la funzione inserita dall'utente *)
                                            message = CheckInput[0, realM, realQ, 0,  m,  q]; (* Controlla se la funzione è corretta *)
                                            message3 = ""; (* Azzera eventuali messaggi precedenti *)
                                        }
                                    ]
                                }
                            ]
                        }
                    ], {{"KeyDown", "."} :> Null}, PassEventsDown -> False
                ],
                TextForm@Dynamic@Style[message3, FontColor -> Red], (* Visualizza eventuali messaggi di errore *)
                (* Grafico dell'equazione inserita *)
                TextCell["Grafico dell'equazione inserita in input:", "Subsection"],
                Framed@Dynamic@Show[
                    callouts = Callout[#, "(" <> ToString[#[[1]]] <> ", " <> ToString[#[[2]]] <> ")", Background->LightBlue, Frame->True, RoundingRadius->5,FrameMargins->5] &/@ points;
                    ListPlot[callouts, ImageSize->Medium, ImageMargins->20, PlotRange->{{-10,10},Automatic}],
                    Plot[fun,{x,-10,10}]
                ],
                Dynamic@TextForm[message], (* Visualizza messaggi di feedback *)
                Dynamic@DisplayForm["Numero errori: " <> ToString[myCounterErrori]], (* Visualizza il numero di errori *)
                Spacer[20],
                Dynamic@DisplayForm@If[myCounterErrori >= 3, Column[{(* Visualizza la matrice dei coefficienti e il vettore dei termini noti *)
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
                            Button["Controlla", {message2 = myCheckMatrix[coefficentMatrix, constantVector, points]}] (* Controlla la matrice dei coefficienti *)
                        }], {{"KeyDown", "."} :> Null}, PassEventsDown -> False
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
(* Codice per creare un'interfaccia grafica che permetta di scegliere la modalità di gioco *)

CreateDynamicWindow[] :=
  DynamicModule[{
    infoWindow
    },
  
    seedMessage = ""; (* Messaggio di feedback sul seed *)
    infoText = "Un seed è un numero di partenza utilizzato dagli algoritmi che generano numeri casuali. Impostare un seed garantisce che l'algoritmo generi la stessa sequenza di esercizi ogni volta che viene eseguito con lo stesso seed. Questo è essenziale per la riproducibilità e la coerenza dei risultati degli esercizi.";
    
    CreateDialog[ (* Crea una finestra di dialogo *)
		DialogNotebook[
			Pane[ (* Utilizza un riquadro per contenere il layout *)
				Column[{ (* Utilizza una colonna per organizzare gli elementi *)
					Row[{ (* Utilizza una riga per organizzare gli elementi orizzontalmente *)
						EventHandler[ 
							Tooltip[Style["ℹ", FontSize -> 16], "Perché inserire un seed?"], {"MouseClicked" :> CreateInfoWindow[infoText]}], (* Icona per informazioni aggiuntive sul seed *)
						Spacer[1],
						TextCell["Inserire Seed:"], (* Etichetta per il campo di inserimento del seed *)
						Spacer[5],
						EventHandler[ (* Gestisce eventi come il click del mouse *)
							InputField[Dynamic[seed], Number, ContinuousAction->True, FieldSize->Tiny], (* Campo di inserimento del seed *)
							 {{"KeyDown", "."} :> Null}, (* Ignora il punto per evitare errori di input *)
							PassEventsDown -> False] (* Passa gli eventi ai livelli più bassi *)
							}],
					Spacer[20],
					Row[{ (* Altra riga per organizzare gli elementi orizzontalmente *)
						Button["Retta", {If[Head[seed] === Integer || Head[seed] === Null, (* Bottone per selezionare la modalità "Retta" *)
										{
											GuessTheFunctionGUI[1]; (* Apre l'interfaccia per indovinare la funzione lineare *)
											DialogReturn[]; (* Chiude la finestra di dialogo *)
											seedMessage = ""; (* Azzera il messaggio di feedback sul seed *)
										},
										seedMessage = "\:26a0\:fe0f Attenzione \:26a0\:fe0f\nIl seed può essere o un intero\n o al più lasciato vuoto." (* Visualizza un messaggio di errore *)
									]}],
						Spacer[20],
						Button["Parabola", {If[Head[seed] === Integer || Head[seed] === Null, (* Bottone per selezionare la modalità "Parabola" *)
										{
											GuessTheFunctionGUI[2]; (* Apre l'interfaccia per indovinare la funzione parabolica *)
											DialogReturn[]; (* Chiude la finestra di dialogo *)
											seedMessage = ""; (* Azzera il messaggio di feedback sul seed *)
										},
										seedMessage = "\:26a0\:fe0f Attenzione \:26a0\:fe0f\nIl seed può essere o un intero\n o al più lasciato vuoto." (* Visualizza un messaggio di errore *)
									]}]
						}], 
					Spacer[10],
					TextForm@Dynamic@Style[seedMessage, FontColor->RGBColor[0.9, 0.3, 0], TextAlignment->Center, Bold] (* Visualizza il messaggio di feedback sul seed *)
					}, Center], (* Allinea il contenuto al centro *)
			    Alignment->{Center}, (* Allinea il riquadro al centro *)
				ImageSizeAction->"Scrollable", (* Consente lo scorrimento nel caso in cui la finestra sia troppo piccola *)
				ImageSize->Full, (* Imposta le dimensioni della finestra *)
				ImageMargins->20 (* Imposta i margini della finestra *)
			]
	   ], 
	   WindowSize-> {All, All}, (* Imposta le dimensioni della finestra *)
	   WindowTitle-> "Scegli esercizio" (* Imposta il titolo della finestra *)
	]
  ]


(* Codice che gestisce l'apertura di una finestra di informazione per l'utente. *)

CreateInfoWindow[infoText_] :=
  CreateDialog[Column[{TextCell[infoText, "Text", FontSize -> 12], Spacer[20], Button[
    "Close", DialogReturn[]]}], WindowSize -> {400, 150}, WindowTitle -> "Perché inserire un seed?"]


(* Dichiarazione di fine del package *)

End[];

EndPackage[];

