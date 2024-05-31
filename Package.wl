(* ::Package:: *)

(* :Title: Guess the function *)
(* :Context: Gioco di geometria analitica con lo scopo di permettere al giocatore di indentificare i coefficienti di una funzione (retta o parabola)*)
(* :Author: Leonardo Dess\[IGrave], Emanuele Grasso, Luca Polese, Davide Spada *)
(* :Summary: Package per giocare con Mathematica *)
(* :Copyright: Leonardo Dess\[IGrave], Emanuele Grasso, Luca Polese, Davide Spada - 2023 *)
(* :Package Version: 1.0.0 *)
(* :Mathematica Version: 14 *)
(* :History: \\ *)
(* :Keywords: funzioni, punti, parabole *)
(* :Sources: \\ *)
(* :Discussion: \\ *)

BeginPackage["Package`"];

(* Carica un file di supporto chiamato "Backend.wl" *)
Get["Backend.wl"];

(* Definisce l'uso delle funzioni definite nel pacchetto *)
(* myGuessTheFunctionGUI::usage = "myGuessTheFunctionGUI[] Genera l'interfaccia per l'esercizio"; *)
(* myCreateDynamicWindow::usage = "myCreateDynamicWindow[] permette di creare l'interfaccia grafica con cui l'utente interagisce per poter avviare il programma"; *)
(* myCreateInfoWindow::usage = "myCreateInfoWindow[] permette di generare la finestra di info per settare il seed per la generazione randomica dei valori"; *)

Begin["`Private`"];

(* Imposta la directory di lavoro al percorso del notebook corrente *)
SetDirectory[NotebookDirectory[]];

(* Dichiarazione di una funzione per controllare l'input *)
myCheckInput[correctCoefficent_, coefficentInput_, myCounterErrori_] := Module[{message = "", condition, localCounterErrori},
	localCounterErrori = myCounterErrori;
	(*Controlla che i punti generati dal kernel siano uguali a quelli dati in input dall'utente*)
    If[correctCoefficent === coefficentInput, 
        {message="Congratulazioni, hai risolto l'esercizio.\nOra cosa vuoi fare:"; localCounterErrori = 0;}, 
        {message = "Hai sbagliato"; localCounterErrori++;}];
    Return[{message, localCounterErrori}];
]

(* Dichiarazione di una funzione per controllare una matrice *)
myCheckMatrix[matrix_, constantVector_, points_] := Module[{message = "", coefficentMatrix, rhsVector, solution, tmpSol},
    (* Invocazione della funzione per la generazione dei coefficienti della matrice di Vandermonde e del vettore dei termini noti *)
    {coefficentMatrix, rhsVector} = myGenerateVandermondeMatrix[points];
    
    (* Verifica se la matrice inserita dall'utente \[EGrave] composta da valori interi non nulli *)
    Which[
        !AllTrue[Flatten[matrix], Head[#] === Integer && Head[#] =!= Null &] || 
        !AllTrue[Flatten[constantVector], Head[#] === Integer && Head[#] =!= Null &], 
        message = "Attenzione! Campi vuoti o numeri non interi inseriti!",
        True,
        (* Risoluzione della matrice *)
	    solution = LinearSolve[coefficentMatrix, rhsVector];
	    (* Calcolo della matrice con i dati inseriti dall'utente *)
	    tmpSol = LinearSolve[matrix, Transpose[constantVector]];
        (* Verifica dell'uguaglianza delle soluzioni *)
	    message = If[solution === tmpSol,
	        "Congratulazioni, la matrice \[EGrave] corretta, ora risolvila ed inserisci i coefficienti in alto.",
	        "\|01f6ab Matrice sbagliata \|01f6ab"
	    ]
    ];
    
    (* Restituisce il messaggio da stampare a schermo *)
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


(*
myGuessTheFunctionGUI[2] := CreateDialog[(* Definisce una finestra di dialogo per l'interfaccia grafica del gioco, nel caso della parabola *)
    DynamicModule[{
        a, b, c, (* Variabili per i coefficienti dell'equazione *)
        realA, realB, realC, (* Coefficienti reali dell'equazione *)
        x, (* Variabile indipendente *)
        message, message2, message3, (* Messaggi di feedback *)
        expr, (* Espressione dell'equazione *)
        points, (*Punti generati dal backend*)
        fun, (*Variabile che rappresenta la funzione*)
        coefficentMatrix, (* Matrice dei coefficienti per il sistema lineare *)
        constantVector,(* Vettore dei termini noti per il sistema lineare *)
        callouts, (* Lista di etichette di punti utilizzata per inserire i punti nel grafico *)
        dims}(*dimensioni della matrice*), 
    
        myCounterErrori = 0; (* Inizializza il contatore degli errori *)

        (* Genera l'equazione e i coefficienti reali *)
        If[Head[seed] === Integer, 
            {expr, realA, realB, realC} = myGenerateEquation[2, seed],
            (* Genera pseudorandomicamente un numero intero da 0 a 9999 *)
            {seed = RandomInteger[9999], {expr, realA, realB, realC} = myGenerateEquation[2, seed]}
        ];
            
		
        points = myGeneratePointsOnLineOrParabola[3]; (* Genera tre punti casuali *)
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
                    TextCell[StringForm["Seed: ``", Dynamic@seed], FontSize->16] (* Visualizza il seed attuale *)
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
                    Row[{
                        DisplayForm["y ="],
                        Spacer[10],
                        InputField[Dynamic[a], Number, FieldHint->"a", FieldSize->3, Alignment->Center],
                        DisplayForm[ToExpression["x^2"]],
                        DisplayForm[" + "],
                        InputField[Dynamic[b], Number, FieldHint->"b", FieldSize->3, Alignment->Center],
                        DisplayForm["x  +  "],
                        InputField[Dynamic[c], Number, FieldHint->"c", FieldSize->3, Alignment->Center],
                        Spacer[30],
                        Button[TextCell[" Inserisci funzione nel grafico ", FontSize->16], {
                            If[Head[a] === Integer && Head[b] === Integer && Head[c] === Integer, {
                                fun = a*x^2 + b*x + c, (* Definisce la funzione inserita dall'utente con i coefficienti*)
                                message = myCheckInput[realA, realB, realC, a,  b,  c]; (* Controlla se la funzione \[EGrave] corretta *)
                                message3 = ""; (* Azzera eventuali messaggi precedenti *)
                                }, message3 = "Attenzione! Campi vuoti o numeri non interi inseriti!"; (* Visualizza un messaggio di errore *)
                    ]}]}], {{"KeyDown", "."} :> Null}, PassEventsDown -> False
                ],
                
                TextForm@Dynamic@Style[message3, FontColor -> Red], (* Visualizza eventuali messaggi di errore *)
                
                (* Grafico dell'equazione inserita *)
                TextCell["Grafico dell'equazione inserita in input:", "Subsection"],
                Framed@Dynamic@Show[
                    callouts = Callout[#, "(" <> ToString[#[[1]]] <> ", " <> ToString[#[[2]]] <> ")", Background->LightBlue, Frame->True, RoundingRadius->5,FrameMargins->5] &/@ points;
                    ListPlot[callouts, ImageSize->Medium, ImageMargins->20, PlotRange->{{-10,10},Automatic}],
                    Plot[fun,{x,-10,10}]
                ],
                
                (* Stampa il messaggio con l'esito dell'esercizio *)
                TextForm@Dynamic@Style[message, TextAlignment->Center],
                
                (* In base all'esito, stampa o il numero degli errori o due pulsanti che permettono di iniziare un nuovo gioco o uscire *)
                Dynamic@DisplayForm@If[StringContainsQ[message, "Congratulazioni"],
					Row[{
						Button[TextCell[" Nuovo esercizio ", FontSize->16],{DialogReturn[], myCreateDynamicWindow[]}],
						Spacer[20],
						Button[TextCell[" Esci ", FontSize->16], DialogReturn[]]
					}],
					Dynamic@DisplayForm["Numero errori: " <> ToString[myCounterErrori]] (* Visualizza il numero di errori *)
				],
				
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
                                 InputField[Dynamic@coefficentMatrix[[i, j]], Number, FieldSize->2, Alignment->Center, FieldHint->ToString[StringForm["\*SubsuperscriptBox[x, ``, ``]", i, j-1]]]], {i, 3}, {j, 3}
                         ]],
                             
                             Style[DisplayForm[" \[Times] "], FontSize->16],
                             Style[MatrixForm[{"c", "b", "a"}], FontSize->16],
                             Style[DisplayForm[" = "], FontSize->16],
                             
                             (*vettore dei termini noti*)
						     MatrixForm[
                                 Table[With[{i = i, j = j},
                                     InputField[Dynamic@constantVector[[i, j]], Number, FieldSize->2, Alignment->Center, FieldHint->ToString[StringForm["\*SubscriptBox[y, ``]", i]]]], {i, 3}, {j, 1}
                             ]],
                             Spacer[20],
						
						(* Premendo il bottone si effettua un iniziale controllo della matrice per verificare che non siano presenti righe formate solo da valori ugualia 0 *)
                          Button["Controlla", {If[ContainsAny[Table[AllTrue[coefficentMatrix[[i]], #==0 &],{i, Length[coefficentMatrix]}], {True}],
                                (* Creazione del messaggio di errore *)
                                message2 = "Attenzione! Il sistema non \[EGrave] risolvibile con coefficienti pari a 0",
                                (* Controlla la matrice dei coefficienti *) 
                                message2 = myCheckMatrix[coefficentMatrix, constantVector, points]]}]
                                
						(*event handler che non permette di inserire "."*)
				    }], {{"KeyDown", "."} :> Null}, PassEventsDown -> False
				],
			    
			    Spacer[10],
				If[StringContainsQ[message2, "Congratulazioni"],
					TextForm@Dynamic@Style[message2, FontColor->RGBColor[0, 0.741, 0]],
					TextForm@Dynamic@Style[message2, FontColor->Red]]
			    }, Alignment->Center],
				""
		]}, Alignment->Center ],
	    Alignment->Center,
        ImageSizeAction->"Scrollable",
        ImageSize->Full,
        ImageMargins->20
	], FontSize->16
]],

WindowTitle -> "SCPARABOLA",
WindowSize -> {Scaled[1],Scaled[1]},
WindowElements->{"VerticalScrollBar", "StatusArea"}
];
*)
myEquationForm /: MakeBoxes[myEquationForm[eqs_], TraditionalForm] := RowBox[{"\[Piecewise]", GridBox[{MakeBoxes[#, TraditionalForm]} & /@ {##} & @@ eqs]}];


(* ::Text:: *)
(*Codice per creare un'interfaccia grafica che permette di eseguire l'esercizio per trovare la retta passante per 2 punti*)


myGuessTheFunctionGUI[grade_, seed_] := CreateDialog[(* Definisce una finestra di dialogo per l'interfaccia grafica del gioco, nel caso della retta *)
    DynamicModule[{
        x, (* Variabile indipendente *)
        message, message2, message3, (* Messaggi di feedback *)
        expr, (* Espressione dell'equazione *)
        coefficentMatrix, (* Matrice dei coefficienti per il sistema lineare *)
        constantVector,(* Vettore dei termini noti per il sistema lineare *)
        dims, (*Dimensioni della matrice*)
        callouts, (* Lista di etichette di punti utilizzata per inserire i punti nel grafico *)
        fun, (*Variabile che rappresenta la funzione*)
        points, (* Punti generati dal backend*)
        coefficentList = ConstantArray[Null, {grade + 1, 1}],
        coefficentListInput = ConstantArray[Null, {grade + 1, 1}],
        myCounterErrori,(* Contatore degli errori commessi dall'utente *)
        visualizzaAiutoFlag = False,
        correctFunction = False
        }, 
		
        (* Genera l'equazione e i coefficienti reali *)
        {expr, coefficentList} = myGenerateEquation[grade, seed];
        
        coefficentList = Reverse[coefficentList];
        points = myGeneratePointsOnLineOrParabola[grade+1, expr]; (* Genera due punti casuali *)
        message = ""; (* Inizializza il messaggio di feedback *)
        message2 = ""; (* Inizializza un altro messaggio di feedback *)
        message3= ""; (* Inizializza un altro messaggio di feedback *)
        dims = {grade+1, grade+1}; (* Dimensioni della matrice dei coefficienti *)
        coefficentMatrix = ConstantArray[Null, dims]; (* Inizializza la matrice dei coefficienti *)
        constantVector =  ConstantArray[Null, {grade+1, 1}]; (* Inizializza il vettore dei termini noti *)
        myCounterErrori = 0;

        (* Crea la grafica dell'interfaccia utente *)
        Style[Pane[
            Column[{
                (* Intestazione del gioco *)
                Row[{
                    TextCell["Guess the function", "Title"],
                    Spacer[20],
                    TextCell[StringForm["Seed: ``", Dynamic@seed], FontSize->16] (* Visualizza il seed attuale *),
                    Button["Torna alla pagina precedente",{DialogReturn[], myCreateDynamicWindow[]}]
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
                            If[grade == 2,
								Row[{
									InputField[Dynamic@coefficentListInput[[3]], Number, FieldSize->2, Alignment->Center, FieldHint->ToString[StringForm["\*SubscriptBox[c, ``]", 2]]],
									TextCell[StringForm["\*SuperscriptBox[x, ``]", 2]],
									DisplayForm["  +  "]
								}],
                                Row[{}]
                            ],
                            InputField[Dynamic@coefficentListInput[[2]], Number, FieldSize->2, Alignment->Center, FieldHint->ToString[StringForm["\*SubscriptBox[c, ``]", 1]]],
                            DisplayForm["x + "],
                            InputField[Dynamic@coefficentListInput[[1]], Number, FieldSize->2, Alignment->Center, FieldHint->ToString[StringForm["\*SubscriptBox[c, ``]", 0]]],
                            
                            
                            Spacer[30],
                            Button[TextCell[" Inserisci funzione nel grafico ", FontSize->16], 
                                {
                                    If[ AllTrue[coefficentListInput, # =!= Null &] && AllTrue[coefficentListInput, Head[#] === Integer &],
                                        {
                                            fun = Sum[coefficentListInput[[i]]*x^(i-1), {i, 1, grade+1}],
                                            {message, myCounterErrori} = myCheckInput[coefficentList, coefficentListInput, myCounterErrori]; (* Controlla se la funzione \[EGrave] corretta *)
                                            message3 = ""; (* Azzera eventuali messaggi precedenti *)
                                        },
                                        {
                                        message3 = "Attenzione! Campi vuoti o numeri non interi inseriti!" (* Visualizza un messaggio di errore *)}
                                    ]
                                }, Enabled -> Dynamic[!correctFunction]
                            ],
                            Button[TextCell[" Pulisci interfaccia", FontSize->16],
								{
								Table[coefficentListInput[[i]] = Null, {i,1, grade+1}],
								fun = Null,
								message = "",
								message2 = "",
								message3 = "",
								myCounterErrori = 0,
								visualizzaAiutoFlag = False,
								coefficentMatrix = ConstantArray[Null, dims],
								constantVector =  ConstantArray[Null, {grade+1, 1}] 
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
                
                TextForm@Dynamic@Style[message, TextAlignment->Center], (* Visualizza messaggi di feedback *)
                Dynamic@DisplayForm@If[StringContainsQ[message, "Congratulazioni"],
                    correctFunction = True;
					Row[{
						
						Button[TextCell[" Nuovo esercizio ", FontSize->16],{DialogReturn[], myCreateDynamicWindow[]}],
						Spacer[20],
						Button[TextCell[" Esci ", FontSize->16], DialogReturn[]]
					}],
					Dynamic@DisplayForm["Numero errori: " <> ToString[myCounterErrori]] (* Visualizza il numero di errori *)
				],
                Spacer[20],
                Dynamic@DisplayForm@If[myCounterErrori >= 3, Column[{(* Visualizza la matrice dei coefficienti e il vettore dei termini noti *)
                    TextCell["Completa e risolvi la matrice di Vandermonde per trovare i coefficienti:", "Subsubsection"],
                    EventHandler[
                        Column[{
                        Row[{
                            MatrixForm[
                                Table[With[{i = i, j = j},
                                  InputField[Dynamic@coefficentMatrix[[i, j]], Number, FieldSize->2, Alignment->Center, FieldHint->ToString[StringForm["\*SubsuperscriptBox[x, ``, ``]", i, j-1]]]],
                                 {i, grade+1}, {j, grade+1}]
                                ],
                             Style[DisplayForm[" \[Times] "], FontSize->16],
                             MatrixForm@Reverse[Table[With[{i = i}, ToString[StringForm["\*SubscriptBox[c, ``]", i-1]]],{i, grade+1}
                             ]],
                             Style[DisplayForm[" = "], FontSize->16],
                             MatrixForm[
                                Table[With[{i = i},
                                  InputField[Dynamic@constantVector[[i]], Number, FieldSize->2, Alignment->Center, FieldHint->ToString[StringForm["\*SubscriptBox[y, ``]", i]]]],
                                 {i, grade+1}]
                                ],
                            Spacer[20],
                            Button[TextCell[" Controlla ", FontSize->16], {If[ContainsAny[Table[AllTrue[coefficentMatrix[[i]], #==0 &],{i, Length[coefficentMatrix]}], {True}],
                                message2 = "Attenzione! Il sistema non \[EGrave] risolvibile con coefficienti pari a 0", 
                                {message2 = myCheckMatrix[coefficentMatrix, constantVector, points]}]
                                }] (* Controlla la matrice dei coefficienti *)
                        }],
                        Row[{
                        Button[TextCell[" Aiuto ", FontSize->16], visualizzaAiutoFlag= True],
                        Button[TextCell[" Mostra soluzione ", FontSize->16]]
                        }],
                        If[visualizzaAiutoFlag,
							Column[{
	                         TextCell["Il seguente sistema lineare descrive la curva che passa per i punti dati.\nRisolvi il sistema di equazioni per trovare la soluzione, ovvero i parametri della curva.", TextAlignment->Center],
							Spacer[1],
							Row[{
									eqs = ConstantArray[Null, {grade + 1, 1}];
									Table[
										eqs[[i]] = ToString@StringForm["`` = ",points[[i,2]]]<>
												If[grade==2, ToString@StringForm["\*SuperscriptBox[``, 2]\*SubscriptBox[c, 2] + ", points[[i,1]]], ""] <>
												ToString@StringForm["``\*SubscriptBox[c, 1] + ``\*SubscriptBox[c, 0]", points[[i,1]], points[[i,1]]];
								        ,{i, grade + 1}];
									TraditionalForm@myEquationForm[eqs]
								}]
							}, Alignment->Center],
							Row[{}]
                        ]
                        }, Alignment->Center]
                        , {{"KeyDown", "."} :> Null}, PassEventsDown -> False
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
      ], FontSize->16]
  ],
WindowTitle -> "SCPARABOLA",
WindowSize -> {All,All},
WindowElements->{"VerticalScrollBar", "StatusArea", "HorizontalScrollBar", "MagnificationPopUp"}
];


(*
Row[{
	eqs = ConstantArray[Null, {grade + 1, 1}];
	Table[
		eqs[[i]] = ToString@StringForm["`` = ",points[[i,2]]]<>
				If[grade==2, ToString@StringForm["\*SuperscriptBox[``, 2]\*SubscriptBox[c, 2] + ", points[[i,1]]], ""] <>
				ToString@StringForm["``\*SubscriptBox[c, 1] + ``\*SubscriptBox[c, 0]", points[[i,1]], points[[i,1]]];
        ,{i, grade + 1}];
	TraditionalForm@myEquationForm[eqs]
}]
*)


(* ::Text:: *)
(* Codice per creare un'interfaccia grafica che permetta di scegliere la modalit\[AGrave] di gioco *)


myCreateDynamicWindow[] :=
	CreateDialog[ (* Crea una finestra di dialogo *)
	DialogNotebook[
	  
      DynamicModule[{infoWindow, seedMessage, seed = 0},
      seedMessage = ""; (* Messaggio di feedback sul seed *)
      
			Pane[ (* Utilizza un riquadro per contenere il layout *)
				Column[{ (* Utilizza una colonna per organizzare gli elementi *)
					Row[{ (* Utilizza una riga per organizzare gli elementi orizzontalmente *)
						EventHandler[ 
							Tooltip[Style["\:2139", FontSize -> 16], "Perch\[EAcute] inserire un seed?"], {"MouseClicked" :> myCreateInfoWindow[]}], (* Icona per informazioni aggiuntive sul seed *)
						Spacer[1],
						TextCell["Inserire Seed:", FontSize->16], (* Etichetta per il campo di inserimento del seed *)
						Spacer[5],
						EventHandler[ (* Gestisce eventi come il click del mouse *)
							InputField[Dynamic[seed], Number, ContinuousAction->True, FieldSize->Tiny], (* Campo di inserimento del seed *)
							 {{"KeyDown", "."} :> Null}, (* Ignora il punto per evitare errori di input *)
							PassEventsDown -> False] (* Passa gli eventi ai livelli pi\[UGrave] bassi *)
							}],
					Spacer[20],
					Row[{ (* Altra riga per organizzare gli elementi orizzontalmente *)
						Button[TextCell["Retta", FontSize->16], {
						    (* Controlliamo se il seed \[EGrave] Null e cos\[IGrave] gli impostiamo un valore randomico da 0 a 9999 *)
						    If[seed === Null, seed = RandomInteger[9999], {}],
						    
						    If[Head[seed] === Integer, (* Bottone per selezionare la modalit\[AGrave] "Retta" *)
								{
									myGuessTheFunctionGUI[1, seed]; (* Apre l'interfaccia per indovinare la funzione lineare *)
									DialogReturn[]; (* Chiude la finestra di dialogo *)
									seedMessage = ""; (* Azzera il messaggio di feedback sul seed *)
								},
									seedMessage = "\:26a0\:fe0f Attenzione \:26a0\:fe0f\nIl seed pu\[OGrave] essere o un intero\n o al pi\[UGrave] lasciato vuoto." (* Visualizza un messaggio di errore *)
						]}],
						Spacer[20],
						Button[TextCell["Parabola", FontSize->16], {
							(* Controlliamo se il seed \[EGrave] Null e cos\[IGrave] gli impostiamo un valore randomico da 0 a 9999 *)
						    If[seed === Null, seed = RandomInteger[9999], Return[seed]],
						    
							If[Head[seed] === Integer || seed === Null, (* Bottone per selezionare la modalit\[AGrave] "Parabola" *)
								{
									myGuessTheFunctionGUI[2, seed]; (* Apre l'interfaccia per indovinare la funzione parabolica *)
									DialogReturn[]; (* Chiude la finestra di dialogo *)
									seedMessage = ""; (* Azzera il messaggio di feedback sul seed *)
								},
								seedMessage = "\:26a0\:fe0f Attenzione \:26a0\:fe0f\nIl seed pu\[OGrave] essere o un intero\n o al pi\[UGrave] lasciato vuoto." (* Visualizza un messaggio di errore *)
						]}]
					}], 
					Spacer[10],
					TextForm@Dynamic@Style[seedMessage, FontColor->RGBColor[0.9, 0.3, 0], TextAlignment->Center, Bold, FontSize->16] (* Visualizza il messaggio di feedback sul seed *)
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
];


(* Codice che gestisce l'apertura di una finestra di informazione per l'utente. *)

myCreateInfoWindow[] :=
  CreateDialog[Column[{TextCell["Un seed \[EGrave] un numero di partenza utilizzato dagli algoritmi che generano numeri casuali.\nImpostare un seed garantisce che l'algoritmo generi la stessa sequenza di esercizi ogni volta che viene eseguito con lo stesso seed.\nQuesto \[EGrave] essenziale per la riproducibilit\[AGrave] e la coerenza dei risultati degli esercizi.\n\nRicorda che lasciare il campo del seed vuoto comporter\[AGrave] la generazione randomica dello stesso.\n"
  , "Text", FontSize -> 16],
  Spacer[20],
  Button[TextCell["Chiudi", FontSize->16], DialogReturn[]]}], WindowSize -> {400, 350}, WindowTitle -> "Perch\[EAcute] inserire un seed?"]


(* Dichiarazione di fine del package *)

End[];

(* Chiamata alla funzione che avviene all'inizio dell'esercizio
   dichiararla qui ci permette di evitare creazione di funzioni Shadowed
   In questo modo la chiamata rimane nello scope del Package ed il suo funzionamento \[EGrave] totalmente trasparente all'utente.*)
myCreateDynamicWindow[];

EndPackage[];

