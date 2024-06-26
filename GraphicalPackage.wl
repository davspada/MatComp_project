(* ::Package:: *)

(* :Title: Guess the function *)
(* :Context: Gioco di geometria analitica con lo scopo di permettere al giocatore di indentificare i coefficienti di una funzione (retta o parabola)*)
(* :Author: Leonardo Dess\[IGrave], Emanuele Grasso, Luca Polese, Davide Spada *)
(* :Summary: Package per giocare con Mathematica *)
(* :Copyright: Leonardo Dess\[IGrave], Emanuele Grasso, Luca Polese, Davide Spada - 2024 *)
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
startGame::usage = "startGame[] routine principale che permette di nascondere all'utente le chiamate per l'avvio del gioco. L'utente visualizzer\[AGrave] solo l'interfaccia grafica del gioco";

Begin["`Private`"];

(* Chiamata alla funzione myCreateDynamicWindow[] che avviene all'avvio dell'esercizio.
   Fare questa chiamata ci permette di evitare la creazione di funzioni Shadowed nel Tutorial.
   In questo modo la chiamata rimane nello scope del Package ed il suo funzionamento \[EGrave] totalmente trasparente all'utente.*)
startGame[] := myCreateDynamicWindow[];

(* Dichiarazione di una funzione per controllare l'input *)
myCheckInput[correctCoefficent_, coefficentInput_, myCounterErrori_] := Module[{message = "", condition, localCounterErrori},
	localCounterErrori = myCounterErrori;
	(* Controlla che i punti generati dal kernel siano uguali a quelli dati in input dall'utente*)
    If[correctCoefficent === coefficentInput, 
        {message="Congratulazioni, hai risolto l'esercizio.\nOra cosa vuoi fare:"; localCounterErrori = 0;}, 
        {message = "Hai sbagliato"; localCounterErrori++;}];
    Return[{message, localCounterErrori}];
]

(* Dichiarazione di una funzione per controllare se la matrice di vandermonde e il vettore dei termini noti sono corretti *)
myCheckMatrix[matrix_, constantVector_, points_] := Module[{message = "", coefficentMatrix, rhsVector, solution, tmpSol},
    (* Invocazione della funzione per la generazione dei coefficienti della matrice di Vandermonde e del vettore dei termini noti *)
    {coefficentMatrix, rhsVector} = myGenerateVandermondeMatrix[points];
    
    (* Verifica se la matrice inserita dall'utente \[EGrave] composta da valori interi non nulli *)
    Which[
        !AllTrue[Flatten[matrix], Head[#] === Integer && Head[#] =!= Null &] || 
        !AllTrue[Flatten[constantVector], Head[#] === Integer && Head[#] =!= Null &], 
        message = "Attenzione! Campi vuoti o numeri non interi inseriti!",
        checkMatrixSingular[matrix],
        message = "Attenzione! La matrice \[EGrave] singolare!",
        True,
        (* Risoluzione della matrice *)
	    solution = LinearSolve[coefficentMatrix, rhsVector];
	    (* Calcolo della matrice con i dati inseriti dall'utente *)
	    tmpSol = LinearSolve[matrix, Transpose[constantVector]];
        (* Verifica dell'uguaglianza delle soluzioni *)
	    message = If[solution === tmpSol,
	        "Congratulazioni, la matrice \[EGrave] corretta.\nOra risolvi il sistema lineare in forma matriciale utilizzando il metodo di Gauss",
	        "\|01f6ab Matrice sbagliata \|01f6ab"
	    ]
    ];
    
    (* Restituisce il messaggio da stampare a schermo *)
    Return[message]
]

(* Dichiarazione di una funzione per controllare che la matrice non sia singolare*)
checkMatrixSingular[matrix_] := Det[matrix] == 0

(* Dichiarazione di una funzione per generare una visualizzazione dei punti nella schermata principale*)
myGeneratePointDisplay[points_] := Module[{},
    Return[
        Column[
            Table[
                TraditionalForm@StringForm["\!\(\*SubscriptBox[\(p\), \(``\)]\) = (``, ``)", i, points[[i, 1]], points[[i, 2]]],
                {i, Length[points]}]
            ]
        ]
]


(*Funzione che permette di formattare l'equazione per la visualizzazione*)
myEquationForm /: MakeBoxes[myEquationForm[eqs_], TraditionalForm] := RowBox[{"\[Piecewise]", GridBox[{MakeBoxes[#, TraditionalForm]} & /@ {##} & @@ eqs]}];


(* ::Text:: *)
(*Codice per creare un'interfaccia grafica che permette di eseguire l'esercizio*)


myGuessTheFunctionGUI[grade_, seed_] := CreateDialog[(* Definisce una finestra di dialogo per l'interfaccia grafica del gioco *)
    DynamicModule[{
        message, message2, message3, (* Messaggi di feedback *)
        expr, (* Espressione dell'equazione che rappresenta la curva *)
        coefficentMatrix, (* Matrice dei coefficienti per il sistema lineare *)
        constantVector,(* Vettore dei termini noti per il sistema lineare *)
        dims, (*Dimensioni della matrice*)
        callouts, (* Lista di etichette di punti utilizzata per inserire i punti nel grafico *)
        fun, (* Variabile che rappresenta la funzione *)
        points, (* Punti generati dal backend *)
        coefficentList = ConstantArray[Null, {grade + 1, 1}], (*Lista dei coefficienti utilizzati nel package *)
        coefficentListInput = ConstantArray[Null, {grade + 1, 1}], (* Lista dei coefficienti in input *)
        myCounterErrori,(* Contatore degli errori commessi dall'utente *)
        visualizzaAiutoFlag = False, (* Variabile booleana per la visualizzazione dell'aiuto *)
        correctFunction = False, (* Variabile booleana utilizzata per la pulizia dell'interfaccia *)
        eqs, (* Variabile contentente le equazioni che descrivono la curva, utilizzata nell'aiuto *)
        prompt (* Variabile necessaria alla generazione della soluzione (in aiuto all'utente) *)
        }, 
		
        (* Genera l'equazione e i coefficienti interi *)
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
                TextCell["In questo esercizio dovrai trovare la funzione della curva che passa per i seguenti punti:", TextAlignment->Left],
                Spacer[1],
                TextCell[TraditionalForm@myGeneratePointDisplay[points], "Abstract"],
                Spacer[1],
                (* Inserimento dei coefficienti dell'equazione *)
                TextCell[ToString@StringForm["Nella seguente sezione \[EGrave] possibile inserire i `` coefficenti dell'equazione di ``\[Degree] grado", grade + 1, grade]],
                TextCell["Cliccando sul bottone \[EGrave] possibile visualizzare la curva sull'asse cartesiano:"],
                Spacer[10],
                EventHandler[
                    Column[
                        {
                            Row[
                                {
                                    DisplayForm["\|01d466 ="],
                                    Spacer[10],
                                    If[grade == 2, (* Controlla se si tratta di una parabola *)
                                        Row[{
                                            InputField[Dynamic@coefficentListInput[[3]], Number, FieldSize->4, Alignment->Center, FieldHint->ToString[StringForm["\*SubscriptBox[c, ``]", 2]]],
                                            TraditionalForm["\!\(\*SuperscriptBox[\(\|01d465\), \(2\)]\)"],
                                            DisplayForm["  +  "]
                                        }],
                                        Row[{}]
                                    ],
                                    InputField[Dynamic@coefficentListInput[[2]], Number, FieldSize->4, Alignment->Center, FieldHint->ToString[StringForm["\*SubscriptBox[c, ``]", 1]]],
                                    TraditionalForm["\|01d465 + "],
                                    InputField[Dynamic@coefficentListInput[[1]], Number, FieldSize->4, Alignment->Center, FieldHint->ToString[StringForm["\*SubscriptBox[c, ``]", 0]]],
                                    Spacer[30]
                                }
                            ],
                            Row[
                                {
                                    Button[TextCell[" Inserisci funzione nel grafico ", FontSize->16], 
                                        {
                                            If[ AllTrue[coefficentListInput, # =!= Null &] && AllTrue[coefficentListInput, Head[#] === Integer &],
                                                {
                                                    fun = Sum[coefficentListInput[[i]]*x^(i-1), {i, 1, grade+1}], (*Costruzione della variabile che contiene l'equazione*)
                                                    {message, myCounterErrori} = myCheckInput[coefficentList, coefficentListInput, myCounterErrori]; (* Controlla se la funzione \[EGrave] corretta *)
                                                    message3 = ""; (* Azzera eventuali messaggi precedenti *)
                                                },
                                                {
                                                message3 = "Attenzione! Campi vuoti o numeri non interi inseriti!" (* Visualizza un messaggio di errore *)}
                                            ]
                                        }, Enabled -> Dynamic[!correctFunction](* Inibisce il "click" sul pulsante quando l'esercizio \[EGrave] risolto correttamente *)
                                    ],
                                    Spacer[10],
                                    Button[TextCell[" Pulisci interfaccia", FontSize->16], (* Gestisce la pulizia dell'interfaccia, ri-inizializzando le variabili utilizzate *)
                                        {
                                        correctFunction = False,
                                        coefficentListInput = ConstantArray[Null, {grade + 1, 1}],
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
                            ],    
                            TextForm@Dynamic@Style[message3, FontColor -> Red], (* Visualizza eventuali messaggi di errore *)
                        }, Alignment -> Center;
                    ], {{"KeyDown", "."} :> Null}, PassEventsDown -> False (* Impedisce l'inserimento dei punti nei campi input *)
                ],
                (* Grafico dell'equazione inserita *)
                TextCell["Grafico dell'equazione inserita in input:", "Subsection"],
                Framed@Dynamic@Show[ (* Gestione della plot della funzione *)
                    callouts = Callout[#, "(" <> ToString[#[[1]]] <> ", " <> ToString[#[[2]]] <> ")", Background->LightBlue, Frame->True, RoundingRadius->5, FrameMargins->5] &/@ points;
                    ListPlot[callouts, ImageSize->Medium, ImageMargins->20, PlotRange->{{-10,10},Automatic}],
                    Plot[fun,{x,-10,10}]
                ],
                TextForm@Dynamic@Style[message, TextAlignment->Center], (* Visualizza messaggi di feedback *)
                Dynamic@DisplayForm@If[StringContainsQ[message, "Congratulazioni"], 
                    correctFunction = True;
					Row[{
						Button[TextCell[" Nuovo esercizio ", FontSize->16],{DialogReturn[], myCreateDynamicWindow[]}], (* Bottone che permette di iniziare nuovamente / cambiare esercizio *)
						Spacer[20],
						Button[TextCell[" Esci ", FontSize->16], DialogReturn[]] (*Bottone che permette di chiudere l'interfaccia e terminare l'esecuzione*)
					}],
					Dynamic@DisplayForm["Numero errori: " <> ToString[myCounterErrori]] (* Visualizza il numero di errori *)
				],
                Spacer[20],
                
                Dynamic@DisplayForm@If[myCounterErrori >= 3, Column[{(* Visualizza la matrice dei coefficienti e il vettore dei termini noti *)
                    Style[
						Column[{
							TraditionalForm["Per trovare i coefficienti, segui questi passaggi:"],
							TraditionalForm["1. Costruisci la matrice di Vandermonde \|01d449 utilizzando i valori \!\(\*SubscriptBox[\(\|01d465\), \(\|01d456\)]\)."],
							TraditionalForm["2. Costruisci il vettore dei termini noti \!\(\*StyleBox[\"\|01d466\",\nFontWeight->\"Bold\"]\) utilizzando i valori \!\(\*SubscriptBox[\(\|01d466\), \(\|01d456\)]\)."],
							TraditionalForm["3. Risolvi il sistema lineare \|01d449\!\(\*StyleBox[\"\|01d44e\",\nFontWeight->\"Bold\"]\) = \!\(\*StyleBox[\"\|01d466\",\nFontWeight->\"Bold\"]\). Questo pu\[OGrave] essere fatto utilizzando il metodo di Gauss"],
							Spacer[{1, 10}],
							TraditionalForm["Inserendo i valori corretti all'interno del seguente sistema espresso in forma matriciale,\npuoi verificare se il sistema che dovrai risolvere \[EGrave] corretto come punto di partenza."]
		                    },
		                 Alignment->Center
		                 ], FontSize->14
		            ],
                    EventHandler[
                        Column[{
                        Row[{
                            MatrixForm[  (* Generazione degli input field per l'inserimento dei valori della matrice di vandermonde*)
                                Table[With[{i = i, j = j},
                                  InputField[Dynamic@coefficentMatrix[[i, j]], Number, FieldSize->4, Alignment->Center, FieldHint->ToString[StringForm["\*SubsuperscriptBox[x, ``, ``]", i, j-1]]]],
                                 {i, grade+1}, {j, grade+1}]
                                ],
                             Style[DisplayForm[" \[Times] "], FontSize->16],
                             MatrixForm@Reverse[Table[With[{i = i}, ToString[StringForm["\*SubscriptBox[c, ``]", i-1]]],{i, grade+1} (*Stampa il vettore dei coefficienti*)
                             ]],
                             Style[DisplayForm[" = "], FontSize->16],
                             MatrixForm[  (* Generazione degli input field per l'inserimento dei valori del vettore dei termini noti*)
                                Table[With[{i = i},
                                  InputField[Dynamic@constantVector[[i]], Number, FieldSize->4, Alignment->Center, FieldHint->ToString[StringForm["\*SubscriptBox[y, ``]", i]]]],
                                 {i, grade+1}]
                                ],
                            Spacer[20],
                            (* Effettua un controllo sui coefficienti inseriti e chiama la funzione per la verifica della matrice *)    
                            Button[TextCell[" Controlla ", FontSize->16], {If[ContainsAny[Table[AllTrue[coefficentMatrix[[i]], #==0 &],{i, Length[coefficentMatrix]}], {True}],
                                message2 = "Attenzione! Il sistema non \[EGrave] risolvibile con coefficienti pari a 0", 
                                {message2 = myCheckMatrix[coefficentMatrix, constantVector, points]}]
                                }] (* Controlla la matrice dei coefficienti *)
                        }],
                        Row[{
                        Button[TextCell[" Aiuto ", FontSize->16], {visualizzaAiutoFlag = !visualizzaAiutoFlag}], (*Permette la visualizzazione dell'aiuto*)
                        Button[TextCell[" Mostra soluzione ", FontSize->16], (* Permette la generazione della finestra contenente la soluzione *)
							CreateDialog[
								Pane[
									Row[{
										If[grade == 2, (* Controlla se si tratta di una parabola o di una retta *)
											prompt = ToString@StringForm["solution to the system `` == ``c_2 + ``c_1 + c_0, `` == ``c_2 + ``c_1 + c_0, `` == ``c_2 + ``c_1 + c_0", points[[1,2]], points[[1,1]]^2, points[[1,1]], points[[2,2]], points[[2,1]]^2, points[[2,1]], points[[3,2]], points[[3,1]]^2, points[[3,1]]],
											prompt = ToString@StringForm["solution to the system `` == ``c_1 + c_0, `` == ``c_1 + c_0", points[[1,2]], points[[1,1]], points[[2,2]], points[[2,1]]]
										];
										WolframAlpha[prompt, (* Genera la soluzione tramite il prompt e WolframAlpha *)
											IncludePods->"Result", (* Include solamente il risultato *)
											AppearanceElements->{"Pods"}, (* Formato di visualizzazione *)
											TimeConstraint->{20,Automatic,Automatic,Automatic}, (* Tempo massimo per l'esecuzione *)
											PodStates->{"Result__Step-by-step solution","Result__Use Gaussian elimination"} (* Mostra soluzione step by step e usa il metodo di risoluzione attraverso l'eliminazione di Gauss*)
										]
									}],
									Alignment->Center,
									ImageSizeAction->"Scrollable",
									ImageSize->{All, All},
									ImageMargins->20
								], 
								WindowSize -> Large, 
								WindowElements->{"VerticalScrollBar", "StatusArea", "HorizontalScrollBar", "MagnificationPopUp"},
                                WindowFrameElements -> {"CloseBox", "ZoomBox", "MinimizeBox", "ResizeArea"}
							]
						]
                        }],
                        If[visualizzaAiutoFlag, (* Controlla se la variabile booleana per la visualizzazione dell'aiuto \[EGrave] "True" *)
							Column[{
								TextCell["Il seguente sistema di equazioni lineari descrive la curva che passa per i punti dati.\nTrasforma il sistema in forma matriciale e risolvilo con il metodo di Gauss per trovare la soluzione,\novvero i coefficienti dell'equazione della curva.\nOsserva che il sistema sottostante \[EGrave] equivamente a quello in forma matriciale.", TextAlignment->Center],
								Spacer[1],
								Row[{
										eqs = ConstantArray[Null, {grade + 1, 1}]; (* Inizializzazione della lista di equazioni (per evitare sovrascritture) *)
										Table[
											(* Creazione e formattazione delle equazioni *)
											eqs[[i]] = ToString@StringForm["`` = ",points[[i,2]]]<>
													If[grade==2, ToString@StringForm["\*SuperscriptBox[``, 2]\*SubscriptBox[c, 2] + ", points[[i,1]]], ""] <>
													ToString@StringForm["``\*SubscriptBox[c, 1] + \*SubscriptBox[c, 0]", points[[i,1]]];
									        ,{i, grade + 1}];
									        (* Stampa delle equazioni *)
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
                        TextForm@Dynamic@Style[message2, FontColor->RGBColor[0, 0.741, 0], TextAlignment->Center],
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
(* WindowSize -> {All,All}, *)
WindowElements->{"VerticalScrollBar", "StatusArea", "HorizontalScrollBar", "MagnificationPopUp"},
WindowFrameElements -> {"CloseBox", "ZoomBox", "MinimizeBox", "ResizeArea"}
];


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
	    WindowTitle-> "Scegli esercizio", (* Imposta il titolo della finestra *)
        Resizable->True
    ]
];


(* Codice che gestisce l'apertura di una finestra di informazione per l'utente. *)

myCreateInfoWindow[] := CreateDialog[
    Column[
        {
            TextCell[
                "Un seed \[EGrave] un numero di partenza utilizzato dagli algoritmi che generano numeri casuali.\nImpostare un seed garantisce che l'algoritmo generi la stessa sequenza di esercizi ogni volta che viene eseguito con lo stesso seed.\nQuesto \[EGrave] essenziale per la riproducibilit\[AGrave] e la coerenza dei risultati degli esercizi.\n\nRicorda che lasciare il campo del seed vuoto comporter\[AGrave] la generazione randomica dello stesso.\n", 
                "Text", 
                FontSize -> 16
            ],
            Spacer[20],
            Button[
                TextCell[
                    "Chiudi", 
                    FontSize->16
                ], 
                DialogReturn[]
            ]
        }], 
    WindowSize -> {400, 350}, 
    WindowTitle -> "Perch\[EAcute] inserire un seed?",
    Resizable->True
]


(* Dichiarazione di fine del package *)

End[];

EndPackage[];

