# 🎯 Scparabola

## 📌 Introduzione
**Scparabola** è un'esperienza interattiva sviluppata in **Mathematica 14** che sfida gli utenti a determinare i coefficienti di una funzione (retta o parabola) analizzando dati grafici. 

📚 **Obiettivo:** Stimolare l'intuizione matematica e le capacità analitiche attraverso un'interfaccia interattiva.

👥 **Autori:** Leonardo Dessì, Emanuele Grasso, Luca Polese, Davide Spada

## 🏗️ Struttura del Progetto
Il progetto è suddiviso nei seguenti componenti:

- **Notebook.nb** → Il cuore del progetto: il file che avvia l’esperienza interattiva.
- **Backend.wl** → Motore computazionale che genera equazioni, punti e sistemi di equazioni.
- **GraphicalPackage.wl** → Interfaccia grafica interattiva che collega utente e matematica.

## ⚙️ Installazione & Avvio
### 📌 Requisiti
✔️ **Mathematica 14.0** o superiore.

### 🚀 Procedura
1. Aprire **Notebook.nb** in Mathematica.
2. Assicurarsi che i file **Backend.wl** e **GraphicalPackage.wl** siano nella stessa directory.
3. Eseguire il notebook per avviare l'interfaccia grafica.
4. Seguire le istruzioni a schermo per completare la sfida!

## 🔍 Funzioni Principali
### 🧠 Backend.wl
- `myGenerateEquation[grade]` → Genera un'equazione di grado specificato.
- `myGeneratePointsOnLineOrParabola[...]` → Crea punti su una retta o parabola.

### 🎨 GraphicalPackage.wl
- Integra **Backend.wl** per generare funzioni e punti dinamici.
- Fornisce un’interfaccia interattiva per l’esperienza di gioco.

## 📜 Licenza
Questo progetto è distribuito sotto **licenza MIT**.

