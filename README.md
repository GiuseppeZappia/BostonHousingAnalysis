# BostonHousingAnalysis

## Descrizione del Progetto
Questo progetto analizza il dataset **Boston Housing**, con l'obiettivo di comprendere la relazione tra il valore mediano delle case (**MEDV**) e varie caratteristiche socio-economiche e ambientali. L'analisi è stata condotta attraverso modelli statistici e tecniche di machine learning. I risulatti delle analisi sono disponibili nel file denominato BLOCCO_RISULTATI_SCRIPT, in modo tale da poterli visualizzare senza dover necessariamente runnare lo script R.

## Struttura del Progetto
Il progetto è suddiviso nelle seguenti sezioni:
1. **Analisi Esplorativa**
   - Statistiche descrittive
   - Matrice di correlazione
   - Visualizzazione dei dati

2. **Modelli Inferenziali**
   - Regressione lineare con selezione dei regressori
   - Analisi dei residui
   - Test di significatività

3. **Test di Diagnosi**
   - Multicollinearità (VIF)
   - Eteroschedasticità (Test di Breusch-Pagan e Test di White)
   - Trasformazioni per migliorare il modello

4. **Modelli Predittivi**
   - Ridge Regression
   - LASSO
   - Elastic Net
   - Confronto delle performance

## Dataset
Il dataset utilizzato è il **Boston Housing**, originariamente pubblicato da Harrison e Rubinfeld nel 1978. Contiene **506 osservazioni** e **14 variabili**, tra cui:
- `CRIM`: Tasso di criminalità
- `ZN`: Percentuale di terreni residenziali
- `RM`: Numero medio di stanze
- `AGE`: Percentuale di case costruite prima del 1940
- `LSTAT`: Percentuale di popolazione con status socio-economico basso
- `MEDV`: Valore mediano delle case (variabile target)

## Risultati Principali
- Identificazione delle variabili più influenti sul valore delle case
- Confronto tra diversi modelli di regressione
- Implementazione di tecniche di regolarizzazione per migliorare la previsione
- Test di validazione statistica per garantire affidabilità
  
---
🚀 **Se trovi utile questo progetto, lascia una ⭐ su GitHub!**

