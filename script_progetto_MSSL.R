# Caricamento delle librerie necessarie
library(ggplot2)
library(car)
library(DAAG)
library(corrplot)
library(olsrr)
library(glmnet)  # Per Ridge, LASSO e Elastic Net
library(caret)   # Per la cross-validation
library(lmtest)
library(sandwich)


# Lettura dei dati
housing_data <- read.csv("C:/Users/giuse/Desktop/HousingData.csv")

# Visualizzazione delle prime righe del dataset
head(housing_data)

# Summary delle variabili
summary(housing_data)

# Verifica della presenza di valori mancanti
sum(is.na(housing_data))
#eliminiamo valori mancanti per evitare che quando magari
#facciamo operazione come quella di grafico tra dati stimati e dati osservati
#ci siano incongruenze tra le dimensioni dei dati
housing_data <- na.omit(housing_data)



# Istogramma della variabile dipendente MEDV
windows()
ggplot(housing_data, aes(x = MEDV)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(title = "Distribuzione di MEDV", x = "MEDV", y = "Frequenza")


#GRAFICO ---MEDIA E VARIANZA----
#Calcolo della media e della varianza
media_medv <- mean(housing_data$MEDV)
varianza_medv <- var(housing_data$MEDV)

# Stampa dei risultati
cat("Media di medv:", media_medv, "\n")
cat("Varianza di medv:", varianza_medv, "\n")

# Grafico della media e della varianza
windows()
barplot(c(media_medv, varianza_medv), 
        names.arg = c("Media", "Varianza"), 
        main = "Media e Varianza di medv", 
        ylab = "Valore", 
        col = c("blue", "red"))



# Matrice di correlazione
cor_matrix <- cor(housing_data, use = "complete.obs")
print(cor_matrix)
# Plotta la matrice di correlazione
corrplot(cor_matrix, 
         method = "color",  # Usa i colori per rappresentare i valori
         type = "upper",    # Mostra solo la parte superiore della matrice
         tl.col = "black",  # Colore delle etichette
         tl.srt = 45,       # Rotazione delle etichette a 45 gradi
         addCoef.col = "black",  # Aggiungi i valori numerici
         number.cex = 0.7,  # Dimensione del testo dei coefficienti
         title = "Matrice di Correlazione",  # Titolo del grafico
         mar = c(0, 0, 1, 0))  # Margini del grafico



# Calcolo della matrice di covarianza
cov_matrix <- cov(housing_data, use = "complete.obs")
print(cov_matrix)

#plot dei dati come ha fatto lui
windows()
plot(housing_data)

#Matrice di dispersione con la retta di regressione, eventualmente controllare se ha senso quelle con rette verticali e se ha senso piu dividere per colonne
windows()
scatterplotMatrix(housing_data, col="black", pch=20, regLine = list(method=lm, lty=1, lwd=2, 
col="chartreuse3"), 
smooth=FALSE, 
diagonal=list(method 
="histogram", breaks="FD"), 
main="Matrice di dispersione 
con rette di regressione", 
data=housing_data) 


#suo commento slide 60 ppt 3_1
plot(housing_data$RM, housing_data$MEDV,
     xlab = "RM", 
     ylab = "MEDV",
     main = "Regressione tra RM e MEDV",
     col = "blue", pch = 20)

# Aggiunta della retta di regressione
model_rm_medv <- lm(MEDV ~ RM, data = housing_data)
abline(model_rm_medv, col = "red", lwd = 2)


#suo commento slide 60 ppt 3_1 CASO DIMINUIZIONE
plot(housing_data$LSTAT, housing_data$MEDV,
     xlab = "LSTAT", 
     ylab = "MEDV",
     main = "Regressione tra LSTAT e MEDV",
     col = "blue", pch = 20)

# Aggiunta della retta di regressione
model_lstat_medv <- lm(MEDV ~ LSTAT, data = housing_data)
abline(model_lstat_medv, col = "red", lwd = 2)


# Calcolo del VIF (Variance Inflation Factor) PER CAPIRE SE C'È MULTICOLLINEARITA'
model <- lm(MEDV ~ ., data = housing_data)
vif_values <- vif(model)
print(vif_values)

#QUA DICIAMO CHE ESSENDO TUTTI <10 NON CE N'È


#Stima del modello che inizialmente secondo noi potrebbe spiegare bene
#la variabile dipendente, abbiamo scelto i regressori in base a quello che pensiamo 
#abbia piu senso
model1RegressoriSceltiDaNoi <- lm(MEDV ~ (CRIM+RM+DIS+TAX+B+LSTAT), data = housing_data)
summary(model1RegressoriSceltiDaNoi)

# Analisi dei residui
#prima con la normale per vedere se vale hp normalita degli errori
residuiNostroModelloEnormale <- rstandard(model1RegressoriSceltiDaNoi)
hist(residuiNostroModelloEnormale,freq=F,xlim=c(-4,4), ylim=c(0,0.6)); curve(dnorm(x),add=T)

#poi da soli plottati per capire se ci sono andamenti sistematici negli stessi
plot(residuiNostroModelloEnormale)



SMEDVnostri<-fitted(model1RegressoriSceltiDaNoi)
windows()
# Grafico dei valori stimati vs osservati
plot(SMEDVnostri, housing_data$MEDV, 
     xlab = "Valori Stimati (SMEDVnostri)", 
     ylab = "Valori Osservati (MEDV)", 
     main = "Valori Stimati vs Osservati", 
     pch = 16, col = "blue")
# Aggiungo la retta a 45 gradi (y = x)
abline(a = 0, b = 1, col = "red", lwd = 2)

#VALORI OSSERVATI E STIMATI, se stessero tutti sulla linea rossa (cioe bisettrice
#cioe retta con angolo 45°avrei overfitting) 



#commenti sul modello ottenuto
#dire che proviamo ora a stimare modello partendo da tutti i regressori

# Stima del primo modello con tutti i regressori
model1 <- lm(MEDV ~ ., data = housing_data)
summary(model1)


# Analisi dei residui
#prima con la normale per vedere se vale hp normalita degli errori
residuim1 <- rstandard(model1)
windows()
hist(residuim1,freq=F,xlim=c(-4,4), ylim=c(0,0.6)); curve(dnorm(x),add=T)

#poi da soli plottati per capire se ci sono andamenti sistematici negli stessi
windows()
plot(residuim1)


#commentare questo modello dicendo cosa cambia rispetto a quello nostro, quindi tipo
#aumenta R^2 ecc quali sono sifninficativi ecc


# Rimozione dei regressori non significativi E SUCCESSIVO COMMENTO SU CHE COSA 
#SUCCEDE, MAGARI PASSIAMO I 3 MODELLI A CHAT E VEDIAMO SUOI COMMENTI PER LA 
#RELAZIONE CAPENDO COSA SCRIVERE
model2 <- update(model1, . ~ . - INDUS - AGE)
summary(model2)

# Analisi dei residui
#prima con la normale per vedere se vale hp normalita degli errori
residuim2 <- rstandard(model2)
windows()
hist(residuim2,freq=F,xlim=c(-4,4), ylim=c(0,0.6)); curve(dnorm(x),add=T)



#ora plotto residui del modello e valori fitted da questo ultimo modello
# Calcola fitted_values e residuals dal modello
fitted_values <- fitted(model2)
residuals <- resid(model2)

# Crea il grafico dei residui vs valori fitted
windows()
ggplot(data = NULL, aes(x = fitted_values, y = residuals)) +
  geom_point() +  # Aggiunge i punti al grafico
  geom_hline(yintercept = 0, color = "red") +  # Aggiunge una linea orizzontale a y = 0
  labs(title = "Residui vs Valori Fitted", x = "Fitted Values", y = "Residuals")




#poi da soli plottati per capire se ci sono andamenti sistematici negli stessi
windows()
plot(residuim2)


# Esecuzione della Best Subset Selection
best_subset <- ols_step_best_subset(model2)
print(best_subset)
plot(best_subset)
#qua vediamo criterio di selezione automatica quale insieme di regressori tra
#quelli che costituivano il secondo modello è meglio usare in base a parametri 
#come AIC,BIC o R^2 ecc


# ------------------------------------------------------------------------------
# CONTROLLO DELL'ETEROSCHEDASTICITÀ CON I TEST DI BREUSCH-PAGAN E WHITE
# ------------------------------------------------------------------------------

# 1. TEST DI BREUSCH-PAGAN
# ------------------------------------------------------------------------------

# Calcolo dei residui al quadrato del modello
residui_m2 <- resid(model1)
residui_m2_quad <- residui_m2^2

# Creazione di un modello ausiliario in cui i residui al quadrato sono la variabile dipendente
# e TUTTI i regressori originali sono le variabili indipendenti
modello_ausiliario_BP <- lm(residui_m2_quad ~ ., data = housing_data)

# Esecuzione del test di Breusch-Pagan
summary(modello_ausiliario_BP)

#SE P-VALUE MINORE DI 0.005 RIFIUTO OMOSCHEDASTICITÀ QUINDI HO ETEROSCHEDASTICITÀ

# ------------------------------------------------------------------------------
# 2. TEST DI WHITE
# ------------------------------------------------------------------------------

# Calcolo delle ordinate stimate (fitted values) e dei loro quadrati
fitted_m2 <- fitted(model1)
fitted_m2_quad <- fitted_m2^2

# Creazione di un modello ausiliario per il test di White
# I residui al quadrato sono la variabile dipendente, mentre le ordinate stimate e i loro quadrati sono le variabili indipendenti
modello_ausiliario_White <- lm(residui_m2_quad ~ fitted_m2 + fitted_m2_quad)

# Esecuzione del test di White
summary(modello_ausiliario_White)

#SE P-VALUE MINORE DI 0.005 RIFIUTO OMOSCHEDASTICITÀ QUINDI HO ETEROSCHEDASTICITÀ

# ------------------------------------------------------------------------------
# 3. GRAFICO DEI RESIDUI VS VALORI FITTED
# ------------------------------------------------------------------------------

# Creazione del grafico dei residui vs valori fitted per visualizzare eventuali pattern di eteroschedasticità
windows()
plot(fitted(model1), residuals(model1), 
     xlab = "Valori Fitted", ylab = "Residui", 
     main = "Residui vs Valori Fitted per Eteroschedasticità",
     pch = 20, col = "blue")
abline(h = 0, col = "red", lwd = 2)


# ------------------------------------------------------------------------------
# 4. TRASFORMAZIONE LOGARITMICA DELLA VARIABILE DIPENDENTE
# ------------------------------------------------------------------------------
# Applico una trasformazione logaritmica alla variabile dipendente
housing_data$log_MEDV <- log(housing_data$MEDV)

# Ristimo il modello con la variabile dipendente trasformata
model1_log <- lm(log_MEDV ~ ., data = housing_data)

# Verifico nuovamente l'eteroschedasticità con il test di Breusch-Pagan
residui_m1_log <- resid(model1_log)
residui_m1_log_quad <- residui_m1_log^2
modello_ausiliario_BP_log <- lm(residui_m1_log_quad ~ ., data = housing_data)
summary_BP_log <- summary(modello_ausiliario_BP_log)
print(summary_BP_log)

#SE P-VALUE MINORE DI 0.005 RIFIUTO OMOSCHEDASTICITÀ QUINDI HO ETEROSCHEDASTICITÀ

# ------------------------------------------------------------------------------
# 5. UTILIZZO ERRORI ROBUSTI
# ------------------------------------------------------------------------------


# Calcolo della matrice di covarianza robusta
vcov_robusto <- vcovHC(model1, type = "HC1")

# Stima dei coefficienti con errori robusti
coef_robusti <- coeftest(model1, vcov = vcov_robusto)
cat("Coefficienti con errori robusti:\n")
print(coef_robusti)
# ------------------------------------------------------------------------------
# FINE DELLA SEZIONE SULL'ETEROSCHEDASTICITÀ
# ------------------------------------------------------------------------------





# ------------------------------------------------------------------------------
# STATISTICAL LEARNING - REGOLARIZZAZIONE: RIDGE, LASSO, ELASTIC NET
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# 1. PREPARAZIONE DEI DATI
# ------------------------------------------------------------------------------

# Separazione delle variabili indipendenti (X) e dipendente (y)
X <- as.matrix(housing_data[, -which(names(housing_data) == "MEDV")])  # Tutte le colonne tranne MEDV
y <- housing_data$MEDV  # Variabile dipendente MEDV (prezzo delle case)

# Standardizzazione dei regressori (buona pratica per la regolarizzazione)
X <- scale(X)

# Creazione della griglia di valori per lambda (parametro di penalizzazione)
lambda_grid <- 10^seq(10, -2, length = 100)

# ------------------------------------------------------------------------------
# 2. RIDGE REGRESSION
# ------------------------------------------------------------------------------

# Stima del modello Ridge Regression
ridge_model <- glmnet(X, y, alpha = 0, lambda = lambda_grid)

# Cross-validation per scegliere il miglior lambda
cv_ridge <- cv.glmnet(X, y, alpha = 0, lambda = lambda_grid)

# Valore ottimale di lambda (quello che minimizza l'errore di previsione)
best_lambda_ridge <- cv_ridge$lambda.min

# Stima del modello finale con il miglior lambda
ridge_final <- glmnet(X, y, alpha = 0, lambda = best_lambda_ridge)

# Coefficienti del modello finale Ridge
coef_ridge <- coef(ridge_final)

# ------------------------------------------------------------------------------
# 3. LASSO REGRESSION
# ------------------------------------------------------------------------------

# Stima del modello LASSO
lasso_model <- glmnet(X, y, alpha = 1, lambda = lambda_grid)

# Cross-validation per scegliere il miglior lambda
cv_lasso <- cv.glmnet(X, y, alpha = 1, lambda = lambda_grid)

# Valore ottimale di lambda
best_lambda_lasso <- cv_lasso$lambda.min

# Stima del modello finale con il miglior lambda
lasso_final <- glmnet(X, y, alpha = 1, lambda = best_lambda_lasso)

# Coefficienti del modello finale LASSO
coef_lasso <- coef(lasso_final)

# ------------------------------------------------------------------------------
# 4. ELASTIC NET
# ------------------------------------------------------------------------------

# Stima del modello Elastic Net (alpha = 0.5, bilancia Ridge e LASSO)
elastic_model <- glmnet(X, y, alpha = 0.5, lambda = lambda_grid)

# Cross-validation per scegliere il miglior lambda
cv_elastic <- cv.glmnet(X, y, alpha = 0.5, lambda = lambda_grid)

# Valore ottimale di lambda
best_lambda_elastic <- cv_elastic$lambda.min

# Stima del modello finale con il miglior lambda
elastic_final <- glmnet(X, y, alpha = 0.5, lambda = best_lambda_elastic)

# Coefficienti del modello finale Elastic Net
coef_elastic <- coef(elastic_final)

# ------------------------------------------------------------------------------
# 5. TABELLA UNICA DEI COEFFICIENTI
# ------------------------------------------------------------------------------

# Creazione di una tabella unica per confrontare i coefficienti dei tre modelli
coefficients_table <- data.frame(
  Variable = rownames(coef_ridge),  # Nomi delle variabili
  Ridge = as.numeric(coef_ridge),   # Coefficienti Ridge
  LASSO = as.numeric(coef_lasso),   # Coefficienti LASSO
  ElasticNet = as.numeric(coef_elastic)  # Coefficienti Elastic Net
)

# Stampa della tabella dei coefficienti
print(coefficients_table)

# ------------------------------------------------------------------------------
# 6. CONFRONTO DEI MODELLI IN TERMINI DI MSE
# ------------------------------------------------------------------------------

# Calcolo del MSE per Ridge
mse_ridge <- min(cv_ridge$cvm)

# Calcolo del MSE per LASSO
mse_lasso <- min(cv_lasso$cvm)

# Calcolo del MSE per Elastic Net
mse_elastic <- min(cv_elastic$cvm)

# Creazione di una tabella di confronto
mse_comparison <- data.frame(
  Model = c("Ridge", "LASSO", "Elastic Net"),
  MSE = c(mse_ridge, mse_lasso, mse_elastic)
)

# Stampa del confronto dei MSE
print(mse_comparison)

# ------------------------------------------------------------------------------
# 7. SELEZIONE DEL MODELLO MIGLIORE
# ------------------------------------------------------------------------------

# Selezione del modello con il minor MSE
best_model <- ifelse(mse_ridge < mse_lasso & mse_ridge < mse_elastic, "Ridge",
                     ifelse(mse_lasso < mse_elastic, "LASSO", "Elastic Net"))

# Stampa del modello migliore
cat("Il modello migliore è:", best_model, "\n")

# ------------------------------------------------------------------------------
# 8. GRAFICI DEI COEFFICIENTI
# ------------------------------------------------------------------------------

# Grafico dei coefficienti per Ridge
windows()
plot(ridge_model, xvar = "lambda", label = TRUE, main = "Ridge Regression Coefficients")

# Grafico dei coefficienti per LASSO
windows()
plot(lasso_model, xvar = "lambda", label = TRUE, main = "LASSO Coefficients")

# Grafico dei coefficienti per Elastic Net
windows()
plot(elastic_model, xvar = "lambda", label = TRUE, main = "Elastic Net Coefficients")

# ------------------------------------------------------------------------------
# FINE
# ------------------------------------------------------------------------------
