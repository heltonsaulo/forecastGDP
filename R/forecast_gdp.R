#' Previsão do PIB
#'
#' Esta função realiza previsões do PIB utilizando diferentes modelos estatísticos.
#'
#' @param variavel_forecasting Variável de interesse para previsão
#' @param horizon Horizonte para validação cruzada
#' @param start_date_series Ano-mês do início da série
#' @param start_date_series_diff Ano-mês do início da série
#' @param end_date_series Ano-mês do fim da série
#' @param start_date_forecast Ano-mês para para início da previsão fora da amostra
#' @param models Vetor com os modelos de previsão a serem considerados
#' @param seed Número da semente para reprodutibilidade
#' @param regularization Booleano para usar regularização via LASSO
#' @return Lista contendo previsões acumuladas e índices
#' @export
#' @examples
#' forecast_pib("y_pib_adjusted.servicos_ajustado", horizon = 2, start_year = 2024, start_month = 12)

##############################################################
## Funções para validação cruzada
##############################################################

##############################################################
##############################################################
##############################################################
##############################################################

f_arima <- function(y, h, seed){
    fit = forecast::auto.arima(y)
forecast::forecast(fit, h)
}

##############################################################
##############################################################
##############################################################
##############################################################

f_ets <- function(y, h, seed){
   fit = forecast::ets(y, model="ZZZ")
forecast::forecast(fit, h)
}

##############################################################
##############################################################
##############################################################
##############################################################

f_bats <- function(y, h, seed){
   fit = forecast::bats(y)
forecast::forecast(fit, h)
}

##############################################################
##############################################################
##############################################################
##############################################################

f_tbats <- function(y, h, seed){
   fit = forecast::tbats(y)
forecast::forecast(fit, h)
}

##############################################################
##############################################################
##############################################################
##############################################################

f_thetaf <- function(y, h, seed){
   fit = forecast::thetaf(y, h)
forecast::forecast(fit, h)
}

##############################################################
##############################################################
##############################################################
##############################################################

f_meanf <- function(y, h, seed){
   fit = forecast::meanf(y, h)
forecast::forecast(fit, h)
}

##############################################################
##############################################################
##############################################################
##############################################################

f_naive <- function(y, h, seed){
  fit =   forecast::naive(y, h)
forecast::forecast(fit, h)
}

##############################################################
##############################################################
##############################################################
##############################################################
f_snaive <- function(y, h, seed){
  fit =   forecast::snaive(y, h)
forecast::forecast(fit, h)
}

##############################################################
##############################################################
##############################################################
##############################################################

f_elm <- function(y, h, xreg, newxreg, seed, regularization = TRUE){
set.seed(seed)
lassoFit <-  glmnet::cv.glmnet(x=model.matrix(~.-1,data=xreg), y=as.vector(y), family="gaussian", intercept = FALSE,
                                alpha =1)

namesCoef <- rownames(coef(lassoFit, s = 'lambda.min'))[coef(lassoFit, s = 'lambda.min')[,1]!= 0] ### returns nonzero coefs
#print(namesCoef)
lfc <- as.vector(coef(lassoFit, s = 'lambda.min')[,1]!= 0)[-1]
if(regularization == TRUE) namesCoef <- colnames(xreg)[lfc]
if(regularization == FALSE) namesCoef <- colnames(xreg)##[lfc]
print(namesCoef)

X_reg_train <- as.matrix(as.data.frame(xreg[,which(colnames(xreg)%in% c(namesCoef))]))
X_reg_test <- as.matrix(as.data.frame(newxreg[,which(colnames(newxreg)%in% c(namesCoef))]))

nnfor::elm(y, xreg = X_reg_train) %>%
     forecast::forecast(h=h, xreg=  rbind(X_reg_train,X_reg_test))

}


##############################################################
##############################################################
##############################################################
##############################################################

f_wavelet <- function(y, h, seed){
  simts <- ts(as.vector(y))
   fit = WaveletArima::WaveletFittingarma(ts=simts,filter ='haar',Waveletlevels=floor(log(length(simts))),
MaxARParam=5,MaxMAParam=5,NForecast=h)
aa <- forecast::meanf(fit$Finalforecast,h=h)
aa$mean <- fit$Finalforecast
aa$lower[,1] <- NA
aa$lower[,2] <- NA
aa$upper[,1] <- NA
aa$upper[,2] <- NA
aa
}


##############################################################
##############################################################
##############################################################
##############################################################

f_lasso <- function(y, h, xreg, newxreg, seed){
set.seed(seed)
lassoFit <-  glmnet::cv.glmnet(x=model.matrix(~.-1,data=xreg), y=as.vector(y), family="gaussian", intercept = FALSE,
                                alpha =1)
lassoPred <- predict(lassoFit, newx = model.matrix(~.-1,data=newxreg),s = lassoFit$lambda.min)
aa <- forecast::meanf(as.vector(lassoPred),h=h)
aa$mean <- as.vector(lassoPred)
aa$lower[,1] <- NA
aa$lower[,2] <- NA
aa$upper[,1] <- NA
aa$upper[,2] <- NA
aa
}


##############################################################
##############################################################
##############################################################
##############################################################


f_sarimax <- function(y, h, xreg, newxreg, seed, regularization = TRUE){
set.seed(seed)
lassoFit <-  glmnet::cv.glmnet(x=model.matrix(~.-1,data=xreg), y=as.vector(y), family="gaussian", intercept = FALSE,
                                alpha =1)

namesCoef <- rownames(coef(lassoFit, s = 'lambda.min'))[coef(lassoFit, s = 'lambda.min')[,1]!= 0] ### returns nonzero coefs
#print(namesCoef)
lfc <- as.vector(coef(lassoFit, s = 'lambda.min')[,1]!= 0)[-1]
if(regularization == TRUE) namesCoef <- colnames(xreg)[lfc]
if(regularization == FALSE) namesCoef <- colnames(xreg)##[lfc]
print(namesCoef)


forecast::auto.arima(y, xreg = as.matrix(as.data.frame(xreg[,which(colnames(xreg)%in% c(namesCoef))])) ) %>%
    forecast::forecast(h=h, xreg= as.matrix(as.data.frame(newxreg[,which(colnames(newxreg)%in% c(namesCoef))])))

## Covariáveis com componentes harmônicos de Fourier
# X_reg_train_harmonic <- as.matrix(as.data.frame( cbind(xreg[,which(colnames(xreg)%in% c(namesCoef))],fourier(y, K = 6))  ) )
# colnames(X_reg_train_harmonic) <-  c(colnames(xreg[,which(colnames(xreg)%in% c(namesCoef))]), colnames(fourier(y, K = 6)))
# X_reg_test_harmonic  <- as.matrix(as.data.frame(cbind(newxreg[,which(colnames(newxreg)%in% c(namesCoef))],fourier(y, K = 6 , h = Hor))  ))
# colnames(X_reg_test_harmonic) <-  c(colnames(xreg[,which(colnames(xreg)%in% c(namesCoef))]), colnames(fourier(y, K = 6)))

#forecast::auto.arima(y, xreg = X_reg_train_harmonic ) %>%
#  forecast::forecast(h=h, xreg = X_reg_test_harmonic )




}



##############################################################
##############################################################
##############################################################
##############################################################

f_gbm <- function(y, h, xreg, newxreg, seed, regularization = TRUE){

set.seed(seed)
lassoFit <-  glmnet::cv.glmnet(x=model.matrix(~.-1,data=xreg), y=as.vector(y), family="gaussian", intercept = FALSE,
                                alpha =1)

namesCoef <- rownames(coef(lassoFit, s = 'lambda.min'))[coef(lassoFit, s = 'lambda.min')[,1]!= 0] ### returns nonzero coefs
#print(namesCoef)
lfc <- as.vector(coef(lassoFit, s = 'lambda.min')[,1]!= 0)[-1]
if(regularization == TRUE) namesCoef <- colnames(xreg)[lfc]
if(regularization == FALSE) namesCoef <- colnames(xreg)##[lfc]
print(namesCoef)

# Define trainControl with cross-validation
trControl <- trainControl(
  method = "cv",          # Cross-validation
  number = 10,            # 10-fold cross-validation
  summaryFunction = defaultSummary,  # Default regression metrics (RMSE, R-squared)
  verboseIter = FALSE,     # Print training progress
  returnResamp = "final", # Save results for the final model only
  savePredictions = "final" # Save predictions for the best model
)

## Define a custom grid (tuneGrid) to tune (hyperparameters)
tuneGrid <- expand.grid(
  n.trees = c(100, 200, 300), ## Number of boosting iterations.
  interaction.depth = c(1, 3, 5), ## Maximum depth of trees
  shrinkage = c(0.01, 0.1), ## Learning rate.
  n.minobsinnode = c(5, 10) ## Minimum number of observations in terminal nodes
)

set.seed(seed)
gbmFit <- caret::train(x=as.matrix(as.data.frame(xreg[,which(colnames(xreg)%in% c(namesCoef))])),
                        y=as.vector(y),
                        method = "gbm",
                        trControl = trControl,
                        tuneGrid = tuneGrid,
                        verbose = FALSE)

gbmPred <- predict(gbmFit, newdata = as.matrix(as.data.frame(newxreg[,which(colnames(newxreg)%in% c(namesCoef))])))
aa <- forecast::meanf(as.vector(gbmPred),h=h)
aa$mean <- as.vector(gbmPred)
aa$lower[,1] <- NA
aa$lower[,2] <- NA
aa$upper[,1] <- NA
aa$upper[,2] <- NA
aa
}


##############################################################
##############################################################
##############################################################
##############################################################

f_xgboost <- function(y, h, xreg, newxreg, seed, regularization = TRUE){

set.seed(seed)
lassoFit <-  glmnet::cv.glmnet(x=model.matrix(~.-1,data=xreg), y=as.vector(y), family="gaussian", intercept = FALSE,
                                alpha =1)

namesCoef <- rownames(coef(lassoFit, s = 'lambda.min'))[coef(lassoFit, s = 'lambda.min')[,1]!= 0] ### returns nonzero coefs
#print(namesCoef)
lfc <- as.vector(coef(lassoFit, s = 'lambda.min')[,1]!= 0)[-1]
if(regularization == TRUE) namesCoef <- colnames(xreg)[lfc]
if(regularization == FALSE) namesCoef <- colnames(xreg)##[lfc]
print(namesCoef)

# # Define trainControl with cross-validation
# trControl <- trainControl(
#   method = "cv",                # Cross-validation
#   number = 5,                   # Number of folds
#   verboseIter = FALSE,           # Show progress during training
#   search = "grid"              # Use grid search for tuning
# )
#
# ## Define a custom grid (tuneGrid) to tune (hyperparameters)
# tuneGrid <- expand.grid(
#   nrounds = c(50, 100, 150),           # Number of boosting iterations
#   max_depth = c(5, 10, 15, 20),              # Depth of trees
#   eta = c(0.01, 0.1, 0.3),             # Learning rate
#   gamma = c(0, 1, 5),                  # Minimum loss reduction
#   colsample_bytree = c(0.7, 0.8, 0.9, 1.0),   # Feature sampling ratio
#   min_child_weight = c(1, 5, 10),      # Minimum sum of instance weight
#   subsample = c(0.6, 0.8, 1)           # Subsampling ratio
# )

trControl <- trainControl(
  method = "cv",
  number = 5,
  search = "random"   # Random search
)

# No need to define a full `tuneGrid`; specify ranges instead
tuneGrid <- NULL  # caret automatically samples random combinations

set.seed(seed)
xgboostFit <- caret::train(x=as.matrix(as.data.frame(xreg[,which(colnames(xreg)%in% c(namesCoef))])),
                        y=as.vector(y),
                        method = "xgbTree",
                        trControl = trControl,
                        tuneGrid = tuneGrid,
                        verbose = FALSE,
                        verbosity = 0)

xgboostPred <- predict(xgboostFit, newdata = as.matrix(as.data.frame(newxreg[,which(colnames(newxreg)%in% c(namesCoef))])))
aa <- forecast::meanf(as.vector(xgboostPred),h=h)
aa$mean <- as.vector(xgboostPred)
aa$lower[,1] <- NA
aa$lower[,2] <- NA
aa$upper[,1] <- NA
aa$upper[,2] <- NA
aa
}




##############################################################
##############################################################
##############################################################
##############################################################

f_rf <- function(y, h, xreg, newxreg, seed, regularization = TRUE){

set.seed(seed)
lassoFit <-  glmnet::cv.glmnet(x=model.matrix(~.-1,data=xreg), y=as.vector(y), family="gaussian", intercept = FALSE,
                                alpha =1)

namesCoef <- rownames(coef(lassoFit, s = 'lambda.min'))[coef(lassoFit, s = 'lambda.min')[,1]!= 0] ### returns nonzero coefs
#print(namesCoef)
lfc <- as.vector(coef(lassoFit, s = 'lambda.min')[,1]!= 0)[-1]
if(regularization == TRUE) namesCoef <- colnames(xreg)[lfc]
if(regularization == FALSE) namesCoef <- colnames(xreg)##[lfc]
print(namesCoef)


trControl <- trainControl(
  method = "cv",           # Cross-validation (alternatively, use "repeatedcv" or "boot")
  number = 10,             # Number of folds (e.g., 10-fold CV)
  search = "grid",         # Grid search for hyperparameter tuning
  savePredictions = "final",  # Save predictions for the final model
  classProbs = FALSE       # Include probabilities for classification problems
)


tuneGrid <- expand.grid(
  mtry = seq(2, ifelse(sqrt(ncol(xreg))<2,2,sqrt(ncol(xreg))), by = 1) # Adjust based on feature count
)

set.seed(seed)
rfFit <- caret::train(x=as.matrix(as.data.frame(xreg[,which(colnames(xreg)%in% c(namesCoef))])),
                      y=as.vector(y),
                      method = "rf",
                      trControl = trControl,   # Training control
                     tuneGrid = tuneGrid     # Tuning grid
                      )

rfPred <- predict(rfFit, newdata = as.matrix(as.data.frame(newxreg[,which(colnames(newxreg)%in% c(namesCoef))])))
aa <- forecast::meanf(as.vector(rfPred),h=h)
aa$mean <- as.vector(rfPred)
aa$lower[,1] <- NA
aa$lower[,2] <- NA
aa$upper[,1] <- NA
aa$upper[,2] <- NA
aa
}


##############################################################
##############################################################
##############################################################
##############################################################

f_pcr <- function(y, h, xreg, newxreg, seed){

trControl <- trainControl(
  method = "cv",           # Cross-validation (e.g., 10-fold CV)
  number = 10,             # Number of folds
  savePredictions = "final",  # Save predictions for the final model
  verboseIter = FALSE        # Show progress during training
)

tuneGrid <- expand.grid(
  ncomp = seq(1, min(ncol(xreg) - 1, 20)) # Number of components to test
)

set.seed(seed)
pcrFit <- caret::train(x=as.matrix(as.data.frame(xreg)),
                        y=as.vector(y),
                        method = "pcr",
                        trControl = trControl,   # Training control settings
                        tuneGrid = tuneGrid      # Grid of `ncomp` values))
                        )

pcrPred <- predict(pcrFit, newdata = as.matrix(as.data.frame(newxreg)))
aa <- forecast::meanf(as.vector(pcrPred),h=h)
aa$mean <- as.vector(pcrPred)
aa$lower[,1] <- NA
aa$lower[,2] <- NA
aa$upper[,1] <- NA
aa$upper[,2] <- NA
aa
}



##############################################################
##############################################################
##############################################################
##############################################################

f_mars <- function(y, h, xreg, newxreg, seed, regularization = TRUE){

set.seed(seed)
lassoFit <-  glmnet::cv.glmnet(x=model.matrix(~.-1,data=xreg), y=as.vector(y), family="gaussian", intercept = FALSE,
                                alpha =1)

namesCoef <- rownames(coef(lassoFit, s = 'lambda.min'))[coef(lassoFit, s = 'lambda.min')[,1]!= 0] ### returns nonzero coefs
#print(namesCoef)
lfc <- as.vector(coef(lassoFit, s = 'lambda.min')[,1]!= 0)[-1]
if(regularization == TRUE) namesCoef <- colnames(xreg)[lfc]
if(regularization == FALSE) namesCoef <- colnames(xreg)##[lfc]
print(namesCoef)


trControl <- trainControl(
  method = "cv",
  number = 10
)

tuneGrid <- expand.grid(
  degree = c(1, 2, 3)#,
  #nprune = seq(2, min(30, (ncol(X_reg_train) - 1) * 2), by = 1)
)

set.seed(seed)
bmarsgcvFit <- caret::train(x=as.matrix(as.data.frame(xreg[,which(colnames(xreg)%in% c(namesCoef))])),
                        y=as.vector(y),
                        method = "bagEarthGCV",
                        tuneGrid = tuneGrid,
                        trControl = trControl)


bmarsgcvPred <- predict(bmarsgcvFit, newdata = as.matrix(as.data.frame(newxreg[,which(colnames(newxreg)%in% c(namesCoef))])))
bmarsgcvaa <- forecast::meanf(as.vector(bmarsgcvPred),h=h)
bmarsgcvaa$mean <- as.vector(bmarsgcvPred)
bmarsgcvaa$lower[,1] <- NA
bmarsgcvaa$lower[,2] <- NA
bmarsgcvaa$upper[,1] <- NA
bmarsgcvaa$upper[,2] <- NA
bmarsgcvaa
}


##############################################################
##############################################################
##############################################################
##############################################################

f_hybrid <- function(y, h, xreg, newxreg, seed, regularization = TRUE){

  ## bats
batsaa <- forecast::bats(y) %>% forecast::forecast(h=h)

## tbats
tbatsaa <- forecast::tbats(y) %>% forecast::forecast(h=h)

## snaive
snaiveaa <- forecast::snaive(y, h=h)

## Arima
arimaaa = forecast::auto.arima(y) %>% forecast::forecast(h)

## Ets
etsaa = forecast::ets(y, model="ZZZ")  %>% forecast::forecast(h)

## waveletarima
simts <- ts(as.vector(y))
fitwaveletarima = WaveletArima::WaveletFittingarma(ts=simts,filter ='haar',Waveletlevels=floor(log(length(simts))),
MaxARParam=5,MaxMAParam=5,NForecast=h)
waveletarimaaa <- forecast::meanf(fitwaveletarima$Finalforecast,h=h)
waveletarimaaa$mean <- fitwaveletarima$Finalforecast
waveletarimaaa$lower[,1] <- NA
waveletarimaaa$lower[,2] <- NA
waveletarimaaa$upper[,1] <- NA
waveletarimaaa$upper[,2] <- NA



## Lasso
set.seed(seed)
lassoFit <-  glmnet::cv.glmnet(x=model.matrix(~.-1,data=xreg), y=as.vector(y), family="gaussian", intercept = FALSE,
                                alpha =1)
lassoPred <- predict(lassoFit, newx = model.matrix(~.-1,data=newxreg),s = lassoFit$lambda.min)
lassoaa <- forecast::meanf(as.vector(lassoPred),h=h)
lassoaa$mean <- as.vector(lassoPred)
lassoaa$lower[,1] <- NA
lassoaa$lower[,2] <- NA
lassoaa$upper[,1] <- NA
lassoaa$upper[,2] <- NA



## covariates
namesCoef <- rownames(coef(lassoFit, s = 'lambda.min'))[coef(lassoFit, s = 'lambda.min')[,1]!= 0] ### returns nonzero coefs
#print(namesCoef)
lfc <- as.vector(coef(lassoFit, s = 'lambda.min')[,1]!= 0)[-1]
if(regularization == TRUE) namesCoef <- colnames(xreg)[lfc]
if(regularization == FALSE) namesCoef <- colnames(xreg)##[lfc]
print(namesCoef)


## elm
X_reg_train <- as.matrix(as.data.frame(xreg[,which(colnames(xreg)%in% c(namesCoef))]))
X_reg_test <- as.matrix(as.data.frame(newxreg[,which(colnames(newxreg)%in% c(namesCoef))]))
elmaa <- nnfor::elm(y, xreg = X_reg_train) %>%
     forecast::forecast(h=h, xreg=  rbind(X_reg_train,X_reg_test))


## sarima-lasso
sarimalassoaa <- forecast::auto.arima(y, xreg = as.matrix(as.data.frame(xreg[,which(colnames(xreg)%in% c(namesCoef))])) ) %>%
    forecast::forecast(h=h, xreg= as.matrix(as.data.frame(newxreg[,which(colnames(newxreg)%in% c(namesCoef))])))




## GBM

# Define trainControl with cross-validation
trControl <- trainControl(
  method = "cv",          # Cross-validation
  number = 10,            # 10-fold cross-validation
  summaryFunction = defaultSummary,  # Default regression metrics (RMSE, R-squared)
  verboseIter = FALSE,     # Print training progress
  returnResamp = "final", # Save results for the final model only
  savePredictions = "final" # Save predictions for the best model
)

## Define a custom grid (tuneGrid) to tune (hyperparameters)
tuneGrid <- expand.grid(
  n.trees = c(100, 200, 300), ## Number of boosting iterations.
  interaction.depth = c(1, 3, 5), ## Maximum depth of trees
  shrinkage = c(0.01, 0.1), ## Learning rate.
  n.minobsinnode = c(5, 10) ## Minimum number of observations in terminal nodes
)

set.seed(seed)
gbmFit <- caret::train(x=as.matrix(as.data.frame(xreg[,which(colnames(xreg)%in% c(namesCoef))])),
                       y=as.vector(y),
                       method = "gbm",
                       trControl = trControl,
                       tuneGrid = tuneGrid,
                       verbose = FALSE)

gbmPred <- predict(gbmFit, newdata = as.matrix(as.data.frame(newxreg[,which(colnames(newxreg)%in% c(namesCoef))])))
gbmaa <- forecast::meanf(as.vector(gbmPred),h=h)
gbmaa$mean <- as.vector(gbmPred)
gbmaa$lower[,1] <- NA
gbmaa$lower[,2] <- NA
gbmaa$upper[,1] <- NA
gbmaa$upper[,2] <- NA





## XGBOOST

# # Define trainControl with cross-validation
# trControl <- trainControl(
#   method = "cv",                # Cross-validation
#   number = 5,                   # Number of folds
#   verboseIter = FALSE,           # Show progress during training
#   search = "grid"              # Use grid search for tuning
# )
#
# ## Define a custom grid (tuneGrid) to tune (hyperparameters)
# tuneGrid <- expand.grid(
#   nrounds = c(50, 100, 150),           # Number of boosting iterations
#   max_depth = c(5, 10, 15, 20),              # Depth of trees
#   eta = c(0.01, 0.1, 0.3),             # Learning rate
#   gamma = c(0, 1, 5),                  # Minimum loss reduction
#   colsample_bytree = c(0.7, 0.8, 0.9, 1.0),   # Feature sampling ratio
#   min_child_weight = c(1, 5, 10),      # Minimum sum of instance weight
#   subsample = c(0.6, 0.8, 1)           # Subsampling ratio
# )

trControl <- trainControl(
  method = "cv",
  number = 5,
  search = "random"   # Random search
)

# No need to define a full `tuneGrid`; specify ranges instead
tuneGrid <- NULL  # caret automatically samples random combinations

set.seed(seed)
xgboostFit <- caret::train(x=as.matrix(as.data.frame(xreg[,which(colnames(xreg)%in% c(namesCoef))])),
                        y=as.vector(y),
                        method = "xgbTree",
                        trControl = trControl,
                        tuneGrid = tuneGrid,
                        verbose = FALSE,
                        verbosity = 0)


xgboostPred <- predict(xgboostFit, newdata = as.matrix(as.data.frame(newxreg[,which(colnames(newxreg)%in% c(namesCoef))])))
xgboostaa <- forecast::meanf(as.vector(xgboostPred),h=h)
xgboostaa$mean <- as.vector(xgboostPred)
xgboostaa$lower[,1] <- NA
xgboostaa$lower[,2] <- NA
xgboostaa$upper[,1] <- NA
xgboostaa$upper[,2] <- NA





## Random Forests

trControl <- trainControl(
  method = "cv",           # Cross-validation (alternatively, use "repeatedcv" or "boot")
  number = 10,             # Number of folds (e.g., 10-fold CV)
  search = "grid",         # Grid search for hyperparameter tuning
  savePredictions = "final",  # Save predictions for the final model
  classProbs = FALSE       # Include probabilities for classification problems
)


tuneGrid <- expand.grid(
  mtry = seq(2, ifelse(sqrt(ncol(xreg))<2,2,sqrt(ncol(xreg))), by = 1) # Adjust based on feature count
)


set.seed(seed)
rfFit <- caret::train(x=as.matrix(as.data.frame(xreg[,which(colnames(xreg)%in% c(namesCoef))])),
                      y=as.vector(y),
                      method = "rf",
                      trControl = trControl,   # Training control
                     tuneGrid = tuneGrid     # Tuning grid
                      )

rfPred <- predict(rfFit, newdata = as.matrix(as.data.frame(newxreg[,which(colnames(newxreg)%in% c(namesCoef))])))
rfaa <- forecast::meanf(as.vector(rfPred),h=h)
rfaa$mean <- as.vector(rfPred)
rfaa$lower[,1] <- NA
rfaa$lower[,2] <- NA
rfaa$upper[,1] <- NA
rfaa$upper[,2] <- NA



## PCR
trControl <- trainControl(
  method = "cv",           # Cross-validation (e.g., 10-fold CV)
  number = 10,             # Number of folds
  savePredictions = "final",  # Save predictions for the final model
  verboseIter = FALSE        # Show progress during training
)

tuneGrid <- expand.grid(
  ncomp = seq(1, min(ncol(xreg) - 1, 20)) # Number of components to test
)

set.seed(seed)
pcrFit <- caret::train(x=as.matrix(as.data.frame(xreg)),
                       y=as.vector(y),
                       method = "pcr",
                       trControl = trControl,   # Training control settings
                       tuneGrid = tuneGrid      # Grid of `ncomp` values)))
                      )

pcrPred <- predict(pcrFit, newdata = as.matrix(as.data.frame(newxreg)))
pcraa <- forecast::meanf(as.vector(pcrPred),h=h)
pcraa$mean <- as.vector(pcrPred)
pcraa$lower[,1] <- NA
pcraa$lower[,2] <- NA
pcraa$upper[,1] <- NA
pcraa$upper[,2] <- NA


## bagEarthGCV

trControl <- trainControl(
  method = "cv",
  number = 10
)

tuneGrid <- expand.grid(
  degree = c(1, 2, 3)#,
  #nprune = seq(2, min(30, (ncol(X_reg_train) - 1) * 2), by = 1)
)

set.seed(seed)
bmarsgcvFit <- caret::train(x=as.matrix(as.data.frame(xreg[,which(colnames(xreg)%in% c(namesCoef))])),
                        y=as.vector(y),
                        method = "bagEarthGCV",
                        tuneGrid = tuneGrid,
                        trControl = trControl)


bmarsgcvPred <- predict(bmarsgcvFit, newdata = as.matrix(as.data.frame(newxreg[,which(colnames(newxreg)%in% c(namesCoef))])))
bmarsgcvaa <- forecast::meanf(as.vector(bmarsgcvPred),h=h)
bmarsgcvaa$mean <- as.vector(bmarsgcvPred)
bmarsgcvaa$lower[,1] <- NA
bmarsgcvaa$lower[,2] <- NA
bmarsgcvaa$upper[,1] <- NA
bmarsgcvaa$upper[,2] <- NA
bmarsgcvaa
##Y_observed <- as.vector(window(y,start=c(start(newxreg)[1],start(newxreg)[2]),end=c(end(newxreg)[1],end(newxreg)[2])))
##as.vector(y[(InitPer+1):(InitPer+h)])
#Y_observed <- as.vector(window(y,
#  start=c(start(arimaaa$mean)[1],start(arimaaa$mean)[2]),
#  end=c(end(arimaaa$mean)[1],end(arimaaa$mean)[2])))

date = seq.Date(from = as.Date('2024-05-01'), by = 'month', length.out = h)

D_bats = data.frame(date, forecast.date=NA, model = "bats", forecast = as.vector(batsaa$mean), se=NA, observed =NA)
D_tbats = data.frame(date, forecast.date=NA, model = "tbats", forecast = as.vector(tbatsaa$mean), se=NA, observed =NA)
D_sarima = data.frame(date, forecast.date=NA, model = "sarima", forecast = as.vector(arimaaa$mean), se=NA, observed =NA)
D_ets = data.frame(date, forecast.date=NA, model = "ets", forecast = as.vector(etsaa$mean), se=NA, observed = NA)
D_waveletarima = data.frame(date, forecast.date=NA, model = "sarima", forecast = as.vector(waveletarimaaa$mean), se=NA, observed =NA)
D_lasso  = data.frame(date, forecast.date=NA,model = "lasso", forecast = as.vector(lassoaa$mean), se=NA,observed =NA)
D_elm = data.frame(date, forecast.date=NA, model = "elm", forecast = as.vector(elmaa$mean), se=NA, observed =NA)
D_sarimalasso  = data.frame(date, forecast.date=NA,model = "sarimalasso", forecast = as.vector(sarimalassoaa$mean), se=NA,observed = NA)
D_gbm  = data.frame(date, forecast.date=NA,model = "gbm", forecast = as.vector(gbmaa$mean), se=NA,observed = NA)
D_xgboost  = data.frame(date, forecast.date=NA,model = "xgboost", forecast = as.vector(xgboostaa$mean), se=NA,observed = NA)
D_rf  = data.frame(date, forecast.date=NA,model = "rf", forecast = as.vector(rfaa$mean), se=NA,observed = NA)
D_pcr  = data.frame(date, forecast.date=NA,model = "pcr", forecast = as.vector(pcraa$mean), se=NA,observed = NA)
D_bagged_mars_gcv  = data.frame(date, forecast.date=NA,model = "bagged_mars_gcv", forecast = as.vector(bmarsgcvaa$mean), se=NA,observed = NA)


D_forecasts_all <- rbind.data.frame(D_bats,D_tbats,
                                    D_sarima,D_ets,D_waveletarima,D_lasso,D_elm,
                                    D_sarimalasso,D_gbm,D_xgboost,D_rf,D_pcr,
                                    D_bagged_mars_gcv)
 # combine forecasts
# combinations_all  =
#    OOS::forecast_combine(
#      D_forecasts_all,
#      method = c('uniform'),
#      burn.in = 5,
#      n.max = 2)
combinations_all <- D_forecasts_all %>%
  group_by(date)  %>%
  summarise(mean_forecast = mean(forecast))


hybridaa <- forecast::meanf(as.vector(combinations_all$mean_forecast),h=h)
hybridaa$mean <- as.vector(combinations_all$mean_forecast)
hybridaa$lower[,1] <- NA
hybridaa$lower[,2] <- NA
hybridaa$upper[,1] <- NA
hybridaa$upper[,2] <- NA

return(hybridaa)
}





##############################################################
## Funções para previsão fora da amostra
##############################################################





##############################################################
##############################################################
##############################################################
##############################################################
## Forecasting with the best model
f_print_arima <- function(pib_go_ts,
                           Hor,
                           Start_Hor,
                           responses_series_level,
                        seed = seed,  variavel_forecasting=variavel_forecasting){

forecast_ahead <- forecast::auto.arima(pib_go_ts) %>% forecast::forecast(h=Hor)

############################################################
## forecast values
series_values <- as.vector(responses_series_level[,paste(variavel_forecasting)])
forecast_ahead_indice <- as.vector(cumsum(c(series_values[length(series_values)],as.vector(forecast_ahead$mean))))[-1]


# Manipulate forecast
date = format(seq.Date(from = as.Date(Start_Hor), by = 'month', length.out = Hor), format= "%b %Y")
df_forecasts <- data.frame(Date=date,Point_Forecast_Diff=forecast_ahead$mean,Point_Forecast_Ind=forecast_ahead_indice)

##############################################################

return(forecast_ahead_indice)
}


##############################################################
##############################################################
##############################################################
##############################################################
f_print_ets <- function(pib_go_ts,
                           Hor,
                           Start_Hor,
                           responses_series_level,
                        seed = seed,  variavel_forecasting=variavel_forecasting){

forecast_ahead <- forecast::ets(pib_go_ts, model="ZZZ") %>% forecast::forecast(h=Hor)


############################################################
## forecast values
series_values <- as.vector(responses_series_level[,paste(variavel_forecasting)])
forecast_ahead_indice <- as.vector(cumsum(c(series_values[length(series_values)],as.vector(forecast_ahead$mean))))[-1]


# Manipulate forecast
date = format(seq.Date(from = as.Date(Start_Hor), by = 'month', length.out = Hor), format= "%b %Y")
df_forecasts <- data.frame(Date=date,Point_Forecast_Diff=forecast_ahead$mean,Point_Forecast_Ind=forecast_ahead_indice)

##############################################################

return(forecast_ahead_indice)
}


##############################################################
##############################################################
##############################################################
##############################################################
f_print_bats <- function(pib_go_ts,
                           Hor,
                           Start_Hor,
                           responses_series_level,
                        seed = seed,  variavel_forecasting=variavel_forecasting){

forecast_ahead <- forecast::bats(pib_go_ts) %>% forecast::forecast(h=Hor)

############################################################
## forecast values
series_values <- as.vector(responses_series_level[,paste(variavel_forecasting)])
forecast_ahead_indice <- as.vector(cumsum(c(series_values[length(series_values)],as.vector(forecast_ahead$mean))))[-1]


# Manipulate forecast
date = format(seq.Date(from = as.Date(Start_Hor), by = 'month', length.out = Hor), format= "%b %Y")
df_forecasts <- data.frame(Date=date,Point_Forecast_Diff=forecast_ahead$mean,Point_Forecast_Ind=forecast_ahead_indice)

##############################################################


return(forecast_ahead_indice)
}

##############################################################
##############################################################
##############################################################
##############################################################
f_print_tbats <- function(pib_go_ts,
                           Hor,
                           Start_Hor,
                           responses_series_level,
                        seed = seed,  variavel_forecasting=variavel_forecasting){

forecast_ahead <- forecast::tbats(pib_go_ts) %>%
                  forecast::forecast(h=Hor)


############################################################
## forecast values
series_values <- as.vector(responses_series_level[,paste(variavel_forecasting)])
forecast_ahead_indice <- as.vector(cumsum(c(series_values[length(series_values)],as.vector(forecast_ahead$mean))))[-1]


# Manipulate forecast
date = format(seq.Date(from = as.Date(Start_Hor), by = 'month', length.out = Hor), format= "%b %Y")
df_forecasts <- data.frame(Date=date,Point_Forecast_Diff=forecast_ahead$mean,Point_Forecast_Ind=forecast_ahead_indice)

##############################################################

return(forecast_ahead_indice)
}


##############################################################
##############################################################
##############################################################
##############################################################
f_print_meanf <- function(pib_go_ts,
                           Hor,
                           Start_Hor,
                           responses_series_level,
                        seed = seed,  variavel_forecasting=variavel_forecasting){

forecast_ahead <- forecast::meanf(pib_go_ts, h=Hor)

############################################################
## forecast values
series_values <- as.vector(responses_series_level[,paste(variavel_forecasting)])
forecast_ahead_indice <- as.vector(cumsum(c(series_values[length(series_values)],as.vector(forecast_ahead$mean))))[-1]


# Manipulate forecast
date = format(seq.Date(from = as.Date(Start_Hor), by = 'month', length.out = Hor), format= "%b %Y")
df_forecasts <- data.frame(Date=date,Point_Forecast_Diff=forecast_ahead$mean,Point_Forecast_Ind=forecast_ahead_indice)

############################################################

return(forecast_ahead_indice)
}

##############################################################
##############################################################
##############################################################
##############################################################
f_print_naive <- function(pib_go_ts,
                           Hor,
                           Start_Hor,
                           responses_series_level,
                        seed = seed,  variavel_forecasting=variavel_forecasting){

forecast_ahead <- forecast::naive(pib_go_ts, h=Hor)


############################################################
## forecast values
series_values <- as.vector(responses_series_level[,paste(variavel_forecasting)])
forecast_ahead_indice <- as.vector(cumsum(c(series_values[length(series_values)],as.vector(forecast_ahead$mean))))[-1]


# Manipulate forecast
date = format(seq.Date(from = as.Date(Start_Hor), by = 'month', length.out = Hor), format= "%b %Y")
df_forecasts <- data.frame(Date=date,Point_Forecast_Diff=forecast_ahead$mean,Point_Forecast_Ind=forecast_ahead_indice)

##############################################################

return(forecast_ahead_indice)
}


##############################################################
##############################################################
##############################################################
##############################################################
f_print_snaive <- function(pib_go_ts,
                           Hor,
                           Start_Hor,
                           responses_series_level,
                        seed = seed,  variavel_forecasting=variavel_forecasting){

forecast_ahead <- forecast::snaive(pib_go_ts, h=Hor)


############################################################
## forecast values
series_values <- as.vector(responses_series_level[,paste(variavel_forecasting)])
forecast_ahead_indice <- as.vector(cumsum(c(series_values[length(series_values)],as.vector(forecast_ahead$mean))))[-1]


# Manipulate forecast
date = format(seq.Date(from = as.Date(Start_Hor), by = 'month', length.out = Hor), format= "%b %Y")
df_forecasts <- data.frame(Date=date,Point_Forecast_Diff=forecast_ahead$mean,Point_Forecast_Ind=forecast_ahead_indice)

##############################################################

return(forecast_ahead_indice)
}



##############################################################
##############################################################
##############################################################
##############################################################
f_print_elm <- function(pib_go_ts=pib_go_ts,
                        Hor=Hor,
                        Start_Hor=Start_Hor,
                        responses_series_level=responses_series_level,
                        covariates_Train_ts=covariates_Train_ts,
                        covariates_Test_ts=covariates_Test_ts,
                        seed = seed,  variavel_forecasting=variavel_forecasting,
                        regularization = TRUE){


set.seed(seed)
lassoFit <-  glmnet::cv.glmnet(x=model.matrix(~.-1,data=covariates_Train_ts), y=as.vector(pib_go_ts), family="gaussian",
                               intercept = FALSE,
                                alpha =1)
namesCoef <- rownames(coef(lassoFit, s = 'lambda.min'))[coef(lassoFit, s = 'lambda.min')[,1]!= 0] ### returns nonzero coefs
#print(namesCoef)
lfc <- as.vector(coef(lassoFit, s = 'lambda.min')[,1]!= 0)[-1]
if(regularization == TRUE) namesCoef <- colnames(covariates_Train_ts)[lfc]
if(regularization == FALSE) namesCoef <- colnames(covariates_Train_ts)##[lfc]
print(namesCoef)



X_reg_train <- as.matrix(as.data.frame(covariates_Train_ts[,which(colnames(covariates_Train_ts)%in% c(namesCoef))]))
X_reg_test <- as.matrix(as.data.frame(covariates_Test_ts[,which(colnames(covariates_Test_ts)%in% c(namesCoef))]))


forecast_ahead <- nnfor::elm(pib_go_ts, xreg = X_reg_train) %>%
                  forecast::forecast(h=Hor, xreg= rbind(X_reg_train,X_reg_test))


############################################################
## forecast values
series_values <- as.vector(responses_series_level[,paste(variavel_forecasting)])
forecast_ahead_indice <- as.vector(cumsum(c(series_values[length(series_values)],as.vector(forecast_ahead$mean))))[-1]


# Manipulate forecast
date = format(seq.Date(from = as.Date(Start_Hor), by = 'month', length.out = Hor), format= "%b %Y")
df_forecasts <- data.frame(Date=date,Point_Forecast_Diff=forecast_ahead$mean,Point_Forecast_Ind=forecast_ahead_indice)

##############################################################

return(forecast_ahead_indice)
}


##############################################################
##############################################################
##############################################################
##############################################################
f_print_wavelet <- function(pib_go_ts,
                           Hor,
                           Start_Hor,
                           responses_series_level,
                        seed = seed,  variavel_forecasting=variavel_forecasting){

simts <- ts(as.vector(pib_go_ts), start=c(start(pib_go_ts)[1],start(pib_go_ts)[2]), frequency = 12)
fit = WaveletArima::WaveletFittingarma(ts=simts,filter ='haar',Waveletlevels=floor(log(length(simts))),
MaxARParam=5,MaxMAParam=5,NForecast=Hor)
aa <- forecast::meanf(pib_go_ts,h=Hor)
aa$mean <- fit$Finalforecast
aa$lower[,1] <- NA
aa$lower[,2] <- NA
aa$upper[,1] <- NA
aa$upper[,2] <- NA
forecast_ahead <- aa


############################################################
## forecast values
series_values <- as.vector(responses_series_level[,paste(variavel_forecasting)])
forecast_ahead_indice <- as.vector(cumsum(c(series_values[length(series_values)],as.vector(forecast_ahead$mean))))[-1]


# Manipulate forecast
date = format(seq.Date(from = as.Date(Start_Hor), by = 'month', length.out = Hor), format= "%b %Y")
df_forecasts <- data.frame(Date=date,Point_Forecast_Diff=forecast_ahead$mean,Point_Forecast_Ind=forecast_ahead_indice)

##############################################################

return(forecast_ahead_indice)
}




##############################################################
##############################################################
##############################################################
##############################################################
f_print_lasso <- function(pib_go_ts=pib_go_ts,
                        Hor=Hor,
                        Start_Hor=Start_Hor,
                        responses_series_level=responses_series_level,
                        covariates_Train_ts=covariates_Train_ts,
                        covariates_Test_ts=covariates_Test_ts,
                        seed = seed,  variavel_forecasting=variavel_forecasting){


set.seed(seed) ## restore state just after set.seed()
lassoFit <-  glmnet::cv.glmnet(x=model.matrix(~.-1,data=covariates_Train_ts), y=as.vector(pib_go_ts), family="gaussian", intercept = FALSE,
                                alpha =1)

namesCoef <- rownames(coef(lassoFit, s = 'lambda.min'))[coef(lassoFit, s = 'lambda.min')[,1]!= 0] ### returns nonzero coefs
#print(namesCoef)
lfc <- as.vector(coef(lassoFit, s = 'lambda.min')[,1]!= 0)[-1]
namesCoef <- colnames(covariates_Train_ts)[lfc]
print(namesCoef)



lassoPred <- predict(lassoFit, newx = model.matrix(~.-1,data=covariates_Test_ts),s = lassoFit$lambda.min)
aa <- forecast::meanf(as.vector(lassoPred),h=Hor)
aa$mean <- as.vector(lassoPred)[1:Hor]
aa$lower[,1] <- NA
aa$lower[,2] <- NA
aa$upper[,1] <- NA
aa$upper[,2] <- NA

forecast_ahead <- aa

############################################################
## forecast values
series_values <- as.vector(responses_series_level[,paste(variavel_forecasting)])
forecast_ahead_indice <- as.vector(cumsum(c(series_values[length(series_values)],as.vector(forecast_ahead$mean))))[-1]


# Manipulate forecast
date = format(seq.Date(from = as.Date(Start_Hor), by = 'month', length.out = Hor), format= "%b %Y")
df_forecasts <- data.frame(Date=date,Point_Forecast_Diff=forecast_ahead$mean,Point_Forecast_Ind=forecast_ahead_indice)

##############################################################

return(forecast_ahead_indice)
}


##############################################################
##############################################################
##############################################################
##############################################################
f_print_sarimax <- function(pib_go_ts=pib_go_ts,
                        Hor=Hor,
                        Start_Hor=Start_Hor,
                        responses_series_level=responses_series_level,
                        covariates_Train_ts=covariates_Train_ts,
                        covariates_Test_ts=covariates_Test_ts,
                        seed = seed,  variavel_forecasting=variavel_forecasting,
                        regularization = TRUE){

set.seed(seed)
lassoFit <-  glmnet::cv.glmnet(x=model.matrix(~.-1,data=covariates_Train_ts), y=as.vector(pib_go_ts), family="gaussian",
                               intercept = FALSE,
                                alpha =1)
namesCoef <- rownames(coef(lassoFit, s = 'lambda.min'))[coef(lassoFit, s = 'lambda.min')[,1]!= 0] ### returns nonzero coefs
#print(namesCoef)
lfc <- as.vector(coef(lassoFit, s = 'lambda.min')[,1]!= 0)[-1]
if(regularization == TRUE) namesCoef <- colnames(covariates_Train_ts)[lfc]
if(regularization == FALSE) namesCoef <- colnames(covariates_Train_ts)##[lfc]
print(namesCoef)


X_reg_train <- as.matrix(as.data.frame(covariates_Train_ts[,which(colnames(covariates_Train_ts)%in% c(namesCoef))]))
X_reg_test <- as.matrix(as.data.frame(covariates_Test_ts[,which(colnames(covariates_Test_ts)%in% c(namesCoef))]))


forecast_ahead <- forecast::auto.arima(pib_go_ts, xreg = X_reg_train) %>%
                  forecast::forecast(h=Hor, xreg= X_reg_test[1:Hor,])




############################################################
## forecast values
series_values <- as.vector(responses_series_level[,paste(variavel_forecasting)])
forecast_ahead_indice <- as.vector(cumsum(c(series_values[length(series_values)],as.vector(forecast_ahead$mean))))[-1]


# Manipulate forecast
date = format(seq.Date(from = as.Date(Start_Hor), by = 'month', length.out = Hor), format= "%b %Y")
df_forecasts <- data.frame(Date=date,Point_Forecast_Diff=forecast_ahead$mean,Point_Forecast_Ind=forecast_ahead_indice)

##############################################################




return(forecast_ahead_indice)
}



##############################################################
##############################################################
##############################################################
##############################################################
f_print_gbm <- function(pib_go_ts=pib_go_ts,
                        Hor=Hor,
                        Start_Hor=Start_Hor,
                        responses_series_level=responses_series_level,
                        covariates_Train_ts=covariates_Train_ts,
                        covariates_Test_ts=covariates_Test_ts,
                        seed = seed,  variavel_forecasting=variavel_forecasting,
                        regularization = TRUE){

set.seed(seed) ## restore state just after set.seed()
lassoFit <-  glmnet::cv.glmnet(x=model.matrix(~.-1,data=covariates_Train_ts), y=as.vector(pib_go_ts), family="gaussian",
                               intercept = FALSE,
                                alpha =1)
namesCoef <- rownames(coef(lassoFit, s = 'lambda.min'))[coef(lassoFit, s = 'lambda.min')[,1]!= 0] ### returns nonzero coefs
#print(namesCoef)
lfc <- as.vector(coef(lassoFit, s = 'lambda.min')[,1]!= 0)[-1]
if(regularization == TRUE) namesCoef <- colnames(covariates_Train_ts)[lfc]
if(regularization == FALSE) namesCoef <- colnames(covariates_Train_ts)##[lfc]
print(namesCoef)


X_reg_train <- as.matrix(as.data.frame(covariates_Train_ts[,which(colnames(covariates_Train_ts)%in% c(namesCoef))]))
X_reg_test <- as.matrix(as.data.frame(covariates_Test_ts[,which(colnames(covariates_Test_ts)%in% c(namesCoef))]))

# Define trainControl with cross-validation
trControl <- trainControl(
  method = "cv",          # Cross-validation
  number = 10,            # 10-fold cross-validation
  summaryFunction = defaultSummary,  # Default regression metrics (RMSE, R-squared)
  verboseIter = FALSE,     # Print training progress
  returnResamp = "final", # Save results for the final model only
  savePredictions = "final" # Save predictions for the best model
)

## Define a custom grid (tuneGrid) to tune (hyperparameters)
tuneGrid <- expand.grid(
  n.trees = c(100, 200, 300), ## Number of boosting iterations.
  interaction.depth = c(1, 3, 5), ## Maximum depth of trees
  shrinkage = c(0.01, 0.1), ## Learning rate.
  n.minobsinnode = c(5, 10) ## Minimum number of observations in terminal nodes
)

set.seed(seed) ## restore state just after set.seed()
gbmFit <- caret::train(x=X_reg_train,
                       y=as.vector(pib_go_ts),
                       method = "gbm",
                       trControl = trControl,
                       tuneGrid = tuneGrid,
                       verbose = FALSE # Suppress GBM output
                       )

gbmPred <- predict(gbmFit, newdata = X_reg_test )
aa <- forecast::meanf(as.vector(gbmPred),h=Hor)
aa$mean <- as.vector(gbmPred)[1:Hor]
aa$lower[,1] <- NA
aa$lower[,2] <- NA
aa$upper[,1] <- NA
aa$upper[,2] <- NA

forecast_ahead <- aa

############################################################
## forecast values
series_values <- as.vector(responses_series_level[,paste(variavel_forecasting)])
forecast_ahead_indice <- as.vector(cumsum(c(series_values[length(series_values)],as.vector(forecast_ahead$mean))))[-1]


# Manipulate forecast
date = format(seq.Date(from = as.Date(Start_Hor), by = 'month', length.out = Hor), format= "%b %Y")
df_forecasts <- data.frame(Date=date,Point_Forecast_Diff=forecast_ahead$mean,Point_Forecast_Ind=forecast_ahead_indice)

##############################################################

return(forecast_ahead_indice)
}








##############################################################
##############################################################
##############################################################
##############################################################
f_print_xgboost <- function(pib_go_ts=pib_go_ts,
                        Hor=Hor,
                        Start_Hor=Start_Hor,
                        responses_series_level=responses_series_level,
                        covariates_Train_ts=covariates_Train_ts,
                        covariates_Test_ts=covariates_Test_ts,
                        seed = seed,  variavel_forecasting=variavel_forecasting,
                        regularization = TRUE){

set.seed(seed) ## restore state just after set.seed()
lassoFit <-  glmnet::cv.glmnet(x=model.matrix(~.-1,data=covariates_Train_ts), y=as.vector(pib_go_ts), family="gaussian",
                               intercept = FALSE,
                                alpha =1)
namesCoef <- rownames(coef(lassoFit, s = 'lambda.min'))[coef(lassoFit, s = 'lambda.min')[,1]!= 0] ### returns nonzero coefs
#print(namesCoef)
lfc <- as.vector(coef(lassoFit, s = 'lambda.min')[,1]!= 0)[-1]
if(regularization == TRUE) namesCoef <- colnames(covariates_Train_ts)[lfc]
if(regularization == FALSE) namesCoef <- colnames(covariates_Train_ts)##[lfc]
print(namesCoef)


X_reg_train <- as.matrix(as.data.frame(covariates_Train_ts[,which(colnames(covariates_Train_ts)%in% c(namesCoef))]))
X_reg_test <- as.matrix(as.data.frame(covariates_Test_ts[,which(colnames(covariates_Test_ts)%in% c(namesCoef))]))

# # Define trainControl with cross-validation
# trControl <- trainControl(
#   method = "cv",                # Cross-validation
#   number = 5,                   # Number of folds
#   verboseIter = FALSE,           # Show progress during training
#   search = "grid"              # Use grid search for tuning
# )
#
# ## Define a custom grid (tuneGrid) to tune (hyperparameters)
# tuneGrid <- expand.grid(
#   nrounds = c(50, 100, 150),           # Number of boosting iterations
#   max_depth = c(5, 10, 15, 20),              # Depth of trees
#   eta = c(0.01, 0.1, 0.3),             # Learning rate
#   gamma = c(0, 1, 5),                  # Minimum loss reduction
#   colsample_bytree = c(0.7, 0.8, 0.9, 1.0),   # Feature sampling ratio
#   min_child_weight = c(1, 5, 10),      # Minimum sum of instance weight
#   subsample = c(0.6, 0.8, 1)           # Subsampling ratio
# )

trControl <- trainControl(
  method = "cv",
  number = 5,
  search = "random"   # Random search
)

# No need to define a full `tuneGrid`; specify ranges instead
tuneGrid <- NULL  # caret automatically samples random combinations

set.seed(seed) ## restore state just after set.seed()
xgboostFit <- caret::train(x=X_reg_train,
                       y=as.vector(pib_go_ts),
                       method = "xgbTree",
                       trControl = trControl,
                       tuneGrid = tuneGrid,
                       verbose = FALSE,
                        verbosity = 0)# Suppress xgboost output


xgboostPred <- predict(xgboostFit, newdata = X_reg_test )
aa <- forecast::meanf(as.vector(xgboostPred),h=Hor)
aa$mean <- as.vector(xgboostPred)[1:Hor]
aa$lower[,1] <- NA
aa$lower[,2] <- NA
aa$upper[,1] <- NA
aa$upper[,2] <- NA

forecast_ahead <- aa

############################################################
## forecast values
series_values <- as.vector(responses_series_level[,paste(variavel_forecasting)])
forecast_ahead_indice <- as.vector(cumsum(c(series_values[length(series_values)],as.vector(forecast_ahead$mean))))[-1]


# Manipulate forecast
date = format(seq.Date(from = as.Date(Start_Hor), by = 'month', length.out = Hor), format= "%b %Y")
df_forecasts <- data.frame(Date=date,Point_Forecast_Diff=forecast_ahead$mean,Point_Forecast_Ind=forecast_ahead_indice)

##############################################################

return(forecast_ahead_indice)
}




##############################################################
##############################################################
##############################################################
##############################################################

f_print_rf <- function(pib_go_ts=pib_go_ts,
                        Hor=Hor,
                        Start_Hor=Start_Hor,
                        responses_series_level=responses_series_level,
                        covariates_Train_ts=covariates_Train_ts,
                        covariates_Test_ts=covariates_Test_ts,
                        seed = seed,  variavel_forecasting=variavel_forecasting,
                        regularization = TRUE){

set.seed(seed) ## restore state just after set.seed()
lassoFit <-  glmnet::cv.glmnet(x=model.matrix(~.-1,data=covariates_Train_ts), y=as.vector(pib_go_ts), family="gaussian",
                               intercept = FALSE,
                                alpha =1)
namesCoef <- rownames(coef(lassoFit, s = 'lambda.min'))[coef(lassoFit, s = 'lambda.min')[,1]!= 0] ### returns nonzero coefs
#print(namesCoef)
lfc <- as.vector(coef(lassoFit, s = 'lambda.min')[,1]!= 0)[-1]
if(regularization == TRUE) namesCoef <- colnames(covariates_Train_ts)[lfc]
if(regularization == FALSE) namesCoef <- colnames(covariates_Train_ts)##[lfc]
print(namesCoef)


X_reg_train <- as.matrix(as.data.frame(covariates_Train_ts[,which(colnames(covariates_Train_ts)%in% c(namesCoef))]))
X_reg_test <- as.matrix(as.data.frame(covariates_Test_ts[,which(colnames(covariates_Test_ts)%in% c(namesCoef))]))

trControl <- trainControl(
  method = "cv",           # Cross-validation (alternatively, use "repeatedcv" or "boot")
  number = 10,             # Number of folds (e.g., 10-fold CV)
  search = "grid",         # Grid search for hyperparameter tuning
  savePredictions = "final",  # Save predictions for the final model
  classProbs = FALSE       # Include probabilities for classification problems
)


tuneGrid <- expand.grid(
  mtry = seq(2, ifelse(sqrt(ncol(X_reg_train))<2,2,sqrt(ncol(X_reg_train))), by = 1) # Adjust based on feature count
)


set.seed(seed) ## restore state just after set.seed()
rfFit <- caret::train(x=X_reg_train,
                      y=as.vector(pib_go_ts),
                      method = "rf" ,
                      trControl = trControl,   # Training control
                      tuneGrid = tuneGrid     # Tuning grid
                      )
rfPred <- predict(rfFit, newdata = X_reg_test )
aa <- forecast::meanf(as.vector(rfPred),h=Hor)
aa$mean <- as.vector(rfPred)[1:Hor]
aa$lower[,1] <- NA
aa$lower[,2] <- NA
aa$upper[,1] <- NA
aa$upper[,2] <- NA

forecast_ahead <- aa

############################################################
## forecast values
series_values <- as.vector(responses_series_level[,paste(variavel_forecasting)])
forecast_ahead_indice <- as.vector(cumsum(c(series_values[length(series_values)],as.vector(forecast_ahead$mean))))[-1]


# Manipulate forecast
date = format(seq.Date(from = as.Date(Start_Hor), by = 'month', length.out = Hor), format= "%b %Y")
df_forecasts <- data.frame(Date=date,Point_Forecast_Diff=forecast_ahead$mean,Point_Forecast_Ind=forecast_ahead_indice)

##############################################################


return(forecast_ahead_indice)
}




##############################################################
##############################################################
##############################################################
##############################################################
f_print_pcr <- function(pib_go_ts=pib_go_ts,
                        Hor=Hor,
                        Start_Hor=Start_Hor,
                        responses_series_level=responses_series_level,
                        covariates_Train_ts=covariates_Train_ts,
                        covariates_Test_ts=covariates_Test_ts,
                        seed = seed,  variavel_forecasting=variavel_forecasting){

X_reg_train <- as.matrix(as.data.frame(covariates_Train_ts))
X_reg_test <- as.matrix(as.data.frame(covariates_Test_ts))

trControl <- trainControl(
  method = "cv",           # Cross-validation (e.g., 10-fold CV)
  number = 10,             # Number of folds
  savePredictions = "final",  # Save predictions for the final model
  verboseIter = FALSE        # Show progress during training
)

tuneGrid <- expand.grid(
  ncomp = seq(1, min(ncol(X_reg_train) - 1, 20)) # Number of components to test
)

set.seed(seed) ## restore state just after set.seed()
pcrFit <- caret::train(x=X_reg_train,
                       y=as.vector(pib_go_ts),
                       method = "pcr",
                       trControl = trControl,   # Training control settings
                       tuneGrid = tuneGrid      # Grid of `ncomp` values)
                      )

pcrPred <- predict(pcrFit, newdata = X_reg_test )
aa <- forecast::meanf(as.vector(pcrPred),h=Hor)
aa$mean <- as.vector(pcrPred)[1:Hor]
aa$lower[,1] <- NA
aa$lower[,2] <- NA
aa$upper[,1] <- NA
aa$upper[,2] <- NA

forecast_ahead <- aa

############################################################
## forecast values
series_values <- as.vector(responses_series_level[,paste(variavel_forecasting)])
forecast_ahead_indice <- as.vector(cumsum(c(series_values[length(series_values)],as.vector(forecast_ahead$mean))))[-1]


# Manipulate forecast
date = format(seq.Date(from = as.Date(Start_Hor), by = 'month', length.out = Hor), format= "%b %Y")
df_forecasts <- data.frame(Date=date,Point_Forecast_Diff=forecast_ahead$mean,Point_Forecast_Ind=forecast_ahead_indice)

##############################################################

return(forecast_ahead_indice)
}












##############################################################
##############################################################
##############################################################
##############################################################
f_print_mars <- function(pib_go_ts=pib_go_ts,
                        Hor=Hor,
                        Start_Hor=Start_Hor,
                        responses_series_level=responses_series_level,
                        covariates_Train_ts=covariates_Train_ts,
                        covariates_Test_ts=covariates_Test_ts,
                        seed = seed,  variavel_forecasting=variavel_forecasting,
                        regularization = TRUE){

set.seed(seed) ## restore state just after set.seed()
lassoFit <-  glmnet::cv.glmnet(x=model.matrix(~.-1,data=covariates_Train_ts), y=as.vector(pib_go_ts), family="gaussian",
                               intercept = FALSE,
                                alpha =1)
namesCoef <- rownames(coef(lassoFit, s = 'lambda.min'))[coef(lassoFit, s = 'lambda.min')[,1]!= 0] ### returns nonzero coefs
#print(namesCoef)
lfc <- as.vector(coef(lassoFit, s = 'lambda.min')[,1]!= 0)[-1]
if(regularization == TRUE) namesCoef <- colnames(covariates_Train_ts)[lfc]
if(regularization == FALSE) namesCoef <- colnames(covariates_Train_ts)##[lfc]
print(namesCoef)



X_reg_train <- as.matrix(as.data.frame(covariates_Train_ts[,which(colnames(covariates_Train_ts)%in% c(namesCoef))]))
X_reg_test <- as.matrix(as.data.frame(covariates_Test_ts[,which(colnames(covariates_Test_ts)%in% c(namesCoef))]))


trControl <- trainControl(
  method = "cv",
  number = 10
)

tuneGrid <- expand.grid(
  degree = c(1, 2, 3)#,
  #nprune = seq(2, min(30, (ncol(X_reg_train) - 1) * 2), by = 1)
)

set.seed(seed) ## restore state just after set.seed()
bmarsgcvFit <- caret::train(x=X_reg_train,
                            y=as.vector(pib_go_ts),
                            method = "bagEarthGCV",
                            tuneGrid = tuneGrid,
                            trControl = trControl
                            )

bmarsgcvPred <- predict(bmarsgcvFit, newdata = X_reg_test )
bmarsgcvaa <- forecast::meanf(as.vector(bmarsgcvPred),h=Hor)
bmarsgcvaa$mean <- as.vector(bmarsgcvPred)[1:Hor]
bmarsgcvaa$lower[,1] <- NA
bmarsgcvaa$lower[,2] <- NA
bmarsgcvaa$upper[,1] <- NA
bmarsgcvaa$upper[,2] <- NA

forecast_ahead <- bmarsgcvaa

############################################################
## forecast values
series_values <- as.vector(responses_series_level[,paste(variavel_forecasting)])
forecast_ahead_indice <- as.vector(cumsum(c(series_values[length(series_values)],as.vector(forecast_ahead$mean))))[-1]


# Manipulate forecast
date = format(seq.Date(from = as.Date(Start_Hor), by = 'month', length.out = Hor), format= "%b %Y")
df_forecasts <- data.frame(Date=date,Point_Forecast_Diff=forecast_ahead$mean,Point_Forecast_Ind=forecast_ahead_indice)

##############################################################

return(forecast_ahead_indice)
}




##############################################################
##############################################################
##############################################################
##############################################################

f_print_hybrid <- function(pib_go_ts=pib_go_ts,
                        Hor=Hor,
                        Start_Hor=Start_Hor,
                        responses_series_level=responses_series_level,
                        covariates_Train_ts=covariates_Train_ts,
                        covariates_Test_ts=covariates_Test_ts,
                        seed = seed,  variavel_forecasting=variavel_forecasting,
                        regularization = TRUE){

## bats
batsaa <- forecast::bats(pib_go_ts) %>% forecast::forecast(h=Hor)

## tbats
tbatsaa <- forecast::tbats(pib_go_ts) %>% forecast::forecast(h=Hor)

## snaive
snaiveaa <- forecast::snaive(pib_go_ts, h=Hor)

## Arima
arimaaa = forecast::auto.arima(pib_go_ts) %>% forecast::forecast(Hor)

## Ets
etsaa = forecast::ets(pib_go_ts, model="ZZZ")  %>% forecast::forecast(Hor)

## Wavelet-arima
simts <- ts(as.vector(pib_go_ts), start=c(start(pib_go_ts)[1],start(pib_go_ts)[2]), frequency = 12)
fitwaveletarima = WaveletArima::WaveletFittingarma(ts=simts,filter ='haar',Waveletlevels=floor(log(length(simts))),
MaxARParam=5,MaxMAParam=5,NForecast=Hor)
waveletarimaaa <- forecast::meanf(pib_go_ts,h=Hor)
waveletarimaaa$mean <- fitwaveletarima$Finalforecast
waveletarimaaa$lower[,1] <- NA
waveletarimaaa$lower[,2] <- NA
waveletarimaaa$upper[,1] <- NA
waveletarimaaa$upper[,2] <- NA


## Lasso
set.seed(seed) ## restore state just after set.seed()
lassoFit <-  glmnet::cv.glmnet(x=model.matrix(~.-1,data=covariates_Train_ts), y=as.vector(pib_go_ts), family="gaussian", intercept = FALSE,
                                alpha =1)
lassoPred <- predict(lassoFit, newx =model.matrix(~.-1,data=covariates_Test_ts),s = lassoFit$lambda.min)
lassoaa <- forecast::meanf(as.vector(lassoPred),h=Hor)
lassoaa$mean <- as.vector(lassoPred)[1:Hor]
lassoaa$lower[,1] <- NA
lassoaa$lower[,2] <- NA
lassoaa$upper[,1] <- NA
lassoaa$upper[,2] <- NA



## covariates
namesCoef <- rownames(coef(lassoFit, s = 'lambda.min'))[coef(lassoFit, s = 'lambda.min')[,1]!= 0] ### returns nonzero coefs
#print(namesCoef)
lfc <- as.vector(coef(lassoFit, s = 'lambda.min')[,1]!= 0)[-1]
if(regularization == TRUE) namesCoef <- colnames(covariates_Train_ts)[lfc]
if(regularization == FALSE) namesCoef <- colnames(covariates_Train_ts)##[lfc]
print(namesCoef)

X_reg_train <- as.matrix(as.data.frame(covariates_Train_ts[,which(colnames(covariates_Train_ts)%in% c(namesCoef))]))
X_reg_test <- as.matrix(as.data.frame(covariates_Test_ts[,which(colnames(covariates_Test_ts)%in% c(namesCoef))]))


## elm
elmaa <- nnfor::elm(pib_go_ts, xreg = X_reg_train) %>%
                  forecast::forecast(h=Hor, xreg= rbind(X_reg_train,X_reg_test))

## Sarima-lasso
sarimalassoaa <- forecast::auto.arima(pib_go_ts, xreg = X_reg_train ) %>%
                  forecast::forecast(h=Hor, xreg= X_reg_test[1:Hor,])



## GBM

# Define trainControl with cross-validation
trControl <- trainControl(
  method = "cv",          # Cross-validation
  number = 10,            # 10-fold cross-validation
  summaryFunction = defaultSummary,  # Default regression metrics (RMSE, R-squared)
  verboseIter = FALSE,     # Print training progress
  returnResamp = "final", # Save results for the final model only
  savePredictions = "final" # Save predictions for the best model
)

## Define a custom grid (tuneGrid) to tune (hyperparameters)
tuneGrid <- expand.grid(
  n.trees = c(100, 200, 300), ## Number of boosting iterations.
  interaction.depth = c(1, 3, 5), ## Maximum depth of trees
  shrinkage = c(0.01, 0.1), ## Learning rate.
  n.minobsinnode = c(5, 10) ## Minimum number of observations in terminal nodes
)

set.seed(seed) ## restore state just after set.seed()
gbmFit <- caret::train(x=X_reg_train,
                       y=as.vector(pib_go_ts),
                       method = "gbm",
                       trControl = trControl,
                       tuneGrid = tuneGrid,
                       verbose = FALSE)

gbmPred <- predict(gbmFit, newdata = X_reg_test)
gbmaa <- forecast::meanf(as.vector(gbmPred),h=Hor)
gbmaa$mean <- as.vector(gbmPred)[1:Hor]
gbmaa$lower[,1] <- NA
gbmaa$lower[,2] <- NA
gbmaa$upper[,1] <- NA
gbmaa$upper[,2] <- NA



## XGBOOST

# # Define trainControl with cross-validation
# trControl <- trainControl(
#   method = "cv",                # Cross-validation
#   number = 5,                   # Number of folds
#   verboseIter = FALSE,           # Show progress during training
#   search = "grid"              # Use grid search for tuning
# )
#
# ## Define a custom grid (tuneGrid) to tune (hyperparameters)
# tuneGrid <- expand.grid(
#   nrounds = c(50, 100, 150),           # Number of boosting iterations
#   max_depth = c(5, 10, 15, 20),        # Depth of trees
#   eta = c(0.01, 0.1, 0.3),             # Learning rate
#   gamma = c(0, 1, 5),                  # Minimum loss reduction
#   colsample_bytree = c(0.7, 0.8, 0.9, 1.0),   # Feature sampling ratio
#   min_child_weight = c(1, 5, 10),      # Minimum sum of instance weight
#   subsample = c(0.6, 0.8, 1)           # Subsampling ratio
# )

trControl <- trainControl(
  method = "cv",
  number = 5,
  search = "random"   # Random search
)

# No need to define a full `tuneGrid`; specify ranges instead
tuneGrid <- NULL  # caret automatically samples random combinations

set.seed(seed) ## restore state just after set.seed()
xgboostFit <- caret::train(x=X_reg_train,
                       y=as.vector(pib_go_ts),
                       method = "xgbTree",
                       trControl = trControl,
                       tuneGrid = tuneGrid,
                       verbose = FALSE,
                        verbosity = 0 # Suppress xgboost output
                       )

xgboostPred <- predict(xgboostFit, newdata = X_reg_test)
xgboostaa <- forecast::meanf(as.vector(xgboostPred),h=Hor)
xgboostaa$mean <- as.vector(xgboostPred)[1:Hor]
xgboostaa$lower[,1] <- NA
xgboostaa$lower[,2] <- NA
xgboostaa$upper[,1] <- NA
xgboostaa$upper[,2] <- NA





## Random Forests

trControl <- trainControl(
  method = "cv",           # Cross-validation (alternatively, use "repeatedcv" or "boot")
  number = 10,             # Number of folds (e.g., 10-fold CV)
  search = "grid",         # Grid search for hyperparameter tuning
  savePredictions = "final",  # Save predictions for the final model
  classProbs = FALSE       # Include probabilities for classification problems
)


tuneGrid <- expand.grid(
  mtry = seq(2, ifelse(sqrt(ncol(X_reg_train))<2,2,sqrt(ncol(X_reg_train))), by = 1) # Adjust based on feature count
)


set.seed(seed) ## restore state just after set.seed()
rfFit <- caret::train(x=X_reg_train,
                      y=as.vector(pib_go_ts),
                      method = "rf",
                     trControl = trControl,   # Training control
                      tuneGrid = tuneGrid     # Tuning grid
                     )

rfPred <- predict(rfFit, newdata = X_reg_test)
rfaa <- forecast::meanf(as.vector(rfPred),h=Hor)
rfaa$mean <- as.vector(rfPred)[1:Hor]
rfaa$lower[,1] <- NA
rfaa$lower[,2] <- NA
rfaa$upper[,1] <- NA
rfaa$upper[,2] <- NA


## PCR

X_reg_train <- as.matrix(as.data.frame(covariates_Train_ts))
X_reg_test <- as.matrix(as.data.frame(covariates_Test_ts))


trControl <- trainControl(
  method = "cv",           # Cross-validation (e.g., 10-fold CV)
  number = 10,             # Number of folds
  savePredictions = "final",  # Save predictions for the final model
  verboseIter = FALSE        # Show progress during training
)

tuneGrid <- expand.grid(
  ncomp = seq(1, min(ncol(X_reg_train) - 1, 20)) # Number of components to test
)

set.seed(seed) ## restore state just after set.seed()
pcrFit <- caret::train(x=X_reg_train,
                       y=as.vector(pib_go_ts),
                       method = "pcr",
                       trControl = trControl,   # Training control settings
                       tuneGrid = tuneGrid      # Grid of `ncomp` values)
                      )

pcrPred <- predict(pcrFit, newdata = X_reg_test )
pcraa <- forecast::meanf(as.vector(pcrPred),h=Hor)
pcraa$mean <- as.vector(pcrPred)[1:Hor]
pcraa$lower[,1] <- NA
pcraa$lower[,2] <- NA
pcraa$upper[,1] <- NA
pcraa$upper[,2] <- NA


## bagEarthGCV

trControl <- trainControl(
  method = "cv",
  number = 10
)

tuneGrid <- expand.grid(
  degree = c(1, 2, 3)#,
  #nprune = seq(2, min(30, (ncol(X_reg_train) - 1) * 2), by = 1)
)

set.seed(seed) ## restore state just after set.seed()
bmarsgcvFit <- caret::train(x=X_reg_train,
                            y=as.vector(pib_go_ts),
                            method = "bagEarthGCV" ,
                            tuneGrid = tuneGrid,
                            trControl = trControl)

bmarsgcvPred <- predict(bmarsgcvFit, newdata = X_reg_test)
bmarsgcvaa <- forecast::meanf(as.vector(bmarsgcvPred),h=Hor)
bmarsgcvaa$mean <- as.vector(bmarsgcvPred)[1:Hor]
bmarsgcvaa$lower[,1] <- NA
bmarsgcvaa$lower[,2] <- NA
bmarsgcvaa$upper[,1] <- NA
bmarsgcvaa$upper[,2] <- NA




## Observed values
#Y_observed <- as.vector(window(pib_go_ts,
#  start=c(start(arimaaa$mean)[1],start(arimaaa$mean)[2]),
#  end=c(end(arimaaa$mean)[1],end(arimaaa$mean)[2])))
##as.vector(y[(InitPer+1):(InitPer+h)])

date = seq.Date(from = as.Date(Start_Hor), by = 'month', length.out = Hor)

D_bats = data.frame(date, forecast.date=NA, model = "bats", forecast = as.vector(batsaa$mean), se=NA, observed =NA)
D_tbats = data.frame(date, forecast.date=NA, model = "tbats", forecast = as.vector(tbatsaa$mean), se=NA, observed =NA)
D_sarima = data.frame(date, forecast.date=NA, model = "sarima", forecast = as.vector(arimaaa$mean), se=NA, observed = NA)
D_waveletarima = data.frame(date, forecast.date=NA, model = "sarima", forecast = as.vector(waveletarimaaa$mean), se=NA, observed = NA)
D_ets = data.frame(date, forecast.date=NA, model = "ets", forecast = as.vector(etsaa$mean), se=NA, observed = NA)
D_lasso  = data.frame(date, forecast.date=NA,model = "lasso", forecast = as.vector(lassoaa$mean), se=NA,observed = NA)
D_elm = data.frame(date, forecast.date=NA, model = "elm", forecast = as.vector(elmaa$mean), se=NA, observed =NA)
D_sarimalasso  = data.frame(date, forecast.date=NA,model = "sarimalasso", forecast = as.vector(sarimalassoaa$mean), se=NA,observed = NA)
D_gbm  = data.frame(date, forecast.date=NA,model = "gbm", forecast = as.vector(gbmaa$mean), se=NA,observed = NA)
D_xgboost  = data.frame(date, forecast.date=NA,model = "xgbTree", forecast = as.vector(xgboostaa$mean), se=NA,observed = NA)
D_rf  = data.frame(date, forecast.date=NA,model = "rf", forecast = as.vector(rfaa$mean), se=NA,observed = NA)
D_pcr  = data.frame(date, forecast.date=NA,model = "pcr", forecast = as.vector(pcraa$mean), se=NA,observed = NA)
D_bagged_mars_gcv  = data.frame(date, forecast.date=NA,model = "bagged_mars_gcv", forecast = as.vector(bmarsgcvaa$mean), se=NA,observed = NA)


D_forecasts_all <- rbind.data.frame(D_bats,D_tbats,
                                    D_sarima,D_ets,D_waveletarima,
                                    D_lasso,D_elm,D_sarimalasso,D_gbm,
                                    D_xgboost,D_rf,
                                    D_pcr,D_bagged_mars_gcv)

combinations_all <- D_forecasts_all %>%
  group_by(date)  %>%
  summarise(mean_forecast = mean(forecast))

# # combine forecasts
#  combinations_all  =
#    OOS::forecast_combine(
#      D_forecasts_all,
#      method = c('uniform'),
#      burn.in = 5,
#      n.max = 2)


hybridaa <- forecast::meanf(as.vector(combinations_all$mean_forecast),h=Hor)
hybridaa$mean <- as.vector(combinations_all$mean_forecast)
hybridaa$lower[,1] <- NA
hybridaa$lower[,2] <- NA
hybridaa$upper[,1] <- NA
hybridaa$upper[,2] <- NA
hybridaa



forecast_ahead <- hybridaa

############################################################
## forecast values
series_values <- as.vector(responses_series_level[,paste(variavel_forecasting)])
forecast_ahead_indice <- as.vector(cumsum(c(series_values[length(series_values)],as.vector(forecast_ahead$mean))))[-1]


# Manipulate forecast
date = format(seq.Date(from = as.Date(Start_Hor), by = 'month', length.out = Hor), format= "%b %Y")
df_forecasts <- data.frame(Date=date,Point_Forecast_Diff=forecast_ahead$mean,Point_Forecast_Ind=forecast_ahead_indice)

##############################################################


return(forecast_ahead_indice)
}






##############################################################
##############################################################
##############################################################
##############################################################
##############################################################
##############################################################
##############################################################
##############################################################
##############################################################
##############################################################
##############################################################
##############################################################


modelType_forecasts <- function(pib_go_ts=pib_go_ts,
                                           Hor=Hor,
                                           Start_Hor=Start_Hor,
                                           responses_series_level=responses_series_level,
                                           covariates_Train_ts=covariates_Train_ts,
                                           covariates_Test_ts=covariates_Test_ts,
                                           modelType = "sarimax",
                                           seed,
                                           variavel_forecasting=variavel_forecasting,
                                           regularization = TRUE){



CV_for <- switch(modelType,
                arima     = f_print_arima(pib_go_ts=pib_go_ts,
                                           Hor=Hor,
                                           Start_Hor=Start_Hor,
                                           responses_series_level=responses_series_level,
                                           seed = seed,  variavel_forecasting=variavel_forecasting),
                ets        = f_print_ets(pib_go_ts=pib_go_ts,
                                           Hor=Hor,
                                           Start_Hor=Start_Hor,
                                           responses_series_level=responses_series_level,
                                           seed = seed,  variavel_forecasting=variavel_forecasting),
                bats       = f_print_bats(pib_go_ts=pib_go_ts,
                                           Hor=Hor,
                                           Start_Hor=Start_Hor,
                                           responses_series_level=responses_series_level,
                                           seed = seed,  variavel_forecasting=variavel_forecasting),
               tbats      = f_print_tbats(pib_go_ts=pib_go_ts,
                                           Hor=Hor,
                                           Start_Hor=Start_Hor,
                                           responses_series_level=responses_series_level,
                                           seed = seed,  variavel_forecasting=variavel_forecasting),
                thetaf     = f_print_thetaf(pib_go_ts=pib_go_ts,
                                           Hor=Hor,
                                           Start_Hor=Start_Hor,
                                           responses_series_level=responses_series_level,
                                           seed = seed,  variavel_forecasting=variavel_forecasting),
                meanf      = f_print_meanf(pib_go_ts=pib_go_ts,
                                           Hor=Hor,
                                           Start_Hor=Start_Hor,
                                           responses_series_level=responses_series_level,
                                           seed = seed,  variavel_forecasting=variavel_forecasting),
                naive      = f_print_naive(pib_go_ts=pib_go_ts,
                                           Hor=Hor,
                                           Start_Hor=Start_Hor,
                                           responses_series_level=responses_series_level,
                                           seed = seed,  variavel_forecasting=variavel_forecasting),
                snaive     =  f_print_snaive(pib_go_ts=pib_go_ts,
                                           Hor=Hor,
                                           Start_Hor=Start_Hor,
                                           responses_series_level=responses_series_level,
                                           seed = seed,  variavel_forecasting=variavel_forecasting),
                elm        =  f_print_elm(pib_go_ts=pib_go_ts,
                                           Hor=Hor,
                                           Start_Hor=Start_Hor,
                                           responses_series_level=responses_series_level,
                                           covariates_Train_ts=covariates_Train_ts,
                                           covariates_Test_ts=covariates_Test_ts,
                                           seed = seed,  variavel_forecasting=variavel_forecasting,
                                           regularization = regularization),
                wavelet =  f_print_wavelet(pib_go_ts=pib_go_ts,
                                           Hor=Hor,
                                           Start_Hor=Start_Hor,
                                           responses_series_level=responses_series_level,
                                           seed = seed,  variavel_forecasting=variavel_forecasting),
                lasso      =  f_print_lasso(pib_go_ts=pib_go_ts,
                                           Hor=Hor,
                                           Start_Hor=Start_Hor,
                                           responses_series_level=responses_series_level,
                                           covariates_Train_ts=covariates_Train_ts,
                                           covariates_Test_ts=covariates_Test_ts,
                                           seed = seed,  variavel_forecasting=variavel_forecasting),
               sarimax    =  f_print_sarimax(pib_go_ts=pib_go_ts,
                                           Hor=Hor,
                                           Start_Hor=Start_Hor,
                                           responses_series_level=responses_series_level,
                                           covariates_Train_ts=covariates_Train_ts,
                                           covariates_Test_ts=covariates_Test_ts,
                                           seed = seed,  variavel_forecasting=variavel_forecasting,
                                            regularization = regularization),
                gbm        =  f_print_gbm(pib_go_ts=pib_go_ts,
                                           Hor=Hor,
                                           Start_Hor=Start_Hor,
                                           responses_series_level=responses_series_level,
                                           covariates_Train_ts=covariates_Train_ts,
                                           covariates_Test_ts=covariates_Test_ts,
                                           seed = seed,  variavel_forecasting=variavel_forecasting,
                                           regularization = regularization),
                xgboost        =  f_print_xgboost(pib_go_ts=pib_go_ts,
                                           Hor=Hor,
                                           Start_Hor=Start_Hor,
                                           responses_series_level=responses_series_level,
                                           covariates_Train_ts=covariates_Train_ts,
                                           covariates_Test_ts=covariates_Test_ts,
                                           seed = seed,  variavel_forecasting=variavel_forecasting,
                                           regularization = regularization),
               rf         =  f_print_rf(pib_go_ts=pib_go_ts,
                                           Hor=Hor,
                                           Start_Hor=Start_Hor,
                                           responses_series_level=responses_series_level,
                                           covariates_Train_ts=covariates_Train_ts,
                                           covariates_Test_ts=covariates_Test_ts,
                                           seed = seed,  variavel_forecasting=variavel_forecasting,
                                           regularization = regularization),
                pcr        =  f_print_pcr(pib_go_ts=pib_go_ts,
                                           Hor=Hor,
                                           Start_Hor=Start_Hor,
                                           responses_series_level=responses_series_level,
                                           covariates_Train_ts=covariates_Train_ts,
                                           covariates_Test_ts=covariates_Test_ts,
                                           seed = seed,  variavel_forecasting=variavel_forecasting),
                mars =  f_print_mars(pib_go_ts=pib_go_ts,
                                           Hor=Hor,
                                           Start_Hor=Start_Hor,
                                           responses_series_level=responses_series_level,
                                           covariates_Train_ts=covariates_Train_ts,
                                           covariates_Test_ts=covariates_Test_ts,
                                           seed = seed,  variavel_forecasting=variavel_forecasting,
                                          regularization = regularization),
                hybrid     =  f_print_hybrid(pib_go_ts=pib_go_ts,
                                           Hor=Hor,
                                           Start_Hor=Start_Hor,
                                           responses_series_level=responses_series_level,
                                           covariates_Train_ts=covariates_Train_ts,
                                           covariates_Test_ts=covariates_Test_ts,
                                           seed = seed,  variavel_forecasting=variavel_forecasting,
                                            regularization = regularization)
                )


return(CV_for)
}



##############################################################
## Função para previsão
##############################################################


forecast_gdp <- function(variavel_forecasting = "y_pib_adjusted.servicos_ajustado", # variável de interesse
                         horizon = 2,  # horizonte para validação cruzada
                         start_date_series = c(2011, 1), # ano-mês do início da série
                         start_date_series_diff = c(2011, 2), # ano-mês do início da série
                         end_date_series  = c(2024, 11), # ano-mês do fim da série
                         start_date_forecast = c(2024, 12), # ano-mês para para início da previsão fora da amostra
                         Start_Hor = '2024-12-01', ## inicio previsão no formato data
                          Hor = 49, # passos para previsão fora da amostra
                          InPer =  162, ## inicio da validação cruzada na amostra
                         models = c("arima", "ets", "bats", "tbats", "elm", "wavelet", "lasso",
                                    "sarimax", "gbm", "xgboost", "rf", "pcr",
                                    "mars", "hybrid"),  # modelos considerados
                         seed = 1117527352, # semente
                          regularization = TRUE, ## selecionar covariáveis via lasso
                         pib_goias_mensal_agro_ind_serv = pib_goias_mensal_agro_ind_serv, ## banco completo
                         covariates_ts = covariates_ts, # covariáveis
                         y_non_seasonal_ts = y_non_seasonal_ts, # variáveis dependentes com sazonalidade
                         y_seasonal_adjusted_ts = y_seasonal_adjusted_ts, # variáveis dependentes sem sazonalidade
                         names_covariates = c(
             "crise_s",
             "abcr_leves_ajustado",
             ##"pms_servicos",
             "pms_servicos_a",
             "pmc_a",
             "pmc_varejo_amp_a",
             "transferencias_correntes",
             "cofins",
             "iof",
             "emprego.formal",
             "emprego.agro",
             "emprego.ind_ext",
             "emprego.formal_go",
             "ibcr.go_a",
             "ibc.br_a",
             "icc",
             "icea",
             "ief",
             "ics_a",
             "ipca_acum_12_br",
             "ipca",
             "ipca_serv",
             "ipca_ex1",
             "ipca_ma_sem.suavizacao",
             "ipca_ma_suavizado",
             "ipca_ex0",
             "ic.br_agro_us.",
             "ic_br_metal_us.",
             "ic_br_energia_us.",
             "ie_a",
             "selic",
             "selic_la12",
             "icms",
             "energia_consumo",
             "serv_a") # covariáveis utilizadas
             ) {

  ## Carregar pacotes
  library(tidyverse)
  library(readxl)
  library(tsibble)
  library(forecast)
  library(dplyr)
  library(nnfor)
  library(WaveletArima)
  library(imputeTS)
  library(caret)
  library(OOS)
  require(writexl)
  library(seasonal)


  # Carregar funções
  #source("source_functions_final.R")
  #source("source_prints_final.R")

  # Definir séries de resposta
  responses_series_level <- cbind(y_non_seasonal_ts, y_seasonal_adjusted_ts)
  colnames(responses_series_level) <- c(colnames(y_non_seasonal_ts), colnames(y_seasonal_adjusted_ts))
  responses_series_level <- as.data.frame(window(responses_series_level, start = start_date_series, end = end_date_series))

  # Processar variáveis dependentes
  y_non_seasonal_diff_ts <- apply(y_non_seasonal_ts, 2, diff)
  y_non_seasonal_diff_ts <- ts(y_non_seasonal_diff_ts, start = start_date_series_diff, frequency = 12)
  y_seasonal_adjusted_diff_ts <- apply(y_seasonal_adjusted_ts, 2, diff)
  y_seasonal_adjusted_diff_ts <- ts(y_seasonal_adjusted_diff_ts, start = start_date_series_diff, frequency = 12)

  dependent_variables_ts <- cbind(y_non_seasonal_diff_ts, y_seasonal_adjusted_diff_ts)
  colnames(dependent_variables_ts) <- c(colnames(y_non_seasonal_diff_ts), colnames(y_seasonal_adjusted_diff_ts))
  dependent_variables_ts <- window(dependent_variables_ts, start = start_date_series_diff, end = end_date_series)

  # Processar covariáveis
  covariates_ts <- covariates_ts[,which(colnames(covariates_ts) %in% names_covariates)]
  covariates_ts <- na.kalman(covariates_ts, model = "auto.arima")
  covariates_diff_ts <- ts(apply(covariates_ts, 2, diff), start = start_date_series_diff, frequency = 12)
  covariates_Train_ts <- window(covariates_diff_ts, end = end_date_series)
  covariates_Test_ts  <- window(covariates_diff_ts, start = start_date_forecast)

  # Definir variável de previsão
  pib_go_ts <- ts(dependent_variables_ts[, variavel_forecasting], start = start_date_series_diff, frequency = 12)

  # Realizar cross-validation
  CV_results <- list()


  c("arima", "ets", "bats", "tbats", "elm", "wavelet", "lasso",
                                    "sarimax", "gbm", "xgboost", "rf", "pcr",
                                    "mars", "hybrid")


  for (model in models) {
    forecast_func <- get(paste0("f_", model))

    if(model %in% c("arima", "ets", "bats", "tbats", "elm", "wavelet")){

       CV_results[[model]] <- tsCV(y = pib_go_ts, forecastfunction = forecast_func, h = horizon,
                                   initial = InPer, seed = seed)
    }
     if(model %in% c("lasso","pcr")){

         CV_results[[model]] <- tsCV(y = pib_go_ts, forecastfunction = forecast_func, h = horizon,
                                 xreg =covariates_Train_ts, initial = InPer, seed = seed)
     } else{

     CV_results[[model]] <- tsCV(y = pib_go_ts, forecastfunction = forecast_func, h = horizon,
                                 xreg =covariates_Train_ts, initial = InPer, seed = seed,
                                 regularization = regularization)
    }
  }

  # Calcular MAE para ordenar modelos
  MAE_results <- sapply(CV_results, function(cv) mean(abs(cv), na.rm = TRUE))
  ordered_models <- names(sort(MAE_results, na.last = TRUE))


  # Gerar previsão com o melhor modelo
  final_forecast <- list()

  for (model in ordered_models) {
  forecast_func <- get(paste0("f_print_", model))
  if(model %in% c("arima", "ets", "bats", "tbats", "elm", "wavelet")){
    final_forecast[[model]] <- modelType_forecasts(
                                       pib_go_ts=pib_go_ts,
                                       Hor=Hor,
                                       Start_Hor=Start_Hor,
                                       responses_series_level=responses_series_level,
                                       covariates_Train_ts=covariates_Train_ts,
                                       covariates_Test_ts=covariates_Test_ts,
                                       modelType = model,
                                       seed = seed,
                                       variavel_forecasting=variavel_forecasting)  }

    if(model %in% c("lasso","pcr")){
    final_forecast[[model]] <- modelType_forecasts(
                                       pib_go_ts=pib_go_ts,
                                       Hor=Hor,
                                       Start_Hor=Start_Hor,
                                       responses_series_level=responses_series_level,
                                       covariates_Train_ts=covariates_Train_ts,
                                       covariates_Test_ts=covariates_Test_ts,
                                       modelType = model,
                                       seed = seed,
                                       variavel_forecasting=variavel_forecasting)

    } else{
        final_forecast[[model]] <- modelType_forecasts(pib_go_ts=pib_go_ts,
                                       Hor=Hor,
                                       Start_Hor=Start_Hor,
                                       responses_series_level=responses_series_level,
                                       covariates_Train_ts=covariates_Train_ts,
                                       covariates_Test_ts=covariates_Test_ts,
                                       modelType = model,
                                       seed = seed,
                                       variavel_forecasting=variavel_forecasting,
                                       regularization = regularization)   }
  }

  # Calcular crescimentos acumulados por ano
  date = format(seq.Date(from = as.Date(Start_Hor), by = 'month', length.out = Hor), format= "%b %Y")
  df_bests_forecasts <- data.frame(Data=date,as.data.frame(final_forecast))

  PIB_TS <- ts(responses_series_level[,which(colnames(responses_series_level)==variavel_forecasting)],
                 start=start_date_series, frequency = 12)
  PIB_HIST <- window(PIB_TS, start=c(2023, 1), end=end_date_series)
  PC_VL <- as.data.frame(rbind(matrix(rep(PIB_HIST,length(ordered_models)),length(PIB_HIST),length(ordered_models)),
                                as.matrix(as.data.frame(final_forecast))))

  colnames(PC_VL) <- c(ordered_models)
  PC_VL$ano <- format(seq.Date(from = as.Date("2023-01-01"), to = as.Date("2028-12-01"), by = 'month'), format= "%Y")

  PC_VL_SUM <- PC_VL %>%
    group_by(ano)  %>%
    summarise(across(everything(), cumsum))

  r_o_c_12 <- function(x, lag = 12){
    n <- length(x)
    val <-  ((x[(1+lag):n] - x[1:(n-lag)])/x[1:(n-lag)])*100
    return(val)
  }

  PC_VL_SUM_YEAR <- round(as.data.frame(apply(PC_VL_SUM[,-which(colnames(PC_VL_SUM) %in% c("ano"))],2,r_o_c_12)),2)

  PC_VL_SUM_YEAR <- PC_VL_SUM_YEAR %>%
  rowwise() %>%
  mutate(
    Mean = round(mean(c_across(), na.rm = TRUE),2),
    Median = round(median(c_across(), na.rm = TRUE),2),
    Trimmed_Mean = round(mean(c_across(), trim = 0.1, na.rm = TRUE),2)
  )

  PC_VL_SUM_YEAR$mes_ano <- format(seq.Date(from = as.Date("2024-01-01"), to = as.Date("2028-12-01"), by = 'month'), format= "%b %Y")
  PC_VL_SUM_YEAR <- PC_VL_SUM_YEAR[, c("mes_ano", setdiff(names(PC_VL_SUM_YEAR), "mes_ano"))]

  PC_VL$ano <- NULL
  PC_VL$mes_ano <- format(seq.Date(from = as.Date("2023-01-01"), to = as.Date("2028-12-01"), by = 'month'), format= "%b %Y")
  PC_VL <- PC_VL[, c("mes_ano", setdiff(names(PC_VL), "mes_ano"))]

  # salvar xlsx
  write_xlsx(PC_VL_SUM_YEAR, paste0(paste0("crescimento_acumulado_",variavel_forecasting),".xlsx"))
  write_xlsx(PC_VL, paste0(paste0("indices_",variavel_forecasting),".xlsx"))

  # Gerar saída final
  return(list(Growth_Forecast = PC_VL_SUM_YEAR, Indices_Forecast = PC_VL))
}

