

## Defining forecast functions

################################################################################
################################################################################
################################################################################
################################################################################

f_prophet <- function(y, h, seed){

dates <- seq(as.Date(paste(start(y)[1], paste0(start(y)[2],"-01"), sep = "-")),
             as.Date(paste(end(y)[1], paste0(end(y)[2],"-01"), sep = "-")),
             by = "month")

df <- data.frame(
  ds = dates,
  y = y
)

m <- prophet::prophet(df,
            yearly.seasonality = TRUE,  # Enable yearly seasonality
            weekly.seasonality = FALSE,  # Enable weekly seasonality
            daily.seasonality = FALSE)  # Disable daily seasonality for this example

# Create future dataframe for predictions
future <- prophet::make_future_dataframe(m, freq = "month", periods = h)

# Make predictions
forecast_prophet <- predict(m, future)

# Predictions
aa <- forecast::meanf(forecast_prophet$yhat[(length(dates)+1):length(future$ds)],h=h)
aa$mean <- forecast_prophet$yhat[(length(dates)+1):length(future$ds)]
aa$lower[,1] <- NA
aa$lower[,2] <- NA
aa$upper[,1] <- NA
aa$upper[,2] <- NA
aa
}


################################################################################
################################################################################
################################################################################
################################################################################

f_sarima <- function(y, h, seed){
    fit = forecast::auto.arima(y)
forecast::forecast(fit, h)
}

################################################################################
################################################################################
################################################################################
################################################################################

f_ets <- function(y, h, seed){
   fit = forecast::ets(y, model="ZZZ")
forecast::forecast(fit, h)
}

################################################################################
################################################################################
################################################################################
################################################################################

f_bats <- function(y, h, seed){
   fit = forecast::bats(y)
forecast::forecast(fit, h)
}

################################################################################
################################################################################
################################################################################
################################################################################

f_tbats <- function(y, h, seed){
   fit = forecast::tbats(y)
forecast::forecast(fit, h)
}

################################################################################
################################################################################
################################################################################
################################################################################

f_thetaf <- function(y, h, seed){
   fit = forecast::thetaf(y, h)
forecast::forecast(fit, h)
}

################################################################################
################################################################################
################################################################################
################################################################################

f_meanf <- function(y, h, seed){
   fit = forecast::meanf(y, h)
forecast::forecast(fit, h)
}

################################################################################
################################################################################
################################################################################
################################################################################

f_naive <- function(y, h, seed){
  fit =   forecast::naive(y, h)
forecast::forecast(fit, h)
}

################################################################################
################################################################################
################################################################################
################################################################################
f_snaive <- function(y, h, seed){
  fit =   forecast::snaive(y, h)
forecast::forecast(fit, h)
}

################################################################################
################################################################################
################################################################################
################################################################################

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


################################################################################
################################################################################
################################################################################
################################################################################

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


################################################################################
################################################################################
################################################################################
################################################################################

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


################################################################################
################################################################################
################################################################################
################################################################################


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



################################################################################
################################################################################
################################################################################
################################################################################

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


################################################################################
################################################################################
################################################################################
################################################################################

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




################################################################################
################################################################################
################################################################################
################################################################################

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


################################################################################
################################################################################
################################################################################
################################################################################

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



################################################################################
################################################################################
################################################################################
################################################################################

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


################################################################################
################################################################################
################################################################################
################################################################################

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


## facebook prophet
dates <- seq(as.Date(paste(start(y)[1], paste0(start(y)[2],"-01"), sep = "-")),
             as.Date(paste(end(y)[1], paste0(end(y)[2],"-01"), sep = "-")),
             by = "month")

df <- data.frame(
  ds = dates,
  y = y
)

m <- prophet::prophet(df,
            yearly.seasonality = TRUE,  # Enable yearly seasonality
            weekly.seasonality = FALSE,  # Enable weekly seasonality
            daily.seasonality = FALSE)  # Disable daily seasonality for this example

# Create future dataframe for predictions
future <- prophet::make_future_dataframe(m, freq = "month", periods = h)

# Make predictions
forecast_prophet <- predict(m, future)

# Predictions
aaprophet <- forecast::meanf(forecast_prophet$yhat[(length(dates)+1):length(future$ds)],h=h)
aaprophet$mean <- forecast_prophet$yhat[(length(dates)+1):length(future$ds)]
aaprophet$lower[,1] <- NA
aaprophet$lower[,2] <- NA
aaprophet$upper[,1] <- NA
aaprophet$upper[,2] <- NA




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

date = seq.Date(from = as.Date('2024-12-01'), by = 'month', length.out = h)

D_prophet = data.frame(date, forecast.date=NA, model = "prophet", forecast = as.vector(aaprophet$mean), se=NA, observed =NA)
D_bats = data.frame(date, forecast.date=NA, model = "bats", forecast = as.vector(batsaa$mean), se=NA, observed =NA)
D_tbats = data.frame(date, forecast.date=NA, model = "tbats", forecast = as.vector(tbatsaa$mean), se=NA, observed =NA)
D_sarima = data.frame(date, forecast.date=NA, model = "sarima", forecast = as.vector(arimaaa$mean), se=NA, observed =NA)
D_ets = data.frame(date, forecast.date=NA, model = "ets", forecast = as.vector(etsaa$mean), se=NA, observed = NA)
D_wavelet = data.frame(date, forecast.date=NA, model = "wavelet", forecast = as.vector(waveletarimaaa$mean), se=NA, observed =NA)
D_lasso  = data.frame(date, forecast.date=NA,model = "lasso", forecast = as.vector(lassoaa$mean), se=NA,observed =NA)
D_elm = data.frame(date, forecast.date=NA, model = "elm", forecast = as.vector(elmaa$mean), se=NA, observed =NA)
D_sarimax  = data.frame(date, forecast.date=NA,model = "sarimax", forecast = as.vector(sarimalassoaa$mean), se=NA,observed = NA)
D_gbm  = data.frame(date, forecast.date=NA,model = "gbm", forecast = as.vector(gbmaa$mean), se=NA,observed = NA)
D_xgboost  = data.frame(date, forecast.date=NA,model = "xgboost", forecast = as.vector(xgboostaa$mean), se=NA,observed = NA)
D_rf  = data.frame(date, forecast.date=NA,model = "rf", forecast = as.vector(rfaa$mean), se=NA,observed = NA)
D_pcr  = data.frame(date, forecast.date=NA,model = "pcr", forecast = as.vector(pcraa$mean), se=NA,observed = NA)
D_mars  = data.frame(date, forecast.date=NA,model = "mars", forecast = as.vector(bmarsgcvaa$mean), se=NA,observed = NA)


D_forecasts_all <- rbind.data.frame(D_prophet,
                                    D_bats,D_tbats,
                                    D_sarima,D_ets,D_wavelet,D_lasso,D_elm,
                                    D_sarimax,D_gbm,D_xgboost,D_rf,D_pcr,
                                    D_mars)
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



