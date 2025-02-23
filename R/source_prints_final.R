
################################################################################
################################################################################
################################################################################
################################################################################

f_print_prophet  <- function(pib_go_ts,
                           Hor,
                           Start_Hor,
                           responses_series_level,
                           seed = seed,  variavel_forecasting=variavel_forecasting){

dates <- seq(as.Date(paste(start(pib_go_ts)[1], paste0(start(pib_go_ts)[2],"-01"), sep = "-")),
             as.Date(paste(end(pib_go_ts)[1], paste0(end(pib_go_ts)[2],"-01"), sep = "-")),
             by = "month")

df <- data.frame(
  ds = dates,
  y = pib_go_ts
)

m <- prophet(df,
            yearly.seasonality = TRUE,  # Enable yearly seasonality
            weekly.seasonality = FALSE,  # Enable weekly seasonality
            daily.seasonality = FALSE)  # Disable daily seasonality for this example

# Create future dataframe for predictions
future <- make_future_dataframe(m, freq = "month", periods = Hor)

# Make predictions
forecast_prophet <- predict(m, future)

# Predictions
aa <- forecast::meanf(forecast_prophet$yhat[(length(dates)+1):length(future$ds)],h=Hor)
aa$mean <- forecast_prophet$yhat[(length(dates)+1):length(future$ds)]
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


################################################################################
################################################################################
################################################################################
################################################################################
## Forecasting with the best model
f_print_sarima <- function(pib_go_ts,
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


################################################################################
################################################################################
################################################################################
################################################################################
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


################################################################################
################################################################################
################################################################################
################################################################################
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

################################################################################
################################################################################
################################################################################
################################################################################
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


################################################################################
################################################################################
################################################################################
################################################################################
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

################################################################################
################################################################################
################################################################################
################################################################################
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


################################################################################
################################################################################
################################################################################
################################################################################
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



################################################################################
################################################################################
################################################################################
################################################################################
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


################################################################################
################################################################################
################################################################################
################################################################################
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




################################################################################
################################################################################
################################################################################
################################################################################
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


################################################################################
################################################################################
################################################################################
################################################################################
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



################################################################################
################################################################################
################################################################################
################################################################################
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








################################################################################
################################################################################
################################################################################
################################################################################
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




################################################################################
################################################################################
################################################################################
################################################################################

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




################################################################################
################################################################################
################################################################################
################################################################################
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












################################################################################
################################################################################
################################################################################
################################################################################
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




################################################################################
################################################################################
################################################################################
################################################################################

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


## facebook prophet
dates <- seq(as.Date(paste(start(pib_go_ts)[1], paste0(start(pib_go_ts)[2],"-01"), sep = "-")),
             as.Date(paste(end(pib_go_ts)[1], paste0(end(pib_go_ts)[2],"-01"), sep = "-")),
             by = "month")

df <- data.frame(
  ds = dates,
  y = pib_go_ts
)

m <- prophet(df,
            yearly.seasonality = TRUE,  # Enable yearly seasonality
            weekly.seasonality = FALSE,  # Enable weekly seasonality
            daily.seasonality = FALSE)  # Disable daily seasonality for this example

# Create future dataframe for predictions
future <- make_future_dataframe(m, freq = "month", periods = Hor)

# Make predictions
forecast_prophet <- predict(m, future)

# Predictions
aaprophet <- forecast::meanf(forecast_prophet$yhat[(length(dates)+1):length(future$ds)],h=Hor)
aaprophet$mean <- forecast_prophet$yhat[(length(dates)+1):length(future$ds)]
aaprophet$lower[,1] <- NA
aaprophet$lower[,2] <- NA
aaprophet$upper[,1] <- NA
aaprophet$upper[,2] <- NA



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


D_prophet = data.frame(date, forecast.date=NA, model = "prophet", forecast = as.vector(aaprophet$mean), se=NA, observed =NA)
D_bats = data.frame(date, forecast.date=NA, model = "bats", forecast = as.vector(batsaa$mean), se=NA, observed =NA)
D_tbats = data.frame(date, forecast.date=NA, model = "tbats", forecast = as.vector(tbatsaa$mean), se=NA, observed =NA)
D_sarima = data.frame(date, forecast.date=NA, model = "sarima", forecast = as.vector(arimaaa$mean), se=NA, observed = NA)
D_wavelet = data.frame(date, forecast.date=NA, model = "wavelet", forecast = as.vector(waveletarimaaa$mean), se=NA, observed = NA)
D_ets = data.frame(date, forecast.date=NA, model = "ets", forecast = as.vector(etsaa$mean), se=NA, observed = NA)
D_lasso  = data.frame(date, forecast.date=NA,model = "lasso", forecast = as.vector(lassoaa$mean), se=NA,observed = NA)
D_elm = data.frame(date, forecast.date=NA, model = "elm", forecast = as.vector(elmaa$mean), se=NA, observed =NA)
D_sarimax  = data.frame(date, forecast.date=NA,model = "sarimax", forecast = as.vector(sarimalassoaa$mean), se=NA,observed = NA)
D_gbm  = data.frame(date, forecast.date=NA,model = "gbm", forecast = as.vector(gbmaa$mean), se=NA,observed = NA)
D_xgboost  = data.frame(date, forecast.date=NA,model = "xgboost", forecast = as.vector(xgboostaa$mean), se=NA,observed = NA)
D_rf  = data.frame(date, forecast.date=NA,model = "rf", forecast = as.vector(rfaa$mean), se=NA,observed = NA)
D_pcr  = data.frame(date, forecast.date=NA,model = "pcr", forecast = as.vector(pcraa$mean), se=NA,observed = NA)
D_mars  = data.frame(date, forecast.date=NA,model = "mars", forecast = as.vector(bmarsgcvaa$mean), se=NA,observed = NA)


D_forecasts_all <- rbind.data.frame(D_prophet,
                                    D_bats,D_tbats,
                                    D_sarima,D_ets,D_wavelet,
                                    D_lasso,D_elm,D_sarimax,D_gbm,
                                    D_xgboost,D_rf,
                                    D_pcr,D_mars)

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






################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################


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
               prophet     = f_print_prophet(pib_go_ts=pib_go_ts,
                                           Hor=Hor,
                                           Start_Hor=Start_Hor,
                                           responses_series_level=responses_series_level,
                                           seed = seed,  variavel_forecasting=variavel_forecasting),
                sarima     = f_print_sarima(pib_go_ts=pib_go_ts,
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
