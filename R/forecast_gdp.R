#' Previsão do PIB
#'
#' Esta função realiza previsões do PIB utilizando diferentes modelos estatísticos.
#'
#' @param variavel_forecasting Variável de interesse para previsão
#' @param horizon Horizonte para validação cruzada
#' @param start_year Ano inicial para previsão fora da amostra
#' @param start_month Mês inicial para previsão fora da amostra
#' @param models Vetor com os modelos de previsão a serem considerados
#' @param seed Número da semente para reprodutibilidade
#' @param regularization Booleano para usar regularização via LASSO
#' @return Lista contendo previsões acumuladas e índices
#' @export
#' @examples
#' forecast_pib("y_pib_adjusted.servicos_ajustado", horizon = 2, start_year = 2024, start_month = 12)



############################################################
## Função para previsão
############################################################


forecast_gdp <- function(variavel_forecasting = "y_pib_adjusted.servicos_ajustado", # variável de interesse
                         horizon = 2,  # horizonte para validação cruzada
                         start_year = 2024, # ano para para início da previsão fora da amostra
                         start_month = 12,  # mês para início da previsão fora da amostra
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
  source("source_functions_final.R")
  source("source_prints_final.R")

  # Definir séries de resposta
  responses_series_level <- cbind(y_non_seasonal_ts, y_seasonal_adjusted_ts)
  colnames(responses_series_level) <- c(colnames(y_non_seasonal_ts), colnames(y_seasonal_adjusted_ts))
  responses_series_level <- as.data.frame(window(responses_series_level, start = c(2011, 1), end = c(2024, 11)))

  # Processar variáveis dependentes
  y_non_seasonal_diff_ts <- apply(y_non_seasonal_ts, 2, diff)
  y_non_seasonal_diff_ts <- ts(y_non_seasonal_diff_ts, start = c(2011, 2), frequency = 12)
  y_seasonal_adjusted_diff_ts <- apply(y_seasonal_adjusted_ts, 2, diff)
  y_seasonal_adjusted_diff_ts <- ts(y_seasonal_adjusted_diff_ts, start = c(2011, 2), frequency = 12)

  dependent_variables_ts <- cbind(y_non_seasonal_diff_ts, y_seasonal_adjusted_diff_ts)
  colnames(dependent_variables_ts) <- c(colnames(y_non_seasonal_diff_ts), colnames(y_seasonal_adjusted_diff_ts))
  dependent_variables_ts <- window(dependent_variables_ts, start = c(2011, 2), end = c(2024, 11))

  # Processar covariáveis
  covariates_ts <- covariates_ts[,which(colnames(covariates_ts) %in% names_covariates)]
  covariates_ts <- na.kalman(covariates_ts, model = "auto.arima")
  covariates_diff_ts <- ts(apply(covariates_ts, 2, diff), start = c(2011, 2), frequency = 12)
  covariates_Train_ts <- window(covariates_diff_ts, end = c(2024, 11))
  covariates_Test_ts  <- window(covariates_diff_ts, start = c(2024, 12))

  # Definir variável de previsão
  pib_go_ts <- ts(dependent_variables_ts[, variavel_forecasting], start = c(2011, 2), frequency = 12)

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
                 start=c(2011,1), frequency = 12)
  PIB_HIST <- window(PIB_TS, start=c(2023,01), end=c(2024,11))
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

