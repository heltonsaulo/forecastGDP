#'
#' Esta função realiza previsões do PIB/Inflação utilizando diversos modelos estatísticos e de aprendizado de máquina.
#'
#' @param variavel_forecasting Nome da variável de interesse para previsão.
#' @param start_series_year Ano de início da série temporal.
#' @param start_series_period Mês de início da série temporal.
#' @param end_series_year Ano final da série temporal.
#' @param end_series_period Mês final da série temporal.
#' @param start_forecast_year Ano de início da previsão.
#' @param start_forecast_period Mês de início da previsão.
#' @param end_forecast_year Ano final da previsão.
#' @param end_forecast_period Mês final da previsão.
#' @param horizon Horizonte de previsão para validação cruzada.
#' @param InPer Número de períodos iniciais utilizados para validação cruzada.
#' @param models Modelos a serem considerados para previsão.
#' @param seed Semente para reprodutibilidade dos resultados.
#' @param cross_validation Indica se a validação cruzada será realizada (TRUE/FALSE).
#' @param regularization Indica se a seleção de variáveis será feita via LASSO (TRUE/FALSE).
#' @param inflation_forecast Indica se a previsão é para inflação (TRUE/FALSE).
#' @param covariates_ts Conjunto de covariáveis para previsão.
#' @param responses_ts Conjunto de séries de resposta.
#' @param names_covariates Vetor de nomes das covariáveis utilizadas.
#'
#' @return Lista contendo previsões de crescimento e índices, além das variáveis selecionadas pelo LASSO.
#' @export
#' @examples
#' forecast_gdp("ajustado_servicos")


forecast_gdp <- function(variavel_forecasting = "ajustado_servicos", # variável de interesse
                         start_series_year   = 2011,           # início do ano da série
                         start_series_period = 1,              # início do mês da série
                         #
                         end_series_year   = 2024,             # fim do ano da série
                         end_series_period = 11,               # fim do mês da série
                         #
                         start_forecast_year   = 2024,  # início do ano de previsão
                         start_forecast_period = 12,    # início do mês de previsão
                         #
                         end_forecast_year     = 2028,           # último do ano da série (previsão)
                         end_forecast_period   = 12,             # último do mês da série (previsão)
                         #
                         horizon = 2,  # horizonte para validação cruzada
                         #
                         InPer =  162, ## inicio da validação cruzada na amostra
                         models = c("sarima", "ets", "bats", "tbats", "prophet", "elm", "wavelet", "lasso",
                                    "sarimax", "gbm", "xgboost", "rf", "pcr",
                                    "mars", "hybrid"),  # modelos considerados
                         seed = 1117527352, # semente
                         cross_validation = FALSE, ## implementar validação cruzada
                         regularization = TRUE, ## selecionar covariáveis via lasso
                         inflation_forecast = FALSE, ## TRUE ser for fazer previsão da inflação
                         ##pib_goias_mensal_agro_ind_serv = pib_goias_mensal_agro_ind_serv, ## banco completo
                         covariates_ts = covariates_remove_seas_forecast_ts, # covariáveis
                         responses_ts  = responses_remove_seas_forecast_ts, # respostas
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
  library(prophet)


  # Determinar o número total de períodos com base na frequência da série
  total_periods <-  (end_forecast_year - start_series_year) * 12 + (end_forecast_period - start_series_period + 1)

  # Passos para previsão fora da amostra
  Hor = (end_forecast_year - start_forecast_year) * 12 + (end_forecast_period - start_forecast_period + 1)

  # Formatar start_forecast_period para garantir dois dígitos e adicionar "-01"
  formatted_period_start_forecast <- sprintf("%02d-01", start_forecast_period)
  formatted_period_end_forecast  <- sprintf("%02d-01", end_forecast_period)

  # Criar a string final
  Start_Hor <- paste0(start_forecast_year, "-", formatted_period_start_forecast)
  End_Hor   <- paste0(end_forecast_year, "-", formatted_period_end_forecast)


  # Definir séries de resposta
  responses_series_level <- responses_ts
  responses_series_level <- as.data.frame(window(responses_series_level,
                                                 start = c(start_series_year,start_series_period),
                                                 end = c(end_series_year,end_series_period)))

  # Processar variáveis dependentes
  dependent_variables_ts <- apply(responses_series_level, 2, diff)
  dependent_variables_ts <- ts(dependent_variables_ts, start = c(start_series_year,(start_series_period+1)), frequency = 12)
  dependent_variables_ts <- window(dependent_variables_ts, end = c(end_series_year,end_series_period) )


  # Processar covariáveis
  covariates_ts <- covariates_ts[,which(colnames(covariates_ts) %in% names_covariates)]
  covariates_diff_ts <- ts(apply(covariates_ts, 2, diff), start = c(start_series_year,(start_series_period+1)), frequency = 12)
  covariates_Train_ts <- window(covariates_diff_ts, end = c(end_series_year, end_series_period))
  covariates_Test_ts  <- window(covariates_diff_ts, start = c(start_forecast_year, start_forecast_period))

  # Definir variável de previsão
  pib_go_ts <- ts(dependent_variables_ts[, variavel_forecasting], start = c(start_series_year,(start_series_period+1)), frequency = 12)

  # Loop cross-validation (TRUE or FALSE)
  if(cross_validation == TRUE) {

  # Realizar cross-validation
  CV_results <- list()

  for (model in models) {
    forecast_func <- get(paste0("f_", model))

    if(model %in% c("sarima", "ets", "bats", "tbats", "elm", "prophet", "wavelet")){

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
  } else{ ordered_models <- models }


  # Gerar previsão com o melhor modelo
  final_forecast <- list()

  for (model in ordered_models) {
  forecast_func <- get(paste0("f_print_", model))
  if(model %in% c("sarima", "ets", "bats", "tbats", "elm", "prophet", "wavelet")){
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

  if(inflation_forecast == FALSE) {
  ################################################
  # Calcular crescimentos acumulados por ano
  ################################################
  date = format(seq.Date(from = as.Date(Start_Hor), by = 'month', length.out = Hor), format= "%b %Y")
  df_bests_forecasts <- data.frame(Data=date,as.data.frame(final_forecast))

  PIB_TS <- ts(responses_series_level[,which(colnames(responses_series_level)==variavel_forecasting)],
                 start=c(start_series_year, start_series_period), frequency = 12)

  PIB_HIST <- window(PIB_TS, start=c(2023, 1), end=c(end_series_year, end_series_period))


  PC_VL <- as.data.frame(rbind(matrix(rep(PIB_HIST,length(ordered_models)),length(PIB_HIST),length(ordered_models)),
                                as.matrix(as.data.frame(final_forecast))))

  colnames(PC_VL) <- c(ordered_models)
  PC_VL$ano <- format(seq.Date(from = as.Date("2023-01-01"), to = as.Date(End_Hor), by = 'month'), format= "%Y")

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

  PC_VL_SUM_YEAR$mes_ano <- format(seq.Date(from = as.Date("2024-01-01"), to = as.Date(End_Hor), by = 'month'), format= "%b %Y")
  PC_VL_SUM_YEAR <- PC_VL_SUM_YEAR[, c("mes_ano", setdiff(names(PC_VL_SUM_YEAR), "mes_ano"))]

  PC_VL$ano <- NULL
  PC_VL$mes_ano <- format(seq.Date(from = as.Date("2023-01-01"), to = as.Date(End_Hor), by = 'month'), format= "%b %Y")
  PC_VL <- PC_VL[, c("mes_ano", setdiff(names(PC_VL), "mes_ano"))]

  # salvar xlsx
  write_xlsx(PC_VL_SUM_YEAR, paste0(paste0("crescimento_acumulado_",variavel_forecasting),".xlsx"))
  write_xlsx(PC_VL, paste0(paste0("indices_",variavel_forecasting),".xlsx"))
  }

  if(inflation_forecast == TRUE) {
  ################################################
  ## Calcular inflação acumulada
  ################################################

   r_o_c <- function(x, lag = 1){
     n <- length(x)
     val <-  ((x[(1+lag):n] - x[1:(n-lag)])/x[1:(n-lag)])*100
   return(val)
  }

  PIB_HIST_I <- window(PIB_TS, start=c(2022, 12), end=c(end_series_year, end_series_period))
  PC_VL_I <- as.data.frame(rbind(matrix(rep(PIB_HIST_I,length(ordered_models)),length(PIB_HIST_I),length(ordered_models)),
                                as.matrix(as.data.frame(final_forecast))))
  colnames(PC_VL_I) <- c(ordered_models)
  PC_VL_I$ano <- format(seq.Date(from = as.Date("2022-12-01"), to = as.Date(End_Hor), by = 'month'), format= "%Y")
  PC_VL_I$mon <- 1

  PC_VL_SUM_YEAR <- round(as.data.frame(apply(PC_VL_I[,-which(colnames(PC_VL_I)%in%c("ano","mon"))],2,r_o_c)),2)
  PC_VL_SUM_YEAR <- 1+PC_VL_SUM_YEAR/100
  PC_VL_SUM_YEAR$ano <- rep(2023:end_forecast_year, each = 12)


  PC_VL_SUM_YEAR <-  PC_VL_SUM_YEAR %>%
                   group_by(ano)  %>%
                  summarise_each(funs(cumprod))

  PC_VL_SUM_YEAR <- subset(PC_VL_SUM_YEAR, ano >= 2024)
  PC_VL_SUM_YEAR$ano <- NULL
  PC_VL_SUM_YEAR <- round((PC_VL_SUM_YEAR-1)*100,2)


  PC_VL_SUM_YEAR <- PC_VL_SUM_YEAR %>%
  rowwise() %>%
  mutate(
    Mean = round(mean(c_across(), na.rm = TRUE),2),
    Median = round(median(c_across(), na.rm = TRUE),2),
    Trimmed_Mean = round(mean(c_across(), trim = 0.1, na.rm = TRUE),2)
  )

  PC_VL_SUM_YEAR$mes_ano <- format(seq.Date(from = as.Date("2024-01-01"), to = as.Date(End_Hor), by = 'month'), format= "%b %Y")
  PC_VL_SUM_YEAR <- PC_VL_SUM_YEAR[, c("mes_ano", setdiff(names(PC_VL_SUM_YEAR), "mes_ano"))]

  PC_VL$ano <- NULL
  PC_VL$mes_ano <- format(seq.Date(from = as.Date("2023-01-01"), to = as.Date(End_Hor), by = 'month'), format= "%b %Y")
  PC_VL <- PC_VL[, c("mes_ano", setdiff(names(PC_VL), "mes_ano"))]

   # salvar xlsx
  write_xlsx(PC_VL_SUM_YEAR, paste0(paste0("inflacao_acumulada_",variavel_forecasting),".xlsx"))
  write_xlsx(PC_VL_I, paste0(paste0("indices_",variavel_forecasting),".xlsx"))
  }

  ################################################
  # Covariáveis escolhidas por LASSO
  ################################################
   set.seed(seed)
   lassoFit <-  glmnet::cv.glmnet(x=model.matrix(~.-1,data=covariates_Train_ts), y=as.vector(pib_go_ts), family="gaussian",
                               intercept = FALSE,
                                alpha =1)
   namesCoef <- rownames(coef(lassoFit, s = 'lambda.min'))[coef(lassoFit, s = 'lambda.min')[,1]!= 0] ### returns nonzero coefs
   ##print(namesCoef)
   lfc <- as.vector(coef(lassoFit, s = 'lambda.min')[,1]!= 0)[-1]
   if(regularization == TRUE) namesCoef <- colnames(covariates_Train_ts)[lfc]
   if(regularization == FALSE) namesCoef <- colnames(covariates_Train_ts)##[lfc]

  ################################################
  # Gerar saída final
  ################################################

  return(list(Growth_Forecast = PC_VL_SUM_YEAR, Indices_Forecast = PC_VL, namesCoef = namesCoef))
}
