#' Previsão das covariáveis
#'
#' Esta função realiza previsões das covariáveis.
#'
#' @title Remove Sazonalidade e Faz Previsão
#' @param dfts Um objeto ts contendo covariáveis.
#' @param start_year Ano inicial da série.
#' @param start_period Período inicial (mês, se frequência mensal).
#' @param end_year Ano final da previsão.
#' @param end_period Período final da previsão.
#' @return Um objeto ts contendo a série ajustada e as previsões.
#' @examples
#' covariates_remove_seas_forecast(dfts = final_data_ts)
#' @export

covariates_remove_seas_forecast <- function(dfts, start_year = 2011, start_period = 1,
                                                  end_year = 2028, end_period = 12) {

  # Pacotes
  library(forecast)
  library(seasonal)
  library(tsibble)
  library(tidyverse)
  library(imputeTS)

  ## remover apenas últimos NAs
  remove_trailing_nas <- function(series) {
  na_positions <- which(is.na(series))  # Identifica posições de NA

  if (length(na_positions) == 0) {
    return(series)  # Retorna a série original se não houver NAs
  }

  last_non_na <- max(which(!is.na(series)))  # Última posição com um valor válido
  clean_series <- series[1:last_non_na]  # Mantém apenas os valores até essa posição
  return(clean_series)
  }

  # Cálculo do total de períodos
   total_periods <- (end_year - start_year) * 12 + (end_period - start_period + 1)

  # Transformar ts em df
  df <- as.data.frame(dfts)

  # Identificar variáveis dicotômicas (com apenas 0 e 1)
  is_dichotomous <- sapply(df, function(col) all(na.omit(col) %in% c(0, 1)))

  # Definir funções de previsão para cada modelo para uso na validação cruzada
  f_cv_arima  <- function(y, h) forecast(auto.arima(y), h = h)
  f_cv_ets    <- function(y, h) forecast(ets(y), h = h)
  f_cv_bats   <- function(y, h) forecast(bats(y), h = h)
  f_cv_tbats  <- function(y, h) forecast(tbats(y), h = h)
  f_cv_nnetar <- function(y, h) forecast(nnetar(y), h = h)

  # Lista de funções de previsão
  forecast_functions <- list(
    arima  = f_cv_arima,
    ets    = f_cv_ets,
    bats   = f_cv_bats,
    tbats  = f_cv_tbats,
    nnetar = f_cv_nnetar
  )

  # Criar um dataframe para armazenar as previsões
  df_previsto <- data.frame(matrix(ncol = ncol(df), nrow = total_periods))
  colnames(df_previsto) <- colnames(df)


  # Para cada coluna (série) do dataframe
  for (col in colnames(df)) {
    print(col)

    if (is_dichotomous[col]) {
      # Se for variável dicotômica, apenas preencher os h passos com zero
      series_ts  <- df[[col]]
      series_ts  <- remove_trailing_nas(series_ts)
      #series_ts  <- ts(series_ts, start = c(start_year, start_period), frequency = 12)
      #series_ts  <- imputeTS::na.kalman(series_ts, model = "auto.arima")
      serie_estendida <- c(as.vector(series_ts), rep(0,total_periods-length(series_ts)))
    } else {
      # Converte a coluna em objeto ts (supondo frequência mensal), remove NA, e faz imputação
      series_ts  <- df[[col]]
      series_ts <- remove_trailing_nas(series_ts)
      series_ts  <- ts(series_ts, start = c(start_year, start_period), frequency = 12)
      series_ts  <- imputeTS::na.kalman(series_ts, model = "auto.arima")
      # Remover sazonalidade
      serie_ajustada <- final(seasonal::seas(series_ts, outlier = NULL, automdl = NULL))

      # Inicializa vetor para armazenar o MAE de cada modelo (usando validação cruzada)
      mae_modelos <- c()

      # Para cada modelo, calcula o erro médio absoluto usando tsCV
      for (modelo in names(forecast_functions)) {
        erros <- tsCV(serie_ajustada, forecastfunction = forecast_functions[[modelo]], initial = ceiling(length(serie_ajustada) * 0.95), h = 5)
        mae <- mean(abs(erros), na.rm = TRUE)
        mae_modelos[modelo] <- mae
      }

      # Escolhe o modelo com menor MAE
      melhor_modelo <- names(which.min(mae_modelos))

      # Realiza a previsão com o melhor modelo usando a série completa
      previsao <- forecast_functions[[melhor_modelo]](serie_ajustada, h = total_periods-length(series_ts))

      # Concatena a série original com os valores previstos
      serie_estendida <- c(as.vector(serie_ajustada), as.vector(previsao$mean))
    }

    # Atualiza a coluna com a série extendida (histórico + previsão ou preenchimento de zeros)
    df_previsto[[col]] <- serie_estendida
  }

  # Retornar ts
  covariates_remove_seas_forecast_ts <- ts(df_previsto, start = c(start_year, start_period), frequency = 12)
  save(covariates_remove_seas_forecast_ts, file = "covariates_remove_seas_forecast_ts.RData")

  return(covariates_remove_seas_forecast_ts)
}
