#' Gerar séries sem sazonalidade
#'
#' Esta função gera séries sem sazonalidade.
#'
#' @title Remove Sazonalidade das Respostas
#' @param dfts Um objeto ts contendo respostas.
#' @param start_year Ano inicial da série.
#' @param start_period Período inicial (mês, se frequência mensal).
#' @param end_year Ano final da previsão.
#' @param end_period Período final da previsão.
#' @return Um objeto ts contendo a série ajustada e as previsões.
#' @examples
#' responses_remove_seas_forecast(dfts = final_data_ts)
#' @export

responses_remove_seas_forecast <- function(dfts,
                                           start_year   = 2011,
                                           start_period = 1,
                                           end_year     = 2028,
                                           end_period   = 12) {

  ## Função auxiliar: Remover apenas os NAs finais
  remove_trailing_nas <- function(series) {
    na_positions <- which(is.na(series))  # Identifica posições de NA

    if (length(na_positions) == 0) {
      return(series)  # Retorna a série original se não houver NAs
    }

    last_non_na <- max(which(!is.na(series)))  # Última posição com um valor válido
    clean_series <- series[1:last_non_na]  # Mantém apenas os valores até essa posição
    return(clean_series)
  }

  if (!inherits(dfts, "ts")) {
    stop("O objeto fornecido não é um dataframe de séries temporais (ts).")
  }

  # Determinar o número total de períodos com base na frequência da série
  total_periods <- (end_year - start_year) * 12 + (end_period - start_period + 1)

  # Criar matrizes para armazenar os dados ajustados e originais estendidos
  df_ajustado <- matrix(NA, nrow = total_periods, ncol = ncol(dfts))  # Matriz inicial preenchida com NA
  df_original_estendido <- matrix(NA, nrow = total_periods, ncol = ncol(dfts))  # Matriz para dfts estendido

  colnames(df_ajustado) <- paste0("ajustado_", colnames(dfts))  # Adiciona prefixo nas colunas ajustadas
  colnames(df_original_estendido) <- colnames(dfts)  # Mantém os nomes originais

  for (i in seq_along(colnames(dfts))) {
    series_ts  <- dfts[, i]
    series_ts <- remove_trailing_nas(series_ts)  # Remove apenas os NAs finais
    series_ts  <- ts(series_ts, start = c(start_year, start_period), frequency = 12)  # Reajustando índice temporal

    # Ajuste sazonal
    ajustado <- final(seasonal::seas(series_ts))

    # Criar vetor preenchido com NA até o tamanho total necessário
    ajustado_full <- rep(NA, total_periods)
    ajustado_full[1:length(ajustado)] <- ajustado  # Substituir pelos valores ajustados

    # Criar vetor original estendido com NA até o final
    original_full <- rep(NA, total_periods)
    original_full[1:length(series_ts)] <- series_ts  # Preencher com os valores originais

    # Adicionar ao novo dataframe
    df_ajustado[, i] <- ajustado_full
    df_original_estendido[, i] <- original_full
  }

  # Unir os dataframes original estendido e ajustado
  df_geral <- cbind(df_original_estendido, df_ajustado)
  colnames(df_geral) <- c(colnames(dfts), colnames(df_ajustado))  # Mantém os nomes corretos

  # Converter para ts mantendo frequência e nova data de início
  df_geral <- ts(df_geral, start = c(start_year, start_period), frequency = 12)
  responses_remove_seas_forecast_ts = df_geral
  save(responses_remove_seas_forecast_ts, file = "responses_remove_seas_forecast_ts.RData")

  return(responses_remove_seas_forecast_ts)
}
