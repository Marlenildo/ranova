# =========================================================
# 07_funcoes_utilitarias.R
# Funcoes genericas de apoio e utilidades
#
# Atencao: estas funcoes NAO ajustam modelos estatisticos.
# Sao utilitarios para inspecao, organizacao e preparacao
# de dados para tabelas, graficos e relatorios.
# =========================================================


# ---------------------------------------------------------
# 1. Remover colunas que contem QUALQUER valor NA
#
# Mantem apenas colunas completamente observadas (100% sem NA).
#
# Util para:
# - tabelas descritivas
# - visualizacao de dados
# - exportacao de bancos "limpos"
#
# Atencao: nao recomendado para uso direto em ANOVA ou modelos,
# pois pode alterar a estrutura experimental.
# ---------------------------------------------------------
#' Remove colunas com qualquer valor ausente
#'
#' @param dados `data.frame` de entrada.
#'
#' @return `data.frame` sem colunas que possuam `NA`.
#' @export
remove_colunas_com_na <- function(dados) {
  dados |>
    dplyr::select(dplyr::where(~ !any(is.na(.))))
}


# ---------------------------------------------------------
# 2. Selecionar apenas colunas que POSSUEM algum NA
#
# Retorna somente as colunas que apresentam pelo menos
# um valor ausente (NA).
#
# Util para:
# - diagnostico de dados faltantes
# - auditoria do banco
# - decidir estrategias de imputacao
# ---------------------------------------------------------
#' Seleciona apenas colunas com algum valor ausente
#'
#' @param dados `data.frame` de entrada.
#'
#' @return `data.frame` contendo somente colunas com `NA`.
#' @export
seleciona_colunas_com_na <- function(dados) {
  dados |>
    dplyr::select(dplyr::where(~ any(is.na(.))))
}


# ---------------------------------------------------------
# 3. Remover apenas colunas TOTALMENTE vazias (todos NA)
#
# Mantem colunas que tem ao menos um valor observado.
#
# Util quando:
# - algumas variaveis nao foram medidas
# - existem colunas "placeholder" no banco
#
# Obs.: mais seguro que remove_colunas_com_na()
# ---------------------------------------------------------
#' Remove colunas totalmente ausentes
#'
#' @param dados `data.frame` de entrada.
#'
#' @return `data.frame` sem colunas onde todos os valores sao `NA`.
#' @export
remove_colunas_todas_na <- function(dados) {
  dados |>
    dplyr::select(dplyr::where(~ !all(is.na(.))))
}


# ---------------------------------------------------------
# 4. Listar nomes das colunas que possuem NA
#
# Retorna um vetor de caracteres com os nomes das colunas
# que apresentam pelo menos um valor ausente.
#
# Util para relatorios automaticos e logs.
# ---------------------------------------------------------
#' Lista nomes de colunas com valores ausentes
#'
#' @param dados `data.frame` de entrada.
#'
#' @return Vetor de caracteres com nomes de colunas contendo `NA`.
#' @export
colunas_com_na <- function(dados) {
  names(dados)[colSums(is.na(dados)) > 0]
}


# ---------------------------------------------------------
# 5. Manter colunas especificas + colunas com algum NA
#
# Mantem:
# - todas as colunas explicitamente informadas pelo usuario
# - TODAS as colunas que possuem ao menos um NA
#
# Util para:
# - auditoria de dados
# - relatorios de qualidade do banco
# - inspecao de variaveis problematicas sem perder fatores
#
# Parametros:
# - dados: data.frame
# - colunas_fixas: vetor de nomes de colunas que SEMPRE
#   devem ser mantidas (ex.: bloco, fatores, id)
# ---------------------------------------------------------
#' Mantem colunas fixas e colunas com valores ausentes
#'
#' @param dados `data.frame` de entrada.
#' @param colunas_fixas Vetor de nomes de colunas que devem ser mantidas.
#'
#' @return `data.frame` filtrado pelas colunas solicitadas.
#' @export
manter_colunas_fixas_e_com_na <- function(dados, colunas_fixas) {
  
  stopifnot(is.character(colunas_fixas))
  
  # Colunas que possuem algum NA
  colunas_na <- names(dados)[colSums(is.na(dados)) > 0]
  
  # Uniao das colunas
  colunas_manter <- union(colunas_fixas, colunas_na)
  
  dados |>
    dplyr::select(dplyr::any_of(colunas_manter))
}
