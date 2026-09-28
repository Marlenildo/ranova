# =========================================================
# 05_funcoes_graficos.R
#
# Funcoes graficas reutilizaveis para experimentos simples e fatoriais
#
# PRINCIPIO ARQUITETURAL:
# - Este arquivo NAO conhece variaveis experimentais
# - Rotulos, siglas e unidades vem do R_local (dic_vars)
# - Todas as funcoes usam resolve_var_label()
#
# Compativel com:
# - DIC e DBC
# - 1, 2 ou n fatores
# - Relatorios HTML, PDF, Shiny
#
# Depende de:
# - ggplot2
# - dplyr
# - ggpubr
# - emmeans
#
# Autor: Marlenildo Ferreira Melo
# =========================================================


# =========================================================
# 1. Grafico simples: media +- erro-padrao
#
# Usado quando os dados ja estao resumidos (mean, se),
# normalmente apos summarise().
# =========================================================
#' Grafico de media e erro-padrao
#'
#' @param dados Tabela com colunas `name`, `mean` e `se`.
#' @param variavel Nome da variavel resposta no campo `name`.
#' @param x_var Variavel do eixo x.
#' @param x_label Rotulo do eixo x.
#' @param dic_vars Dicionario opcional com colunas `var`, `sigla`, `label`, `description`.
#' @param label_type Tipo de rotulo da variavel resposta.
#'
#' @return Objeto `ggplot2`.
#' @export
grafico_media_ep <- function(
    dados,
    variavel,
    x_var,
    x_label,
    dic_vars   = NULL,
    label_type = c("label", "sigla", "description", "var")
) {
  
  label_type <- match.arg(label_type)
  
  # Resolve rotulo da variavel resposta
  y_label <- resolve_var_label(
    variavel,
    dic_vars = dic_vars,
    type     = label_type
  )
  
  ggplot2::ggplot(
    dados |> dplyr::filter(.data$name == variavel),
    ggplot2::aes(x = {{ x_var }}, y = .data$mean)
  ) +
    ggplot2::geom_col(fill = "grey80", color = "black") +
    ggplot2::geom_errorbar(
      ggplot2::aes(
        ymin = .data$mean - .data$se,
        ymax = .data$mean + .data$se
      ),
      width = 0.2
    ) +
    ggplot2::labs(
      x = x_label,
      y = y_label
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid = ggplot2::element_blank())
}


# =========================================================
# 2. Painel com multiplas variaveis
#
# Combina varios graficos de media ? EP em um layout unico
# =========================================================
#' Painel com multiplas variaveis resposta
#'
#' @param dados Tabela com dados resumidos.
#' @param variaveis Vetor com nomes das variaveis resposta.
#' @param x_var Variavel do eixo x.
#' @param x_label Rotulo do eixo x.
#' @param dic_vars Dicionario opcional com colunas `var`, `sigla`, `label`, `description`.
#' @param label_type Tipo de rotulo da variavel resposta.
#' @param ncol Numero de colunas no painel.
#' @param nrow Numero de linhas no painel.
#' @param labels Se `TRUE`, adiciona letras A, B, C.
#'
#' @return Objeto `ggpubr::ggarrange`.
#' @export
grafico_multiplas_variaveis <- function(
    dados,
    variaveis,
    x_var,
    x_label,
    dic_vars   = NULL,
    label_type = c("label", "sigla", "description", "var"),
    ncol = NULL,
    nrow = NULL,
    labels = TRUE
) {
  
  label_type <- match.arg(label_type)
  
  graficos <- lapply(
    variaveis,
    function(var) {
      grafico_media_ep(
        dados      = dados,
        variavel   = var,
        x_var      = {{ x_var }},
        x_label    = x_label,
        dic_vars   = dic_vars,
        label_type = label_type
      )
    }
  )
  
  n_graficos <- length(graficos)
  
  # Definicao automatica do layout
  if (is.null(ncol) & is.null(nrow)) {
    ncol <- 2
    nrow <- ceiling(n_graficos / ncol)
  } else if (is.null(ncol)) {
    ncol <- ceiling(n_graficos / nrow)
  } else if (is.null(nrow)) {
    nrow <- ceiling(n_graficos / ncol)
  }
  
  ggpubr::ggarrange(
    plotlist = graficos,
    ncol     = ncol,
    nrow     = nrow,
    labels   = if (labels) LETTERS[seq_along(graficos)] else NULL
  )
}


# =========================================================
# 3. Grafico de medias fatoriais com letras (CLD)
#
# Usa o MESMO modelo da ANOVA
# =========================================================
#' Grafico de medias com letras de comparacao
#'
#' @param dados `data.frame` com os dados experimentais.
#' @param resposta Nome da variavel resposta.
#' @param fator_interesse Nome do fator para comparacao de medias.
#' @param bloco Nome do bloco (DBC). Use `NULL` para DIC.
#' @param fatores Vetor de nomes dos fatores do modelo.
#' @param dic_vars Dicionario opcional com colunas `var`, `sigla`, `label`, `description`.
#' @param label_type Tipo de rotulo da variavel resposta.
#' @param digitos Numero de casas decimais.
#'
#' @return Objeto `ggplot2`.
#' @export
grafico_medias_fatorial <- function(
    dados,
    resposta,
    fator_interesse,
    bloco = NULL,
    fatores,
    dic_vars   = NULL,
    label_type = c("label", "sigla", "description", "var"),
    digitos = 2
) {
  
  label_type <- match.arg(label_type)
  
  # Medias ajustadas + letras
  res <- medias_fatorial_cld(
    dados           = dados,
    resposta        = resposta,
    fator_interesse = fator_interesse,
    bloco           = bloco,
    fatores         = fatores
  )
  
  # Rotulo semantico da variavel resposta
  y_label <- resolve_var_label(
    resposta,
    dic_vars = dic_vars,
    type     = label_type
  )
  
  ggplot2::ggplot(
    res,
    ggplot2::aes(x = .data$nivel, y = .data$media)
  ) +
    ggplot2::geom_col(fill = "grey80", color = "black") +
    ggplot2::geom_errorbar(
      ggplot2::aes(
        ymin = .data$media - .data$se,
        ymax = .data$media + .data$se
      ),
      width = 0.2
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = .data$grupo),
      vjust = -0.5,
      size = 5
    ) +
    ggplot2::labs(
      x = fator_interesse,
      y = y_label
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid = ggplot2::element_blank())
}


# =========================================================
# 4. Grafico de interacao fatorial
#
# Representa medias ajustadas para dois fatores
# =========================================================
#' Grafico de interacao para dois fatores
#'
#' @param dados `data.frame` com os dados experimentais.
#' @param resposta Nome da variavel resposta.
#' @param fator_x Fator no eixo x.
#' @param fator_traco Fator representado por cor/linha.
#' @param bloco Nome do bloco (DBC). Use `NULL` para DIC.
#' @param fatores Vetor de nomes dos fatores do modelo.
#' @param dic_vars Dicionario opcional com colunas `var`, `sigla`, `label`, `description`.
#' @param label_type Tipo de rotulo da variavel resposta.
#' @param digitos Numero de casas decimais.
#'
#' @return Objeto `ggplot2`.
#' @export
grafico_interacao_fatorial <- function(
    dados,
    resposta,
    fator_x,
    fator_traco,
    bloco = NULL,
    fatores,
    dic_vars   = NULL,
    label_type = c("label", "sigla", "description", "var"),
    digitos = 2
) {
  
  label_type <- match.arg(label_type)
  
  # Ajuste do modelo fatorial completo
  modelo <- ajusta_modelo_fatorial(
    dados    = dados,
    resposta = resposta,
    bloco    = bloco,
    fatores  = fatores
  )
  
  # Medias ajustadas
  em <- emmeans::emmeans(
    modelo,
    stats::as.formula(paste("~", fator_x, "*", fator_traco))
  )
  
  df <- as.data.frame(em)
  
  # Rotulo da variavel resposta
  y_label <- resolve_var_label(
    resposta,
    dic_vars = dic_vars,
    type     = label_type
  )
  
  ggplot2::ggplot(
    df,
    ggplot2::aes_string(
      x     = fator_x,
      y     = "emmean",
      group = fator_traco,
      color = fator_traco
    )
  ) +
    ggplot2::geom_point(size = 3) +
    ggplot2::geom_line() +
    ggplot2::geom_errorbar(
      ggplot2::aes(
        ymin = .data$emmean - .data$SE,
        ymax = .data$emmean + .data$SE
      ),
      width = 0.15
    ) +
    ggplot2::labs(
      x     = fator_x,
      y     = y_label,
      color = fator_traco
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid = ggplot2::element_blank())
}

