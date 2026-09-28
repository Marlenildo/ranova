# =========================================================
# 06_funcoes_modelo_fatorial_medias.R
#
# Conjunto de funcoes genericas para:
# - Ajuste de modelos de experimentos simples e fatoriais (DIC ou DBC)
# - Analise de variancia (ANOVA)
# - Testes de comparacao de medias
#
# PRINCIPIO ARQUITETURAL:
# - Este arquivo NAO conhece variaveis experimentais
# - Nomes, siglas e unidades sao definidos no R_local
# - A biblioteca apenas CONSOME um dicionario opcional
#
# - Aceita 1 fator (experimento simples) ou mais (esquema fatorial)
# - Aceita DIC (sem bloco) ou DBC (com bloco)
# - Funciona para qualquer banco de dados
#
# Autor: Marlenildo Ferreira Melo
# =========================================================


# =========================================================
# FUNCOES AUXILIARES GERAIS
# =========================================================

# ---------------------------------------------------------
# Simbolos de significancia estatistica
# ---------------------------------------------------------
#' @keywords internal
#' @noRd
sig_star <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.001) "***"
  else if (p < 0.01) "**"
  else if (p < 0.05) "*"
  else ""
}

# ---------------------------------------------------------
# Formatacao de p-valor
# ---------------------------------------------------------
#' @keywords internal
#' @noRd
formata_p_valor <- function(p, digitos = 4) {
  if (is.na(p)) return("")
  if (p < 10^(-digitos)) {
    paste0("< ", formatC(10^(-digitos), digits = digitos, format = "f"))
  } else {
    formatC(p, digits = digitos, format = "f")
  }
}

# ---------------------------------------------------------
# Pintar celulas com significancia (kable)
# ---------------------------------------------------------
#' @keywords internal
#' @noRd
pinta_se_signif <- function(x) {
  ifelse(
    grepl("\\*", x),
    kableExtra::cell_spec(
      x,
      background = "#FFF3CD",
      format = "html"
    ),
    x
  )
}

# ---------------------------------------------------------
# Resolver rotulo de variavel via dicionario (GENERICO)
#
# - dic_vars E DEFINIDO NO R_local
# - Estrutura esperada:
#   tibble(var, sigla, label, description)
# ---------------------------------------------------------
#' Resolve rotulo de variavel por dicionario
#'
#' @param var Nome da variavel.
#' @param dic_vars Dicionario opcional com colunas `var`, `sigla`, `label`, `description`.
#' @param type Tipo de rotulo retornado.
#'
#' @return Vetor de caracteres com o rotulo resolvido.
#' @export
resolve_var_label <- function(
    var,
    dic_vars = NULL,
    type = c("label", "sigla", "description", "var")
) {
  type <- match.arg(type)
  
  # Sem dicionario -> fallback total
  if (is.null(dic_vars)) return(var)
  
  # Verificacao minima de contrato
  stopifnot("var" %in% names(dic_vars))
  
  # Variavel fora do dicionario -> fallback
  if (!var %in% dic_vars$var) return(var)
  
  linha <- dic_vars[dic_vars$var == var, , drop = FALSE]

  if (type == "var") return(var)
  if (!type %in% names(linha)) return(var)

  valor <- linha[[type]][1]
  if (is.null(valor) || is.na(valor) || !nzchar(as.character(valor))) return(var)

  as.character(valor)
}


# =========================================================
# 1. Ajuste generico do modelo (experimento simples ou fatorial)
# =========================================================
#' Ajusta modelo de experimento simples ou fatorial (DIC ou DBC)
#'
#' Modelo com todos os efeitos principais e interacoes dos fatores e, no DBC,
#' o efeito de bloco. Para parcelas subdivididas, use [ranova_ajuste()].
#'
#' @param dados `data.frame` com os dados experimentais.
#' @param resposta Nome da variavel resposta.
#' @param bloco Nome da coluna de blocos (DBC). Use `NULL` para DIC.
#' @param fatores Vetor com nomes dos fatores.
#'
#' @return Objeto `aov`.
#' @export
ajusta_modelo_fatorial <- function(
    dados,
    resposta,
    bloco = NULL,
    fatores
) {
  stopifnot(
    is.character(resposta),
    is.character(fatores)
  )
  
  termo_fatores <- paste(fatores, collapse = " * ")
  
  formula_txt <- if (!is.null(bloco)) {
    paste(resposta, "~", bloco, "+", termo_fatores)
  } else {
    paste(resposta, "~", termo_fatores)
  }
  
  stats::aov(stats::as.formula(formula_txt), data = dados)
}


# =========================================================
# 2. Tabela de ANOVA (quadrados medios)
# =========================================================
#' Gera tabela de ANOVA
#'
#' Quadro da ANOVA de experimentos simples ou fatoriais em DIC ou DBC, para uma
#' ou varias respostas. Para parcelas subdivididas, use [ranova_anova()].
#'
#' @param dados `data.frame` com os dados experimentais.
#' @param variaveis Vetor de nomes das variaveis resposta.
#' @param bloco Nome da coluna de blocos (DBC). Use `NULL` para DIC.
#' @param fatores Vetor com nomes dos fatores.
#' @param dic_vars Dicionario opcional com colunas `var`, `sigla`, `label`, `description`.
#' @param label_type Tipo de rotulo para variaveis resposta.
#' @param formato Formato de exibicao da tabela (`qm_star`, `f_p_colunas`, `f_p_inline`).
#' @param caption Legenda da tabela.
#' @param digitos Numero de casas decimais.
#'
#' @return Objeto `knitr_kable`.
#' @export
anova_fatorial_qm_tabela <- function(
    dados,
    variaveis,
    bloco = NULL,
    fatores,
    dic_vars = NULL,
    label_type = c("label", "sigla", "description", "var"),
    formato = c("qm_star", "f_p_colunas", "f_p_inline"),
    caption = "Resumo da an\u00E1lise de vari\u00E2ncia.",
    digitos = 4
) {
  stopifnot(
    is.character(variaveis),
    is.character(fatores)
  )
  label_type <- match.arg(label_type)
  formato <- match.arg(formato)
  
  colunas_usadas <- c(bloco, fatores, variaveis)
  colunas_usadas <- colunas_usadas[!is.null(colunas_usadas)]
  
  dados_anova <- dados |>
    dplyr::select(dplyr::all_of(colunas_usadas))
  
  termo_fatores <- paste(fatores, collapse = " * ")
  
  formula_base <- if (!is.null(bloco)) {
    paste(bloco, "+", termo_fatores)
  } else {
    termo_fatores
  }
  
  modelo_ref <- stats::aov(
    stats::as.formula(paste(variaveis[1], "~", formula_base)),
    data = dados_anova
  )
  
  anova_ref <- summary(modelo_ref)[[1]]
  
  tabela <- data.frame(
    FV = sub("^Residuals$", "Res\u00edduo", trimws(rownames(anova_ref))),
    GL = anova_ref$Df,
    stringsAsFactors = FALSE
  )
  
  for (v in variaveis) {
    
    modelo <- stats::aov(
      stats::as.formula(paste(v, "~", formula_base)),
      data = dados_anova
    )
    
    anova_tab <- summary(modelo)[[1]]
    
    qm <- anova_tab$`Mean Sq`
    f  <- anova_tab$`F value`
    p  <- anova_tab$`Pr(>F)`
    
    if (formato == "qm_star") {
      tabela[[v]] <- mapply(
        function(qm_i, p_i) {
          if (is.na(qm_i)) return("")
          
          texto <- paste0(
            formatC(qm_i, digits = digitos, format = "f"),
            ifelse(sig_star(p_i) == "", "", paste0(" ", sig_star(p_i)))
          )
          pinta_se_signif(texto)
        },
        qm,
        p
      )
    }
    
    if (formato == "f_p_colunas") {
      tabela[[paste0(v, "_F")]] <- ifelse(
        is.na(f), "",
        formatC(f, digits = digitos, format = "f")
      )
      tabela[[paste0(v, "_p")]] <- sapply(p, formata_p_valor, digitos = digitos)
    }
    
    if (formato == "f_p_inline") {
      tabela[[v]] <- mapply(
        function(f_i, p_i) {
          if (is.na(f_i)) return("")
          paste0(
            formatC(f_i, digits = digitos, format = "f"),
            " (",
            formata_p_valor(p_i, digitos = digitos),
            ")"
          )
        },
        f,
        p
      )
    }
  }
  
  cv_valores <- sapply(variaveis, function(v) {
    modelo <- stats::aov(
      stats::as.formula(paste(v, "~", formula_base)),
      data = dados_anova
    )
    anova_tab <- summary(modelo)[[1]]
    qm_erro <- utils::tail(anova_tab$`Mean Sq`, 1)
    media   <- mean(dados_anova[[v]], na.rm = TRUE)
    (sqrt(qm_erro) / media) * 100
  })
  
  linha_cv <- c("CV (%)", "")
  
  if (formato == "f_p_colunas") {
    cv_expandido <- as.vector(rbind(sprintf("%.2f", cv_valores), rep("", length(cv_valores))))
    linha_cv <- c(linha_cv, cv_expandido)
  } else {
    linha_cv <- c(linha_cv, sprintf("%.2f", cv_valores))
  }
  
  tabela <- rbind(tabela, linha_cv)
  
  nomes_cols <- sapply(
    variaveis,
    resolve_var_label,
    dic_vars = dic_vars,
    type = label_type
  )
  
  header_top <- NULL
  nota_rodape <- NULL
  
  if (formato == "f_p_colunas") {
    colnames(tabela) <- c("FV", "GL", rep(c("F", "p"), length(variaveis)))
    header_top <- c(" " = 2, stats::setNames(rep(2, length(variaveis)), nomes_cols))
    nota_rodape <- "F = valor do teste F; p = valor-p."
  } else if (formato == "f_p_inline") {
    colnames(tabela) <- c("FV", "GL", nomes_cols)
    header_top <- c(" " = 2, "F (p)" = length(variaveis))
    nota_rodape <- "F (p) = valor do teste F com valor-p entre par\u00EAnteses."
  } else {
    colnames(tabela) <- c("FV", "GL", nomes_cols)
    header_top <- c(" " = 2, "QM" = length(variaveis))
    nota_rodape <- "QM = quadrado m\u00E9dio; * p < 0,05; ** p < 0,01; *** p < 0,001"
  }
  
  tab_html <- tabela |>
    knitr::kable(
      caption = caption,
      escape  = FALSE,
      align   = "l",
      format  = "html"
    )
  
  tab_html <- tab_html |>
    kableExtra::add_header_above(header_top, align = "l") |>
    kableExtra::kable_classic(
      bootstrap_options = "striped",
      full_width = FALSE
    ) |>
    kableExtra::footnote(
      nota_rodape,
      general_title = ""
    )
  
  tab_html
}


# =========================================================
# 3. Escolha automatica do teste de medias
# =========================================================
#' @keywords internal
#' @noRd
escolhe_teste_medias <- function(dados, fator) {
  stopifnot(is.character(fator))
  if (length(unique(dados[[fator]])) == 2) "t" else "tukey"
}


# =========================================================
# 4. Erros-padrao descritivos (simples e interacao)
# =========================================================
#' @keywords internal
#' @noRd
se_descritivo <- function(dados, resposta, fator) {
  dados |>
    dplyr::group_by(.data[[fator]]) |>
    rstatix::get_summary_stats(
      !!rlang::sym(resposta),
      type = "mean_se"
    ) |>
    dplyr::select(nivel = .data[[fator]], se_desc = se)
}

#' @keywords internal
#' @noRd
se_descritivo_interacao <- function(
    dados,
    resposta,
    fator_linha,
    fator_coluna
) {
  dados |>
    dplyr::group_by(
      .data[[fator_linha]],
      .data[[fator_coluna]]
    ) |>
    rstatix::get_summary_stats(
      !!rlang::sym(resposta),
      type = "mean_se"
    ) |>
    dplyr::select(
      nivel_linha  = .data[[fator_linha]],
      nivel_coluna = .data[[fator_coluna]],
      se_desc = se
    )
}


# =========================================================
# 5. Medias ajustadas + CLD
# =========================================================
#' Calcula medias ajustadas e grupos de comparacao
#'
#' @param dados `data.frame` com os dados experimentais.
#' @param resposta Nome da variavel resposta.
#' @param fator_interesse Nome do fator para comparacao.
#' @param bloco Nome da coluna de blocos (DBC). Use `NULL` para DIC.
#' @param fatores Vetor com nomes dos fatores.
#' @param alpha Nivel de significancia.
#' @param tipo_se Tipo de erro-padrao (`modelo` ou `descritivo`).
#'
#' @return `tibble` com niveis, medias, erro-padrao e grupos.
#' @export
medias_fatorial_cld <- function(
    dados,
    resposta,
    fator_interesse,
    bloco = NULL,
    fatores,
    alpha = 0.05,
    tipo_se = c("modelo", "descritivo")
) {
  tipo_se <- match.arg(tipo_se)
  
  modelo <- ajusta_modelo_fatorial(
    dados, resposta, bloco, fatores
  )
  
  teste <- escolhe_teste_medias(dados, fator_interesse)
  
  em <- emmeans::emmeans(
    modelo,
    stats::as.formula(paste("~", fator_interesse))
  )
  
  letras <- multcomp::cld(
    em,
    alpha    = alpha,
    adjust   = ifelse(teste == "t", "none", "tukey"),
    Letters  = letters,
    reversed = TRUE
  )
  
  resultado <- tibble::tibble(
    nivel = letras[[fator_interesse]],
    media = letras$emmean,
    se    = letras$SE,
    grupo = letras$.group
  )
  
  if (tipo_se == "descritivo") {
    se_desc <- se_descritivo(dados, resposta, fator_interesse)
    resultado <- resultado |>
      dplyr::left_join(se_desc, by = "nivel") |>
      dplyr::mutate(se = se_desc) |>
      dplyr::select(-se_desc)
  }
  
  resultado
}


# =========================================================
# 6. Tabela final de medias
# =========================================================
#' Monta tabela de medias com letras
#'
#' @param dados `data.frame` com os dados experimentais.
#' @param variaveis Vetor de nomes das variaveis resposta.
#' @param fator_interesse Nome do fator para comparacao.
#' @param bloco Nome da coluna de blocos (DBC). Use `NULL` para DIC.
#' @param fatores Vetor com nomes dos fatores.
#' @param dic_vars Dicionario opcional com colunas `var`, `sigla`, `label`, `description`.
#' @param label_type Tipo de rotulo para variaveis resposta.
#' @param caption Legenda da tabela.
#' @param digitos Numero de casas decimais.
#' @param tipo_se Tipo de erro-padrao (`modelo` ou `descritivo`).
#'
#' @return Objeto `knitr_kable`.
#' @export
tabela_medias_fatorial <- function(
    dados,
    variaveis,
    fator_interesse,
    bloco = NULL,
    fatores,
    dic_vars = NULL,
    label_type = c("label", "sigla", "description", "var"),
    caption = "M\u00E9dias ajustadas \u00B1 erro-padr\u00E3o.",
    digitos = 2,
    tipo_se = c("modelo", "descritivo")
) {
  label_type <- match.arg(label_type)
  tipo_se    <- match.arg(tipo_se)
  
  resultado <- purrr::map_dfr(
    variaveis,
    function(v) {
      res <- medias_fatorial_cld(
        dados, v, fator_interesse,
        bloco, fatores, tipo_se = tipo_se
      )
      
      nome_var <- resolve_var_label(
        v, dic_vars, label_type
      )
      
      res |>
        dplyr::mutate(
          variavel = nome_var,
          media_grupo = paste0(
            round(media, digitos),
            " \u00B1 ",
            round(se, digitos),
            " ",
            grupo
          )
        ) |>
        dplyr::select(nivel, variavel, media_grupo)
    }
  )
  
  resultado |>
    tidyr::pivot_wider(
      names_from  = variavel,
      values_from = media_grupo
    ) |>
    knitr::kable(caption = caption) |>
    kableExtra::kable_classic(
      html_font = "Arial",
      bootstrap_options = "striped",
      full_width = FALSE
    )
}


# ---------------------------------------------------------
# 7. Tabela de desdobramento da interacao
#
# Suporta dicionario externo para rotulagem da variavel
# resposta, definido no R_local.
# ---------------------------------------------------------
#' Tabela de desdobramento da interacao
#'
#' @param dados `data.frame` com os dados experimentais.
#' @param resposta Nome da variavel resposta.
#' @param fator_linha Fator para linhas.
#' @param fator_coluna Fator para colunas.
#' @param bloco Nome da coluna de blocos (DBC). Use `NULL` para DIC.
#' @param fatores Vetor com nomes dos fatores.
#' @param dic_vars Dicionario opcional com colunas `var`, `sigla`, `label`, `description`.
#' @param label_type Tipo de rotulo para variavel resposta.
#' @param digitos Numero de casas decimais.
#' @param alpha Nivel de significancia.
#' @param caption Legenda da tabela.
#'
#' @return Objeto `knitr_kable`.
#' @export
tabela_interacao_fatorial <- function(
    dados,
    resposta,
    fator_linha,
    fator_coluna,
    bloco = NULL,
    fatores,
    dic_vars = NULL,
    label_type = c("label", "sigla", "description", "var"),
    digitos = 2,
    alpha = 0.05,
    caption = "Desdobramento da intera\u00E7\u00E3o."
) {
  
  stopifnot(
    is.character(resposta),
    is.character(fator_linha),
    is.character(fator_coluna),
    is.character(fatores)
  )
  
  label_type <- match.arg(label_type)
  
  # Nome da variavel resposta (semantico)
  nome_var <- resolve_var_label(
    resposta,
    dic_vars = dic_vars,
    type     = label_type
  )
  
  # Ajuste do modelo fatorial completo
  modelo <- ajusta_modelo_fatorial(
    dados    = dados,
    resposta = resposta,
    bloco    = bloco,
    fatores  = fatores
  )
  
  # =================================================
  # 1. COLUNAS -> letras MAIUSCULAS
  # =================================================
  teste_col <- escolhe_teste_medias(dados, fator_coluna)
  
  em_col <- emmeans::emmeans(
    modelo,
    stats::as.formula(paste("~", fator_coluna, "|", fator_linha))
  )
  
  col_cld <- multcomp::cld(
    em_col,
    alpha    = alpha,
    adjust   = ifelse(teste_col == "t", "none", "tukey"),
    Letters  = LETTERS,
    reversed = TRUE
  ) |>
    dplyr::mutate(
      nivel_linha  = .data[[fator_linha]],
      nivel_coluna = .data[[fator_coluna]],
      letra_col    = trimws(.group)
    )
  
  # =================================================
  # 2. LINHAS -> letras minusculas
  # =================================================
  teste_lin <- escolhe_teste_medias(dados, fator_linha)
  
  em_lin <- emmeans::emmeans(
    modelo,
    stats::as.formula(paste("~", fator_linha, "|", fator_coluna))
  )
  
  lin_cld <- multcomp::cld(
    em_lin,
    alpha    = alpha,
    adjust   = ifelse(teste_lin == "t", "none", "tukey"),
    Letters  = letters,
    reversed = TRUE
  ) |>
    dplyr::mutate(
      nivel_linha  = .data[[fator_linha]],
      nivel_coluna = .data[[fator_coluna]],
      letra_lin    = trimws(.group)
    )
  
  # =================================================
  # 3. Combinacao final
  # =================================================
  tabela_final <- col_cld |>
    dplyr::left_join(
      lin_cld |>
        dplyr::select(nivel_linha, nivel_coluna, letra_lin),
      by = c("nivel_linha", "nivel_coluna")
    ) |>
    dplyr::mutate(
      media_fmt = paste0(
        round(.data$emmean, digitos), " ",
        .data$letra_lin, .data$letra_col
      )
    ) |>
    dplyr::select(nivel_linha, nivel_coluna, media_fmt) |>
    stats::setNames(c(fator_linha, fator_coluna, "media_fmt")) |>
    tidyr::pivot_wider(
      names_from  = dplyr::all_of(fator_coluna),
      values_from = .data$media_fmt
    )
  
  knitr::kable(
    tabela_final,
    caption = paste(nome_var, "-", caption),
    escape  = FALSE
  ) |>
    kableExtra::kable_classic(
      html_font  = "Arial",
      full_width = FALSE
    ) |>
    kableExtra::footnote(
      general_title = "",
      "Letras maiusculas comparam medias na coluna e letras minusculas comparam medias na linha.
       Teste t (2 niveis) ou Tukey (>= 3 niveis), p < 0,05."
    )
}


# ---------------------------------------------------------
# 8. Desdobramento da interacao
#    para multiplas variaveis resposta
#
# Totalmente compativel com dicionario externo
# ---------------------------------------------------------
#' Desdobra a interacao para varias respostas
#'
#' @param dados `data.frame` com os dados experimentais.
#' @param variaveis Vetor de nomes das variaveis resposta.
#' @param fator_linha Fator para linhas.
#' @param fator_coluna Fator para colunas.
#' @param bloco Nome da coluna de blocos (DBC). Use `NULL` para DIC.
#' @param fatores Vetor com nomes dos fatores.
#' @param dic_vars Dicionario opcional com colunas `var`, `sigla`, `label`, `description`.
#' @param label_type Tipo de rotulo para variavel resposta.
#' @param digitos Numero de casas decimais.
#' @param alpha Nivel de significancia.
#' @param tipo_se Tipo de erro-padrao (`modelo` ou `descritivo`).
#' @param caption Legenda da tabela.
#'
#' @return Objeto `knitr_kable`.
#' @export
tabela_interacao_fatorial_multivariaveis <- function(
    dados,
    variaveis,
    fator_linha,
    fator_coluna,
    bloco = NULL,
    fatores,
    dic_vars = NULL,
    label_type = c("label", "sigla", "description", "var"),
    digitos = 2,
    alpha = 0.05,
    tipo_se = c("modelo", "descritivo"),
    caption = "Desdobramento da intera\u00E7\u00E3o."
) {
  
  stopifnot(
    is.character(variaveis),
    is.character(fator_linha),
    is.character(fator_coluna),
    is.character(fatores)
  )
  
  label_type <- match.arg(label_type)
  tipo_se    <- match.arg(tipo_se)
  
  resultado <- purrr::map_dfr(
    variaveis,
    function(v) {
      
      nome_var <- resolve_var_label(
        v,
        dic_vars = dic_vars,
        type     = label_type
      )
      
      modelo <- ajusta_modelo_fatorial(
        dados    = dados,
        resposta = v,
        bloco    = bloco,
        fatores  = fatores
      )
      
      # COLUNAS
      teste_col <- escolhe_teste_medias(dados, fator_coluna)
      em_col <- emmeans::emmeans(
        modelo,
        stats::as.formula(paste("~", fator_coluna, "|", fator_linha))
      )
      
      col_cld <- multcomp::cld(
        em_col,
        alpha    = alpha,
        adjust   = ifelse(teste_col == "t", "none", "tukey"),
        Letters  = LETTERS,
        reversed = TRUE
      ) |>
        dplyr::mutate(
          nivel_linha  = .data[[fator_linha]],
          nivel_coluna = .data[[fator_coluna]],
          letra_col    = trimws(.group)
        )
      
      # LINHAS
      teste_lin <- escolhe_teste_medias(dados, fator_linha)
      em_lin <- emmeans::emmeans(
        modelo,
        stats::as.formula(paste("~", fator_linha, "|", fator_coluna))
      )
      
      lin_cld <- multcomp::cld(
        em_lin,
        alpha    = alpha,
        adjust   = ifelse(teste_lin == "t", "none", "tukey"),
        Letters  = letters,
        reversed = TRUE
      ) |>
        dplyr::mutate(
          nivel_linha  = .data[[fator_linha]],
          nivel_coluna = .data[[fator_coluna]],
          letra_lin    = trimws(.group)
        )
      
      tab_inter <- col_cld |>
        dplyr::left_join(
          lin_cld |>
            dplyr::select(nivel_linha, nivel_coluna, letra_lin),
          by = c("nivel_linha", "nivel_coluna")
        )
      
      if (tipo_se == "descritivo") {
        se_desc <- se_descritivo_interacao(
          dados, v, fator_linha, fator_coluna
        )
        
        tab_inter <- tab_inter |>
          dplyr::left_join(
            se_desc,
            by = c("nivel_linha", "nivel_coluna")
          ) |>
          dplyr::mutate(SE = se_desc) |>
          dplyr::select(-se_desc)
      }
      
      tab_inter |>
        dplyr::mutate(
          Variavel = nome_var,
          media_fmt = paste0(
            round(.data$emmean, digitos), " \u00B1 ",
            round(.data$SE, digitos), " ",
            .data$letra_lin, .data$letra_col
          )
        ) |>
        dplyr::select(Variavel, nivel_linha, nivel_coluna, media_fmt) |>
        stats::setNames(c("Variavel", fator_linha, fator_coluna, "media_fmt"))
    }
  )
  
  tabela_final <- resultado |>
    tidyr::pivot_wider(
      names_from  = dplyr::all_of(fator_coluna),
      values_from = .data$media_fmt
    )
  
  grupos_variavel <- rle(as.character(tabela_final$Variavel))
  indice_grupos <- stats::setNames(
    grupos_variavel$lengths,
    grupos_variavel$values
  )
  
  tabela_final |>
    dplyr::select(-.data$Variavel) |>
    knitr::kable(
      caption = caption,
      escape  = FALSE
    ) |>
    kableExtra::kable_classic(
      html_font  = "Arial",
      full_width = FALSE
    ) |>
    kableExtra::pack_rows(index = indice_grupos)
}


