library(shiny)
library(dplyr)
library(ggplot2)
library(rhandsontable)
library(ranova)

options(knitr.table.format = "html")

`%||%` <- function(x, y) if (is.null(x)) y else x

APP_VERSION <- tryCatch(
  readLines("VERSION", warn = FALSE)[1],
  error = function(e) "dev"
)

PACOTE_VERSAO <- tryCatch(
  as.character(utils::packageVersion("ranova")),
  error = function(e) "dev"
)

COR_PRIMARIA <- "#548238"
COR_SECUNDARIA <- "#ffc000"
PALETA_FATORES <- c("#548238", "#ffc000", "#264653", "#b43d36", "#7a8576", "#8fbf6a", "#e9a13b", "#4f7d8c")

MAX_FATORES <- 3

TEXTO_PRIVACIDADE <- paste(
  "Os dados digitados são usados apenas durante a sessão ativa para realizar os cálculos.",
  "A MelonMundi não armazena esses dados em banco de dados, arquivos, cookies ou armazenamento do navegador.",
  "Ao atualizar, fechar ou sair da página, as informações são descartadas."
)

# ---------------------------------------------------------
# Componentes de interface compartilhados por ui.R e server.R
# ---------------------------------------------------------

normal_card <- function(title, content, class = NULL) {
  div(
    class = paste("mm-surface-card mm-section-box ranova-panel-card", class),
    h3(class = "ranova-section-title", title),
    div(class = "ranova-panel-body", content)
  )
}

# ---------------------------------------------------------
# Leitura e conversão de dados
# ---------------------------------------------------------

texto_vazio <- function(x) {
  is.na(x) | trimws(as.character(x)) == ""
}

converter_numero <- function(x) {
  if (is.numeric(x)) {
    return(as.numeric(x))
  }
  texto <- trimws(as.character(x))
  texto[texto == ""] <- NA_character_
  texto <- gsub("\\s+", "", texto)
  com_virgula <- !is.na(texto) & grepl(",", texto, fixed = TRUE)
  texto[com_virgula] <- gsub(".", "", texto[com_virgula], fixed = TRUE)
  texto[com_virgula] <- sub(",", ".", texto[com_virgula], fixed = TRUE)
  suppressWarnings(as.numeric(texto))
}

valores_invalidos <- function(x) {
  original_preenchido <- !texto_vazio(x)
  convertido <- converter_numero(x)
  which(original_preenchido & is.na(convertido))
}

separar_lista <- function(texto) {
  if (is.null(texto) || length(texto) == 0) {
    return(character())
  }
  itens <- unlist(strsplit(as.character(texto), "[;\n]"))
  itens <- trimws(itens)
  unique(itens[itens != ""])
}

detectar_separador_csv <- function(caminho) {
  linhas <- readLines(caminho, n = 5, warn = FALSE, encoding = "UTF-8")
  linhas <- linhas[linhas != ""]
  if (length(linhas) == 0) {
    return(",")
  }
  contagem <- vapply(c(";", ",", "\t"), function(sep) {
    sum(lengths(regmatches(linhas, gregexpr(sep, linhas, fixed = TRUE))))
  }, numeric(1))
  names(contagem)[which.max(contagem)]
}

ler_planilhas_excel <- function(caminho) {
  readxl::excel_sheets(caminho)
}

ler_arquivo_dados <- function(caminho, nome_arquivo, aba = NULL) {
  extensao <- tolower(tools::file_ext(nome_arquivo))

  dados <- if (extensao %in% c("xlsx", "xls")) {
    readxl::read_excel(caminho, sheet = if (is.null(aba)) 1 else aba, .name_repair = "minimal")
  } else if (extensao %in% c("csv", "txt")) {
    utils::read.table(
      caminho,
      header = TRUE,
      sep = detectar_separador_csv(caminho),
      quote = "\"",
      dec = ".",
      colClasses = "character",
      check.names = FALSE,
      na.strings = c("", "NA"),
      encoding = "UTF-8",
      fileEncoding = "UTF-8",
      comment.char = "",
      strip.white = TRUE
    )
  } else {
    stop("Formato não suportado. Envie um arquivo .xlsx, .xls ou .csv.")
  }

  dados <- as.data.frame(dados, stringsAsFactors = FALSE, check.names = FALSE)
  nomes <- trimws(names(dados))
  nomes[nomes == ""] <- paste0("Coluna ", which(nomes == ""))
  names(dados) <- make.unique(nomes, sep = " ")

  preenchida <- vapply(dados, function(coluna) any(!texto_vazio(coluna)), logical(1))
  dados <- dados[, preenchida, drop = FALSE]
  linhas_preenchidas <- apply(dados, 1, function(linha) any(!texto_vazio(linha)))
  dados <- dados[linhas_preenchidas, , drop = FALSE]
  rownames(dados) <- NULL

  if (ncol(dados) == 0 || nrow(dados) == 0) {
    stop("O arquivo não possui dados preenchidos.")
  }

  dados
}

dados_para_planilha <- function(dados) {
  dados <- as.data.frame(dados, stringsAsFactors = FALSE, check.names = FALSE)
  dados[] <- lapply(dados, function(coluna) {
    if (is.numeric(coluna)) {
      texto <- format(coluna, trim = TRUE, scientific = FALSE, drop0trailing = TRUE)
      texto[is.na(coluna)] <- ""
      texto
    } else {
      texto <- as.character(coluna)
      texto[is.na(texto)] <- ""
      texto
    }
  })
  dados
}

# ---------------------------------------------------------
# Nomes seguros para fórmulas
# ---------------------------------------------------------

nome_seguro <- function(nomes) {
  seguro <- iconv(nomes, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")
  seguro[is.na(seguro)] <- ""
  seguro <- gsub("[^A-Za-z0-9_]+", "_", seguro)
  seguro <- gsub("^_+|_+$", "", seguro)
  seguro[seguro == ""] <- "var"
  seguro <- ifelse(grepl("^[A-Za-z]", seguro), seguro, paste0("v_", seguro))
  make.unique(seguro, sep = "_")
}

# ---------------------------------------------------------
# Planilha para digitação
# ---------------------------------------------------------

gerar_planilha_modelo <- function(fatores, n_repeticoes, delineamento, respostas) {
  niveis <- lapply(fatores, function(f) f$niveis)
  names(niveis) <- vapply(fatores, function(f) f$nome, character(1))

  tratamentos <- do.call(
    expand.grid,
    c(rev(niveis), list(stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE))
  )
  tratamentos <- tratamentos[, names(niveis), drop = FALSE]

  coluna_rep <- if (identical(delineamento, "DBC")) "Bloco" else "Repetição"
  planilha <- do.call(rbind, lapply(seq_len(n_repeticoes), function(r) {
    cbind(stats::setNames(data.frame(as.character(r), stringsAsFactors = FALSE), coluna_rep), tratamentos)
  }))

  for (resposta in respostas) {
    planilha[[resposta]] <- ""
  }
  rownames(planilha) <- NULL
  dados_para_planilha(planilha)
}

dados_exemplo <- function() {
  set.seed(2026)
  dados <- expand.grid(
    Bloco = as.character(1:4),
    Dose = c("0", "50", "100", "150"),
    Cultivar = c("Gália", "Amarelo"),
    stringsAsFactors = FALSE,
    KEEP.OUT.ATTRS = FALSE
  )
  dose <- as.numeric(dados$Dose)
  cultivar_b <- dados$Cultivar == "Amarelo"
  bloco <- as.numeric(dados$Bloco)
  dados$`Produtividade (t/ha)` <- round(
    28 + 0.09 * dose - 0.0004 * dose^2 + 3.5 * cultivar_b + 0.02 * dose * cultivar_b +
      0.6 * bloco + stats::rnorm(nrow(dados), 0, 1.4),
    2
  )
  dados$`Sólidos solúveis (°Brix)` <- round(
    10.5 + 0.006 * dose + 1.1 * cultivar_b + stats::rnorm(nrow(dados), 0, 0.45),
    2
  )
  dados$`Massa do fruto (kg)` <- round(
    1.6 + 0.003 * dose + 0.2 * cultivar_b + stats::rnorm(nrow(dados), 0, 0.12),
    3
  )
  dados <- dados[order(as.numeric(dados$Bloco)), ]
  rownames(dados) <- NULL
  dados_para_planilha(dados)
}

# ---------------------------------------------------------
# Sugestão de estrutura a partir das colunas
# ---------------------------------------------------------

coluna_parece_numerica <- function(coluna) {
  preenchidos <- coluna[!texto_vazio(coluna)]
  length(preenchidos) == 0 || all(!is.na(converter_numero(preenchidos)))
}

coluna_parece_fator_numerico <- function(coluna) {
  preenchidos <- trimws(as.character(coluna[!texto_vazio(coluna)]))
  if (length(preenchidos) < 4) {
    return(FALSE)
  }
  contagem <- table(preenchidos)
  length(contagem) >= 2 && length(contagem) <= length(preenchidos) / 2 && length(unique(as.integer(contagem))) == 1
}

sugerir_estrutura <- function(dados) {
  nomes <- names(dados)
  if (length(nomes) == 0) {
    return(list(delineamento = "DIC", bloco = "", fatores = character(), respostas = character()))
  }
  normalizados <- tolower(nome_seguro(nomes))
  eh_bloco <- grepl("^(bloco|blocos|block|blocks)(_|$)", normalizados)
  eh_repeticao <- grepl("^(rep|repeticao|repeticoes|repetition|parcela|unidade)(_|$)", normalizados)

  bloco <- if (any(eh_bloco)) nomes[which(eh_bloco)[1]] else ""
  candidatos <- nomes[!eh_bloco & !eh_repeticao]
  numericas <- vapply(dados[candidatos], coluna_parece_numerica, logical(1))
  fator_numerico <- vapply(dados[candidatos], coluna_parece_fator_numerico, logical(1))

  fatores <- candidatos[!numericas | fator_numerico]
  respostas <- candidatos[numericas & !fator_numerico]

  list(
    delineamento = if (nzchar(bloco)) "DBC" else "DIC",
    bloco = bloco,
    fatores = utils::head(fatores, MAX_FATORES),
    respostas = respostas
  )
}

# ---------------------------------------------------------
# Preparação e validação para análise
# ---------------------------------------------------------

preparar_dados_analise <- function(dados, delineamento, bloco, fatores, respostas) {
  erros <- character()
  bloco <- if (identical(delineamento, "DBC") && !is.null(bloco) && nzchar(bloco)) bloco else NULL

  if (length(fatores) == 0) {
    erros <- c(erros, "Selecione pelo menos um fator.")
  }
  if (length(fatores) > MAX_FATORES) {
    erros <- c(erros, "Selecione no máximo três fatores.")
  }
  if (length(respostas) == 0) {
    erros <- c(erros, "Selecione pelo menos uma variável resposta.")
  }
  if (identical(delineamento, "DBC") && is.null(bloco)) {
    erros <- c(erros, "Selecione a coluna de blocos para o delineamento em blocos casualizados (DBC).")
  }

  usadas <- c(bloco, fatores, respostas)
  repetidas <- unique(usadas[duplicated(usadas)])
  if (length(repetidas) > 0) {
    erros <- c(erros, paste0("A mesma coluna foi usada em mais de uma função: ", paste(repetidas, collapse = ", "), "."))
  }
  faltantes <- setdiff(usadas, names(dados))
  if (length(faltantes) > 0) {
    erros <- c(erros, paste0("Colunas não encontradas na planilha: ", paste(faltantes, collapse = ", "), "."))
  }
  if (length(erros) > 0) {
    return(list(ok = FALSE, erros = erros))
  }

  base <- dados[, usadas, drop = FALSE]
  classificadoras <- c(bloco, fatores)
  linhas_validas <- apply(base[, classificadoras, drop = FALSE], 1, function(linha) all(!texto_vazio(linha)))
  linhas_descartadas <- sum(!linhas_validas & apply(base, 1, function(linha) any(!texto_vazio(linha))))
  base <- base[linhas_validas, , drop = FALSE]

  if (nrow(base) == 0) {
    return(list(ok = FALSE, erros = "Nenhuma linha possui todos os fatores preenchidos."))
  }

  for (resposta in respostas) {
    invalidas <- valores_invalidos(base[[resposta]])
    if (length(invalidas) > 0) {
      erros <- c(erros, sprintf(
        "A variável \"%s\" possui valores não numéricos (linhas %s).",
        resposta,
        paste(utils::head(invalidas, 5), collapse = ", ")
      ))
    }
    valores <- converter_numero(base[[resposta]])
    if (sum(!is.na(valores)) < 3) {
      erros <- c(erros, sprintf("A variável \"%s\" precisa de pelo menos três valores numéricos.", resposta))
    } else if (stats::var(valores, na.rm = TRUE) == 0) {
      erros <- c(erros, sprintf("A variável \"%s\" não possui variação entre as observações.", resposta))
    }
  }

  for (fator in fatores) {
    n_niveis <- length(unique(trimws(as.character(base[[fator]]))))
    if (n_niveis < 2) {
      erros <- c(erros, sprintf("O fator \"%s\" precisa ter pelo menos dois níveis.", fator))
    }
  }
  if (!is.null(bloco) && length(unique(trimws(as.character(base[[bloco]])))) < 2) {
    erros <- c(erros, "A coluna de blocos precisa ter pelo menos dois blocos.")
  }
  if (length(erros) > 0) {
    return(list(ok = FALSE, erros = erros))
  }

  nomes_seguros <- nome_seguro(usadas)
  mapa <- stats::setNames(usadas, nomes_seguros)
  seguro <- function(x) if (is.null(x)) NULL else names(mapa)[match(x, mapa)]

  analise <- base
  names(analise) <- nomes_seguros
  for (coluna in seguro(classificadoras)) {
    valores <- trimws(as.character(analise[[coluna]]))
    niveis <- unique(valores)
    numeros <- suppressWarnings(converter_numero(niveis))
    if (all(!is.na(numeros))) {
      niveis <- niveis[order(numeros)]
    }
    analise[[coluna]] <- factor(valores, levels = niveis)
  }
  for (coluna in seguro(respostas)) {
    analise[[coluna]] <- converter_numero(analise[[coluna]])
  }

  combinacoes <- interaction(analise[seguro(fatores)], drop = TRUE)
  todas <- prod(vapply(analise[seguro(fatores)], nlevels, numeric(1)))
  if (nlevels(combinacoes) < todas) {
    return(list(ok = FALSE, erros = "Existem combinações de tratamentos sem observações. Confira se todos os níveis dos fatores foram combinados na planilha."))
  }
  if (min(table(combinacoes)) < 2 && is.null(bloco)) {
    return(list(ok = FALSE, erros = "Cada tratamento precisa de pelo menos duas repetições para estimar o erro experimental."))
  }

  dic_vars <- data.frame(
    var = names(mapa),
    sigla = unname(mapa),
    label = unname(mapa),
    description = unname(mapa),
    stringsAsFactors = FALSE
  )

  list(
    ok = TRUE,
    dados = analise,
    delineamento = delineamento,
    bloco = seguro(bloco),
    fatores = seguro(fatores),
    respostas = seguro(respostas),
    mapa = mapa,
    dic_vars = dic_vars,
    linhas_descartadas = linhas_descartadas,
    n_obs = nrow(analise)
  )
}

rotulo <- function(prep, x) {
  unname(ifelse(x %in% names(prep$mapa), prep$mapa[x], x))
}

# ---------------------------------------------------------
# Análise com o pacote ranova
# ---------------------------------------------------------

silenciar <- function(expr) {
  suppressWarnings(suppressMessages(expr))
}

tabela_html <- function(tabela) {
  HTML(as.character(tabela))
}

tentar <- function(expr) {
  tryCatch(
    list(ok = TRUE, valor = silenciar(expr)),
    error = function(e) list(ok = FALSE, erro = conditionMessage(e))
  )
}

formatar_p <- function(p) {
  ifelse(
    is.na(p),
    "",
    ifelse(p < 0.0001, "< 0,0001", formatC(p, digits = 4, format = "f", decimal.mark = ","))
  )
}

termos_significativos <- function(prep, alpha) {
  linhas <- lapply(prep$respostas, function(v) {
    modelo <- silenciar(ajusta_modelo_fatorial(prep$dados, v, prep$bloco, prep$fatores))
    tab <- summary(modelo)[[1]]
    termos <- trimws(rownames(tab))
    data.frame(
      variavel = v,
      termo = termos,
      p = tab$`Pr(>F)`,
      stringsAsFactors = FALSE
    )
  })
  resultado <- do.call(rbind, linhas)
  resultado <- resultado[!is.na(resultado$p) & resultado$termo != "Residuals", , drop = FALSE]
  if (!is.null(prep$bloco)) {
    resultado <- resultado[resultado$termo != prep$bloco, , drop = FALSE]
  }
  resultado$interacao <- grepl(":", resultado$termo, fixed = TRUE)
  resultado$significativo <- resultado$p < alpha
  resultado
}

rotulo_termo <- function(prep, termo) {
  partes <- strsplit(termo, ":", fixed = TRUE)[[1]]
  paste(rotulo(prep, partes), collapse = " × ")
}

tabela_diagnostico <- function(prep, alpha) {
  bruto <- silenciar(anova_diagnostico(
    dados = prep$dados,
    variaveis = prep$respostas,
    bloco = prep$bloco,
    fatores = prep$fatores,
    alpha = alpha,
    mostrar_graficos = FALSE
  ))$dados_brutos

  data.frame(
    `Variável` = rotulo(prep, bruto$Variavel),
    `p (Shapiro-Wilk)` = formatar_p(bruto$p_normalidade),
    `Normalidade` = ifelse(is.na(bruto$p_normalidade), "Não avaliado", ifelse(bruto$p_normalidade < alpha, "Não atendida", "Atendida")),
    `p (Levene)` = formatar_p(bruto$p_homogeneidade),
    `Homogeneidade` = ifelse(is.na(bruto$p_homogeneidade), "Não avaliado", ifelse(bruto$p_homogeneidade < alpha, "Não atendida", "Atendida")),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

tabela_diagnostico_html <- function(tabela) {
  classe <- function(valor) if (valor == "Atendida") "ranova-ok" else if (valor == "Não atendida") "ranova-alerta" else ""
  tags$div(
    class = "ranova-table-scroll",
    tags$table(
      class = "table ranova-diag-table",
      tags$thead(tags$tr(lapply(names(tabela), tags$th))),
      tags$tbody(lapply(seq_len(nrow(tabela)), function(i) {
        tags$tr(
          tags$td(tabela[i, 1]),
          tags$td(tabela[i, 2]),
          tags$td(tags$span(class = paste("ranova-pill", classe(tabela[i, 3])), tabela[i, 3])),
          tags$td(tabela[i, 4]),
          tags$td(tags$span(class = paste("ranova-pill", classe(tabela[i, 5])), tabela[i, 5]))
        )
      }))
    )
  )
}

executar_analise <- function(prep, opcoes) {
  argumentos <- list(dados = prep$dados, bloco = prep$bloco, fatores = prep$fatores)

  anova <- tentar(anova_fatorial_qm_tabela(
    dados = prep$dados,
    variaveis = prep$respostas,
    bloco = prep$bloco,
    fatores = prep$fatores,
    dic_vars = prep$dic_vars,
    formato = opcoes$formato,
    digitos = opcoes$digitos_anova,
    caption = "Resumo da análise de variância."
  ))

  significancia <- tentar(termos_significativos(prep, opcoes$alpha))
  diagnostico <- tentar(tabela_diagnostico(prep, opcoes$alpha))

  medias <- lapply(prep$fatores, function(fator) {
    tentar(tabela_medias_fatorial(
      dados = prep$dados,
      variaveis = prep$respostas,
      fator_interesse = fator,
      bloco = prep$bloco,
      fatores = prep$fatores,
      dic_vars = prep$dic_vars,
      digitos = opcoes$digitos,
      tipo_se = opcoes$tipo_se,
      caption = paste0("Médias ajustadas ± erro-padrão para ", rotulo(prep, fator), ".")
    ))
  })
  names(medias) <- prep$fatores

  list(
    prep = prep,
    opcoes = opcoes,
    anova = anova,
    significancia = significancia,
    diagnostico = diagnostico,
    medias = medias,
    gerado_em = Sys.time()
  )
}

tabela_interacao <- function(prep, opcoes, fator_linha, fator_coluna) {
  tentar(tabela_interacao_fatorial_multivariaveis(
    dados = prep$dados,
    variaveis = prep$respostas,
    fator_linha = fator_linha,
    fator_coluna = fator_coluna,
    bloco = prep$bloco,
    fatores = prep$fatores,
    dic_vars = prep$dic_vars,
    digitos = opcoes$digitos,
    alpha = opcoes$alpha,
    tipo_se = opcoes$tipo_se,
    caption = paste0("Desdobramento da interação ", rotulo(prep, fator_linha), " × ", rotulo(prep, fator_coluna), ".")
  ))
}

# ---------------------------------------------------------
# Gráficos
# ---------------------------------------------------------

tema_ranova <- function() {
  theme_bw(base_size = 13) +
    theme(
      panel.grid = element_blank(),
      axis.title = element_text(face = "bold", color = "#264653"),
      axis.text = element_text(color = "#264653"),
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      plot.margin = margin(12, 12, 8, 8)
    )
}

grafico_medias <- function(prep, opcoes, resposta, fator) {
  medias <- silenciar(medias_fatorial_cld(
    dados = prep$dados,
    resposta = resposta,
    fator_interesse = fator,
    bloco = prep$bloco,
    fatores = prep$fatores,
    alpha = opcoes$alpha,
    tipo_se = opcoes$tipo_se
  ))
  medias$nivel <- factor(medias$nivel, levels = levels(prep$dados[[fator]]))
  medias$grupo <- trimws(medias$grupo)
  topo <- max(medias$media + medias$se, na.rm = TRUE)

  ggplot(medias, aes(x = .data$nivel, y = .data$media)) +
    geom_col(fill = COR_PRIMARIA, color = "#31511f", width = 0.65) +
    geom_errorbar(aes(ymin = .data$media - .data$se, ymax = .data$media + .data$se), width = 0.18, color = "#264653") +
    geom_text(aes(y = .data$media + .data$se, label = .data$grupo), vjust = -0.6, size = 4.6, fontface = "bold", color = "#264653") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.12)), limits = c(0, topo * 1.12)) +
    labs(x = rotulo(prep, fator), y = rotulo(prep, resposta)) +
    tema_ranova()
}

grafico_interacao <- function(prep, resposta, fator_x, fator_traco) {
  grafico <- silenciar(grafico_interacao_fatorial(
    dados = prep$dados,
    resposta = resposta,
    fator_x = fator_x,
    fator_traco = fator_traco,
    bloco = prep$bloco,
    fatores = prep$fatores,
    dic_vars = prep$dic_vars
  ))
  n_cores <- nlevels(prep$dados[[fator_traco]])
  cores <- rep(PALETA_FATORES, length.out = n_cores)
  grafico +
    scale_color_manual(values = cores) +
    labs(x = rotulo(prep, fator_x), y = rotulo(prep, resposta), color = rotulo(prep, fator_traco)) +
    tema_ranova()
}

grafico_residuos <- function(prep, resposta) {
  modelo <- silenciar(ajusta_modelo_fatorial(prep$dados, resposta, prep$bloco, prep$fatores))
  residuos <- data.frame(
    ajustado = stats::fitted(modelo),
    residuo = stats::rstandard(modelo)
  )
  residuos <- residuos[is.finite(residuos$residuo), , drop = FALSE]

  dispersao <- ggplot(residuos, aes(x = .data$ajustado, y = .data$residuo)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "#7a8576") +
    geom_hline(yintercept = c(-3, 3), linetype = "dotted", color = "#b43d36") +
    geom_point(color = COR_PRIMARIA, size = 2.6, alpha = 0.85) +
    labs(x = "Valores ajustados", y = "Resíduos padronizados", title = "Resíduos × ajustados") +
    tema_ranova()

  qq <- ggplot(residuos, aes(sample = .data$residuo)) +
    stat_qq_line(color = "#7a8576", linetype = "dashed") +
    stat_qq(color = COR_PRIMARIA, size = 2.6, alpha = 0.85) +
    labs(x = "Quantis teóricos", y = "Resíduos padronizados", title = "Normal Q-Q") +
    tema_ranova()

  ggpubr::ggarrange(dispersao, qq, ncol = 2)
}

# ---------------------------------------------------------
# Relatório HTML
# ---------------------------------------------------------

imagem_base64 <- function(grafico, largura = 7, altura = 4.5) {
  arquivo <- tempfile(fileext = ".png")
  on.exit(unlink(arquivo), add = TRUE)
  ggsave(arquivo, grafico, width = largura, height = altura, dpi = 150, bg = "white")
  knitr::image_uri(arquivo)
}

gerar_relatorio_html <- function(resultado, caminho, fator_linha = NULL, fator_coluna = NULL) {
  prep <- resultado$prep
  opcoes <- resultado$opcoes
  css_lightable <- tryCatch(
    paste(readLines(system.file("lightable-0.0.1", "lightable.css", package = "kableExtra"), warn = FALSE), collapse = "\n"),
    error = function(e) ""
  )

  bloco_tabela <- function(resultado_tabela) {
    if (isTRUE(resultado_tabela$ok)) {
      tabela_html(resultado_tabela$valor)
    } else {
      tags$p(class = "erro", paste("Não foi possível gerar esta tabela:", resultado_tabela$erro))
    }
  }

  graficos <- list()
  for (resposta in prep$respostas) {
    for (fator in prep$fatores) {
      grafico <- tentar(grafico_medias(prep, opcoes, resposta, fator))
      if (isTRUE(grafico$ok)) {
        graficos[[length(graficos) + 1]] <- tags$figure(
          tags$img(src = imagem_base64(grafico$valor), alt = paste(rotulo(prep, resposta), "por", rotulo(prep, fator))),
          tags$figcaption(paste0(rotulo(prep, resposta), " em função de ", rotulo(prep, fator), "."))
        )
      }
    }
    if (!is.null(fator_linha) && !is.null(fator_coluna)) {
      grafico <- tentar(grafico_interacao(prep, resposta, fator_linha, fator_coluna))
      if (isTRUE(grafico$ok)) {
        graficos[[length(graficos) + 1]] <- tags$figure(
          tags$img(src = imagem_base64(grafico$valor), alt = paste("Interação para", rotulo(prep, resposta))),
          tags$figcaption(paste0("Interação ", rotulo(prep, fator_linha), " × ", rotulo(prep, fator_coluna), " para ", rotulo(prep, resposta), "."))
        )
      }
    }
  }

  interacao <- if (!is.null(fator_linha) && !is.null(fator_coluna)) {
    tagList(
      tags$h2("Desdobramento da interação"),
      bloco_tabela(tabela_interacao(prep, opcoes, fator_linha, fator_coluna))
    )
  }

  diagnostico <- if (isTRUE(resultado$diagnostico$ok)) {
    tabela_diagnostico_html(resultado$diagnostico$valor)
  } else {
    tags$p(class = "erro", resultado$diagnostico$erro)
  }

  pagina <- tags$html(
    lang = "pt-BR",
    tags$head(
      tags$meta(charset = "utf-8"),
      tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
      tags$title("Relatório Ranova"),
      tags$style(HTML(css_lightable)),
      tags$style(HTML("
        body { font-family: Arial, sans-serif; color: #264653; max-width: 980px; margin: 0 auto; padding: 24px 16px 48px; }
        h1 { color: #548238; margin-bottom: 4px; }
        h2 { color: #31511f; border-bottom: 2px solid #eef6e8; padding-bottom: 6px; margin-top: 32px; }
        .meta { color: #7a8576; margin-top: 0; }
        .resumo { background: #eef6e8; border-left: 5px solid #548238; border-radius: 8px; padding: 12px 16px; }
        .resumo p { margin: 4px 0; }
        table { border-collapse: collapse; margin: 12px 0; }
        .ranova-diag-table th, .ranova-diag-table td { border-bottom: 1px solid #d7dde0; padding: 6px 10px; text-align: left; }
        .ranova-diag-table th { background: #548238; color: #fff; }
        .ranova-pill { font-weight: bold; }
        .ranova-alerta { color: #b43d36; }
        .ranova-ok { color: #3a5d27; }
        figure { margin: 18px 0; page-break-inside: avoid; }
        figure img { max-width: 100%; height: auto; border: 1px solid #e2e8e0; border-radius: 8px; }
        figcaption { font-size: 13px; color: #7a8576; }
        .erro { color: #b43d36; }
        footer { margin-top: 40px; font-size: 12px; color: #7a8576; }
      "))
    ),
    tags$body(
      tags$h1("Relatório de análise de variância"),
      tags$p(class = "meta", paste0("Gerado em ", format(resultado$gerado_em, "%d/%m/%Y %H:%M"), " pelo Ranova v", APP_VERSION, " (pacote ranova ", PACOTE_VERSAO, ").")),
      tags$div(
        class = "resumo",
        tags$p(tags$strong("Delineamento: "), if (identical(prep$delineamento, "DBC")) "Blocos casualizados (DBC)" else "Inteiramente casualizado (DIC)"),
        tags$p(tags$strong("Fatores: "), paste(rotulo(prep, prep$fatores), collapse = ", ")),
        if (!is.null(prep$bloco)) tags$p(tags$strong("Blocos: "), rotulo(prep, prep$bloco)),
        tags$p(tags$strong("Variáveis resposta: "), paste(rotulo(prep, prep$respostas), collapse = ", ")),
        tags$p(tags$strong("Observações: "), prep$n_obs),
        tags$p(tags$strong("Nível de significância: "), formatC(opcoes$alpha, format = "f", digits = 2, decimal.mark = ","))
      ),
      tags$h2("Análise de variância"),
      bloco_tabela(resultado$anova),
      tags$h2("Pressupostos da ANOVA"),
      diagnostico,
      tags$h2("Médias"),
      lapply(resultado$medias, bloco_tabela),
      interacao,
      if (length(graficos) > 0) tagList(tags$h2("Gráficos"), graficos),
      tags$footer(
        tags$p("Médias seguidas pela mesma letra não diferem entre si pelo teste t (2 níveis) ou Tukey (3 ou mais níveis)."),
        tags$p(paste0("MelonMundi - Ranova v", APP_VERSION, ". ", TEXTO_PRIVACIDADE))
      )
    )
  )

  writeLines(c("<!DOCTYPE html>", as.character(pagina)), caminho, useBytes = TRUE)
  invisible(caminho)
}
