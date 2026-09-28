# =========================================================
# 08_ajuste_ranova.R
#
# Motor de analise para fatoriais (DIC e DBC) e parcelas
# subdivididas (DIC e DBC), com o erro correto para cada
# teste F e para cada comparacao de medias.
# =========================================================

#' Delineamentos suportados por `ranova_ajuste()`
#'
#' - `"DIC"`: inteiramente casualizado, fatorial com 1 a 3 fatores.
#' - `"DBC"`: blocos casualizados, fatorial com 1 a 3 fatores.
#' - `"PSDIC"`: parcelas subdivididas em DIC (fator 1 na parcela,
#'   fator 2 na subparcela; exige a coluna de repeticao).
#' - `"PSDBC"`: parcelas subdivididas em DBC (fator 1 na parcela,
#'   fator 2 na subparcela; exige a coluna de blocos).
#'
#' @keywords internal
#' @noRd
DELINEAMENTOS_RANOVA <- c("DIC", "DBC", "PSDIC", "PSDBC")

#' Ajusta o modelo de um experimento fatorial ou em parcelas subdivididas
#'
#' Ajusta um modelo linear (`lm`) com os termos adequados ao delineamento e
#' guarda os quadrados medios de erro usados nos testes F e nas comparacoes de
#' medias. Em parcelas subdivididas, o termo parcela (bloco x fator da parcela
#' no DBC, ou repeticao dentro do fator da parcela no DIC) e o erro (a), e o
#' residuo e o erro (b).
#'
#' @param dados `data.frame` com os dados experimentais.
#' @param resposta Nome da variavel resposta.
#' @param fatores Nomes dos fatores. Em parcelas subdivididas, o primeiro e o
#'   fator da parcela e o segundo, o da subparcela.
#' @param delineamento `"DIC"`, `"DBC"`, `"PSDIC"` ou `"PSDBC"`.
#' @param bloco Coluna de blocos (obrigatoria em `"DBC"` e `"PSDBC"`).
#' @param repeticao Coluna que identifica a repeticao (parcela) dentro de cada
#'   nivel do fator da parcela (obrigatoria em `"PSDIC"`).
#'
#' @return Objeto da classe `ranova_ajuste`.
#' @export
#'
#' @examples
#' dados <- expand.grid(bloco = factor(1:4), A = factor(1:3), B = factor(1:2))
#' dados$y <- 10 + as.numeric(dados$A) + stats::rnorm(nrow(dados))
#' ajuste <- ranova_ajuste(dados, "y", c("A", "B"), "PSDBC", bloco = "bloco")
#' ranova_anova(ajuste)
ranova_ajuste <- function(
    dados,
    resposta,
    fatores,
    delineamento = c("DIC", "DBC", "PSDIC", "PSDBC"),
    bloco = NULL,
    repeticao = NULL
) {
  delineamento <- match.arg(delineamento)
  stopifnot(is.character(resposta), length(resposta) == 1, is.character(fatores))
  parcela_sub <- delineamento %in% c("PSDIC", "PSDBC")

  if (parcela_sub && length(fatores) != 2) {
    stop("Parcelas subdivididas exigem dois fatores: o da parcela e o da subparcela.", call. = FALSE)
  }
  if (!parcela_sub && (length(fatores) < 1 || length(fatores) > 3)) {
    stop("Use de 1 a 3 fatores.", call. = FALSE)
  }
  if (delineamento %in% c("DBC", "PSDBC") && is.null(bloco)) {
    stop("Informe a coluna de blocos.", call. = FALSE)
  }
  if (identical(delineamento, "PSDIC") && is.null(repeticao)) {
    stop("Em parcelas subdivididas no DIC, informe a coluna de repeticao.", call. = FALSE)
  }
  if (!delineamento %in% c("DBC", "PSDBC")) bloco <- NULL
  if (!identical(delineamento, "PSDIC")) repeticao <- NULL

  dados <- as.data.frame(dados)
  for (coluna in c(fatores, bloco, repeticao)) {
    if (!is.factor(dados[[coluna]])) dados[[coluna]] <- factor(dados[[coluna]])
  }

  termo_erro_a <- NULL
  if (identical(delineamento, "PSDBC")) {
    termo_erro_a <- paste0(bloco, ":", fatores[1])
    direita <- paste(bloco, "+", fatores[1], "+", termo_erro_a, "+", fatores[2], "+", paste0(fatores[1], ":", fatores[2]))
  } else if (identical(delineamento, "PSDIC")) {
    termo_erro_a <- paste0(fatores[1], ":", repeticao)
    direita <- paste(fatores[1], "+", termo_erro_a, "+", fatores[2], "+", paste0(fatores[1], ":", fatores[2]))
  } else {
    direita <- paste(c(bloco, paste(fatores, collapse = " * ")), collapse = " + ")
  }

  formula <- stats::as.formula(paste(resposta, "~", direita))
  modelo <- stats::lm(formula, data = dados)
  tabela <- stats::anova(modelo)
  termos <- trimws(rownames(tabela))

  erro_b <- list(qm = tabela[["Mean Sq"]][termos == "Residuals"], gl = tabela[["Df"]][termos == "Residuals"])
  erro_a <- if (!is.null(termo_erro_a)) {
    i <- which(termos == termo_erro_a)
    if (length(i) == 0) {
      # A ordem dos nomes na interacao pode vir invertida no lm
      partes <- strsplit(termo_erro_a, ":", fixed = TRUE)[[1]]
      i <- which(termos == paste(rev(partes), collapse = ":"))
    }
    termo_erro_a <- termos[i]
    list(qm = tabela[["Mean Sq"]][i], gl = tabela[["Df"]][i])
  }

  structure(
    list(
      modelo = modelo,
      anova_lm = tabela,
      dados = dados[stats::complete.cases(dados[, c(resposta, fatores, bloco, repeticao), drop = FALSE]), , drop = FALSE],
      resposta = resposta,
      fatores = fatores,
      delineamento = delineamento,
      bloco = bloco,
      repeticao = repeticao,
      termo_erro_a = termo_erro_a,
      erro_a = erro_a,
      erro_b = erro_b
    ),
    class = "ranova_ajuste"
  )
}

#' @export
print.ranova_ajuste <- function(x, ...) {
  cat("Ajuste ranova:", x$delineamento, "| resposta:", x$resposta, "| fatores:", paste(x$fatores, collapse = ", "), "\n")
  print(ranova_anova(x))
  invisible(x)
}

#' Quadro da analise de variancia
#'
#' Em parcelas subdivididas, o bloco e o fator da parcela sao testados contra o
#' erro (a); o fator da subparcela e a interacao, contra o erro (b).
#'
#' @param ajuste Objeto retornado por `ranova_ajuste()`.
#'
#' @return `data.frame` com as colunas `FV`, `GL`, `SQ`, `QM`, `F` e `p`. O
#'   atributo `cv` traz o(s) coeficiente(s) de variacao (%).
#' @export
ranova_anova <- function(ajuste) {
  stopifnot(inherits(ajuste, "ranova_ajuste"))
  tab <- ajuste$anova_lm
  termos <- trimws(rownames(tab))
  media <- mean(ajuste$dados[[ajuste$resposta]], na.rm = TRUE)

  saida <- data.frame(
    FV = termos,
    GL = tab[["Df"]],
    SQ = tab[["Sum Sq"]],
    QM = tab[["Mean Sq"]],
    F = tab[["F value"]],
    p = tab[["Pr(>F)"]],
    stringsAsFactors = FALSE
  )

  if (is.null(ajuste$erro_a)) {
    saida$FV[saida$FV == "Residuals"] <- "Resíduo"
    cv <- c("CV (%)" = sqrt(ajuste$erro_b$qm) / media * 100)
  } else {
    contra_a <- c(ajuste$bloco, ajuste$fatores[1])
    for (termo in contra_a) {
      i <- which(saida$FV == termo)
      saida$F[i] <- saida$QM[i] / ajuste$erro_a$qm
      saida$p[i] <- stats::pf(saida$F[i], saida$GL[i], ajuste$erro_a$gl, lower.tail = FALSE)
    }
    i_a <- which(saida$FV == ajuste$termo_erro_a)
    saida$F[i_a] <- NA
    saida$p[i_a] <- NA
    saida$FV[i_a] <- "Erro (a)"
    saida$FV[saida$FV == "Residuals"] <- "Erro (b)"
    ordem <- c(ajuste$bloco, ajuste$fatores[1], "Erro (a)", ajuste$fatores[2],
               saida$FV[grepl(":", saida$FV, fixed = TRUE)], "Erro (b)")
    saida <- saida[match(ordem, saida$FV), , drop = FALSE]
    cv <- c("CV a (%)" = sqrt(ajuste$erro_a$qm) / media * 100,
            "CV b (%)" = sqrt(ajuste$erro_b$qm) / media * 100)
  }

  rownames(saida) <- NULL
  attr(saida, "cv") <- cv
  saida
}

# ---------------------------------------------------------
# Erro usado na comparacao das medias de `fator` (opcionalmente
# dentro de cada nivel de `dentro`).
# ---------------------------------------------------------
#' @keywords internal
#' @noRd
erro_comparacao <- function(ajuste, fator, dentro = NULL) {
  if (is.null(ajuste$erro_a)) {
    return(c(ajuste$erro_b, list(descricao = "resíduo")))
  }
  parcela <- ajuste$fatores[1]
  sub <- ajuste$fatores[2]
  if (identical(fator, parcela) && is.null(dentro)) {
    return(c(ajuste$erro_a, list(descricao = "erro (a)")))
  }
  if (identical(fator, sub)) {
    return(c(ajuste$erro_b, list(descricao = "erro (b)")))
  }
  # Fator da parcela dentro de cada nivel da subparcela: erro combinado com
  # graus de liberdade de Satterthwaite.
  b <- nlevels(ajuste$dados[[sub]])
  qma <- ajuste$erro_a$qm
  qmb <- ajuste$erro_b$qm
  qm <- (qma + (b - 1) * qmb) / b
  gl <- (qma + (b - 1) * qmb)^2 / (qma^2 / ajuste$erro_a$gl + ((b - 1) * qmb)^2 / ajuste$erro_b$gl)
  list(qm = qm, gl = gl, descricao = "erro combinado (Satterthwaite)")
}

#' Medias com letras pelo teste escolhido
#'
#' Calcula as medias dos niveis de `fator` (ou dentro de cada nivel de
#' `dentro`, para o desdobramento de interacoes) e compara pelo teste indicado,
#' usando o quadrado medio e os graus de liberdade corretos para o
#' delineamento. Em parcelas subdivididas, o fator da parcela usa o erro (a);
#' o fator da subparcela, o erro (b); e o fator da parcela dentro de cada nivel
#' da subparcela, o erro combinado com graus de liberdade de Satterthwaite.
#'
#' @param ajuste Objeto retornado por `ranova_ajuste()`.
#' @param fator Fator cujas medias serao comparadas.
#' @param dentro Fator fixado no desdobramento (opcional).
#' @param teste Teste de medias; veja [letras_teste()].
#' @param alpha Nivel de significancia.
#' @param controle Nivel de referencia para o teste de Dunnett (padrao: o
#'   primeiro nivel).
#' @param tipo_se Erro-padrao do modelo (`"modelo"`) ou dos dados
#'   (`"descritivo"`).
#' @param maiusculas Se `TRUE`, usa letras maiusculas.
#'
#' @return `data.frame` com `dentro` (se informado), `nivel`, `media`, `se`,
#'   `n` e `grupo`. Os atributos `teste`, `qm`, `gl` e `erro` descrevem a
#'   comparacao.
#' @export
ranova_medias <- function(
    ajuste,
    fator,
    dentro = NULL,
    teste = "auto",
    alpha = 0.05,
    controle = NULL,
    tipo_se = c("modelo", "descritivo"),
    maiusculas = FALSE
) {
  stopifnot(inherits(ajuste, "ranova_ajuste"))
  tipo_se <- match.arg(tipo_se)
  dados <- ajuste$dados
  y <- ajuste$resposta
  erro <- erro_comparacao(ajuste, fator, dentro)
  niveis <- levels(dados[[fator]])

  # Medias dos tratamentos completos (todos os fatores) e, a partir delas, as
  # medias marginais com peso igual para cada combinacao (medias ajustadas em
  # dados balanceados; em DIC/DBC desbalanceados usa-se emmeans).
  medias_marginais <- function(sub) {
    usar_emmeans <- !ajuste$delineamento %in% c("PSDIC", "PSDBC")
    if (usar_emmeans) {
      forma <- if (is.null(dentro)) stats::as.formula(paste("~", fator)) else stats::as.formula(paste("~", fator, "|", dentro))
      em <- as.data.frame(suppressMessages(emmeans::emmeans(ajuste$modelo, forma)))
      return(em)
    }
    celulas <- stats::aggregate(dados[[y]], dados[ajuste$fatores], mean, na.rm = TRUE)
    names(celulas)[ncol(celulas)] <- "media"
    grupos <- c(fator, dentro)
    m <- stats::aggregate(celulas$media, celulas[grupos], mean)
    names(m)[ncol(m)] <- "emmean"
    m
  }

  base <- medias_marginais()
  grupos_dentro <- if (is.null(dentro)) list(NULL) else as.list(levels(dados[[dentro]]))

  resultado <- lapply(grupos_dentro, function(d) {
    linhas <- if (is.null(d)) base else base[as.character(base[[dentro]]) == d, , drop = FALSE]
    linhas <- linhas[match(niveis, as.character(linhas[[fator]])), , drop = FALSE]
    obs <- if (is.null(d)) dados else dados[as.character(dados[[dentro]]) == d, , drop = FALSE]
    contagem <- vapply(niveis, function(nv) sum(!is.na(obs[[y]][as.character(obs[[fator]]) == nv])), numeric(1))
    n_h <- length(contagem) / sum(1 / contagem)
    medias <- stats::setNames(linhas$emmean, niveis)
    se <- if (identical(tipo_se, "descritivo")) {
      vapply(niveis, function(nv) {
        v <- obs[[y]][as.character(obs[[fator]]) == nv]
        stats::sd(v, na.rm = TRUE) / sqrt(sum(!is.na(v)))
      }, numeric(1))
    } else {
      rep(sqrt(erro$qm / n_h), length(niveis))
    }
    grupo <- letras_teste(medias, n = n_h, qm = erro$qm, gl = erro$gl, teste = teste,
                          alpha = alpha, controle = controle, maiusculas = maiusculas)
    saida <- data.frame(nivel = niveis, media = unname(medias), se = unname(se), n = unname(contagem),
                        grupo = unname(grupo), stringsAsFactors = FALSE)
    if (!is.null(d)) saida <- cbind(dentro = d, saida, stringsAsFactors = FALSE)
    saida
  })

  resultado <- do.call(rbind, resultado)
  rownames(resultado) <- NULL
  attr(resultado, "teste") <- teste_efetivo(teste, length(niveis))
  attr(resultado, "qm") <- erro$qm
  attr(resultado, "gl") <- erro$gl
  attr(resultado, "erro") <- erro$descricao
  resultado
}
