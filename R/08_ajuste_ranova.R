# =========================================================
# 08_ajuste_ranova.R
#
# Motor de analise para fatoriais (DIC e DBC), parcelas
# subdivididas (com fatorial na parcela ou na subparcela) e
# parcelas subsubdivididas, com o erro correto para cada
# teste F e para cada comparacao de medias.
# =========================================================

#' Ajusta o modelo de um experimento fatorial, em parcelas subdivididas ou subsubdivididas
#'
#' Ajusta um modelo linear (`lm`) com os termos adequados ao delineamento e
#' guarda os quadrados medios de erro de cada estrato. Cada fator pertence a um
#' estrato: 1 (parcela), 2 (subparcela) ou 3 (subsubparcela). Um termo de
#' tratamento pertence ao maior estrato entre os seus fatores e e testado contra
#' o erro desse estrato.
#'
#' - `"DIC"` e `"DBC"`: fatorial com 1 a 3 fatores, um unico erro (residuo).
#' - `"PSDIC"` e `"PSDBC"`: parcelas subdivididas, erros (a) e (b). Com dois
#'   fatores, o primeiro fica na parcela e o segundo na subparcela; com tres,
#'   informe `estratos` (por exemplo, `c(1, 1, 2)` para fatorial na parcela ou
#'   `c(1, 2, 2)` para fatorial na subparcela).
#' - `"PSSDIC"` e `"PSSDBC"`: parcelas subsubdivididas com tres fatores
#'   (parcela, subparcela e subsubparcela), erros (a), (b) e (c).
#'
#' No DIC com parcelas, a coluna `repeticao` identifica a repeticao de cada
#' parcela dentro da combinacao dos fatores da parcela.
#'
#' @param dados `data.frame` com os dados experimentais.
#' @param resposta Nome da variavel resposta.
#' @param fatores Nomes dos fatores.
#' @param delineamento `"DIC"`, `"DBC"`, `"PSDIC"`, `"PSDBC"`, `"PSSDIC"` ou
#'   `"PSSDBC"`.
#' @param bloco Coluna de blocos (obrigatoria nos delineamentos em blocos).
#' @param repeticao Coluna de repeticao (obrigatoria nas parcelas em DIC).
#' @param estratos Estrato de cada fator (1 = parcela, 2 = subparcela,
#'   3 = subsubparcela), na ordem de `fatores`. Opcional nos casos padrao.
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
    delineamento = c("DIC", "DBC", "PSDIC", "PSDBC", "PSSDIC", "PSSDBC"),
    bloco = NULL,
    repeticao = NULL,
    estratos = NULL
) {
  delineamento <- match.arg(delineamento)
  stopifnot(is.character(resposta), length(resposta) == 1, is.character(fatores))
  em_blocos <- delineamento %in% c("DBC", "PSDBC", "PSSDBC")
  n_estratos <- switch(substr(delineamento, 1, 3), PSS = 3L, PSD = 2L, 1L)
  if (delineamento %in% c("DIC", "DBC")) n_estratos <- 1L

  if (length(fatores) < 1 || length(fatores) > 3) stop("Use de 1 a 3 fatores.", call. = FALSE)
  if (is.null(estratos)) {
    estratos <- if (n_estratos == 1) {
      rep(1L, length(fatores))
    } else if (n_estratos == 2 && length(fatores) == 2) {
      c(1L, 2L)
    } else if (n_estratos == 3 && length(fatores) == 3) {
      c(1L, 2L, 3L)
    } else {
      stop("Informe `estratos` (1 = parcela, 2 = subparcela, 3 = subsubparcela) para cada fator.", call. = FALSE)
    }
  }
  estratos <- as.integer(estratos)
  if (length(estratos) != length(fatores)) stop("`estratos` deve ter um valor por fator.", call. = FALSE)
  if (!identical(sort(unique(estratos)), seq_len(n_estratos))) {
    stop(sprintf("Com o delineamento %s, os fatores devem ocupar os estratos %s.", delineamento,
                 paste(seq_len(n_estratos), collapse = ", ")), call. = FALSE)
  }
  names(estratos) <- fatores
  if (em_blocos && is.null(bloco)) stop("Informe a coluna de blocos.", call. = FALSE)
  if (!em_blocos && n_estratos > 1 && is.null(repeticao)) {
    stop("Em parcelas no DIC, informe a coluna de repeticao.", call. = FALSE)
  }
  if (!em_blocos) bloco <- NULL
  if (em_blocos || n_estratos == 1) repeticao <- NULL

  dados <- as.data.frame(dados)
  for (coluna in c(fatores, bloco, repeticao)) {
    if (!is.factor(dados[[coluna]])) dados[[coluna]] <- factor(dados[[coluna]])
  }

  # Termos de tratamento (todas as combinacoes de fatores), agrupados pelo
  # estrato do termo: o maior estrato entre os seus fatores.
  subconjuntos <- unlist(lapply(seq_along(fatores), function(k) utils::combn(fatores, k, simplify = FALSE)), recursive = FALSE)
  estrato_termo <- vapply(subconjuntos, function(x) max(estratos[x]), integer(1))
  termos_trat <- vapply(subconjuntos, paste, character(1), collapse = ":")

  # Unidade experimental de cada estrato (menos o ultimo, que e o residuo)
  unidade <- if (em_blocos) bloco else repeticao
  termos_erro <- character()
  for (e in seq_len(n_estratos - 1)) {
    termos_erro[e] <- paste(c(unidade, fatores[estratos <= e]), collapse = ":")
  }

  direita <- c(if (em_blocos) bloco)
  for (e in seq_len(n_estratos)) {
    direita <- c(direita, termos_trat[estrato_termo == e])
    if (e < n_estratos) direita <- c(direita, termos_erro[e])
  }
  formula <- stats::as.formula(paste(resposta, "~", paste(direita, collapse = " + ")))
  modelo <- stats::lm(stats::terms(formula, keep.order = TRUE), data = dados)
  tabela <- stats::anova(modelo)
  termos <- trimws(rownames(tabela))

  # Compara termos sem depender da ordem dos nomes (A:rep ou rep:A).
  chave <- function(x) vapply(strsplit(x, ":", fixed = TRUE), function(p) paste(sort(p), collapse = ":"), character(1))
  erros <- vector("list", n_estratos)
  for (e in seq_len(n_estratos)) {
    i <- if (e < n_estratos) which(chave(termos) == chave(termos_erro[e])) else which(termos == "Residuals")
    rotulo_termo <- termos[i]
    if (length(i) == 0) stop("Nao foi possivel estimar o erro do estrato ", e, ". Confira a estrutura dos dados.", call. = FALSE)
    erros[[e]] <- list(qm = tabela[["Mean Sq"]][i], gl = tabela[["Df"]][i], termo = rotulo_termo)
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
      estratos = estratos,
      n_estratos = n_estratos,
      estrato_termo = stats::setNames(estrato_termo, termos_trat),
      erros = erros,
      # Compatibilidade com a versao 0.5.0
      erro_a = if (n_estratos > 1) erros[[1]],
      erro_b = erros[[n_estratos]]
    ),
    class = "ranova_ajuste"
  )
}

#' @export
print.ranova_ajuste <- function(x, ...) {
  cat("Ajuste ranova:", x$delineamento, "| resposta:", x$resposta, "| fatores:",
      paste0(x$fatores, " (estrato ", x$estratos, ")", collapse = ", "), "\n")
  print(ranova_anova(x))
  invisible(x)
}

#' Quadro da analise de variancia
#'
#' Cada termo e testado contra o erro do seu estrato: bloco e termos da parcela
#' contra o erro (a); termos com fator da subparcela contra o erro (b); termos
#' com fator da subsubparcela contra o erro (c).
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
  n_e <- ajuste$n_estratos

  saida <- data.frame(
    FV = termos, GL = tab[["Df"]], SQ = tab[["Sum Sq"]], QM = tab[["Mean Sq"]],
    F = tab[["F value"]], p = tab[["Pr(>F)"]], stringsAsFactors = FALSE
  )

  if (n_e == 1) {
    saida$FV[saida$FV == "Residuals"] <- "Resíduo"
    cv <- c("CV (%)" = sqrt(ajuste$erros[[1]]$qm) / media * 100)
  } else {
    letras_erro <- letters[seq_len(n_e)]
    estrato_de <- function(termo) {
      if (!is.null(ajuste$bloco) && identical(termo, ajuste$bloco)) return(1L)
      e <- ajuste$estrato_termo[termo]
      if (is.na(e)) {
        partes <- sort(strsplit(termo, ":", fixed = TRUE)[[1]])
        chaves <- vapply(strsplit(names(ajuste$estrato_termo), ":", fixed = TRUE), function(x) paste(sort(x), collapse = ":"), character(1))
        e <- ajuste$estrato_termo[match(paste(partes, collapse = ":"), chaves)]
      }
      as.integer(e)
    }
    termos_erro <- vapply(ajuste$erros, `[[`, character(1), "termo")
    for (i in seq_len(nrow(saida))) {
      if (saida$FV[i] %in% termos_erro) {
        saida$F[i] <- NA
        saida$p[i] <- NA
        next
      }
      e <- estrato_de(saida$FV[i])
      if (is.na(e)) next
      erro <- ajuste$erros[[e]]
      saida$F[i] <- saida$QM[i] / erro$qm
      saida$p[i] <- stats::pf(saida$F[i], saida$GL[i], erro$gl, lower.tail = FALSE)
    }
    for (e in seq_len(n_e)) saida$FV[saida$FV == termos_erro[e]] <- paste0("Erro (", letras_erro[e], ")")
    cv <- stats::setNames(
      vapply(ajuste$erros, function(x) sqrt(x$qm) / media * 100, numeric(1)),
      paste0("CV ", letras_erro, " (%)")
    )
  }

  rownames(saida) <- NULL
  attr(saida, "cv") <- cv
  saida
}

# ---------------------------------------------------------
# Erro usado na comparacao das medias de `fator` (opcionalmente
# dentro de cada nivel de `dentro`). Quando `fator` esta num
# estrato inferior ao de `dentro`, usa o erro combinado com
# graus de liberdade de Satterthwaite.
# ---------------------------------------------------------
#' @keywords internal
#' @noRd
erro_comparacao <- function(ajuste, fator, dentro = NULL) {
  n_e <- ajuste$n_estratos
  descricao <- function(e) if (n_e == 1) "resíduo" else paste0("erro (", letters[e], ")")
  i <- ajuste$estratos[[fator]]
  j <- if (is.null(dentro)) i else ajuste$estratos[[dentro]]
  if (j <= i) {
    return(c(ajuste$erros[[i]][c("qm", "gl")], list(descricao = descricao(i))))
  }
  k <- nlevels(ajuste$dados[[dentro]])
  qmi <- ajuste$erros[[i]]$qm
  qmj <- ajuste$erros[[j]]$qm
  qm <- (qmi + (k - 1) * qmj) / k
  gl <- (qmi + (k - 1) * qmj)^2 / (qmi^2 / ajuste$erros[[i]]$gl + ((k - 1) * qmj)^2 / ajuste$erros[[j]]$gl)
  list(qm = qm, gl = gl, descricao = paste0("erro combinado (", letters[i], " e ", letters[j], ", Satterthwaite)"))
}

#' Medias com letras pelo teste escolhido
#'
#' Calcula as medias dos niveis de `fator` (ou dentro de cada nivel de
#' `dentro`, para o desdobramento de interacoes) e compara pelo teste indicado,
#' usando o quadrado medio e os graus de liberdade corretos para o
#' delineamento. Em parcelas subdivididas, o fator da parcela usa o erro (a);
#' o fator da subparcela, o erro (b); e o fator da parcela dentro de cada nivel
#' da subparcela, o erro combinado com graus de liberdade de Satterthwaite
#' (a mesma regra vale para fatorial na parcela e para parcelas
#' subsubdivididas, estrato a estrato).
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
    usar_emmeans <- ajuste$n_estratos == 1
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
