# =========================================================
# 09_testes_medias.R
#
# Testes de comparacao de medias a partir de medias, numero
# de repeticoes, quadrado medio e graus de liberdade do erro.
# =========================================================

#' Testes de medias disponiveis
#'
#' Nomes aceitos pelo argumento `teste` de [letras_teste()] e
#' [ranova_medias()], com o rotulo em portugues.
#'
#' @format Vetor nomeado de caracteres.
#' @export
TESTES_MEDIAS <- c(
  "auto" = "Autom\u00E1tico (t para 2 n\u00EDveis, Tukey para 3 ou mais)",
  "tukey" = "Tukey",
  "t" = "t (LSD de Fisher)",
  "bonferroni" = "t com corre\u00E7\u00E3o de Bonferroni",
  "duncan" = "Duncan",
  "snk" = "Student-Newman-Keuls (SNK)",
  "scott-knott" = "Scott-Knott",
  "dunnett" = "Dunnett (comparado ao controle)"
)

#' @keywords internal
#' @noRd
teste_efetivo <- function(teste, k) {
  if (identical(teste, "auto")) if (k <= 2) "t" else "tukey" else teste
}

#' Letras de comparacao de medias
#'
#' Compara as medias pelo teste escolhido e devolve as letras (medias com a
#' mesma letra nao diferem), com `a` para a maior media. No teste de Dunnett,
#' devolve `*` para os niveis que diferem do controle.
#'
#' - `tukey`: diferenca minima significativa pela amplitude estudentizada.
#' - `t`: t de Student sem correcao (LSD de Fisher).
#' - `bonferroni`: t com nivel dividido pelo numero de comparacoes.
#' - `duncan` e `snk`: amplitudes multiplas, com o nivel protegido de Duncan
#'   (`1 - (1 - alpha)^(p - 1)`) ou o nivel fixo do SNK.
#' - `scott-knott`: agrupamento sem sobreposicao (Scott e Knott, 1974).
#' - `dunnett`: cada nivel contra o controle, bilateral.
#'
#' @param medias Vetor nomeado de medias.
#' @param n Numero de repeticoes de cada media (media harmonica se desigual).
#' @param qm Quadrado medio do erro.
#' @param gl Graus de liberdade do erro.
#' @param teste Um dos nomes de [TESTES_MEDIAS].
#' @param alpha Nivel de significancia.
#' @param controle Nivel de referencia para Dunnett (padrao: o primeiro).
#' @param maiusculas Se `TRUE`, usa letras maiusculas.
#'
#' @return Vetor de caracteres com o grupo de cada media, na ordem de `medias`.
#' @export
#'
#' @examples
#' medias <- c(A = 10, B = 12, C = 15, D = 15.5)
#' letras_teste(medias, n = 4, qm = 2, gl = 12, teste = "tukey")
#' letras_teste(medias, n = 4, qm = 2, gl = 12, teste = "scott-knott")
letras_teste <- function(medias, n, qm, gl, teste = "auto", alpha = 0.05, controle = NULL, maiusculas = FALSE) {
  if (is.null(names(medias))) names(medias) <- seq_along(medias)
  k <- length(medias)
  teste <- teste_efetivo(teste, k)
  if (!teste %in% names(TESTES_MEDIAS)) stop("Teste de m\u00E9dias desconhecido: ", teste, call. = FALSE)
  conjunto <- if (maiusculas) LETTERS else letters
  if (k < 2 || !is.finite(qm) || qm <= 0) return(stats::setNames(rep(conjunto[1], k), names(medias)))

  if (identical(teste, "dunnett")) {
    return(marcas_dunnett(medias, n, qm, gl, alpha, controle))
  }
  if (identical(teste, "scott-knott")) {
    return(letras_scott_knott(medias, n, qm, gl, alpha, conjunto))
  }

  ordem <- order(medias, decreasing = TRUE)
  m <- medias[ordem]
  ep <- sqrt(qm / n)
  dif <- abs(outer(m, m, "-"))
  significativo <- matrix(FALSE, k, k)

  if (teste %in% c("tukey", "t", "bonferroni")) {
    dms <- switch(teste,
      tukey = stats::qtukey(1 - alpha, k, gl) * ep,
      t = stats::qt(1 - alpha / 2, gl) * sqrt(2) * ep,
      bonferroni = stats::qt(1 - alpha / (k * (k - 1)), gl) * sqrt(2) * ep
    )
    significativo <- dif > dms
  } else {
    # Duncan e SNK: amplitude p = numero de medias entre i e j (inclusive), com
    # a regra de que um par so difere se todos os intervalos que o contem diferem.
    amplitude <- function(p) {
      nivel <- if (identical(teste, "duncan")) 1 - (1 - alpha)^(p - 1) else alpha
      stats::qtukey(1 - nivel, p, gl) * ep
    }
    for (p in k:2) {
      for (i in 1:(k - p + 1)) {
        j <- i + p - 1
        contido <- p == k || all(c(
          if (i > 1) significativo[i - 1, j] else TRUE,
          if (j < k) significativo[i, j + 1] else TRUE
        ))
        significativo[i, j] <- significativo[j, i] <- contido && (m[i] - m[j]) > amplitude(p)
      }
    }
  }

  letras <- letras_de_matriz(significativo, conjunto)
  stats::setNames(letras[match(seq_len(k), ordem)], names(medias))
}

# Letras a partir da matriz de diferencas significativas (medias ordenadas da
# maior para a menor): algoritmo de insercao e absorcao de multcompView, com as
# letras renomeadas para seguir a ordem das medias.
#' @keywords internal
#' @noRd
letras_de_matriz <- function(significativo, conjunto) {
  k <- nrow(significativo)
  rotulos <- sprintf("L%03d", seq_len(k))
  pares <- utils::combn(k, 2)
  diferentes <- stats::setNames(significativo[t(pares)], paste(rotulos[pares[1, ]], rotulos[pares[2, ]], sep = "-"))
  brutas <- multcompView::multcompLetters(diferentes, Letters = c(letters, LETTERS, paste0(letters, "'")))$Letters
  brutas <- brutas[rotulos]
  # Renomeia: a primeira letra encontrada (maior media) vira "a", e assim por diante.
  vistas <- character()
  for (x in brutas) for (ch in strsplit(x, "")[[1]]) if (!ch %in% vistas) vistas <- c(vistas, ch)
  mapa <- stats::setNames(conjunto[seq_along(vistas)], vistas)
  vapply(brutas, function(x) {
    chars <- strsplit(x, "")[[1]]
    paste(sort(mapa[chars]), collapse = "")
  }, character(1), USE.NAMES = FALSE)
}

#' @keywords internal
#' @noRd
letras_scott_knott <- function(medias, n, qm, gl, alpha, conjunto) {
  ordem <- order(medias, decreasing = TRUE)
  m <- unname(medias[ordem])
  s2 <- qm / n
  dividir <- function(idx) {
    k <- length(idx)
    if (k < 2) return(list(idx))
    x <- m[idx]
    total <- sum(x)
    b0 <- vapply(seq_len(k - 1), function(j) {
      sum(x[1:j])^2 / j + sum(x[(j + 1):k])^2 / (k - j) - total^2 / k
    }, numeric(1))
    corte <- which.max(b0)
    sigma2 <- (sum((x - mean(x))^2) + gl * s2) / (k + gl)
    lambda <- pi / (2 * (pi - 2)) * b0[corte] / sigma2
    if (lambda > stats::qchisq(1 - alpha, k / (pi - 2))) {
      c(dividir(idx[1:corte]), dividir(idx[(corte + 1):k]))
    } else {
      list(idx)
    }
  }
  grupos <- dividir(seq_along(m))
  letra <- character(length(m))
  for (g in seq_along(grupos)) letra[grupos[[g]]] <- conjunto[g]
  stats::setNames(letra[match(seq_along(m), ordem)], names(medias))
}

#' @keywords internal
#' @noRd
marcas_dunnett <- function(medias, n, qm, gl, alpha, controle = NULL) {
  k <- length(medias)
  if (is.null(controle) || !controle %in% names(medias)) controle <- names(medias)[1]
  correlacao <- matrix(0.5, k - 1, k - 1)
  diag(correlacao) <- 1
  estado <- if (exists(".Random.seed", envir = globalenv())) get(".Random.seed", envir = globalenv()) else NULL
  set.seed(20260928)
  critico <- mvtnorm::qmvt(1 - alpha, tail = "both.tails", df = max(1, round(gl)), corr = correlacao)$quantile
  if (is.null(estado)) rm(".Random.seed", envir = globalenv()) else assign(".Random.seed", estado, envir = globalenv())
  dms <- critico * sqrt(2 * qm / n)
  marcas <- ifelse(abs(medias - medias[controle]) > dms, "*", "")
  marcas[controle] <- ""
  stats::setNames(unname(marcas), names(medias))
}
