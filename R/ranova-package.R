#' ranova: Analise de Variancia para Experimentos
#'
#' Ferramentas para ajustar experimentos simples e em esquema fatorial (DIC e
#' DBC), em parcelas subdivididas e subsubdivididas, gerar ANOVA, testes de
#' medias, desdobramentos, diagnostico de pressupostos, tabelas e graficos
#' para relatorios. O motor principal e [ranova_ajuste()], com
#' [ranova_anova()] e [ranova_medias()].
#'
#' @importFrom graphics par
#' @importFrom rlang .data sym
#' @importFrom stats aov as.formula median residuals setNames shapiro.test
#' @importFrom utils tail
#' @keywords internal
#' @docType package
"_PACKAGE"
