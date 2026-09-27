# =========================================================
# 08_app.R
# Aplicativo Shiny Ranova
# =========================================================

#' Executa o aplicativo Shiny Ranova
#'
#' Abre a interface grafica do pacote para digitar ou importar dados
#' (Excel ou CSV), escolher delineamento (DIC ou DBC) e de um a tres
#' fatores e gerar ANOVA, pressupostos, medias com letras, desdobramento
#' da interacao, graficos e relatorio HTML.
#'
#' O aplicativo requer os pacotes sugeridos `shiny`, `rhandsontable`,
#' `readxl` e `writexl`.
#'
#' @param ... Argumentos repassados para [shiny::runApp()], como `port`
#'   ou `launch.browser`.
#'
#' @return Chamado pelo efeito colateral de iniciar o aplicativo.
#' @export
#'
#' @examples
#' if (interactive()) {
#'   executar_app()
#' }
executar_app <- function(...) {
  necessarios <- c("shiny", "rhandsontable", "readxl", "writexl")
  faltando <- necessarios[!vapply(necessarios, requireNamespace, logical(1), quietly = TRUE)]
  if (length(faltando) > 0) {
    stop(
      "Instale os pacotes necessarios para o aplicativo: ",
      paste0("install.packages(c(", paste0("\"", faltando, "\"", collapse = ", "), "))"),
      call. = FALSE
    )
  }

  diretorio <- system.file("app", package = "ranova")
  if (!nzchar(diretorio)) {
    stop("Diretorio do aplicativo nao encontrado. Reinstale o pacote ranova.", call. = FALSE)
  }

  shiny::runApp(diretorio, ...)
}
