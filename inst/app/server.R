server <- function(input, output, session) {
  planilha_base <- reactiveVal(NULL)
  resultado <- reactiveVal(NULL)
  mensagens <- reactiveVal(NULL)
  arquivo_atual <- reactiveVal(NULL)

  # ---------------------------------------------------------
  # Estrutura do experimento
  # ---------------------------------------------------------

  aplicar_estrutura <- function(dados, estrutura) {
    colunas <- names(dados)
    updateRadioButtons(session, "delineamento", selected = estrutura$delineamento)
    updateSelectInput(session, "coluna_bloco", choices = colunas, selected = estrutura$bloco)
    updateSelectizeInput(session, "colunas_fatores", choices = colunas, selected = estrutura$fatores)
    updateSelectizeInput(session, "colunas_respostas", choices = colunas, selected = estrutura$respostas)
  }

  carregar_planilha <- function(dados, estrutura = sugerir_estrutura(dados)) {
    planilha_base(dados)
    resultado(NULL)
    mensagens(NULL)
    aplicar_estrutura(dados, estrutura)
  }

  dados_atuais <- reactive({
    base <- planilha_base()
    req(base)
    editada <- input$planilha
    if (is.null(editada)) {
      return(base)
    }
    dados <- tryCatch(hot_to_r(editada), error = function(e) NULL)
    if (is.null(dados) || !identical(names(dados), names(base))) {
      return(base)
    }
    dados <- as.data.frame(dados, stringsAsFactors = FALSE, check.names = FALSE)
    dados[] <- lapply(dados, function(coluna) {
      texto <- as.character(coluna)
      texto[is.na(texto)] <- ""
      texto
    })
    dados
  })

  # ---------------------------------------------------------
  # Entrada: digitar
  # ---------------------------------------------------------

  fatores_digitados <- function() {
    n <- as.integer(input$n_fatores)
    lapply(seq_len(n), function(i) {
      list(
        nome = trimws(input[[paste0("fator_nome_", i)]] %||% ""),
        niveis = separar_lista(input[[paste0("fator_niveis_", i)]])
      )
    })
  }

  validar_digitacao <- function(fatores, respostas, n_rep) {
    erros <- character()
    nomes <- vapply(fatores, function(f) f$nome, character(1))
    for (i in seq_along(fatores)) {
      if (!nzchar(fatores[[i]]$nome)) {
        erros <- c(erros, sprintf("Informe o nome do fator %d.", i))
      }
      if (length(fatores[[i]]$niveis) < 2) {
        erros <- c(erros, sprintf("O fator %d precisa de pelo menos dois níveis diferentes.", i))
      }
    }
    if (length(respostas) == 0) {
      erros <- c(erros, "Informe pelo menos uma variável resposta.")
    }
    todos <- c(nomes, respostas, "Bloco", "Repetição")
    if (any(duplicated(tolower(todos[nzchar(todos)])))) {
      erros <- c(erros, "Os nomes de fatores e variáveis resposta devem ser diferentes entre si e diferentes de Bloco e Repetição.")
    }
    if (is.na(n_rep) || n_rep < 2 || n_rep > 50) {
      erros <- c(erros, "Informe entre 2 e 50 repetições ou blocos.")
    }
    n_linhas <- prod(vapply(fatores, function(f) length(f$niveis), numeric(1))) * max(n_rep, 0, na.rm = TRUE)
    if (length(erros) == 0 && n_linhas > 5000) {
      erros <- c(erros, "A planilha teria mais de 5.000 linhas. Reduza o número de níveis ou de repetições.")
    }
    erros
  }

  montar_planilha_digitada <- function(mostrar_erros = TRUE) {
    fatores <- fatores_digitados()
    respostas <- separar_lista(input$respostas_digitar)
    n_rep <- suppressWarnings(as.integer(input$n_repeticoes))
    erros <- validar_digitacao(fatores, respostas, n_rep)
    if (length(erros) > 0) {
      if (mostrar_erros) {
        showNotification(tagList(lapply(erros, tags$div)), type = "error", duration = 8)
      }
      return(invisible(FALSE))
    }

    delineamento <- input$delineamento %||% "DIC"
    dados <- gerar_planilha_modelo(fatores, n_rep, delineamento, respostas)
    carregar_planilha(dados, list(
      delineamento = delineamento,
      bloco = if (identical(delineamento, "DBC")) "Bloco" else "",
      fatores = vapply(fatores, function(f) f$nome, character(1)),
      respostas = respostas
    ))
    invisible(TRUE)
  }

  session$onFlushed(function() {
    isolate(if (is.null(planilha_base())) montar_planilha_digitada(mostrar_erros = FALSE))
  }, once = TRUE)

  observeEvent(input$montar_planilha, {
    if (isTRUE(montar_planilha_digitada())) {
      showNotification("Planilha montada. Digite ou cole os valores das variáveis resposta.", type = "message", duration = 5)
    }
  })

  # ---------------------------------------------------------
  # Entrada: importar
  # ---------------------------------------------------------

  importar_arquivo <- function(aba = NULL) {
    arquivo <- arquivo_atual()
    req(arquivo)
    dados <- tryCatch(
      ler_arquivo_dados(arquivo$datapath, arquivo$name, aba),
      error = function(e) {
        showNotification(paste("Não foi possível ler o arquivo:", conditionMessage(e)), type = "error", duration = 8)
        NULL
      }
    )
    if (!is.null(dados)) {
      carregar_planilha(dados_para_planilha(dados))
      showNotification(
        sprintf("Arquivo importado: %d linhas e %d colunas. Confira a estrutura sugerida.", nrow(dados), ncol(dados)),
        type = "message",
        duration = 6
      )
    }
  }

  observeEvent(input$arquivo_dados, {
    arquivo_atual(input$arquivo_dados)
    importar_arquivo()
  })

  output$seletor_aba <- renderUI({
    arquivo <- arquivo_atual()
    req(arquivo)
    req(tolower(tools::file_ext(arquivo$name)) %in% c("xlsx", "xls"))
    abas <- tryCatch(ler_planilhas_excel(arquivo$datapath), error = function(e) character())
    req(length(abas) > 1)
    selectInput("aba_excel", "Aba da planilha", choices = abas, selected = abas[1])
  })

  observeEvent(input$aba_excel, {
    importar_arquivo(input$aba_excel)
  }, ignoreInit = TRUE)

  output$baixar_modelo <- downloadHandler(
    filename = function() "modelo_ranova.xlsx",
    content = function(file) {
      writexl::write_xlsx(dados_exemplo(), file)
    }
  )

  # ---------------------------------------------------------
  # Entrada: exemplo
  # ---------------------------------------------------------

  observeEvent(input$carregar_exemplo, {
    dados <- dados_exemplo()
    carregar_planilha(dados, list(
      delineamento = "DBC",
      bloco = "Bloco",
      fatores = c("Dose", "Cultivar"),
      respostas = names(dados)[4:6]
    ))
    showNotification("Exemplo carregado. Clique em Analisar para ver os resultados.", type = "message", duration = 5)
  })

  # ---------------------------------------------------------
  # Planilha editável
  # ---------------------------------------------------------

  output$planilha <- renderRHandsontable({
    dados <- planilha_base()
    req(dados)
    altura <- min(460, 32 + 24 * max(nrow(dados), 8))
    rhandsontable(
      dados,
      rowHeaders = TRUE,
      stretchH = "all",
      height = altura,
      useTypes = TRUE,
      search = FALSE
    ) |>
      hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) |>
      hot_cols(colWidths = 120, manualColumnResize = TRUE) |>
      hot_table(highlightCol = TRUE, highlightRow = TRUE)
  })

  output$resumo_planilha <- renderUI({
    dados <- planilha_base()
    req(dados)
    atual <- dados_atuais()
    tags$p(
      class = "ranova-data-summary",
      tags$span(class = "mm-chip", icon("list"), sprintf(" %d linhas", nrow(atual))),
      tags$span(class = "mm-chip", icon("columns"), sprintf(" %d colunas", ncol(atual)))
    )
  })

  output$baixar_dados <- downloadHandler(
    filename = function() paste0("dados_ranova_", format(Sys.Date(), "%Y%m%d"), ".xlsx"),
    content = function(file) {
      dados <- dados_atuais()
      writexl::write_xlsx(dados, file)
    }
  )

  # ---------------------------------------------------------
  # Análise
  # ---------------------------------------------------------

  observeEvent(input$analisar, {
    dados <- tryCatch(dados_atuais(), error = function(e) NULL)
    if (is.null(dados)) {
      mensagens(list(tipo = "erro", itens = "Monte, importe ou carregue uma planilha antes de analisar."))
      return()
    }

    prep <- preparar_dados_analise(
      dados = dados,
      delineamento = input$delineamento,
      bloco = input$coluna_bloco,
      fatores = input$colunas_fatores,
      respostas = input$colunas_respostas
    )
    if (!isTRUE(prep$ok)) {
      mensagens(list(tipo = "erro", itens = prep$erros))
      return()
    }

    opcoes <- list(
      alpha = as.numeric(input$alpha),
      digitos = as.integer(input$digitos),
      digitos_anova = max(2L, as.integer(input$digitos) + 1L),
      formato = input$formato_anova,
      tipo_se = input$tipo_se
    )

    analise <- withProgress(message = "Analisando o experimento...", value = 0.3, {
      executar_analise(prep, opcoes)
    })

    avisos <- character()
    if (prep$linhas_descartadas > 0) {
      avisos <- c(avisos, sprintf("%d linha(s) com fatores ou blocos em branco foram ignoradas.", prep$linhas_descartadas))
    }
    n_ausentes <- sum(vapply(prep$respostas, function(v) sum(is.na(prep$dados[[v]])), numeric(1)))
    if (n_ausentes > 0) {
      avisos <- c(avisos, sprintf("%d valor(es) ausente(s) nas variáveis resposta foram desconsiderados no ajuste do modelo.", n_ausentes))
    }
    if (!isTRUE(analise$anova$ok)) {
      mensagens(list(tipo = "erro", itens = c("Não foi possível ajustar a ANOVA com os dados informados.", analise$anova$erro)))
      return()
    }

    mensagens(if (length(avisos) > 0) list(tipo = "aviso", itens = avisos) else NULL)
    resultado(analise)
    updateTabsetPanel(session, "abas_resultado", selected = "anova")
  })

  output$mensagens_analise <- renderUI({
    msg <- mensagens()
    req(msg)
    classe <- if (identical(msg$tipo, "erro")) "ranova-message ranova-message-error" else "ranova-message ranova-message-warning"
    div(
      class = classe,
      role = "alert",
      icon(if (identical(msg$tipo, "erro")) "exclamation-circle" else "info-circle"),
      div(
        tags$strong(if (identical(msg$tipo, "erro")) "Confira os dados" else "Observações"),
        tags$ul(lapply(msg$itens, tags$li))
      )
    )
  })

  opcoes_fatores <- function(prep) {
    stats::setNames(prep$fatores, rotulo(prep, prep$fatores))
  }

  opcoes_respostas <- function(prep) {
    stats::setNames(prep$respostas, rotulo(prep, prep$respostas))
  }

  output$painel_resultados <- renderUI({
    res <- resultado()
    if (is.null(res)) {
      return(normal_card(
        "Resultados",
        div(
          class = "mm-empty-box ranova-empty",
          icon("bar-chart"),
          tags$p(class = "mm-empty-message-title", "Nenhuma análise ainda"),
          tags$p(class = "mm-empty-message-detail", "Preencha a planilha, confira a estrutura do experimento e clique em Analisar.")
        )
      ))
    }

    prep <- res$prep
    varios_fatores <- length(prep$fatores) >= 2
    fatores <- opcoes_fatores(prep)
    respostas <- opcoes_respostas(prep)

    abas <- list(
      tabPanel(
        title = "ANOVA",
        value = "anova",
        uiOutput("resumo_significancia"),
        div(class = "ranova-table-scroll", tabela_html(res$anova$valor))
      ),
      tabPanel(
        title = "Pressupostos",
        value = "pressupostos",
        if (isTRUE(res$diagnostico$ok)) tabela_diagnostico_html(res$diagnostico$valor) else tags$p(class = "text-danger", res$diagnostico$erro),
        tags$p(class = "ranova-hint", "Shapiro-Wilk avalia a normalidade dos resíduos e Levene avalia a homogeneidade de variâncias entre tratamentos. Quando algum pressuposto não for atendido, considere transformar a variável ou usar outro modelo."),
        selectInput("var_residuos", "Resíduos da variável", choices = respostas, width = "100%"),
        plotOutput("grafico_residuos", height = 330)
      ),
      tabPanel(
        title = "Médias",
        value = "medias",
        uiOutput("aviso_medias_interacao"),
        lapply(names(res$medias), function(fator) {
          item <- res$medias[[fator]]
          div(
            class = "ranova-table-scroll ranova-table-block",
            if (isTRUE(item$ok)) tabela_html(item$valor) else tags$p(class = "text-danger", paste0(rotulo(prep, fator), ": ", item$erro))
          )
        }),
        tags$p(class = "ranova-hint", "Médias seguidas pela mesma letra não diferem entre si pelo teste t (2 níveis) ou Tukey (3 ou mais níveis).")
      )
    )

    if (varios_fatores) {
      abas[[length(abas) + 1]] <- tabPanel(
        title = "Interação",
        value = "interacao",
        div(
          class = "mm-field-grid",
          selectInput("fator_linha", "Fator nas linhas", choices = fatores, selected = prep$fatores[1]),
          selectInput("fator_coluna", "Fator nas colunas", choices = fatores, selected = prep$fatores[2])
        ),
        uiOutput("tabela_interacao")
      )
    }

    abas[[length(abas) + 1]] <- tabPanel(
      title = "Gráficos",
      value = "graficos",
      h4(class = "ranova-subtitle", "Médias com letras"),
      div(
        class = "mm-field-grid",
        selectInput("var_grafico", "Variável resposta", choices = respostas),
        selectInput("fator_grafico", "Fator", choices = fatores)
      ),
      plotOutput("grafico_medias", height = 360),
      downloadButton("baixar_grafico_medias", "Baixar gráfico (.png)", class = "btn mm-btn-secondary ranova-small-btn"),
      if (varios_fatores) {
        tagList(
          tags$hr(),
          h4(class = "ranova-subtitle", "Gráfico de interação"),
          div(
            class = "mm-field-grid",
            selectInput("fator_x", "Fator no eixo X", choices = fatores, selected = prep$fatores[1]),
            selectInput("fator_traco", "Fator nas linhas (cores)", choices = fatores, selected = prep$fatores[2])
          ),
          plotOutput("grafico_interacao", height = 360),
          downloadButton("baixar_grafico_interacao", "Baixar gráfico (.png)", class = "btn mm-btn-secondary ranova-small-btn")
        )
      }
    )

    tagList(
      normal_card(
        "Resultados",
        tagList(
          tags$p(
            class = "ranova-result-meta",
            tags$span(class = "mm-chip", if (identical(prep$delineamento, "DBC")) "DBC" else "DIC"),
            tags$span(class = "mm-chip", paste(rotulo(prep, prep$fatores), collapse = " × ")),
            tags$span(class = "mm-chip", sprintf("%d observações", prep$n_obs)),
            tags$span(class = "mm-chip", paste0("α = ", formatC(res$opcoes$alpha, format = "f", digits = 2, decimal.mark = ",")))
          ),
          do.call(tabsetPanel, c(list(id = "abas_resultado", type = "pills"), abas))
        ),
        class = "ranova-results-card"
      ),
      div(
        class = "mm-surface-card ranova-input-box report-box",
        h4("Relatório"),
        tags$p("Baixe um relatório HTML com a ANOVA, os pressupostos, as tabelas de médias, o desdobramento da interação e os gráficos. Abra no navegador para imprimir ou salvar em PDF."),
        downloadButton("baixar_relatorio", "Baixar relatório (.html)", class = "btn mm-btn-primary")
      )
    )
  })

  significancia_atual <- reactive({
    res <- resultado()
    req(res, isTRUE(res$significancia$ok))
    res$significancia$valor
  })

  output$resumo_significancia <- renderUI({
    res <- resultado()
    req(res)
    prep <- res$prep
    sig <- significancia_atual()
    interacoes <- sig[sig$interacao & sig$significativo, , drop = FALSE]
    principais <- sig[!sig$interacao & sig$significativo, , drop = FALSE]

    negrito <- function(x) paste0("<strong>", htmltools::htmlEscape(x), "</strong>")
    itens <- character()
    for (i in seq_len(nrow(interacoes))) {
      itens <- c(itens, paste0(
        negrito(rotulo(prep, interacoes$variavel[i])), ": interação ",
        negrito(rotulo_termo(prep, interacoes$termo[i])), " significativa (p = ",
        formatar_p(interacoes$p[i]), "). Interprete pelo desdobramento na aba Interação."
      ))
    }
    sem_interacao <- setdiff(unique(principais$variavel), unique(interacoes$variavel))
    for (v in sem_interacao) {
      termos <- principais$termo[principais$variavel == v]
      itens <- c(itens, paste0(
        negrito(rotulo(prep, v)), ": efeito significativo de ",
        htmltools::htmlEscape(paste(vapply(termos, function(t) rotulo_termo(prep, t), character(1)), collapse = ", ")),
        ". Veja a aba Médias."
      ))
    }
    sem_efeito <- setdiff(prep$respostas, unique(sig$variavel[sig$significativo]))
    for (v in sem_efeito) {
      itens <- c(itens, paste0(negrito(rotulo(prep, v)), ": nenhum efeito significativo dos fatores."))
    }

    div(
      class = "mm-detail-panel mm-detail-panel-accent ranova-significance",
      tags$strong("Leitura rápida"),
      tags$ul(lapply(itens, function(item) tags$li(HTML(item))))
    )
  })

  output$aviso_medias_interacao <- renderUI({
    res <- resultado()
    req(res, length(res$prep$fatores) >= 2)
    sig <- significancia_atual()
    variaveis <- unique(sig$variavel[sig$interacao & sig$significativo])
    req(length(variaveis) > 0)
    div(
      class = "mm-note-warning",
      icon("exclamation-triangle"),
      p(paste0(
        "Há interação significativa para ", paste(rotulo(res$prep, variaveis), collapse = ", "),
        ". Nesses casos, as médias de cada fator isolado devem ser interpretadas com cautela; prefira o desdobramento na aba Interação."
      ))
    )
  })

  output$grafico_residuos <- renderPlot({
    res <- resultado()
    req(res, input$var_residuos %in% res$prep$respostas)
    grafico_residuos(res$prep, input$var_residuos)
  }, res = 96)

  fatores_interacao <- reactive({
    res <- resultado()
    req(res, input$fator_linha, input$fator_coluna)
    validate(need(input$fator_linha != input$fator_coluna, "Escolha fatores diferentes para linhas e colunas."))
    req(all(c(input$fator_linha, input$fator_coluna) %in% res$prep$fatores))
    c(input$fator_linha, input$fator_coluna)
  })

  output$tabela_interacao <- renderUI({
    res <- resultado()
    pares <- fatores_interacao()
    tabela <- tabela_interacao(res$prep, res$opcoes, pares[1], pares[2])
    if (!isTRUE(tabela$ok)) {
      return(tags$p(class = "text-danger", paste("Não foi possível gerar o desdobramento:", tabela$erro)))
    }
    tagList(
      div(class = "ranova-table-scroll", tabela_html(tabela$valor)),
      tags$p(
        class = "ranova-hint",
        "Letras maiúsculas comparam os níveis do fator nas colunas dentro de cada linha; letras minúsculas comparam os níveis do fator nas linhas dentro de cada coluna.",
        if (length(res$prep$fatores) == 3) " Com três fatores, as médias são ajustadas sobre os níveis do fator não exibido."
      )
    )
  })

  grafico_medias_atual <- reactive({
    res <- resultado()
    req(res, input$var_grafico %in% res$prep$respostas, input$fator_grafico %in% res$prep$fatores)
    grafico <- tentar(grafico_medias(res$prep, res$opcoes, input$var_grafico, input$fator_grafico))
    validate(need(isTRUE(grafico$ok), paste("Não foi possível gerar o gráfico:", grafico$erro)))
    grafico$valor
  })

  grafico_interacao_atual <- reactive({
    res <- resultado()
    req(res, input$var_grafico %in% res$prep$respostas, input$fator_x, input$fator_traco)
    validate(need(input$fator_x != input$fator_traco, "Escolha fatores diferentes para o eixo X e para as linhas."))
    grafico <- tentar(grafico_interacao(res$prep, input$var_grafico, input$fator_x, input$fator_traco))
    validate(need(isTRUE(grafico$ok), paste("Não foi possível gerar o gráfico:", grafico$erro)))
    grafico$valor
  })

  output$grafico_medias <- renderPlot(grafico_medias_atual(), res = 96)
  output$grafico_interacao <- renderPlot(grafico_interacao_atual(), res = 96)

  nome_grafico <- function(prefixo) {
    paste0(prefixo, "_", nome_seguro(rotulo(resultado()$prep, input$var_grafico)), ".png")
  }

  output$baixar_grafico_medias <- downloadHandler(
    filename = function() nome_grafico("medias"),
    content = function(file) ggsave(file, grafico_medias_atual(), width = 7, height = 4.5, dpi = 300, bg = "white")
  )

  output$baixar_grafico_interacao <- downloadHandler(
    filename = function() nome_grafico("interacao"),
    content = function(file) ggsave(file, grafico_interacao_atual(), width = 7, height = 4.5, dpi = 300, bg = "white")
  )

  output$baixar_relatorio <- downloadHandler(
    filename = function() paste0("relatorio_ranova_", format(Sys.time(), "%Y%m%d_%H%M"), ".html"),
    content = function(file) {
      res <- resultado()
      req(res)
      pares <- if (length(res$prep$fatores) >= 2) {
        escolhidos <- c(input$fator_linha, input$fator_coluna)
        if (length(escolhidos) == 2 && escolhidos[1] != escolhidos[2]) escolhidos else res$prep$fatores[1:2]
      }
      withProgress(message = "Gerando relatório...", value = 0.4, {
        gerar_relatorio_html(res, file, pares[1], pares[2])
      })
    }
  )
}
