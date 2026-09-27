ranova_brand <- function(variant = c("horizontal", "vertical"), class = NULL, alt = "Ranova") {
  variant <- match.arg(variant)
  src <- if (identical(variant, "horizontal")) "ranova_horizontal.svg" else "ranova_vertical.svg"
  tags$img(src = src, class = class, alt = alt)
}

app_footer <- function() {
  tags$footer(
    class = "mm-footer",
    div(
      class = "mm-footer-logos",
      img(
        class = "mm-footer-logo app-footer-logo-melonmundi",
        src = "nova_logomm.png",
        alt = "MelonMundi"
      ),
      ranova_brand("vertical", class = "mm-footer-logo app-footer-logo-ranova")
    ),
    tags$div(
      class = "mm-footer-content",
      tags$div(
        class = "mm-footer-line mm-footer-line-main",
        tags$span(HTML("&copy; 2026 MelonMundi - Global Solutions. Todos os direitos reservados."))
      ),
      tags$div(
        class = "mm-footer-line mm-footer-line-legal",
        tags$span(paste0("Ranova v", APP_VERSION, " · pacote ranova ", PACOTE_VERSAO))
      )
    )
  )
}

step_title <- function(number, title) {
  h3(class = "ranova-step-title", tags$span(class = "ranova-step-number", number), title)
}

fator_input <- function(i, nome, niveis) {
  div(
    class = "ranova-factor-box",
    div(
      class = "ranova-factor-grid",
      textInput(paste0("fator_nome_", i), paste("Fator", i), value = nome),
      textInput(paste0("fator_niveis_", i), "Níveis (separe com ;)", value = niveis)
    )
  )
}

entrada_digitar <- function() {
  div(
    class = "mm-field-stack",
    tags$p(class = "mm-muted-text", "Defina o experimento e clique em Montar planilha. Depois digite ou cole os valores na planilha ao lado."),
    selectInput(
      "n_fatores",
      "Número de fatores",
      choices = c("1 fator" = 1, "2 fatores" = 2, "3 fatores" = 3),
      selected = 2
    ),
    fator_input(1, "Dose", "0; 50; 100; 150"),
    conditionalPanel("input.n_fatores >= 2", fator_input(2, "Cultivar", "A; B")),
    conditionalPanel("input.n_fatores >= 3", fator_input(3, "Época", "Seca; Chuvosa")),
    textInput("respostas_digitar", "Variáveis resposta (separe com ;)", value = "Produtividade; Altura", width = "100%"),
    numericInput("n_repeticoes", "Repetições ou blocos", value = 4, min = 2, max = 50, step = 1),
    actionButton("montar_planilha", "Montar planilha", icon = icon("table"), class = "btn mm-btn-secondary ranova-full-btn")
  )
}

entrada_importar <- function() {
  div(
    class = "mm-field-stack",
    tags$p(class = "mm-muted-text", "Envie uma planilha com uma linha por parcela e uma coluna para cada fator, bloco e variável resposta."),
    fileInput(
      "arquivo_dados",
      "Arquivo de dados",
      accept = c(".xlsx", ".xls", ".csv", ".txt"),
      buttonLabel = "Escolher...",
      placeholder = "Excel (.xlsx, .xls) ou CSV"
    ),
    uiOutput("seletor_aba"),
    downloadLink("baixar_modelo", tagList(icon("download"), " Baixar planilha modelo (.xlsx)"))
  )
}

formula_card <- function(title, formulas, description, icon_name = NULL) {
  div(
    class = "mm-info-card mm-formula-card ranova-calc-card",
    if (!is.null(icon_name)) icon(icon_name),
    h4(title),
    div(
      class = "mm-formula-list",
      lapply(as.character(formulas), function(formula) tags$span(class = "mm-formula-badge", formula))
    ),
    p(description)
  )
}

about_app_content <- function() {
  div(
    class = "mm-about-page ranova-about-page",
    div(
      class = "mm-about-header",
      div(
        class = "mm-brand-showcase mm-brand-showcase-centered",
        ranova_brand("vertical", class = "mm-brand-showcase-image mm-brand-showcase-image-vertical")
      ),
      p(class = "mm-product-id", paste0("MelonMundi - Ranova v", APP_VERSION)),
      h2("Sobre o Ranova"),
      p("O Ranova é a interface do pacote R ranova para análise de variância de experimentos fatoriais. Ele reúne em um só lugar a ANOVA, os testes de pressupostos, a comparação de médias com letras, o desdobramento de interações e os gráficos prontos para relatório.")
    ),
    div(
      class = "mm-session-card mm-accent-panel",
      icon("shield-halved"),
      div(
        h3("Sessão e privacidade"),
        p(TEXTO_PRIVACIDADE)
      )
    ),
    div(
      class = "mm-card-grid mm-card-grid-3",
      div(
        class = "mm-info-card",
        icon("keyboard"),
        h4("Digite ou cole"),
        p("Informe os fatores, os níveis e as variáveis resposta. O app monta a planilha com todas as combinações de tratamentos para você digitar ou colar os valores do Excel.")
      ),
      div(
        class = "mm-info-card",
        icon("upload"),
        h4("Importe planilhas"),
        p("Envie arquivos .xlsx, .xls ou .csv com uma linha por parcela. O app sugere quais colunas são blocos, fatores e variáveis resposta, e você pode ajustar.")
      ),
      div(
        class = "mm-info-card",
        icon("bar-chart"),
        h4("Resultados completos"),
        p("ANOVA com coeficiente de variação, pressupostos, médias com letras, desdobramento da interação, gráficos e relatório HTML para download.")
      )
    ),
    h3("Como organizar os dados"),
    p("Use o formato longo: cada linha é uma parcela (unidade experimental) e cada coluna é uma informação sobre ela."),
    tags$table(
      class = "mm-rule-table ranova-example-table",
      tags$thead(tags$tr(tags$th("Bloco"), tags$th("Dose"), tags$th("Cultivar"), tags$th("Produtividade"), tags$th("Brix"))),
      tags$tbody(
        tags$tr(tags$td("1"), tags$td("0"), tags$td("A"), tags$td("28,4"), tags$td("10,2")),
        tags$tr(tags$td("1"), tags$td("50"), tags$td("A"), tags$td("31,9"), tags$td("10,9")),
        tags$tr(tags$td("…"), tags$td("…"), tags$td("…"), tags$td("…"), tags$td("…")),
        tags$tr(tags$td("4"), tags$td("150"), tags$td("B"), tags$td("37,1"), tags$td("12,4"))
      )
    ),
    h3("Como usar"),
    tags$ol(
      class = "mm-steps",
      tags$li(tags$span("1"), "Escolha Digitar para montar a planilha a partir dos fatores, Importar para enviar um arquivo ou Exemplo para conhecer o app."),
      tags$li(tags$span("2"), "Confira a planilha. Você pode editar células, colar dados do Excel e inserir ou remover linhas."),
      tags$li(tags$span("3"), "Em Estrutura do experimento, escolha o delineamento (DIC ou DBC), a coluna de blocos, os fatores (até três) e as variáveis resposta."),
      tags$li(tags$span("4"), "Ajuste o nível de significância, as casas decimais, o formato da ANOVA e o tipo de erro-padrão."),
      tags$li(tags$span("5"), "Clique em Analisar e navegue pelas abas de resultados."),
      tags$li(tags$span("6"), "Baixe o relatório HTML com todas as tabelas e gráficos. Ele pode ser aberto no navegador, impresso ou salvo em PDF.")
    ),
    h3("Como os resultados são calculados"),
    div(
      class = "mm-card-grid mm-card-grid-2",
      formula_card("Modelo", c("DIC: y ~ A * B * C", "DBC: y ~ bloco + A * B * C"), "O modelo inclui todos os efeitos principais e interações entre os fatores escolhidos. No DBC, o bloco entra como efeito aditivo.", "sitemap"),
      formula_card("Coeficiente de variação", "CV (%) = √QM(resíduo) / média × 100", "Mede a precisão experimental. Aparece na última linha da tabela de ANOVA.", "percent"),
      formula_card("Pressupostos", c("Normalidade: Shapiro-Wilk", "Homogeneidade: Levene (mediana)"), "Os testes usam os resíduos do mesmo modelo da ANOVA. Valores de p abaixo da significância indicam violação do pressuposto.", "square-check"),
      formula_card("Comparação de médias", c("2 níveis: teste t", "3 ou mais níveis: Tukey"), "As médias ajustadas (emmeans) recebem letras; médias seguidas pela mesma letra não diferem entre si.", "arrow-down-wide-short"),
      formula_card("Desdobramento", c("Maiúsculas: comparam na coluna", "Minúsculas: comparam na linha"), "Quando a interação é significativa, compare os níveis de um fator dentro de cada nível do outro.", "th"),
      formula_card("Significância", c("* p < 0,05", "** p < 0,01", "*** p < 0,001"), "No formato com quadrados médios, os asteriscos indicam o nível de significância do teste F.", "star")
    ),
    div(
      class = "mm-note-warning",
      icon("info-circle"),
      p("Com três fatores, o desdobramento e o gráfico de interação mostram dois fatores por vez, usando médias ajustadas sobre os níveis do terceiro fator. Se a interação tripla for significativa, avalie também análises separadas por nível do terceiro fator.")
    )
  )
}

ui <- fluidPage(
  lang = "pt-BR",
  title = "Ranova - Análise de experimentos fatoriais",
  tags$head(
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = "anonymous"),
    tags$link(
      rel = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&display=swap"
    ),
    includeCSS("www/melonmundi-theme.css"),
    includeCSS("www/estilo.css"),
    tags$link(
      rel = "stylesheet",
      href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css"
    ),
    tags$link(rel = "icon", type = "image/svg+xml", href = "ranova_vertical.svg"),
    tags$script(HTML("
      (function() {
        var hasSessionData = false;

        function markSessionData() {
          hasSessionData = true;
        }

        document.addEventListener('input', markSessionData, true);
        document.addEventListener('change', markSessionData, true);
        document.addEventListener('keydown', function(event) {
          if (event.target.closest('.handsontable')) {
            markSessionData();
          }
        }, true);
        document.addEventListener('paste', markSessionData, true);

        window.addEventListener('beforeunload', function(event) {
          if (!hasSessionData) {
            return;
          }
          event.preventDefault();
          event.returnValue = '';
        });
      })();
    "))
  ),
  htmltools::htmlDependency(
    name = "lightable",
    version = "0.0.1",
    src = system.file("lightable-0.0.1", package = "kableExtra"),
    stylesheet = "lightable.css"
  ),
  navbarPage(
    title = tags$div(class = "navbar-brand-ranova", ranova_brand("horizontal", class = "navbar-brand-ranova-image")),
    windowTitle = "Ranova",
    id = "navegacao",
    inverse = FALSE,
    collapsible = TRUE,
    fluid = TRUE,
    tabPanel(
      title = tagList(icon("bar-chart"), "Análise"),
      value = "analise",
      span(class = "mm-page-title", "Análise de experimentos fatoriais"),
      tags$p("Analise experimentos com um, dois ou três fatores em delineamento inteiramente casualizado (DIC) ou em blocos casualizados (DBC)."),
      tags$p("Digite ou importe os dados, confira a estrutura do experimento e clique em Analisar para gerar a ANOVA, os testes de pressupostos, as médias com letras e os gráficos."),
      br(),
      fluidRow(
        class = "mm-workspace-grid",
        column(
          4,
          div(
            class = "mm-surface-card ranova-input-box",
            step_title("1", "Dados"),
            radioButtons(
              "modo_entrada",
              NULL,
              choices = c("Digitar" = "digitar", "Importar" = "importar", "Exemplo" = "exemplo"),
              selected = "digitar",
              inline = TRUE
            ),
            conditionalPanel("input.modo_entrada == 'digitar'", entrada_digitar()),
            conditionalPanel("input.modo_entrada == 'importar'", entrada_importar()),
            conditionalPanel(
              "input.modo_entrada == 'exemplo'",
              tags$p(class = "mm-muted-text", "Experimento fictício de melão em DBC com 4 blocos, fatorial 4 doses × 2 cultivares e três variáveis resposta."),
              actionButton("carregar_exemplo", "Carregar exemplo", icon = icon("flask"), class = "btn mm-btn-secondary ranova-full-btn")
            )
          ),
          div(
            class = "mm-surface-card ranova-input-box",
            step_title("2", "Estrutura do experimento"),
            div(
              class = "mm-field-stack",
              radioButtons(
                "delineamento",
                "Delineamento",
                choices = c("DIC" = "DIC", "DBC" = "DBC"),
                selected = "DIC",
                inline = TRUE
              ),
              conditionalPanel(
                "input.delineamento == 'DBC'",
                selectInput("coluna_bloco", "Coluna de blocos", choices = character())
              ),
              selectizeInput(
                "colunas_fatores",
                "Fatores (até 3)",
                choices = character(),
                multiple = TRUE,
                options = list(maxItems = MAX_FATORES, placeholder = "Selecione os fatores")
              ),
              selectizeInput(
                "colunas_respostas",
                "Variáveis resposta",
                choices = character(),
                multiple = TRUE,
                options = list(placeholder = "Selecione as variáveis")
              )
            )
          ),
          div(
            class = "mm-surface-card ranova-input-box",
            step_title("3", "Opções da análise"),
            div(
              class = "mm-field-stack",
              div(
                class = "mm-field-grid",
                selectInput("alpha", "Significância", choices = c("5%" = 0.05, "1%" = 0.01, "10%" = 0.10), selected = 0.05),
                selectInput("digitos", "Casas decimais", choices = 1:4, selected = 2)
              ),
              selectInput(
                "formato_anova",
                "Formato da ANOVA",
                choices = c(
                  "Quadrado médio com asteriscos" = "qm_star",
                  "F e p em colunas" = "f_p_colunas",
                  "F (p) na mesma célula" = "f_p_inline"
                )
              ),
              selectInput(
                "tipo_se",
                "Erro-padrão das médias",
                choices = c("Do modelo (ajustado)" = "modelo", "Descritivo (dos dados)" = "descritivo")
              )
            ),
            div(
              class = "ranova-input-actions",
              actionButton("analisar", "Analisar", icon = icon("play"), class = "btn mm-btn-primary ranova-calc-btn")
            )
          )
        ),
        column(
          8,
          normal_card(
            "Planilha de dados",
            tagList(
              uiOutput("resumo_planilha"),
              div(class = "ranova-hot-wrap", rHandsontableOutput("planilha")),
              tags$p(
                class = "ranova-hint",
                icon("info-circle"),
                " Use Ctrl+C / Ctrl+V para colar dados do Excel. Clique com o botão direito para inserir ou remover linhas. Aceita vírgula ou ponto como separador decimal."
              ),
              div(
                class = "ranova-inline-actions",
                downloadButton("baixar_dados", "Baixar dados (.xlsx)", class = "btn mm-btn-secondary")
              )
            )
          ),
          uiOutput("mensagens_analise"),
          uiOutput("painel_resultados")
        )
      )
    ),
    tabPanel(
      title = tagList(icon("balance-scale"), "Aviso Legal"),
      value = "aviso",
      div(
        class = "mm-legal-shell",
        div(
          class = "mm-alert-warning ranova-legal-card",
          div(class = "mm-brand-showcase mm-brand-showcase-legal", ranova_brand("vertical", class = "mm-brand-showcase-image mm-brand-showcase-image-legal")),
          p(class = "mm-product-id", paste0("MelonMundi - Ranova v", APP_VERSION)),
          p(class = "mm-alert-title", icon("balance-scale"), " Aviso Legal"),
          p("O Ranova é uma ferramenta de apoio à análise estatística de experimentos. Os resultados dependem diretamente da qualidade dos dados informados, do planejamento experimental e da adequação do modelo escolhido."),
          p("A interpretação dos resultados, a verificação dos pressupostos da análise de variância e a escolha de transformações ou modelos alternativos são de responsabilidade do usuário e devem ser acompanhadas por profissional habilitado."),
          p("A MelonMundi não se responsabiliza por decisões técnicas, científicas, comerciais ou de publicação tomadas com base nos resultados gerados pela aplicação."),
          p(TEXTO_PRIVACIDADE)
        )
      )
    ),
    tabPanel(
      title = tagList(icon("question-circle"), "Ajuda"),
      value = "ajuda",
      about_app_content()
    )
  ),
  app_footer()
)
