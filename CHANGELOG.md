# Changelog

## [0.5.0] - 2026-09-27

### Added
- Aplicativo Shiny Ranova em `inst/app/`, executado com `executar_app()`: digitação com planilha gerada a partir dos fatores, importação de Excel/CSV, DIC ou DBC com 1 a 3 fatores, ANOVA, pressupostos, médias com letras, desdobramento da interação, gráficos e relatório HTML.
- `shiny`, `rhandsontable`, `readxl` e `writexl` em `Suggests`.

### Changed
- `anova_fatorial_qm_tabela()`: a linha de resíduos passa a se chamar `Resíduo` e a linha do CV deixa a coluna GL em branco em vez de `NA`.

## [0.4.4] - 2026-02-12

### Changed
- Melhoria visual em `tabela_interacao_fatorial_multivariaveis()`: agrupamento por variavel com linha de separacao entre blocos usando `kableExtra::pack_rows()`.

## [0.4.3] - 2026-02-12

### Fixed
- Padronizacao da exibicao de media e erro-padrao nas tabelas de medias para usar o simbolo `±` em vez de `+/-`.

## [0.4.2] - 2026-02-12

### Fixed
- Inclusao de `multcompView` em `Imports` no `DESCRIPTION` para garantir instalacao da dependencia requerida por comparacoes de medias/CLD.

## [0.4.1] - 2026-02-11

### Fixed
- Ajustes de codificacao e documentacao para validacao formal de pacote.
- Geracao de documentacao via roxygen2 com man/ e NAMESPACE consistentes.
- Correcoes para R CMD check (namespace, NSE e portabilidade de codigo).

## [0.4.0] - 2026-02-11

### Changed
- Migracao do projeto de scripts para pacote R valido (`rlibfatorial`).
- Escopo inicial focado em analise fatorial (DIC e DBC).
- Remocao dos scripts `R/01_pacotes.R` e `R/02_funcoes_anova.R` do fluxo principal.

### Added
- Estrutura formal de pacote: `DESCRIPTION`, `NAMESPACE`, `LICENSE`, `.Rbuildignore`.
- Funcao `configurar_ambiente_rlib()` para setup opcional do ambiente.
- Documentacao roxygen para API publica.

### Notes
- API central mantida em modelo fatorial, diagnostico, tabelas, graficos e utilitarios.

## [0.3.0] - 2026-02-10

### Added
- Novo argumento `formato` em `anova_fatorial_qm_tabela()` com tres opcoes de saida.
- `qm_star`: quadrado medio com asteriscos de significancia (padrao).
- `f_p_colunas`: duas colunas por variavel resposta (`F` e `p`).
- `f_p_inline`: uma coluna por variavel no formato `F (p)`.
- Cabecalhos e nota de rodape dinamicos conforme o formato selecionado.
- Formatacao consistente de p-valor, incluindo limiar minimo (`< 0.0001`).

## [0.1.0] - 2026-02-04

### Added
- Funcoes genericas para ANOVA fatorial (DIC e DBC)
- Ajuste automatico de modelos com 1 ou mais fatores
- Tabela de ANOVA com quadrados medios e significancia
- Calculo automatico do CV (%)
- Testes de comparacao de medias (t ou Tukey)
- Medias ajustadas com erro-padrao e letras (CLD)
- Desdobramento completo de interacao fatorial
- Suporte a multiplas variaveis resposta
- Opcao de erro-padrao do modelo ou descritivo

### Notes
- Codigo preparado para uso via `source()`
- Pensado para integracao com R Markdown, Quarto e Shiny

