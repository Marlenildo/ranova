# ranova

Pacote R para análise de variância de experimentos agrícolas e biológicos em
DIC e DBC: experimentos simples, em esquema fatorial, em parcelas subdivididas
e em parcelas subsubdivididas.

## Visão geral

O `ranova` organiza um fluxo único para:

- ajuste do modelo com o erro correto para cada teste F;
- ANOVA para uma ou várias respostas, com CV;
- diagnóstico de pressupostos (Shapiro-Wilk e Levene);
- comparação de médias por Tukey, t (LSD), Bonferroni, Duncan, SNK,
  Scott-Knott ou Dunnett;
- desdobramento de interações;
- tabelas e gráficos para relatórios.

## Nomenclatura dos experimentos

| Situação | Nome | Delineamento no pacote |
|---|---|---|
| 1 fator | Experimento simples | `"DIC"` ou `"DBC"` |
| 2 ou 3 fatores sorteados juntos | Esquema fatorial | `"DIC"` ou `"DBC"` |
| 1 fator nas parcelas e 1 nas subparcelas | Parcelas subdivididas | `"PSDIC"` ou `"PSDBC"` |
| 2 fatores combinados nas parcelas e 1 nas subparcelas | Parcelas subdivididas com esquema fatorial nas parcelas | `"PSDIC"`/`"PSDBC"` com `estratos = c(1, 1, 2)` |
| 1 fator nas parcelas e 2 combinados nas subparcelas | Parcelas subdivididas com esquema fatorial nas subparcelas | `"PSDIC"`/`"PSDBC"` com `estratos = c(1, 2, 2)` |
| 1 fator em cada nível (parcela, subparcela, subsubparcela) | Parcelas subsubdivididas | `"PSSDIC"` ou `"PSSDBC"` |

Um fator nas parcelas e outro nas subparcelas **não** é um esquema fatorial:
os fatores são sorteados em etapas e cada um tem o seu erro. O termo "esquema
fatorial nas parcelas" (ou "nas subparcelas") só se aplica quando dois fatores
são combinados naquele nível.

## Instalação

```r
install.packages("remotes")
remotes::install_github("Marlenildo/ranova")
```

Para desenvolvimento:

```r
install.packages(c("devtools", "roxygen2"))
devtools::load_all(".")
```

## Dependências principais

- `dplyr`, `tidyr`, `purrr`, `tibble`
- `ggplot2`, `ggpubr`
- `emmeans`, `multcomp`, `multcompView`, `mvtnorm`, `car`, `rstatix`
- `knitr`, `kableExtra`, `DT`, `htmltools`

## Estrutura dos dados

Um `data.frame` com:

- uma coluna para cada fator;
- uma coluna de bloco (DBC) ou, nas parcelas em DIC, uma coluna de repetição;
- uma ou mais variáveis resposta numéricas.

## Motor principal: `ranova_ajuste()`

```r
library(ranova)

# Experimento simples ou esquema fatorial em DBC
ajuste <- ranova_ajuste(dados, "prod", c("dose", "cultivar"), "DBC", bloco = "bloco")
ranova_anova(ajuste)                                   # FV, GL, SQ, QM, F, p e CV
ranova_medias(ajuste, "dose", teste = "tukey")         # médias com letras
ranova_medias(ajuste, "dose", dentro = "cultivar")     # desdobramento

# Parcelas subdivididas: irrigação nas parcelas, cultivar nas subparcelas (DBC)
ajuste <- ranova_ajuste(dados, "prod", c("irrigacao", "cultivar"), "PSDBC", bloco = "bloco")
ranova_anova(ajuste)                                                            # erros (a) e (b), CV a e CV b
ranova_medias(ajuste, "irrigacao", teste = "tukey")                             # erro (a)
ranova_medias(ajuste, "cultivar", dentro = "irrigacao", teste = "scott-knott")  # erro (b)
ranova_medias(ajuste, "irrigacao", dentro = "cultivar", teste = "duncan")       # erro combinado (Satterthwaite)

# Nas parcelas em DIC, informe a coluna que identifica a parcela (repetição)
ranova_ajuste(dados, "prod", c("irrigacao", "cultivar"), "PSDIC", repeticao = "rep")

# Esquema fatorial nas parcelas (A x B nas parcelas, C nas subparcelas)
ranova_ajuste(dados, "prod", c("A", "B", "C"), "PSDBC", bloco = "bloco", estratos = c(1, 1, 2))

# Esquema fatorial nas subparcelas (A nas parcelas, B x C nas subparcelas)
ranova_ajuste(dados, "prod", c("A", "B", "C"), "PSDBC", bloco = "bloco", estratos = c(1, 2, 2))

# Parcelas subsubdivididas (A, B e C): erros (a), (b) e (c)
ranova_ajuste(dados, "prod", c("A", "B", "C"), "PSSDBC", bloco = "bloco")

# Testes de médias disponíveis
TESTES_MEDIAS
letras_teste(c(A = 10, B = 12, C = 15), n = 4, qm = 2, gl = 12, teste = "snk")
```

Os resultados foram conferidos com `agricolae` (Tukey, t, Bonferroni, Duncan,
SNK e parcelas subsubdivididas), `ExpDes.pt` (Scott-Knott e parcelas
subdivididas, inclusive o erro combinado) e `aov(... + Error())` (esquema
fatorial nas parcelas e nas subparcelas).

## Funções de tabelas e gráficos (DIC e DBC)

As funções abaixo trabalham com experimentos simples e em esquema fatorial em
DIC ou DBC e devolvem tabelas `kable` e gráficos `ggplot2` prontos para
relatório. Os nomes com `fatorial` são mantidos por compatibilidade.

```r
# ANOVA para várias respostas
anova_fatorial_qm_tabela(
  dados = dados,
  variaveis = c("ci", "gs", "mvr"),
  bloco = "bloco",               # NULL para DIC
  fatores = c("dose", "hid"),
  formato = "qm_star"            # qm_star, f_p_colunas ou f_p_inline
)

# Diagnóstico de pressupostos
anova_diagnostico(
  dados = dados,
  variaveis = c("ci", "gs", "mvr"),
  bloco = "bloco",
  fatores = c("dose", "hid"),
  mostrar_graficos = TRUE
)

# Tabela de médias com letras
tabela_medias_fatorial(
  dados = dados,
  variaveis = c("ci", "gs", "mvr"),
  fator_interesse = "hid",
  bloco = "bloco",
  fatores = c("dose", "hid"),
  tipo_se = "modelo"             # modelo ou descritivo
)

# Desdobramento da interação (uma ou várias respostas)
tabela_interacao_fatorial(
  dados = dados, resposta = "ci",
  fator_linha = "dose", fator_coluna = "hid",
  bloco = "bloco", fatores = c("dose", "hid")
)
tabela_interacao_fatorial_multivariaveis(
  dados = dados, variaveis = c("ci", "gs"),
  fator_linha = "dose", fator_coluna = "hid",
  bloco = "bloco", fatores = c("dose", "hid")
)

# Gráficos
grafico_medias_fatorial(
  dados = dados, resposta = "ci", fator_interesse = "hid",
  bloco = "bloco", fatores = c("dose", "hid")
)
grafico_interacao_fatorial(
  dados = dados, resposta = "ci", fator_x = "dose", fator_traco = "hid",
  bloco = "bloco", fatores = c("dose", "hid")
)
```

## Aplicativo Shiny

O aplicativo **Ranova** usa este pacote para fazer as análises sem programar:
digitar, colar ou importar os dados, ANOVA, pressupostos, médias,
desdobramentos, gráficos e relatórios em PDF e HTML. Ele vive em repositório
próprio: [Marlenildo/ranova-app](https://github.com/Marlenildo/ranova-app).

## Dicionário de variáveis (opcional)

Para usar rótulos amigáveis nas tabelas e nos gráficos:

```r
dic_vars <- tibble::tribble(
  ~var,    ~sigla,    ~label,                                   ~description,
  "ce",    "CE",      "CE (dS m⁻¹)",                            "Condutividade eletrica (dS m⁻¹)",
  "as",    "AS",      "AS (mM)",                                "Acido salicilico (mM)",
  "af",    "AF",      "AF (cm²)",                               "Area foliar (cm²)",
  "cla",   "Cla",     "Cla (CFI)",                              "Clorofila a (CFI)",
  "clb",   "Clb",     "Clb (CFI)",                              "Clorofila b (CFI)",
  "clab",  "Cla/Clb", "Cla/Clb (CFI)",                          "Razao clorofila a/b (CFI)",
  "clt",   "Clt",     "Clt (CFI)",                              "Clorofila total (CFI)",
  "cpa",   "CPA",     "CPA (cm)",                               "Comprimento da parte aerea (cm)",
  "cr",    "CR",      "CR (cm)",                                "Comprimento de raiz (cm)",
  "cra",   "CRA",     "CRA (%)",                                "Conteudo relativo de agua (%)",
  "ee",    "EE",      "EE (%)",                                 "Extravasamento de eletrolitos (%)",
  "dns",   "DNS",     "DNS (mm)",                               "Diametro ao nivel do solo (mm)",
  "f0",    "F₀",      "F₀",                                     "Fluorescencia inicial (F₀)",
  "fm",    "Fm",      "Fm",                                     "Fluorescencia maxima (Fm)",
  "fv_fm", "Fv/Fm",   "Fv/Fm",                                  "Eficiencia fotoquimica maxima (Fv/Fm)",
  "iqd",   "IQD",     "IQD",                                    "Indice de qualidade de Dickson",
  "msc",   "MSC",     "MSC (g)",                                "Massa seca do caule (g)",
  "msf",   "MSF",     "MSF (g)",                                "Massa seca da folha (g)",
  "msr",   "MSR",     "MSR (g)",                                "Massa seca da raiz (g)",
  "mst",   "MST",     "MST (g)",                                "Massa seca total (g)",
  "mspa",  "MSPA",    "MSPA (g)",                               "Massa seca da parte aerea (g)",
  "nf",    "NF",      "NF",                                     "Numero de folhas",
  "nff",   "NFF",     "NFF",                                    "Numero de foliolos",
  "np",    "NP",      "NP",                                     "Numero de pinas",
  "vr",    "VR",      "VR (cm³)",                               "Volume de raiz (cm³)"
)
```

Passe `dic_vars` e `label_type` nas funções que aceitam esses argumentos.
Opções de `label_type`: `"var"`, `"sigla"`, `"label"` ou `"description"`.

## API principal

### Modelagem e inferência

- `ranova_ajuste()`, `ranova_anova()`, `ranova_medias()`
- `letras_teste()`, `TESTES_MEDIAS`
- `ajusta_modelo_fatorial()`, `anova_fatorial_qm_tabela()`, `medias_fatorial_cld()`

### Diagnóstico

- `anova_diagnostico()`

### Tabelas

- `tabela_medias_fatorial()`
- `tabela_interacao_fatorial()`
- `tabela_interacao_fatorial_multivariaveis()`
- `tabela_dt_exportavel()`

### Gráficos

- `grafico_media_ep()`
- `grafico_multiplas_variaveis()`
- `grafico_medias_fatorial()`
- `grafico_interacao_fatorial()`

### Utilitários

- `resolve_var_label()`
- `remove_colunas_com_na()`, `remove_colunas_todas_na()`
- `seleciona_colunas_com_na()`, `colunas_com_na()`
- `manter_colunas_fixas_e_com_na()`
- `configurar_ambiente_rlib()`

## Qualidade

- Testes automatizados com `testthat`, com valores de referência de
  `agricolae`, `ExpDes.pt` e `aov(... + Error())`.
- `R CMD check --no-manual` sem erros nem avisos.

## Autor

Marlenildo Ferreira Melo

## Licença

MIT (`LICENSE`).
