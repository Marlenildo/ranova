# ranova

Pacote R para analise de experimentos fatoriais em DIC e DBC.

## Visao geral

O `ranova` organiza um fluxo unico para:

- ajuste de modelo fatorial
- ANOVA para uma ou varias respostas
- diagnostico de pressupostos
- comparacao de medias (t ou Tukey)
- tabelas para relatorio
- graficos de medias e interacao

Escopo atual:

- DIC e DBC
- 1, 2, 3 ou mais fatores
- parcelas subdivididas em DIC e DBC, com fatorial na parcela ou na subparcela
- parcelas subsubdivididas em DIC e DBC
- testes de medias: Tukey, t (LSD), Bonferroni, Duncan, SNK, Scott-Knott e Dunnett
- numero livre de niveis por fator

## Instalacao

### GitHub

```r
install.packages("remotes")
remotes::install_github("Marlenildo/ranova")
```

### Local (tar.gz)

```r
install.packages("ranova_0.4.1.tar.gz", repos = NULL, type = "source")
```

### Desenvolvimento

```r
install.packages(c("devtools", "roxygen2"))
devtools::load_all(".")
```

## Dependencias principais

- `dplyr`, `tidyr`, `purrr`, `tibble`
- `ggplot2`, `ggpubr`
- `emmeans`, `multcomp`, `car`, `rstatix`
- `knitr`, `kableExtra`, `DT`, `htmltools`

## Inicio rapido

```r
library(ranova)

# Opcional: ajustes globais de ambiente
configurar_ambiente_rlib()
```

## Estrutura minima de dados

Seu `data.frame` precisa ter:

- uma coluna para cada fator
- opcionalmente uma coluna de bloco (para DBC)
- uma ou mais variaveis resposta numericas

Exemplo de nomes:

- `bloco`
- `dose`, `hid`, `cultivar`
- `ci`, `gs`, `mvr`

## Exemplo de uso (DBC)

```r
# ANOVA para varias respostas
anova_fatorial_qm_tabela(
  dados = dados,
  variaveis = c("ci", "gs", "mvr"),
  bloco = "bloco",
  fatores = c("dose", "hid"),
  formato = "qm_star" # qm_star, f_p_colunas, f_p_inline
)

# Diagnostico de pressupostos
anova_diagnostico(
  dados = dados,
  variaveis = c("ci", "gs", "mvr"),
  bloco = "bloco",
  fatores = c("dose", "hid"),
  mostrar_graficos = TRUE
)

# Tabela de medias com letras
Tabela <- tabela_medias_fatorial(
  dados = dados,
  variaveis = c("ci", "gs", "mvr"),
  fator_interesse = "hid",
  bloco = "bloco",
  fatores = c("dose", "hid"),
  tipo_se = "modelo" # modelo ou descritivo
)
```

## Exemplo de uso (DIC)

```r
anova_fatorial_qm_tabela(
  dados = dados,
  variaveis = c("ci", "gs"),
  bloco = NULL,
  fatores = c("dose", "hid")
)
```

## Parcelas subdivididas e outros testes de medias

```r
# Fator 1 na parcela, fator 2 na subparcela (DBC)
ajuste <- ranova_ajuste(dados, "prod", c("irrigacao", "cultivar"), "PSDBC", bloco = "bloco")
ranova_anova(ajuste)              # erro (a), erro (b), CV a e CV b

# Medias com letras pelo teste escolhido
ranova_medias(ajuste, "irrigacao", teste = "tukey")                     # erro (a)
ranova_medias(ajuste, "cultivar", dentro = "irrigacao", teste = "scott-knott")  # erro (b)
ranova_medias(ajuste, "irrigacao", dentro = "cultivar", teste = "duncan")      # erro combinado (Satterthwaite)

# No DIC, informe a coluna que identifica a parcela (repeticao)
ranova_ajuste(dados, "prod", c("irrigacao", "cultivar"), "PSDIC", repeticao = "rep")

# Fatorial na parcela (A x B na parcela, C na subparcela)
ranova_ajuste(dados, "prod", c("A", "B", "C"), "PSDBC", bloco = "bloco", estratos = c(1, 1, 2))

# Parcelas subsubdivididas (A parcela, B subparcela, C subsubparcela): erros (a), (b) e (c)
ranova_ajuste(dados, "prod", c("A", "B", "C"), "PSSDBC", bloco = "bloco")

# Testes disponiveis
TESTES_MEDIAS
letras_teste(c(A = 10, B = 12, C = 15), n = 4, qm = 2, gl = 12, teste = "snk")
```

Os resultados foram conferidos com `agricolae` (Tukey, t, Bonferroni, Duncan, SNK e parcelas subsubdivididas), `ExpDes.pt` (Scott-Knott e parcelas subdivididas, inclusive o erro combinado) e `aov(... + Error())` (fatorial na parcela e na subparcela).

## Interacao fatorial

```r
# Uma variavel resposta
tabela_interacao_fatorial(
  dados = dados,
  resposta = "ci",
  fator_linha = "dose",
  fator_coluna = "hid",
  bloco = "bloco",
  fatores = c("dose", "hid")
)

# Varias variaveis resposta
tabela_interacao_fatorial_multivariaveis(
  dados = dados,
  variaveis = c("ci", "gs"),
  fator_linha = "dose",
  fator_coluna = "hid",
  bloco = "bloco",
  fatores = c("dose", "hid")
)
```

## Graficos

```r
grafico_medias_fatorial(
  dados = dados,
  resposta = "ci",
  fator_interesse = "hid",
  bloco = "bloco",
  fatores = c("dose", "hid")
)

grafico_interacao_fatorial(
  dados = dados,
  resposta = "ci",
  fator_x = "dose",
  fator_traco = "hid",
  bloco = "bloco",
  fatores = c("dose", "hid")
)
```

## Aplicativo Shiny

O aplicativo **Ranova** usa este pacote para fazer as analises sem programar
(digitar ou importar dados, ANOVA, medias, desdobramento, graficos e relatorio).
Ele vive em repositorio proprio: [Marlenildo/ranova-app](https://github.com/Marlenildo/ranova-app).

## Dicionario de variaveis (opcional)

Para usar rotulos amigaveis nas tabelas/graficos:

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

Passe `dic_vars` e `label_type` nas funcoes que suportam isso.
Opcoes de `label_type`: `"var"`, `"sigla"`, `"label"` ou `"description"`.

## API principal

### Modelagem e inferencia

- `ranova_ajuste()`, `ranova_anova()`, `ranova_medias()`
- `letras_teste()`, `TESTES_MEDIAS`
- `ajusta_modelo_fatorial()`
- `anova_fatorial_qm_tabela()`
- `medias_fatorial_cld()`

### Diagnostico

- `anova_diagnostico()`

### Tabelas

- `tabela_medias_fatorial()`
- `tabela_interacao_fatorial()`
- `tabela_interacao_fatorial_multivariaveis()`
- `tabela_dt_exportavel()`

### Graficos

- `grafico_media_ep()`
- `grafico_multiplas_variaveis()`
- `grafico_medias_fatorial()`
- `grafico_interacao_fatorial()`

### Utilitarios

- `remove_colunas_com_na()`
- `seleciona_colunas_com_na()`
- `remove_colunas_todas_na()`
- `colunas_com_na()`
- `manter_colunas_fixas_e_com_na()`

## Fluxo recomendado no projeto

1. Organizar dados e fatores no script local.
2. Ajustar/avaliar ANOVA com `anova_fatorial_qm_tabela()`.
3. Verificar pressupostos com `anova_diagnostico()`.
4. Gerar tabelas de medias e interacao.
5. Gerar graficos finais para relatorio.

## Qualidade

Validacao local executada:

- `R CMD build`
- `R CMD check --no-manual`

Status mais recente: `OK`.

## Roadmap

Proxima frente prevista:

- parcelas subdivididas em DIC e DBC, com fatorial na parcela ou na subparcela
- parcelas subsubdivididas em DIC e DBC, mantendo o mesmo padrao de API.

## Autor

Marlenildo Ferreira Melo

## Licenca

MIT (`LICENSE`).


