# Valores de referencia conferidos com agricolae (HSD.test, duncan.test,
# SNK.test, LSD.test) e ExpDes.pt (Scott-Knott) para o mesmo conjunto de dados.
dados_dbc <- function() {
  set.seed(42)
  d <- expand.grid(bloco = factor(1:4), trat = factor(paste0("T", 1:6)))
  d$y <- 20 + c(0, 1.5, 1.8, 3, 5, 5.3)[as.numeric(d$trat)] + as.numeric(d$bloco) * 0.5 + stats::rnorm(nrow(d), 0, 1)
  d
}

test_that("testes de medias reproduzem as referencias", {
  aj <- ranova_ajuste(dados_dbc(), "y", "trat", "DBC", bloco = "bloco")
  grupos <- function(teste) ranova_medias(aj, "trat", teste = teste)$grupo
  expect_equal(grupos("tukey"), c("c", "bc", "ab", "b", "ab", "a"))
  expect_equal(grupos("duncan"), c("d", "c", "bc", "bc", "ab", "a"))
  expect_equal(grupos("snk"), c("d", "c", "bc", "bc", "ab", "a"))
  expect_equal(grupos("t"), c("d", "c", "bc", "bc", "ab", "a"))
  expect_equal(grupos("bonferroni"), c("c", "bc", "ab", "bc", "ab", "a"))
  expect_equal(grupos("scott-knott"), c("d", "c", "b", "b", "a", "a"))
  expect_equal(grupos("dunnett"), c("", "", "*", "*", "*", "*"))
})

test_that("parcelas subdivididas reproduzem o ExpDes.pt", {
  set.seed(7)
  p <- expand.grid(bloco = factor(1:4), A = factor(c("a1", "a2", "a3")), B = factor(c("b1", "b2", "b3", "b4")))
  ef <- stats::rnorm(12, 0, 1.2)
  names(ef) <- levels(interaction(p$bloco, p$A))
  p$y <- 30 + 2 * as.numeric(p$A) + 1.5 * as.numeric(p$B) + (p$A == "a3") * (p$B == "b4") * 3 +
    ef[as.character(interaction(p$bloco, p$A))] + stats::rnorm(nrow(p), 0, 1)

  aj <- ranova_ajuste(p, "y", c("A", "B"), "PSDBC", bloco = "bloco")
  tab <- ranova_anova(aj)
  expect_equal(tab$FV, c("bloco", "A", "Erro (a)", "B", "A:B", "Erro (b)"))
  expect_equal(round(tab$F[tab$FV == "A"], 4), 9.8222)
  expect_equal(round(unname(attr(tab, "cv")), 4), c(10.2180, 2.1307))

  a_em_b <- ranova_medias(aj, "A", dentro = "B", teste = "tukey")
  expect_equal(round(attr(a_em_b, "qm"), 5), 4.39538)
  expect_equal(round(attr(a_em_b, "gl"), 4), 7.6386)
  expect_equal(a_em_b$grupo[a_em_b$dentro == "b2"], c("b", "ab", "a"))

  b_em_a <- ranova_medias(aj, "B", dentro = "A", teste = "tukey")
  expect_equal(b_em_a$grupo[b_em_a$dentro == "a3"], c("c", "c", "b", "a"))

  names(p)[1] <- "rep"
  aj_dic <- ranova_ajuste(p, "y", c("A", "B"), "PSDIC", repeticao = "rep")
  expect_equal(round(ranova_anova(aj_dic)$F[1], 3), 14.052)
})

dados_tres_fatores <- function() {
  set.seed(11)
  d <- expand.grid(bloco = factor(1:4), A = factor(c("a1", "a2")), B = factor(c("b1", "b2", "b3")), C = factor(c("c1", "c2", "c3")))
  ea <- stats::rnorm(24, 0, 1.5)
  names(ea) <- levels(interaction(d$bloco, d$A, d$B))
  invisible(stats::rnorm(72, 0, 0.8)) # mesmo sorteio do script de conferência
  d$y <- 20 + 2 * as.numeric(d$A) + as.numeric(d$B) + 1.5 * as.numeric(d$C) + (d$A == "a2") * (d$C == "c3") * 2 +
    ea[as.character(interaction(d$bloco, d$A, d$B))] + stats::rnorm(nrow(d), 0, 1)
  d
}

test_that("fatorial na parcela reproduz aov com Error()", {
  aj <- ranova_ajuste(dados_tres_fatores(), "y", c("A", "B", "C"), "PSDBC", bloco = "bloco", estratos = c(1, 1, 2))
  tab <- ranova_anova(aj)
  expect_equal(tab$FV, c("bloco", "A", "B", "A:B", "Erro (a)", "C", "A:C", "B:C", "A:B:C", "Erro (b)"))
  expect_equal(round(tab$F[tab$FV == "A"], 3), 66.117)
  expect_equal(round(tab$F[tab$FV == "A:C"], 3), 5.346)
})

test_that("parcelas subsubdivididas reproduzem agricolae::ssp.plot", {
  aj <- ranova_ajuste(dados_tres_fatores(), "y", c("A", "B", "C"), "PSSDBC", bloco = "bloco")
  tab <- ranova_anova(aj)
  expect_equal(tab$GL[tab$FV %in% c("Erro (a)", "Erro (b)", "Erro (c)")], c(3, 12, 36))
  expect_equal(round(tab$F[tab$FV == "A"], 4), 45.4758)
  expect_equal(round(tab$F[tab$FV == "B"], 4), 3.7095)
  expect_equal(round(unname(attr(tab, "cv")), 1), c(7.5, 5.8, 3.2))
  a_em_c <- ranova_medias(aj, "A", dentro = "C")
  expect_equal(round(attr(a_em_c, "qm"), 4), 1.9622)
  expect_equal(round(attr(a_em_c, "gl"), 3), 5.472)
})

test_that("fatorial na subparcela em DIC reproduz aov com Error()", {
  d <- dados_tres_fatores()
  names(d)[1] <- "rep"
  aj <- ranova_ajuste(d, "y", c("A", "B", "C"), "PSDIC", repeticao = "rep", estratos = c(1, 2, 2))
  tab <- ranova_anova(aj)
  expect_equal(round(tab$F[tab$FV == "A"], 2), 69.65)
  expect_equal(tab$GL[tab$FV == "Erro (b)"], 48)
})
