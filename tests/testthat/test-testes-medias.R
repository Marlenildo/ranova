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
