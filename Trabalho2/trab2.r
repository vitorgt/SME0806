knitr::opts_chunk$set(echo = TRUE)
library(xtable)
library(tinytex)
library(readxl)
library(psych)
options(tinytex.verbose = TRUE)
options(OutDec = ",")

set.seed(31)
df <- read_xlsx("Trabalho2/tab_pib_2018-10-1.xlsx")
df1 <- df[-c(1:10),]
df1 <- na.omit(df1)
df_fim <- setNames(df1, c("Cidade", "Agro", "Ind",
                 "Adm",
                 "TotalSem",
                 "Total", "Impostos", "PIB",
                 "PIBCapita"))
df_fim[, c(2:9)] <- sapply(df_fim[, c(2:9)], as.numeric)



gini <- function(x, y) {
  z <- abs(x - y)
}

ex_1 <- function(df, r) {

  g <- c()
  n <- nrow(df)

  for (i in 1:r) {
    am <- sample(as.matrix(df[, 9]), n, replace = T)
    matriz <- outer(am, am, gini)
    mu <- mean(am)
    g[i] <- sum(matriz) / (2 * (n ^ 2) * mu)
  }

  cat("Estimativa pontual para o Coeficiente Gini = ", round(mean(g), 4),
      "com", r, "repetições", "\n")

  cat("Intervalo de 95% para o Coeficiente Gini:", "[",
      round(quantile(g, .025), 4), ";", round(quantile(g, .975), 4),
      "]", "com", r, "repetições", "\n")
}

ex_1(df = df_fim, r = 500)
ex_1(df = df_fim, r = 1000)
ex_1(df = df_fim, r = 3000)


corr_mat <- cor(df_fim[, c(2:6)], method = "s")
corPlot(corr_mat, cex = 1.2)
title("Mapa de Calor dos Coeficientes Correlações de Spearman")


ex_2 <- function(df, r) {

  g <- c()
  n <- nrow(df)

  for (i in 1:r) {
    index <- sample(c(1:n), n, replace = T)
    am1 <- df$TotalSem[index]
    am2 <- df$Total[index]
    g[i] <- cor(x = am1, y = am2, method = "s")
  }

  cat("Estimativa pontual para o Coeficiente de Correlação de Spearman = ",
      round(mean(g), 4), "com", r, "repetições", "\n")

  cat("Intervalo de 95% para o Coeficiente de Correlação de Spearman:", "[",
      round(quantile(g, .025), 4), ";", round(quantile(g, .975), 4),
      "]", "com", r, "repetições", "\n")
}

ex_2(df = df_fim, r = 500)
ex_2(df = df_fim, r = 1000)
ex_2(df = df_fim, r = 3000)


library("rmarkdown")
render("Trabalho2/trab2.Rmd", "pdf_document")
