knitr::opts_chunk$set(echo = TRUE)
set.seed(2021)

library(tidyverse)
library(rBeta2009)
library(kableExtra)
library(latex2exp)
library(magrittr)
library(pwr)

gx <- function(x) {
  return(0.5 * exp(-abs(x)))
}

fx <- function(x) {
  return(exp(-(abs(x) ^ 3) / 3))
}

fgx <- function(x) {
  return(fx(x) / gx(x))
}

m <- fgx(-1)

m_gx <- function(x, m) {
  return(m * gx(x))
}

par(mfrow = c(2, 2))

curve(fx, -5, 5, xlab = "x", ylab = "f(x)",
      col = "darkgrey", lwd = 2, main = "Gráfico de f(x)")

curve(gx, -5, 5, xlab = "y", ylab = "g(y)",
      col = "darkgrey", lwd = 2, main = "Gráfico de g(y)")

curve(fgx, -4, 4, ylab = "f(x)/g(x)", main = "Gráfico de f(x)/g(x)")
points(c(-1, 1), c(m, m), pch = 20, col = "blue")
abline(h = m, lty = 2, col = "blue")
segments(c(-1, 1), c(0, 0), c(-1, 1), c(m, m), lty = 2, col = "blue")

curve(m_gx(x, m), -5, 5, col = "red", ylab = "f(x) e m*g(x)",
      main = "Gráfico de f(x) e mg(x)")
curve(fx(x), add = TRUE)
legend("topright", c("f(x)", "m*g(x)"), col = c("black", "red"),
       lty = 1, bty = "n")

gerador <- function(n) {
  nger <- n0 <- 0
  ax <- c()
  while (n0 < n) {
    rej <- TRUE
    while (rej) {
      nger <- nger + 1
      u <- runif(1)
      y <- ifelse(u <= 0.5, log(2 * u), - log(2 * (1 - u)))

      if (m * runif(1) <= fgx(y)) {
        n0 <- n0 + 1
        ax[n0] <- y
        rej <- FALSE
      }
    }
  }

  result <- list(sample = ax, n_gerado = nger)
  return(result)

}

amostra_gerada_50 <- gerador(50)
ax_50 <- amostra_gerada_50$sample

amostra_gerada_100 <- gerador(100)
ax_100 <- amostra_gerada_100$sample

amostra_gerada_400 <- gerador(400)
ax_400 <- amostra_gerada_400$sample

par(mfrow = c(2, 2))
hist(ax_50, freq = FALSE, main = "Histograma da amostra n = 50",
     xlab = "x", ylab = "Densidade",
     col = "orange", border = "brown")
hist(ax_100, freq = FALSE, main = "Histograma da amostra n = 100",
     xlab = "x", ylab = "Densidade",
     col = "orange", border = "brown")
hist(ax_400, freq = FALSE, main = "Histograma da amostra n = 400",
     xlab = "x", ylab = "Densidade",
     col = "orange", border = "brown")

amostra <- c(ax_50, ax_100, ax_400)
tamanho <- c(rep(50, length(ax_50)),
             rep(100, length(ax_100)),
             rep(400, length(ax_400)))

df <- data.frame(amostra, tamanho)

boxplot(amostra ~ tamanho,
        data = df,
        main = "Diferentes Boxplots para\ncada tamanho de amostra",
        xlab = "Tamanho da amostra",
        ylab = "Observações",
        col = "orange",
        border = "brown"
)

r <- 1000
lambda <- 2
po_sample <- function(r, lambda, n) {

  pvalue <- c()
  aceptance <- c()
  medias_v <- c()
  poder_dif <- seq(2.2, 4, length.out = 5) - 2
  power_1 <- c()


  for (i in 1:r) {

    sample_p <- rpois(n, lambda)
    media <- mean(sample_p)

    p_value <- pnorm(media, mean = lambda, sd = sqrt(1 / n * lambda))
    pvalue[i] <- p_value

    medias_v[i] <- media

    if (pvalue[i] >= 0.05) {
      aceptance[i] <- 1
    }
    else {
      aceptance[i] <- 0
    }
  }

  poder_1 <- power.t.test(n = n, delta = poder_dif[1],
                          sd = sqrt(1 / n * lambda),
                          sig.level = .05,
                          alternative = "two.sided",
                          type = "one.sample")
  power_1[1] <- (as.numeric(unlist(poder_1[5])))

  poder_2 <- power.t.test(n = n, delta = poder_dif[2],
                          sd = sqrt(1 / n * lambda),
                          sig.level = .05,
                          alternative = "one.sided",
                          type = "one.sample")
  power_1[2] <- as.numeric(unlist(poder_2[5]))

  poder_3 <- power.t.test(n = n, delta = poder_dif[3],
                          sd = sqrt(1 / n * lambda),
                          sig.level = .05,
                          alternative = "one.sided",
                          type = "one.sample")
  power_1[3] <- as.numeric(unlist(poder_3[5]))

  poder_4 <- power.t.test(n = n, delta = poder_dif[4],
                          sd = sqrt(1 / n * lambda),
                          sig.level = .05,
                          alternative = "one.sided",
                          type = "one.sample")
  power_1[4] <- as.numeric(unlist(poder_4[5]))

  poder_5 <- power.t.test(n = n, delta = poder_dif[5],
                          sd = sqrt(1 / n * lambda),
                          sig.level = .05,
                          alternative = "one.sided",
                          type = "one.sample")
  power_1[5] <- as.numeric(unlist(poder_5[5]))

  results <- list(pvalor = pvalue, aceita = aceptance,
                  media = medias_v, poder = power_1)

  return(results)
}

a <- po_sample(r, lambda, 10)
b <- po_sample(r, lambda, 30)
c <- po_sample(r, lambda, 75)
d <- po_sample(r, lambda, 100)

freqa <- table(a$aceita) * 100 / sum(table(a$aceita))
freqb <- table(b$aceita) * 100 / sum(table(b$aceita))
freqc <- table(c$aceita) * 100 / sum(table(c$aceita))
freqd <- table(d$aceita) * 100 / sum(table(d$aceita))

tabela <- rbind("Amostra n = 10" = freqa, "Amostra n = 30" = freqb,
                "Amostra n = 75" = freqc, "Amostra n = 100" = freqd)
tabela_1 <- as.data.frame(tabela)

tabela_1 %>% mutate_all(linebreak) %>%
knitr::kable(tabela_1, align = "c",
             caption = "Tabela de Erro Tipo I (em %)",
             col.names = c("Rejeita Hipótese Nula",
                           "Não Rejeita Hipótese Nula"),
             format = "latex") %>%
    kable_styling(position = "center")

tabela_2 <- rbind("Amostra n = 10" = a$poder, "Amostra n = 30" = b$poder,
                  "Amostra n = 75" = c$poder, "Amostra n = 100" = d$poder)
p <- seq(2.2, 4, length.out = 5)
df2 <- data.frame(round(tabela_2, 2))

knitr::kable(df2, align = "c",
             caption = "Tabela do Poder do Teste $\\lambda \\in[2,2;4]$",
             col.names = c(paste("$\\lambda =$", p[1]),
                           paste("$\\lambda =$", p[2]),
                           paste("$\\lambda =$", p[3]),
                           paste("$\\lambda =$", p[4]),
                           paste("$\\lambda =$", p[5])),
             format = "latex", escape = F) %>%
kable_styling(position = "center")

library("rmarkdown")
render("Trabalho1/trab1.Rmd", "pdf_document")

update.packages(ask = FALSE, checkBuilt = TRUE)
tinytex::tlmgr_update()
