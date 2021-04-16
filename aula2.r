prod2 <- outer(0:9, 0:9)
frdet <- table(outer(prod2, prod2, FUN = "-"))
plot(frdet, xlab = "det", ylab = "freq", col = "blue")
