# VaR y CVaR -------------------------------------------------------------------

library(quantmod)
library(PerformanceAnalytics)



# Obtencion de datos con Rendimientos Logaritmicos -----------------------------

# Funcion para bajar precios y generar rendimientos:
getlogret <- function(symbol, start, end) {
    # obtenemos los datos de YahooFinance
    stock <- getSymbols(symbol,
                        src = "yahoo",
                        periodicity = "daily",
                        from = start,
                        to = end,
                        auto.assign = FALSE)
    # mantenemos la columna con Precios de Cierre
    stock <- Cl(stock)
    # eliminamos valores faltantes
    stock <- na.omit(stock)
    # rendimientos logaritmicos   ln(P1/P0)
    # r <- CalculateReturns(stock, method = "log")
    r <- periodReturn(stock,
                      period = "daily",
                      type = "log",
                      subset = paste0(c(start, end), sep = "::"))
    # renombramos la columna
    colnames(r) <- paste0(symbol, ".LogReturn")
    # hacemos los datos accesibles
    assign(symbol, r, envir = .GlobalEnv)
}

# Llamar la funcion para cada activo particular:
start <- as.Date("2007-01-01", format = "%Y-%m-%d")
end <- as.Date("2021-06-30", format = "%Y-%m-%d")
getlogret("AAPL", start, end)

# Grafico e Histograma de rendimientos logaritmicos:
par(mfrow = c(2, 1))
plot(AAPL, type = "l", lwd = 1, col = "blue",
     main = colnames(AAPL), ylab = "LogReturn")
chart.Histogram(AAPL, methods = c("add.normal", "add.density"), colorset = c("gray", "blue", "red"))
legend("topright", legend = c("hist AAPL", "dist AAPL", "dnorm AAPL"),
       col = c("gray", "blue", "red"), lty = "solid", cex = 0.7)
par(mfrow = c(1, 1))



# VaR y CVaR historico (empirico o no parametrico) -----------------------------

# alpha=95% (nivel de confianza) --> 1-alpha=5% (nivel de significancia)

# VaR_95% por cuantiles de la distribucion empirica
# perdida maxima esperada al 95% de las veces
hVaR_95 <- as.numeric(quantile(AAPL, probs = 0.05))

# CVaR_95%
# promedio de las mayores perdidas esperadas al 95% de las veces
hCVaR_95 <- mean(AAPL[AAPL <= hVaR_95])



# VaR y CVaR Gausiano (Normal o parametrico) -----------------------------------

# z = (x - mu) / sigma --> x = mu + sigma * z
# F(z) = P[X<z] = 5% --> z = F^-1(5%)
# Z = f(z) / 5%

# VaR_95% gausiano
z <- qnorm(0.05)
gVaR_95 <- mean(AAPL) + sd(AAPL) * z

# CVaR_95% gausiano
Z <- dnorm(z) / 0.05
gCVaR_95 <- mean(AAPL) - sd(AAPL) * Z



# VaR y CVaR Modificado (Cornish-Fisher) ---------------------------------------

# VaR modificado
s <- skewness(AAPL)
k <- kurtosis(AAPL)
w <- z + (z ^ 2 - 1) * s / 6 + (z ^ 3 - 3 * z) * k / 24 - (2 * z ^ 3 - 5 * z) * s ^ 2 / 36
mVaR_95 <- mean(AAPL) + sd(AAPL) * w

# CVaR modificado
W <- (1 + z * s / 6 + (z ^ 2 - 1) * k / 24 + (1 - 2 * z ^ 2) * s ^ 2 / 36) * Z
mCVaR_95 <- mean(AAPL) - sd(AAPL) * W


# Visualizamos todo en un histograma
VaRs <- c(gVaR_95, hVaR_95, mVaR_95, gCVaR_95, hCVaR_95, mCVaR_95)
cols <- c("#C05ABE", "#5D5CC2", "#2A8E5A", "#F4DE00", "#F98923", "#FD0100")
leg <- c("gaussianVaR", "historicalVaR", "modifiedVaR", "gaussianCVaR", "historicalCVaR", "modifiedCVaR")
hist(AAPL, breaks = 150, freq = TRUE, col = "#D9D9D9", border = "#808080",
     main = colnames(AAPL), xlab = "LogReturns")
abline(v = VaRs, col = cols, lwd = 2)
legend("topleft", title = "alpha = 95%", legend = paste(leg, "=", round(VaRs, 4)),
       col = cols, lty = "solid", lwd = 2, cex = 0.8)

# Resumir VaR y CVaR
VaR_CVaR <- function(x, p = 0.95) {
    y <- c("gaussianVaR" = VaR(R = x, p = p, method = "gaussian"),
           "historicalVaR" = VaR(R = x, p = p, method = "historical"),
           "modifiedVaR" = VaR(R = x, p = p, method = "modified"),
           "gaussianCVaR" = ES(R = x, p = p, method = "gaussian"), #CVaR()
           "historicalCVaR" = ES(R = x, p = p, method = "historical"),
           "modifiedCVaR" = ES(R = x, p = p, method = "modified"))
    return(y)
}
VaR_CVaR(AAPL, p = 0.95)



# Backtesting del VaR y CVaR ---------------------------------------------------

# Ventana de estimacion
r <- rollapply(AAPL, width = 500, FUN = VaR_CVaR, p = 0.95, by.column = FALSE, align = "right")
chart.TimeSeries(merge.xts(AAPL,r), legend.loc = "bottom", colorset = c("black", cols))
# el CVaR modificado es el metdodo mas robusto para determinar las perdidas

# Podemos ver cuantas veces la perdida supero el VaR (violaciones al VaR)
chart.TimeSeries(merge.xts(AAPL,r[, 1:3]),
                 colorset = c("black", cols[1:3]), main = names(AAPL))
lines(AAPL[AAPL <= r[, 1]],  type = "p", pch = 15, col = cols[1], lwd = 3)
lines(AAPL[AAPL <= r[, 2]],  type = "p", pch = 0, col = cols[2], lwd=2)
lines(AAPL[AAPL <= r[, 3]],  type = "p", pch = 4, col = cols[3], lwd=1)
addLegend("bottom", title = "VaR violations", legend.names = paste("losses >", leg[1:3]),
          lty = "solid", pch = c(15, 0, 4), col = cols[1:3])
