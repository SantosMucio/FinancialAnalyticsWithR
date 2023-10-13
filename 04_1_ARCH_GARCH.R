# Modelo de volatilidad condicional heterocedástica tipo GARCH -----------------

library(quantmod) # datos de la web
library(rugarch) # modelos GARCH univariados
library(fBasics) # estadisticas basicas
library(TSA) # analisis de series de tiempo
library(aTSA) # analisis de series de tiempo alternativas
library(forecast) # pronostico de series de tiempo
library(PerformanceAnalytics) # herramientas de econometria



# Obtencion Datos --------------------------------------------------------------

# Funcion para bajar precios y generar rendimientos:
getreturns <- function(symbol, start, end) {
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
    # rendimientos simples   P1/P0 - 1
    # r <- Delt(stock)
    r <- periodReturn(stock,
                      period = "daily",
                      type = "arithmetic",
                      subset = paste0(c(start, end), sep = "::"))
    # renombramos la columna
    colnames(r) <- paste0(symbol, ".Return")
    # hacemos los datos accesibles
    assign(symbol, r, envir = .GlobalEnv)
}

# Llamar la funcion para cada activo particular:
start <- as.Date("2017-01-01", format = "%Y-%m-%d")
end <- as.Date("2021-06-30", format = "%Y-%m-%d")
getreturns("SPY", start, end)

# Grafico de rendimientos:
plot(SPY, type = "l", lwd = 1, col = "blue",
     main = colnames(SPY), ylab = "Return")
# se observan 3 clusters evidentes de volatilidad disparada



# Sesgo y Curtosis -------------------------------------------------------------

# Estadisticas basicas:
basicStats(SPY)

# Ho: sesgo = 0
# p-value < 0.05 --> Rechazamos Ho
t3 <- skewness(SPY) / sqrt(6 / length(SPY))
pv_t3 <- 2 * pt(q = abs(t3), df = length(SPY) - 1, lower.tail = FALSE)
pv_t3

# Ho: curtosis = 0
# p-value < 0.05 --> Rechazamos Ho
t4 <- kurtosis(SPY) / sqrt(24 / length(SPY))
pv_t4 <- 2 * (1 - pnorm(q = abs(t4)))
pv_t4

# Grafico de densidades:
chart.Histogram(SPY,
                methods = c("add.normal", "add.density"),
                colorset = c("gray", "blue", "red"))
legend("topright", legend = c("hist", "dist", "dnorm"),
       col = c("gray", "blue", "red"), lty = "solid")
# Serie con colas largas y sesgo significativo negativo (a la izquierda),
# y curtosis significativa positiva (leptocurtica).
# Podemos emplear una distribucion t student sesgada (sstd) en el modelo GARCH,
# que requerira de 2 parametros adicionales a la distribucion normal:
# shape - A menor valor, mas largas seran las colas.
# epsilon - epsilon=1 distribucion simetrica, epsilon>1(epsilon<1) sesgo positivo(negativo).



# Modelo de Media de Rendimientos ----------------------------------------------

# Modelos candidatos:
eacf(SPY, 10, 10) # seria un ARMA(1,4)=ARIMA(1,0,4)
auto.arima(SPY) # seria un ARIMA(0,0,2)=MA(2) con media diferente de cero

# Desarollo modelos:
arma <- arima(SPY, order = c(1, 0, 4))
arma
tsdiag(arma)
ma <- arima(SPY, order = c(0, 0, 2))
ma
tsdiag(ma)
# modelos similares en sus metricas AIC, tomemos el segundo por su simplicidad



# Efecto heterocedastico (ARCH) ------------------------------------------------

# Efecto ARCH con ACF y PACF:
layout(matrix(c(1, 1, 2, 3), 2, 2, byrow = TRUE))
plot(resid(ma)^2, type = "l", lwd = 1, col = "blue", main = "Squared Residuals")
acf(resid(ma)^2, main = "ACF of Squared Residuals")
pacf(resid(ma)^2, main = "PACF of Squared Residuals")
par(mfrow = c(1, 1))
# Los cuadrados de los residuales no son constantes (no hay ruido blanco),
# es decir, existe heterocedasticidad.
# Los cuadrados de los residuales son significativamente diferentes de cero,
# lo cual sugiere un modelo GARCH.

# Test de heterocedasticidad residual:
# Ho: Homocedasticidad (residuos constantes)
# Ha: Heterocedasticidad
# p-value<0.05 --> Rechazamos Ho
arch.test(ma)
# En ambos casos (PQ y LM), los resagos(order) presentan heterocedasticidad (prob<0.05),
# como contamos con resagos altos, entonces empleamos GARCH, de lo contrario ARCH.



# Modelo de Volatilidad --------------------------------------------------------

# 1) ugarchspec(): Especifica el modelo GARCH (media, varianza, distribucion de innovaciones)
# 2) ugarchfit(): Estima el modelo GARCH en la serie de rendimientos
# 3) ugarchforecast(): Emplea el modelo estimado GARCH para predicciones de volatilidad

# Modelo standard GARCH(1,1) + ARMA(0,2) con ~t no sesgada (std)
mGARCH <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                     mean.model = list(armaOrder = c(0, 2), include.mean = TRUE),
                     distribution.model = "std")
# con "sstd" hay correlacion serial en los residuales
fGARCH <- ugarchfit(spec = mGARCH, data = SPY)
fGARCH
# Se cumple Ho: No hay correlacion serial en los residuales (p-value>0.05 --> no rechazamos Ho)
# ma2 y omega no son significativos (p-value>0.05)
# Con los parametros optimos se pronostican los rendimientos y volatilidad
fGARCH@fit$coef

# Grafico de los residuales^2 y variacion condicional:
plot(fGARCH@fit$residuals^2, type = "l", col = "blue")
lines(fGARCH@fit$var, col = "red")
legend("topright", legend = c("residuals^2", "variance"),
       col = c("blue", "red"), lty = "solid")

#plot(fGARCH, which = "all")
plot(fGARCH, which = 9) # qq plot

# Media y Volatilidad (riesgo o desv. est.) de rendimientos a largo plazo:
mv <- merge.xts(fitted(fGARCH), uncmean(fGARCH), sigma(fGARCH), sqrt(uncvariance(fGARCH)))
plot(mv[, 1], type = "l", lwd = 1, col = "blue",
     main = "SPY Rendimientos futuros", xlab = "Fecha", ylab = "Rendimiento futuro")
lines(mv[, 2], col = "red")
uncmean(fGARCH) # media
plot(mv[, 3], type = "l", lwd = 1, col = "blue",
     main = "SPY Volatilidades futuras", xlab = "Fecha", ylab = "Volatilidad futura")
lines(mv[, 4], col = "red")
sqrt(uncvariance(fGARCH)) # volatilidad

# Pronostico de volatilidad:
forc <- ugarchforecast(fitORspec = fGARCH, n.ahead = 10)
forc
plot(forc, which = 1) # Time Series Prediction
plot(forc, which = 3) # Sigma Prediction

# Simulacion de los rendimientos y volatilidades futuras:
sim <- ugarchsim(fit = fGARCH, n.sim = 1000, m.sim = 25, startMethod = "sample")
plot(sim, which = "all", cex = 0.05)
