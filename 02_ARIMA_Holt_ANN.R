# Modelos de ajuste y pronostico de datos financieros --------------------------

library(quantmod) # datos de la web
library(tseries) # series de tiempo
library(forecast) # pronostico de series de tiempo
library(Metrics) # metricas de error
library(TSA) # analisis de series de tiempo
library(lmtest) # test de modelos de regresion lineal
library(tsfeatures) # caracteristicas de series de tiempo
library(gridExtra) # n graficas de ggplot2 en una



# Obtencion de datos -----------------------------------------------------------

# Obtenemos los Precios de Cierre (columna 4) de AMAZON:
getSymbols("AMZN", src = "yahoo",
           from = as.Date("2020-08-01", format="%Y-%m-%d"),
           to = as.Date("2021-03-31", format="%Y-%m-%d"))
AMZN <- Cl(AMZN)
AMZN <- na.omit(AMZN)

# Graficamos la serie:
plot(AMZN, type = "l", col = "blue",
     main = colnames(AMZN), ylab = "Precios de Cierre")

# Convertimos a serie de tiempo para poder trabajar:
nday <- as.numeric(index(AMZN)[1] - as.Date(paste0(format(index(AMZN)[1], format = "%Y"),
                                                   "-01-01"), format = "%Y-%m-%d")) + 1
ts <- ts(AMZN, start = c(as.numeric(format(index(AMZN)[1], format = "%Y")), nday), frequency = 365.25)

# Segmentamos la serie de tiempo, tomando el 7% para la prueba:
h <- round(length(ts) * 0.07, digits = 0) # generalmente h=12 dias
train <- head(ts, -h) # entrenamiento
test <- tail(ts, h) # prueba



# Modelos ARIMA ----------------------------------------------------------------

# Veamos si la serie es estacionaria con la prueba de Augmented Dickey-Fuller:
# Ho: la serie no es estacionaria (presencia de una raiz unitaria)
# p-value>0.05 --> no rechazamos Ho
adf.test(train) # fUnitRoots::adfTest()

# Como la serie no es estacionaria, la diferenciamos y vemos si ya es estacionaria:
dtrain <- diff(train)
adf.test(dtrain)
# Como p-value<0.05 --> rechazamos Ho (Ha: la serie es estacionaria)

# Graficas de ambos:
grid.arrange(autoplot(train), # original (no estacionaria)
             autoplot(dtrain, col = "blue"), # diferenciada (estacionaria o ruido blanco)
             nrow = 2)


# Ya estacionaria, definamos un candidato de modelo ARMA(p,q):
# EACF: Extended Autocorrelation Function.
# La clave para asignar los niveles p y q, es la identificacion
# de un triangulo de "o" con un vertice superior izquierdo.
eacf(dtrain, 15, 10) # grid de 15 filas x 10 columnas
#   2 3 4
# 6 x x o
# 7 x o o
# 8 x x o
# El la fila 7 y columna 3 se forma el vertice de un triangulo de "o",
# entonces, seria un modelo ARMA(7,3), pero como la diferenciamos una vez
# para ser estacionaria, entonces, seria un ARIMA(7,1,3).

# Definamos otro candidato de modelo ARIMA(p,d,q):
auto.arima(train, seasonal = TRUE) # minimizar el AIC
# Seria un ARIMA(1,0,0) = AR(1)


# Modelacion ARIMA(7,1,3):
arima <- Arima(train, order = c(7, 1, 3), method = "ML") # ML = Maxima Verosimilitud (para modelos MA)
summary(arima)
coeftest(arima) # coeficientes significativos
tsdiag(arima) # time series diagnostic
# Los residuos se ven bien.
# Ningun retraso/residuo es significativo (puro ruido blanco).
# p-values elevados.

# Modelacion AR(1):
ar <- Arima(train, order = c(1, 0, 0), method = "ML")
summary(ar)
coeftest(ar)
tsdiag(ar)
# Comportamiento muy similar al anterior modelo.

# El modelo preferido es el que tiene el valor minimo de AIC y BIC.
# Sin embargo, son preferibles las metricas de error como el MRSE y MAPE.


# Modelos pronostico:
farima <- forecast(arima, h = h)
summary(farima)

far <- forecast(ar, h = h)
summary(far)


# Graficos de los modelos pronostico:
grid.arrange(autoplot(farima, include = 50),
             autoplot(far, col = "blue", include = 50),
             nrow = 2)

# Otro grafico integral de los modelos pronostico:
autoplot(tail(train, 50)) +
    autolayer(tail(arima$fitted, 50), series = "ARIMA(7,1,3)") +
    autolayer(tail(ar$fitted, 50), series = "AR(1)") +
    autolayer(farima$mean, series = "fcst ARIMA(7,1,3)") +
    autolayer(far$mean, series = "fcst AR(1)")


# Midamos el error de pronostico (metricas de desempeno):
# RMSE: Error Cuadratico Medio.
# MAPE: Error Porcentual Absoluto Medio.
mt <- data.frame(Modelo = c("ARIMA(7,1,3)", "AR(1)"),
                 RMSE = c(rmse(test, farima$mean), rmse(test, far$mean)),
                 MAPE = c(mape(test, farima$mean), mape(test, far$mean)))
mt
# El mejor modelo es el que presente menor RMSE y MAPE. En este caso es el ARIMA.



# Modelos de Suavizamiento Exponencial -----------------------------------------

# alpha: nivel [alpha=1 --> caminata aleatoria (suavisamiento muy bajo)]
# beta: tendencia [beta=0 --> la tendencia cambia muy lento o no tiene tendencia]
# gamma: estacionalidad

# A partir de los mismos datos, grafiquemos la serie:
autoplot(train)
# No hay una tendencia o joroba evidente en la grafica (beta=0),
# por lo que probemos modelos de suavizamiento simples.


# Primer Modelo: Suavizado Exponencial Simple (SES).
fses <- ses(train, h = h, alpha = NULL)
summary(fses)
autoplot(fses, include = 50) + autolayer(tail(fitted(fses), 50), series = "SES") # modela solo el nivel

# Segundo Modelo: Suavizado Exponencial Doble (Holt).
# Podriamos probar este modelo, auque no hay una tendencia evidente (beta=0).
fholt <- holt(train, h = h, alpha = NULL, beta = NULL)
summary(fholt)
autoplot(fholt, include = 50) + autolayer(tail(fitted(fholt), 50), series = "Holt")

# Tercer Modelo: Suavizado Exponencial Triple (Holt-Winters).
# Recordemos que la serie no es estacionaria (gamma=FALSE).
fhw <- forecast(HoltWinters(train, alpha = NULL, beta = NULL, gamma = FALSE), h = h)
summary(fhw)
autoplot(fhw, include = 50) + autolayer(tail(fitted(fhw), 50), series = "Holt-Winters")

# Cuarto Modelo: Error, Tendencia y Estacionalidad (ETS), que determine el modelo.
fets <- forecast(ets(train,
            model = "ZZZ", # ajustar ETS
            damped = FALSE, # sin tendencia
            alpha = NULL, beta = NULL, gamma = NULL, phi = NULL, # estimar las mejores
            lambda = FALSE, # sin tranformar datos
            additive.only = FALSE,
            allow.multiplicative.trend = FALSE), # sin tendencia multiplicativa
            h = h)
summary(fets)
# Seria un ETS(A,N,N) = Modelo Exponencial Simple (SES)
# E=A: errores aditivos, T=N: sin tendencia, S=N: sin estacionalidad
# alpha=1 --> caminata aleatoria.
autoplot(fets, include = 50) + autolayer(tail(fitted(fets), 50), series = "ETS")


# Metricas de desempeno del pronostico:
mt[nrow(mt) + 1, ] <- list("SES", rmse(test, fses$mean), mape(test, fses$mean))
mt[nrow(mt) + 1, ] <- list("Holt", rmse(test, fholt$mean), mape(test, fholt$mean))
mt[nrow(mt) + 1, ] <- list("Holt-Winters", rmse(test, fhw$mean), mape(test, fhw$mean))
mt[nrow(mt) + 1, ] <- list("ETS", rmse(test, fets$mean), mape(test, fets$mean))
mt
# El mejor modelo es el que presente menor RMSE y MAPE.
# En este caso, los modelos exponenciales (Holt y Holt-Winters) son mejores que los ARIMA.



# Redes Neuronales Feed Forward ------------------------------------------------

# Neural Network Time Series Forecasts
# https://pkg.robjhyndman.com/forecast/reference/nnetar.html

# A partir de los mismos datos, grafiquemos la serie:
autoplot(train)


# Autoregressive Neural Network:
# En datos de precios, se deben transformar los datos (lambda=TRUE)
# para tratar de que los residuos sean cercanos a homocedasticos,
# es decir, estable en su varianza (serie homogenea).
# Para seleccionar automatimante una transformacion Box-Cox,
# se usa la opcion (lambda="auto")
fnnar_z <- forecast(nnetar(train, lambda = TRUE), h = h, PI = TRUE)
summary(fnnar_z)
# Seria un NNAR(p=12,k=6)
# 12 resagos que entran en la primera capa, con 6 nodos en la capa oculta.
autoplot(fnnar_z, include = 50) + autolayer(tail(fitted(fnnar_z), 50), series = "NNAR")


# Autoregressive Neural Network con AR(p):
# Recordemos que teniamos un modelo ARIMA(7,1,3),
# con la parte AR(7) que podemos incluir:
fnnar_ar7 <- forecast(nnetar(train, p = 7, lambda = TRUE), h = h, PI = TRUE)
summary(fnnar_ar7)
# Seria un NNAR(p=7,k=4)
autoplot(fnnar_ar7, include = 50) + autolayer(tail(fitted(fnnar_ar7), 50), series = "NNAR")

# El modelo con "sigma^2 estimated" mas bajo es el mejor.
# Sin embargo, son preferibles las metricas de error como el MRSE y MAPE.


# Metricas de error del pronostico:
mt[nrow(mt) + 1, ] <- list("NNAR_z", rmse(test, fnnar_z$mean), mape(test, fnnar_z$mean))
mt[nrow(mt) + 1, ] <- list("NNAR_ar7", rmse(test, fnnar_ar7$mean), mape(test, fnnar_ar7$mean))
mt
# El mejor modelo es el que presente menor RMSE y MAPE.
# En este caso, los modelos neuronales no son tan buenos como los anteriores,
# y los mejores son Holt y Holt-Winters.



# Caracteristicas de la serie --------------------------------------------------

nonlinearity(train)
# nonlinearity=0 --> serie lineal (los modelos lineales se ajustan mejor a la serie)
# nonlinearity=1 --> serie no lineal
entropy(train)
# entropy=1 --> serie dificil de pronosticar
# entropy=0 --> serie facil de pronosticar
