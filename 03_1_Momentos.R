# Momentos estadisticos en las series financieras ------------------------------

library(quantmod) # datos de la web
library(fBasics) # estadisticas basicas
library(PerformanceAnalytics) # herramientas de econometria



# Rendimientos -----------------------------------------------------------------

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

# Llamar la funcion para los rendimientos de cada activo particular
start <- as.Date("2018-02-01", format = "%Y-%m-%d")
end <- as.Date("2020-12-31", format = "%Y-%m-%d")

getreturns("META", start, end) # Facebook
getreturns("FORD", start, end)

# Unimos los rendimientos
m <- merge.xts(META, FORD, join='inner')
head(m)

# Grafico de rendimientos
plot(m, type = "l", col = c("blue", "red"),
     main = "META & FORD Rendimientos",
     ylab = "Rendimiento")
addLegend(legend.loc = "topright",
          legend.names = colnames(m), 
          lty = "solid", lwd = 2,
          col = c("blue", "red"))


# Estadisticas basicas ---------------------------------------------------------

# Estadisticas basicas
basicStats(m)

# Student's t-Test
# Ho: media = 0
# p-value > 0.05 --> No rechazamos Ho
# t = (mean(x) - 0) / (sd(x) / sqrt(length(x)))
# p = 2 * pt(q = abs(t), df = length(x) - 1, lower.tail = FALSE)
apply(m, MARGIN = 2, FUN = t.test)
# cbind(t.test(as.vector(META)), t.test(as.vector(FORD)))

# Estadistico de prueba t3~t (sesgo o asimetria)
t3 <- apply(m, MARGIN = 2,
            FUN = function(x) skewness(x) / sqrt(6 / length(x))) # t3 = g1 / sqrt(6 / T)
# consultar t3 en tabla de probabilidades t-student
# Ho: sesgo = 0
# p-value > 0.05 --> No rechazamos Ho
pv_t3 <- 2 * pt(q = abs(t3),
                df = nrow(m) - 1,
                lower.tail = FALSE) # 2 * pt(|t3|, T-1, P(X > x))

# Estadistico de prueba t4~t (exceso de curtosis)
t4 <- apply(m, MARGIN = 2,
            FUN = function(x) kurtosis(x) / sqrt(24 / length(x))) # t4 = g2 / sqrt(24 / T)
# consultar t4 en tabla de probabilidades t-student
# Ho: curtosis = 0
# p-value > 0.05 --> No rechazamos Ho
pv_t4 <- 2 * (1 - pnorm(q = abs(t4)))
# pv_t3 <- 2 * pt(q = abs(t4), df = nrow(m) - 1, lower.tail = FALSE)

data.frame(pv_t3, pv_t4)

# Prueba de Jaque Bera
# Ho: X ~ Normal
# p-value > 0.05 --> No rechazamos Ho
apply(m, MARGIN = 2, function(x) normalTest(x, method = "jb"))


# Grafica de densidades
col = c("gray", "blue", "red")
par(mfrow = c(1, 2))
chart.Histogram(META, methods = c("add.normal", "add.density"), colorset = col)
legend("topright", legend = c("hist META", "dist META", "dnorm META"),
       col = col, lty = "solid", cex = 0.7)
chart.Histogram(FORD, methods = c("add.normal", "add.density"), colorset = col)
legend("topright", legend = c("hist FORD", "dist FORD", "dnorm FORD"),
       col = col, lty = 1, cex = 0.7)
par(mfrow = c(1, 1))

# Conclusiones:
# Ambas distribuciones son leptocurticas
# Es mas riesgosa una distribucion con sesgo negativo (META)
# Un sesgo positivo es mas viable para invertir (FORD)
