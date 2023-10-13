# Tipos de Datos en Econometria ------------------------------------------------

library(dplyr) # operaciones en dataframes
library(quantmod) # datos de la web
library(dygraphs) # graficas interactivas
library(htmltools) # graficas interactivas
library(RColorBrewer) # paleta de colores



# Series de Tiempo -------------------------------------------------------------

# Funcion para obtener los precios de cierre y volumenes de una o varias acciones:
getPyV <- function(tickers, start, end) {
    # inicializamos el dataframe con las series de tiempo
    x <- xts(); tclass(x) <- "Date"
    for (i in tickers) {
        # obtenemos los stocks de YahooFinance
        stock <- getSymbols(i,
                            src = "yahoo",
                            periodicity = "daily",
                            from = start,
                            to = end,
                            auto.assign = FALSE)
        # mantenemos columnas con Precios de Cierre y Volumenes (columnas 4 y 5)
        stock <- merge.xts(Cl(stock), Vo(stock)) #stock[, 4:5]
        # unimos las series de los stocks
        x <- merge.xts(x, stock)
        # eliminamos valores faltantes
        x <- na.omit(x)
    }
    return(x)
}

# Llamamos a la funcion para las acciones con la fecha de inicio y fecha de cierre:
tickers <- c("AMZN", "NFLX", "IBM", "SPY")
start <- as.Date("2014-01-01", format = "%Y-%m-%d")
end <- as.Date("2020-07-01", format = "%Y-%m-%d")
PyV <- getPyV(tickers = tickers, start = start, end = end)
round(head(PyV, 5), 2) # primeros 5 registros redondeados a 2 decimales
round(tail(PyV, 5), 2) # ultimos 5 registros redondeados a 2 decimales



# Datos de Panel ---------------------------------------------------------------

# Grafica interactiva de los precios de cierre (serie de tiempo multivariada):
precios <- dygraph(Cl(PyV), #PyV[, seq(1, 7, by = 2)]
                   main = "Precios de Cierre") %>%
    dyAxis("x", label = "Fecha") %>%
    dyAxis("y", label = "Precio de Cierre") %>%
    dyRangeSelector(dateWindow = c(start, end)) %>%
    dyOptions(colors = brewer.pal(length(tickers), "Set1"))
# Grafica interactiva de los volumenes:
volumenes <- dygraph(Vo(PyV), main = "Volumenes") %>%
    dyAxis("x", label = "Fecha") %>%
    dyAxis("y", label = "Volumen") %>%
    dyRangeSelector(dateWindow = c(start, end)) %>%
    dyOptions(colors = brewer.pal(length(tickers), "Set1"))
# Representamos los objetos dygraphs en un solo grafico:
browsable(tagList(list(precios, volumenes)))



# Datos Transversales ----------------------------------------------------------

# Seleccionamos los precios de cierre de AMZN (1ra columna) del 2014 y del 2020:
# para el 2014
AMZN_2014 <- subset(PyV[, 1], between(index(PyV), as.Date("2014-01-01"), as.Date("2014-12-31")))
head(AMZN_2014, 5)
# para el 2020
AMZN_2020 <- subset(PyV[, 1], between(index(PyV), as.Date("2020-01-01"), as.Date("2020-12-31")))
head(AMZN_2020, 5)

# Visualizamos los precios con un histograma:
par(mfrow = c(2, 1))
hist(AMZN_2014, freq = FALSE, col = "yellow", border = "blue",
     main = "Densidades de los Precios AMZN en 2014", xlab = "Precios Cierre")
lines(density(AMZN_2014), lwd = 2, col = "red")
hist(AMZN_2020, freq = FALSE, col = "green", border = "blue",
     main = "Densidades de los Precios AMZN en 2020", xlab = "Precios Cierre")
lines(density(AMZN_2020), lwd = 2, col = "red")
par(mfrow = c(1, 1))
