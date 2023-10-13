# Relacion Riesgo-Rendimiento --------------------------------------------------

library(dplyr) # operaciones en dataframes
library(zoo) # fomatos de serie de tiempo
library(ggplot2) # graficos con dataframe
library(cowplot) # alinear graficos de ggplo2



# Datos ------------------------------------------------------------------------

# Los datos provienen del sitio del profesor Keneth French, disponibles en:
# https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html#Research
# https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/Data_Library/f-f_factors.html
# Variables:
# Mkt.Rf - Exceso de rendimiento en el portafolio de mercado (Rm-Rf)
# SMB - Tamano de las carteras pequenas y grandes (Small Minus Big)
# HML - Valor de las carteras de valor y crecimiento (High Minus Low)
# Rf - Tasa libre de riesgo (rendimiento de bonos)

ff <- read.csv("csv/FF_Research_Data_Factors_2020.csv") # leer archivo
ff$Date <- as.yearmon(as.character(ff$Date), format="%Y%m") # formato de fechas
ff[, 2:ncol(ff)] <- ff[, 2:ncol(ff)] / 100 # porcentaje a decimal
ff <- ff[, c(1, 2, 5)] # seleccionamos columnas

# Segmetamos un periodo de 29 anos dic 1990 - dic 2019
ff <- subset(ff, between(ff$Date, as.yearmon("1990-12"), as.yearmon("2019-12")))
row.names(ff) <- NULL # resetear indices
rbind(head(ff), tail(ff))



# Calculo de rendimientos ------------------------------------------------------

# Rendimiento de mercado (acciones/stocks) (Mkt.Rf=Rm-Rf --> Rm=Mkt.Rf+Rf)
ff <- ff %>% mutate(Rm = Mkt.Rf + Rf)

# Rendimiento bruto del mercado y de la tasa libre de riesgo
ff <- ff %>% mutate(Rf.gross = 1 + Rf,
                    Rm.gross = 1 + Rm)
ff[1, 5:6] <- 1 # los rendimientos se pagan a fin de mes

# Rendimiento acumulado del mercado y de la tasa libre de riesgo
ff <- ff %>% mutate(Rf.cum = cumprod(Rf.gross),
                    Rm.cum = cumprod(Rm.gross))

rbind(head(ff), tail(ff))
tail(ff, 1)[, 8] / tail(ff, 1)[, 7]
# Una inversion en stocks genera 9 veces mas que en bonos al final del periodo



# Graficos de Rendimiento y Volatilidad ----------------------------------------

p1 <- ggplot(ff, aes(x = Date)) + 
    geom_line(aes(y = Rm.cum, colour = "Stocks")) + 
    geom_line(aes(y = Rf.cum, colour = "Bonos")) +
    labs(title = "Retornos Acumulados Stocks vs Bonos") +
    xlab ("Fecha") +
    ylab ("Valor de $1 invertido") +
    scale_color_manual(name = "",
                       values = c("Stocks" = "blue", "Bonos" = "red"))

p2 <- ggplot(ff, aes(x = Date)) + 
    geom_line(aes(y = Rm, colour = "Stocks")) + 
    geom_line(aes(y = Rf, colour = "Bonos")) +
    labs(title = "Volatilidad de Retornos Stocks vs Bonos") +
    xlab ("Fecha") +
    ylab ("Rendimiento") +
    scale_color_manual(name = "",
                       values = c("Stocks" = "blue", "Bonos" = "red"))

plot_grid(plotlist = list(p1, p2), ncol = 1, align = 'v')
# A mayor riesgo mayor ganancia
