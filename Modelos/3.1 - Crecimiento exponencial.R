
# 3.1 - Crecimiento exponencial


# Preparación                                                             ----

# Limpiar el abiente de R
remove(list = ls())
dev.off()
gc(full = T)
cat("\f")


# Fijar opciones
options(max.print = 200)


# Actualizar y cargar paquetes
update.packages(ask = F)
library(tidyverse)
library(gridExtra)
library(data.table)




# Crecimiento exponencial                                                 ----

# Limpiar el abiente de R
remove(list = ls())
dev.off()
gc(full = T)
cat("\f")


# Función de estadísticas descriptivas
source("Función descriptivas.R")


# Importar datos
covid <- fread("https://covid.ourworldindata.org/data/owid-covid-data.csv",
               na.strings = "")


# Solo observaciones de Colombia
covid <- covid[location == "Colombia"]


# Declarar fecha
covid[, date := as.Date(date)]


# Guardar valores constates
constantes <-
    covid[1, .(iso_code, location, population, population_density, median_age,
               aged_65_older, aged_70_older, gdp_per_capita, extreme_poverty,
               cvd_death_rate, diabetes_prevalence, female_smokers, male_smokers,
               handwashing_facilities, hospital_beds_per_100k)]


# Eliminar variables constantes de tabla principal
covid <-
    covid[, !c("iso_code", "location", "population", "population_density",
               "median_age", "aged_65_older", "aged_70_older", "gdp_per_capita",
               "extreme_poverty", "cvd_death_rate", "diabetes_prevalence",
               "female_smokers", "male_smokers", "handwashing_facilities",
               "hospital_beds_per_100k")]


# Diferencia de días con el primer caso confirmado
covid[, t := date - as.Date("2020-03-07")]


# Función para rellenar en caso de 0
locf.0 <- function(x) {
    v <- (x != 0)
    c(0, x[v])[cumsum(v) + 1]
}


# Rellenar casos totales
covid[, total_cases := locf.0(total_cases)]


# Valor de alpha según modelo exponencial
covid[, alpha := fifelse((1 / as.integer(t)) * log(total_cases) < Inf,
                         (1 / as.integer(t)) * log(total_cases),
                         as.numeric(NA))]


# Graficar comportamieto de alpha
ggplot(covid, aes(x = date, y = alpha)) + geom_point() +
    
    ylab(expression(alpha)) + xlab("Fecha") +
    
    ggtitle("Valores de alpha en el tiempo para Colombia") + 
    
    geom_ribbon(aes(ymin = 0.2, ymax = 0.38), alpha = 0.25, show.legend = T,
                fill = "red") +
    
    # geom_hline(yintercept = mean(covid$alpha, na.rm = T), 
    #            colour = "blue") +
    
    geom_hline(yintercept = mean(covid$alpha[-1:-5], na.rm = T),
               linetype = "dashed") +
    
    geom_vline(xintercept = as.Date("2020-03-24"),
               linetype = "dotted") +
    
    theme(axis.text = element_text(size = 20),
          axis.title = element_text(size = 23),
          plot.title = element_text(size = 30),
          panel.background = element_blank())


# Estimar alpha por MCO
lm(data = covid[-1:-3], I(log(total_cases)) ~ t - 1) %>% summary
# Estimación de 0. 1845, altamente sigificativo


#### Replicar gráficas figura 1

# Calcular i
covid[, i := total_cases * 10^6 / constantes$population]


# Fecha del umbral
umbral_i <- covid[i >= 2, date][1]
# 19 de marzo


## Gráficos de infecciones

# Gráfico lineal
g_i_lineal <- 
    ggplot(covid[date >= umbral_i & date - umbral_i <= 20],
           aes(x = date, y = i)) +
    
    geom_point() + geom_line() +
    
    ylab("i = infecionces / millones") +
    
    xlab("primeros 20 días desde que i > 2") +
    
    ggtitle("Infecciones reportadas acumuladas") +
    
    theme_bw() + theme(panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),
                       plot.title = element_text(hjust = 0.5, size = 25),
                       axis.text = element_text(size = 20),
                       axis.title = element_text(size = 23))


# Gráfico con escala logaritmica
g_i_log <- g_i_lineal + scale_y_log10()




# Calcular d
covid[, d := total_deaths * 10^6 / constantes$population]


# Fecha del umbral
umbral_d <- covid[d >= 0.2, date][1]
# 31 de marzo




## Gráficos de muertes

# Gráfico lineal
g_d_lineal <- 
    ggplot(covid[date >= umbral_d & date - umbral_d <= 15],
           aes(x = date, y = d)) +
    
    geom_point() + geom_line() +
    
    ylab("d = infecionces / millones") +
    
    xlab("primeros 15 días desde que d > 0.2") +
    
    ggtitle("Muertes reportadas acumuladas") +
    
    theme_bw() + theme(panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),
                       plot.title = element_text(hjust = 0.5, size = 25),
                       axis.text = element_text(size = 20),
                       axis.title = element_text(size = 23))


# Gráfico con escala logaritmica
g_d_log <- g_d_lineal + scale_y_log10()


# Combinar graficos lineales
grid.arrange(g_i_lineal, g_d_lineal, ncol = 2)


# Combinar graficos logarítmicos
grid.arrange(g_i_log, g_d_log, ncol = 2)


