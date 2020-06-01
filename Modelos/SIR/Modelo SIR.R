#Estimación del modelo SIR
#Laura Daniela Tenjo Galvis

# Limpiar el ambiente de R
remove(list = ls())
gc(full = T)
cat("\f")

#Cargar paquetes
library(readxl)
library(tidyr)
library(dplyr)
library(deSolve)
library(lubridate)
library(ggplot2)

#Importar datos
setwd("~/OneDrive - Universidad de Los Andes/Uniandes - Pregrado/Séptimo Semestre/Seminario de Mineria de Datos/Modelo SIR")

df <- read_excel("baseINS.xlsx", 
                 col_types = c("date", "numeric", "numeric", 
                               "numeric"))
df$date <- as.Date(df$date)

#Crear datos acumulados

df <- mutate(df, active = confirmed - death - recovered) %>%
    mutate(
    confirmed_cum = cumsum(confirmed),
    death_cum = cumsum(death),
    recovered_cum = cumsum(recovered),
    active_cum = cumsum(active)
  )


# Fechas de los datos
sir_fecha_inicio <- "2020-03-06"
sir_fecha_final <- "2020-05-29"

# Vector de infectados
Infected <- subset(df, date >= ymd(sir_fecha_inicio) & date <= ymd(sir_fecha_final))$active_cum

# Vector día
Day <- 1:(length(df$date))


# Condiciones iniciales
N <- 50839173
init <- c(
  S = N - Infected[1],
  I = Infected[1],
  R = 0
)

# Función de evolucion del modelo SIR
SIR <- function(time, state, parameters) {
  par <- as.list(c(state, parameters))
  with(par, {
    dS <- -beta * I * S / N
    dI <- beta * I * S / N - gamma * I
    dR <- gamma * I
    list(c(dS, dI, dR))
  })
}

# Integración numérica (Usando método Runge-Kutta de orden 4)
RSS <- function(parameters) {
  names(parameters) <- c("beta", "gamma")
  out <- ode(y = init, times = Day, func = SIR, parms = parameters)
  fit <- out[, 3]
  sum((Infected - fit)^2)
}

# Optimización
Opt <- optim(c(2.1, 2.1),
               RSS,
               method = "L-BFGS-B",
               lower = c(2, 2),
               upper = c(2.2, 2.2)
)

# Revisar convergencia
Opt$message

# Parámetros
Opt_par <- setNames(Opt$par, c("beta", "gamma"))
Opt_par

# Tiempo desde el primer caso
t <- 1:as.integer(ymd(sir_fecha_final) + 1 - ymd(sir_fecha_inicio))

# Obtener los valores del modelo SIR ajustado a los datos
infectados_acumulados_fit <- data.frame(ode(
  y = init, times = t,
  func = SIR, parms = Opt_par
))

# Añadir base de datos con fecha e infectados
infectados_acumulados_fit <- infectados_acumulados_fit %>%
  mutate(
    Date = ymd(sir_fecha_inicio) + days(t - 1),
    infectados_acumulados = Infected
  )

# Gráfica datos reales vs. ajustados
infectados_acumulados_fit %>%
  ggplot(aes(x = Date)) +
  geom_line(aes(y = I), colour = "red") +
  geom_point(aes(y = infectados_acumulados), colour = "blue") +
  labs(
    y = "Infectados acumulados",
    x = "Fecha",
    title = "Datos de infectados vs datos ajustados de infectados",
    subtitle = "(Rojo = modelo SIR ajustado, Azul = observado)"
  ) +
  theme_minimal()

#Graficar comportamiento de I en el largo plazo ---------------------------------------------------------------
# Rango de tiempo desde el primer caso
t <- 1:200

# Ajustar el modelo al nuevo rango de tiempo
infectados_acumulados_fit <- data.frame(ode(
  y = init, times = t,
  func = SIR, parms = Opt_par
))

# Gráficar curva de infectados
infectados_acumulados_fit %>%
  ggplot(aes(x = t)) +
  geom_line(aes(y = I), colour = "red") +
  labs(
    y = "Infectados acumulados",
    title = "Comportamiento curva de infectados modelo SIR",
    x = "Número de días desde el primer caso"
  ) +
  theme_minimal()

#Pico de infectados
#Número de infectados
max(infectados_acumulados_fit$I)
#Día
which.max(infectados_acumulados_fit$I)
