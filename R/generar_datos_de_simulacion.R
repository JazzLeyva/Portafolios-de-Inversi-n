# load the functions, libraries etc
setwd("c:/Users/Jazmin/Documents/R/TAREA2/")
source("R/functions.R")

# GENERACION DE DATOS ALEATORIOS BASADO EN LA MEDIA Y VARIANZA 
# QUE GENERO CADA ACCION


# FORD
ford_mean = 0.0088
ford_sd = 0.1423
df <- data.table(x = rnorm(10000, mean = ford_mean, sd = ford_sd))

# VOO
voo_mean = 0.0107
voo_sd = 0.0387
df[, b := rmultvar(x, r = 0, voo_mean, voo_sd)]
# MCN
mcn_mean = -0.0019
mcn_sd = 0.0488
df[, c := rmultvar(x, r = 0, mcn_mean, mcn_sd)]

# NEE
nee_mean = 0.0122
nee_sd = 0.0495
df[, d := rmultvar(x, r = 0, nee_mean, nee_sd)]

# NESN.SW
nesnsw_mean = 0.0074 
nesnsw_sd = 0.0366
df[, e := rmultvar(x, r = 0, nesnsw_mean, nesnsw_sd)]

# NVDA
nvda_mean = 0.0340 
nvda_sd = 0.1322
df[, f := rmultvar(x, r = 0, nvda_mean, nvda_sd)]

# NIO
nio_mean = 0.1058
nio_sd = 0.3497
df[, g := rmultvar(x, r = 0, nio_mean, nio_sd)]

# PALAF
palaf_mean = 0.0743
palaf_sd = 0.6627
df[, h := rmultvar(x, r = 0, palaf_mean, palaf_sd)]

qqq_mean = 0.0127
qqq_sd = 0.0508
df[, i := rmultvar(x, r = 0, qqq_mean, qqq_sd)]

write.csv(df, file = "data/datos_de_simulacion.csv", row.names = FALSE)