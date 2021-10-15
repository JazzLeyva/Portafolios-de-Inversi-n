# load the functions, libraries etc
setwd("c:/Users/Jazmin/Documents/R/TAREA2/")
source("R/functions.R")

# GENERACION DE DATOS ALEATORIOS BASADO EN LA MEDIA Y VARIANZA 
# QUE GENERO CADA ACCION
set.seed(12345)
# AMZN
amzn_mean = 0.0261
amzn_sd = 0.0794
df <- data.table(x = rnorm(10000, mean = amzn_mean, sd = amzn_sd))

# CX
cx_mean = 0.0079
cx_sd = 0.1212
df[, a := rmultvar(x, r = 0, cx_mean, cx_sd)]

# FOR
for_mean = 0.0068
for_sd = 0.1101
df[, b := rmultvar(x, r = 0, for_mean, for_sd)]


# NEE
nee_mean = 0.0144
nee_sd = 0.0464
df[, c := rmultvar(x, r = 0, nee_mean, nee_sd)]

# NSRGY
nvda_mean = 0.0068
nvda_sd = 0.0405
df[, d := rmultvar(x, r = 0, nvda_mean, nvda_sd)]

# NVDA
nvda_mean = 0.0393
nvda_sd = 0.1174
df[, e := rmultvar(x, r = 0, nvda_mean, nvda_sd)]

# PALAF
palaf_mean = 0.0907
palaf_sd = 0.7861
df[, f := rmultvar(x, r = 0, palaf_mean, palaf_sd)]

qqq_mean = 0.0158
qqq_sd = 0.0444
df[, g := rmultvar(x, r = 0, qqq_mean, qqq_sd)]

# VOO
voo_mean = 0.0107
voo_sd = 0.0387
df[, h := rmultvar(x, r = 0, voo_mean, voo_sd)]

write.csv(df, file = "data/datos_de_simulacion.csv", row.names = F)