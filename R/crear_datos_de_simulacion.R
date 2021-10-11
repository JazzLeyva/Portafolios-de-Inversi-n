# load the functions, libraries etc
source("R/functions.R")
setwd("c:/Users/Jazmin/Documents/R/Portafolios-de-Inversi-n/")

# SELECCIONA TU EXPERIMENTO, OPCIONES (1,2 3)
EXPERIMENTO = 2



# GENERACION DE DATOS ALEATORIOS BASADO EN LA MEDIA Y VARIANZA 
# QUE GENERO CADA ACCION

if (EXPERIMENTO == 1) { # FALSE, VOO Y MCN
    x_m = 0.0088
    x_sd = 0.1423
    y_m = 0.0107 
    y_sd = 0.0387
    z_m = -0.0019
    z_sd = 0.0488
    
} else {
    if(EXPERIMENTO == 2) { # NEE, NESN.S Y NVDA
        x_m = 0.0122
        x_sd = 0.0495
        y_m = 0.0074 
        y_sd = 0.0366
        x_m = 0.0340 
        z_sd = 0.1322
    } else {# NIO, PALAF Y QQQ
        x_m = 0.1058
        x_sd = 0.3497
        y_m = 0.0743
        y_sd = 0.6627
        z_m = 0.0127
        z_sd = 0.0508
    }
} 

set.seed(13254)
df <- data.table(x = rnorm(10000, mean = x_m, sd = x_sd))

y_mean <- y_m
y_sd <- y_sd

z_mean <- z_m
z_sd <- z_sd

df[, y := rmultvar(x, r = 0, y_mean, y_sd)]
df[, z := rmultvar(x, r = 0, z_mean, z_sd)]

write.csv(df, file = paste(c("data/Experimentos/Experimento",EXPERIMENTO,"csv"),collapse=""), row.names = F)


