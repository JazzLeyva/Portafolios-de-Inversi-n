library(data.table)
library(scales)
library(ggplot2)

#para ejecutarlo lo primero que de be hacer es poner el diorectorio de trabajo como en este ejemplo
# Directorio de trabajo
setwd("c:/Users/Jazmin/Documents/R/TAREA2/")



# Descargando datasets de acciones

link1 <- "https://raw.githubusercontent.com/JazzLeyva/Portafolios-de-Inversi-n/master/Data/Mercados/Ford.csv"
link2 <- "https://raw.githubusercontent.com/JazzLeyva/Portafolios-de-Inversi-n/master/Data/Mercados/VOO.csv"
link3 <- "https://raw.githubusercontent.com/JazzLeyva/Portafolios-de-Inversi-n/master/Data/Mercados/MCN.csv"
link4 <- "https://raw.githubusercontent.com/JazzLeyva/Portafolios-de-Inversi-n/master/Data/Mercados/NEE.csv"
link5 <- "https://raw.githubusercontent.com/JazzLeyva/Portafolios-de-Inversi-n/master/Data/Mercados/NESN.SW.csv"
link6 <- "https://raw.githubusercontent.com/JazzLeyva/Portafolios-de-Inversi-n/master/Data/Mercados/NVDA.csv"
link7 <- "https://raw.githubusercontent.com/JazzLeyva/Portafolios-de-Inversi-n/master/Data/Mercados/NIO.csv"
link8 <- "https://raw.githubusercontent.com/JazzLeyva/Portafolios-de-Inversi-n/master/Data/Mercados/PALAF.csv"
link9 <- "https://raw.githubusercontent.com/JazzLeyva/Portafolios-de-Inversi-n/master/Data/Mercados/QQQ.csv"

dt1 <-data.table(read.csv(link1, header=TRUE))
dt2 <-data.table(read.csv(link2, header=TRUE))
dt3 <-data.table(read.csv(link3, header=TRUE))
dt4 <-data.table(read.csv(link4, header=TRUE))
dt5 <-data.table(read.csv(link5, header=TRUE))
dt6 <-data.table(read.csv(link6, header=TRUE))
dt7 <-data.table(read.csv(link7, header=TRUE))
dt8 <-data.table(read.csv(link8, header=TRUE))
dt9 <-data.table(read.csv(link9, header=TRUE))



dt1 <-data.table(read.csv(link1, header=TRUE))
dt2 <-data.table(read.csv(link2, header=TRUE))
dt3 <-data.table(read.csv(link3, header=TRUE))
dt4 <-data.table(read.csv(link4, header=TRUE))
dt5 <-data.table(read.csv(link5, header=TRUE))
dt6 <-data.table(read.csv(link6, header=TRUE))
dt7 <-data.table(read.csv(link7, header=TRUE))
dt8 <-data.table(read.csv(link8, header=TRUE))
dt9 <-data.table(read.csv(link9, header=TRUE))
# SELECCIONA TU EXPERIMENTO, OPCIONES (1,2 3)

dt = rbind(dt1, dt2, dt3, dt4, dt5, dt6, dt7, dt8, dt9)

# Convertimos la columna data en un elemento de tiempo
dt[, date := as.Date(date)]

# Agregamos una columna con valores indexados
dt[, idx_price := price/price[1], by = ticker]


# plot the indexed values
plot_two <- ggplot(dt, aes(x = date, y = idx_price, color = ticker)) +
  geom_line() +
  # Miscellaneous Formatting
  theme_bw() + ggtitle("Precio desarrollado") +
  xlab("Fecha") + ylab("Precio\n(Indexed 2000 = 1)") +
  scale_color_discrete(name = "Compania")

plot_two

# Calculando los retornos aritmeticos
dt[, ret := price / shift(price, 1) - 1, by = ticker]
dt
# Resumen de tabla
# take only non-na values
tab <- dt[!is.na(ret), .(ticker, ret)]
 

# Calculando los valores esperados (media de los datos historicos)
# y (Desviacion estandar)
tab <- tab[, .(er = round(mean(ret), 4), 
               sd = round(sd(ret), 4)), 
           by = "ticker"]
tab

# Graficando resultados
plot_three <- ggplot(tab, aes(x = sd, y = er, color = ticker)) +
  geom_point(size = 5) +
   theme_bw() + ggtitle("Compensacion riesgo - rendimiento") +
  xlab("Volatilidad") + ylab("Respuesta esperada") +
  scale_y_continuous(label = percent, limits = c(-0.2, 0.2)) +
  scale_x_continuous(label = percent, limits = c(-0.2, 0.7))
plot_three

#ggsave(filename =  paste(c("resultados/grafico_de_portafolio.png"),collapse=""), plot_two, scale = 1, dpi=600)
#ggsave(filename =  paste(c("resultados/correlaciones_de_portafolio.png"),collapse=""), plot_three, scale = 1, dpi=600)