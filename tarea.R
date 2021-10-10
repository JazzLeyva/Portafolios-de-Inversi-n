library(data.table)
library(scales)
library(ggplot2)

link1 <- "https://raw.githubusercontent.com/JazzLeyva/Portafolios-de-Inversi-n/master/GOOGL.csv"
link2 <- "https://raw.githubusercontent.com/JazzLeyva/Portafolios-de-Inversi-n/master/MCN.csv"
link3 <- "https://raw.githubusercontent.com/JazzLeyva/Portafolios-de-Inversi-n/master/NEE.csv"
dt1 <-data.table(read.csv(link1))
dt2 <-data.table(read.csv(link2))
dt3 <-data.table(read.csv(link3))
dt = combineddataset = rbind(dt1, dt2, dt3)

dt[, date := as.Date (date)]
fix(dt)
 
# create indexed values
dt[, idx_price := price/price[1], by = ticker]
 

# plot the indexed values
ggplot(dt, aes(x = date, y = idx_price, color = ticker)) +
    geom_line() + theme_bw() + ggtitle("Price Developments") +
    xlab("Date") + ylab("Price\n(Indexed 2000 = 1)") +
    scale_color_discrete(name = "Company")
