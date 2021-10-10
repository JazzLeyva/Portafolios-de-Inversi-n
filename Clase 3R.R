library(data.table)
library(scales)
library(ggplot2)

link <- "https://raw.githubusercontent.com/DavZim/Efficient_Frontier/master/data/fin_data.csv"
dt <-data.table(read.csv(link))
fix (dt)
dt[, date := as.Date (date)]
fix (dt)


dt [, idx_price:= price/price[1], by = ticker]

ggplot(dt,aes(x=date,y=idx_price,color=ticker)) + geom_line()+theme_bw()+ggtitle("Desarrollo del precio")+xlab("Fecha")+ylab("Precio en  (Indexed 2000=1)")+scale_color_discrete(name="Compa�ia")

dt[, ret := price / shift(price, 1) - 1, by = ticker]
tab <- dt[!is.na(ret), .(ticker, ret)]
tab
tab <- tab[, .(er = round(mean(ret), 4),
    sd = round(sd(ret), 4)),
    by = "ticker"
    ]

tab

link <- "https://raw.githubusercontent.com/DavZim/Efficient_Frontier/master/data/mult_assets.csv"
df <- data.table(read.csv(link))
link

er_x<- mean(df$x)
er_y<- mean(df$y)

sd_x <- sd(df$x)
sd_y <- sd(df$y)
cov_xy<- cov(df$x,df$y)

x_weights <- seq(from = 0, to = 1, length.out = 1000)


two_assets <- data.table(wx = x_weights, wy = 1 - x_weights)

two_assets[, ':=' (er_p = wx * er_x + wy * er_y, sd_p = sqrt(wx^2 * sd_x^2 +
wy^2 * sd_y^2 + 2 * wx * (1 - wx) * cov_xy))]
ggplot() +
geom_point(data = two_assets, aes(x = sd_p, y = er_p, color = wx)) +
geom_point(data = data.table(sd = c(sd_x, sd_y), mean = c(er_x, er_y)),
aes(x = sd, y = mean), color = "red", size = 3, shape = 18) +
theme_bw() + ggtitle("Possible Portfolios with Two Risky Assets") +
xlab("Volatility") + ylab("Expected Returns") +
scale_y_continuous(label = percent, limits = c(0, max(two_assets$er_p) * 1.2)) +
scale_x_continuous(label = percent, limits = c(0, max(two_assets$sd_p) * 1.2)) +
scale_color_continuous(name = expression(omega[x]), labels = percent)

#tres activos#

er_z <- mean(df$z)
sd_z <- sd(df$z)
cov_xz <- cov(df$x, df$z)
cov_yz <- cov(df$y, df$z)

three_assets <- data.table(wx = rep(x_weights, each = length(x_weights)),wy = rep(x_weights, length(x_weights)))

three_assets[, wz := 1 - wx - wy]
three_assets[, ':=' (er_p = wx * er_x + wy * er_y + wz * er_z,
sd_p = sqrt(wx^2 * sd_x^2 +
wy^2 * sd_y^2 +
wz^2 * sd_z^2 +
2 * wx * wy * cov_xy +
2 * wx * wz * cov_xz +
2 * wy * wz * cov_yz))]
fix(three_assets)


three_assets <- three_assets[wx >= 0 & wy >= 0 & wz >= 0]
fix(three_assets)

ggplot() +
geom_point(data = three_assets, aes(x = sd_p, y = er_p, color = wx - wz)) +
geom_point(data = data.table(sd = c(sd_x, sd_y, sd_z), mean = c(er_x, er_y, er_z)),
aes(x = sd, y = mean), color = "red", size = 3, shape = 18) +
# Miscellaneous Formatting
theme_bw() + ggtitle("Portafolios Posibles con Tres Activos Riesgosos") +
xlab("Volatilidad") + ylab("Rendimientos Esperados") +
scale_y_continuous(label = percent, limits = c(0, max(three_assets$er_p) * 1.2)) +
scale_x_continuous(label = percent, limits = c(0, max(three_assets$sd_p) * 1.2)) +
scale_color_gradientn(colors = c("red", "blue", "yellow"),
name = expression(omega[x] - omega[z]), labels = percent)


calcEFParams <- function(rets) {

retbar <- colMeans(rets, na.rm = T)
covs <- var(rets, na.rm = T) # calculates the covariance of the returns
invS <- solve(covs)
i <- matrix(1, nrow = length(retbar))

alpha <- t(i) %*% invS %*% i
beta <- t(i) %*% invS %*% retbar
gamma <- t(retbar) %*% invS %*% retbar
delta <- alpha * gamma - beta * beta

retlist <- list(alpha = as.numeric(alpha),
beta = as.numeric(beta),
gamma = as.numeric(gamma),
delta = as.numeric(delta))

return(retlist)
}
#
abcds <- calcEFParams (df)
#
calcEFValues <- function(x, abcds, upper = T) {
alpha <- abcd$alpha
beta <- abcd$beta
gamma <- abcd$gamma
delta <- abcd$delta

if (upper) {
retval <- beta / alpha + sqrt((beta / alpha) ^ 2 - (gamma - delta * x ^ 2) / (alpha))
} else {
retval <- beta / alpha - sqrt((beta / alpha) ^ 2 - (gamma - delta * x ^ 2) / (alpha))
}

return(retval)
}


head(df)

df_table <- melt(df)[, .(er = mean(value),sd = sd(value)), by = variable]

ggplot(df_table, aes(x = sd, y = er)) +
geom_point(size = 4, color = "red", shape = 18) +
stat_function(fun = calcEFValues, args = list(abcd = abcds, upper = T), n = 10000,
color = "red", size = 1) +
stat_function(fun = calcEFValues, args = list(abcd = abcds, upper = F), n = 10000,
color = "blue", size = 1) +
theme_bw() + ggtitle("Efficient Frontier with Short-Selling") +
xlab("Volatility") + ylab("Expected Returns") +
scale_y_continuous(label = percent, limits = c(0, max(df_table$er) * 1.2)) +
scale_x_continuous(label = percent, limits = c(0, max(df_table$sd) * 1.2))

install.packages("tseries")

library(tseries)

er_vals <- seq(from = min(df_table$er), to = max(df_table$er), length.out = 1000)

sd_vals <- sapply(er_vals, function(er) {
op <- portfolio.optim(as.matrix(df), er)
return(op$ps)
})
plot_dt <- data.table(sd = sd_vals, er = er_vals)


