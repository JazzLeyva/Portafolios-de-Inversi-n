library(data.table)
library(ggplot2)
library(data.table)

# Directorio de trabajo
setwd("c:/Users/Jazmin/Documents/R/TAREA2/")
# Descargando datasets de acciones

link <- "https://raw.githubusercontent.com/JazzLeyva/Portafolios-de-Inversi-n/master/data/datos_de_simulacion.csv"
df <-data.table(read.csv(link, header=TRUE))
df
df_table <- melt(df)[, .(mean = mean(value), sd = sd(value)), by = variable]
df_table



er_x <- mean(df$x)
er_y <- mean(df$g)
er_z <- mean(df$h)
sd_x <- sd(df$x)
sd_y <- sd(df$g)
sd_z <- sd(df$h)
cov_xy <- cov(df$x, df$g)
cov_xz <- cov(df$x, df$h)
cov_yz <- cov(df$g, df$h)


# three assets
three_assets_seq <- seq(from = 0, to = 1, length.out = 1000)

three <- data.table(wx = rep(three_assets_seq, each = length(three_assets_seq)),
                      wy = rep(three_assets_seq, length(three_assets_seq)))

three[, wz := 1 - wx - wy]

three[, ':=' (er_p = wx * er_x + wy * er_y + wz * er_z,
                sd_p = sqrt(wx^2 * sd_x^2 +
                              wy^2 * sd_y^2 +
                              wz^2 * sd_z^2 +
                              2 * wx * wy * cov_xy +
                              2 * wx * wz * cov_xz +
                              2 * wy * wz * cov_yz))]

three <- three[wx >= 0 & wy >= 0 & wz >= 0]

plot_b <- ggplot() +
  geom_point(data = three, aes(x = sd_p, y = er_p, color = wx - wz)) +
  geom_point(data = df_table, aes(x = sd, y = mean), color = "red", size = 3, shape = 18) +
  theme_bw() + ggtitle("Possible Portfolios with Three Risky Assets") +
  xlab("Volatility") + ylab("Expected Returns") +
  scale_y_continuous(label = percent, limits = c(0, max(three$er_p) * 1.2)) +
  scale_x_continuous(label = percent, limits = c(0, max(three$sd_p) * 1.2)) +
  scale_color_gradientn(colors = c("red", "blue", "yellow"),
                        name = expression(omega[x] - omega[z]), labels = percent)

plot_b

ggsave(filename = "resultados/frontera_3d_amazon_qqq_voo.png", plot_b, scale = 1, dpi=600)

