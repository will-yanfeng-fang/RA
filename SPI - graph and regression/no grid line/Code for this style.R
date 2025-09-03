#1. White background, No grid line, OECD starts at 60, No equation, No $ for x-axis.

install.packages(c("tidyverse", "readxl", "ggrepel", "scales"))  
library(tidyverse)
library(readxl)
library(ggrepel)
library(scales)
library(dplyr)

setwd("/Users/fangxiaoling/Documents/Nathan Nunn RA/Book/SPI - graph and regression/")

# 1) Load data
df <- read_excel("data.xlsx") |>
  select(code, country, SPI_2025, GDPpc_USD_2025, logG_2025) |>
  rename(spi = SPI_2025, gdp = GDPpc_USD_2025, lgdp = logG_2025) |>
  mutate(
    gdp = as.numeric(gdp),
    lgdp = as.numeric(lgdp),
    spi = as.numeric(spi)
  ) |>
  filter(!is.na(country), !is.na(gdp), !is.na(spi))

y_breaks <- seq(0, 100, by = 10)

# 2) Scatter (Ln GDPpc vs SPI)
p_scatter <- ggplot(df, aes(x = lgdp, y = spi)) +
  geom_point(alpha = 0.8, size = 2) +
  geom_text_repel(aes(label = country), size = 3, max.overlaps = 60, box.padding = 0.3) +
  labs(
    title = "Ln of GDP vs Social Progress Index (2025)",
    x = "Ln of GDP per capita (2025, USD)",
    y = "Social Progress Index (2025)"
  ) +
  scale_x_continuous(breaks = pretty_breaks(8)) +
  scale_y_continuous(breaks = y_breaks, limits = c(25, 100), expand = c(0, 0)) + 
  theme_classic(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 16))

ggsave("scatter_lgdp_spi_2025.png", p_scatter,
       width = 24, height = 18, dpi = 300, limitsize = FALSE)

# 3) Linear regression (all countries, no equation text)
p_with_lm <- p_scatter +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.9, fullrange = FALSE)

ggsave("scatter_lgdp_spi_2025_lm.png", p_with_lm,
       width = 24, height = 18, dpi = 300, limitsize = FALSE)

# 4) OECD only
oecd_countries <- c(
  "Australia","Austria","Belgium","Canada","Chile","Colombia","Costa Rica",
  "Czech Republic","Denmark","Estonia","Finland","France","Germany","Greece",
  "Hungary","Iceland","Ireland","Israel","Italy","Japan","South Korea",
  "Latvia","Lithuania","Luxembourg","Mexico","Netherlands","New Zealand",
  "Norway","Poland","Portugal","Slovakia","Slovenia","Spain","Sweden",
  "Switzerland","Turkey","United Kingdom","United States"
)

df_oecd <- df %>%
  filter(country %in% oecd_countries)

# 5) Scatter OECD only
p_scatter <- ggplot(df_oecd, aes(x = lgdp, y = spi)) +
  geom_point(alpha = 0.8, size = 2) +
  geom_text_repel(aes(label = country), size = 3, max.overlaps = 60, box.padding = 0.3) +
  labs(
    title = "Ln of GDP vs Social Progress Index (2025, OECD only)",
    x = "Ln of GDP per capita (2025, USD)",
    y = "Social Progress Index (2025)"
  ) +
  scale_x_continuous(breaks = pretty_breaks(8)) +
  scale_y_continuous(breaks = seq(60, 100, by = 5),
                     limits = c(60, 100),
                     expand = c(0, 0)) +
  theme_classic(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 16))

ggsave("scatter_lgdp_spi_2025_oecd.png", p_scatter,
       width = 24, height = 18, dpi = 300, limitsize = FALSE)

# 6) Linear regression (OECD only, no equation text)
p_with_lm <- p_scatter +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.9, fullrange = FALSE)

ggsave("scatter_lgdp_spi_2025_oecd_lm.png", p_with_lm,
       width = 24, height = 18, dpi = 300, limitsize = FALSE)
