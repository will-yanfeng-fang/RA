# ---- setup ----
install.packages(c("tidyverse", "readxl", "ggrepel", "scales"))  # run once
library(tidyverse)
library(readxl)
library(ggrepel)
library(scales)

setwd("/Users/fangxiaoling/Documents/Nathan Nunn RA/Book/SPI - graph and regression/")

# 1) Load data
df <- read_excel("data.xlsx") |>
  select(code, country, SPI_2025, GDPpc_USD_2025) |>
  rename(spi = SPI_2025, gdp = GDPpc_USD_2025) |>
  mutate(
    gdp = as.numeric(gdp),
    spi = as.numeric(spi)
  ) |>
  filter(!is.na(country), !is.na(gdp), !is.na(spi))

x_breaks <- seq(0, 120000, by = 20000)
y_breaks <- seq(0, 100, by = 10)

scale_axes <- list(
  scale_x_continuous(breaks = x_breaks, labels = label_k, limits = c(0, 120000)),
  scale_y_continuous(breaks = y_breaks, limits = c(0, 100)),
  coord_fixed(ratio = 0.0005)
)

# 2) Scatter with labels (no fit line)
p_scatter <- ggplot(df, aes(x = gdp, y = spi)) +
  geom_point(alpha = 0.8, size = 2) +
  geom_text_repel(aes(label = country), size = 3, max.overlaps = 60, box.padding = 0.3) +
  labs(
    title = "GDP vs Social Progress Index (2025)",
    x = "GDP per capita (2025, USD)",
    y = "Social Progress Index (2025)"
  ) +
  scale_x_continuous(labels = label_dollar(scale = 1, suffix = "", accuracy = 1),
                     breaks = pretty_breaks(8)) +
  scale_y_continuous(breaks = y_breaks, limits = c(25, 100), expand = c(0, 0)) + 
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 16))

ggsave("scatter_gdp_spi_2025.png", p_scatter, width = 24, height = 18, dpi = 300, limitsize = FALSE)

# 3A) Scatter + linear regression line (what your assignment asks)
p_with_lm <- p_scatter +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.9, fullrange = FALSE)

ggsave("scatter_gdp_spi_2025_lm.png", p_with_lm, width = 24, height = 18, dpi = 300, limitsize = FALSE)