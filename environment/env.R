# ==== Packages ====
suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(ggrepel)
  library(scales)
  library(dplyr)
})

# ==== Path & file ====
setwd("/Users/fangxiaoling/Documents/Nathan Nunn RA/Book/environment/")
dat <- read_excel("GGDP.xlsx")

# ==== EDIT THIS: your indicator column name ====
indicator_col <- "LGGDP2019"   # e.g. "LGGDP2019", "SPI_2025", "NewIndicator"

# ---- Tidy to expected names: country, ind, lgdp (and optional lggdp) ----
df <- dat |>
  rename(
    country   = !!names(dat)[grepl("^country$", names(dat), ignore.case = TRUE)],
    LGDP2019  = !!names(dat)[grepl("^lgdp2019$|^logg?_?gdp.*2025$|^lgdp$", names(dat), ignore.case = TRUE)],
    LGGDP2019 = !!names(dat)[grepl("^lggdp2019$|^log_?green.*gdp", names(dat), ignore.case = TRUE)]
  ) |>
  mutate(
    ind  = suppressWarnings(as.numeric(.data[[indicator_col]])),
    lgdp = suppressWarnings(as.numeric(LGDP2019)),
    lggdp = suppressWarnings(as.numeric(LGGDP2019))
  ) |>
  filter(!is.na(country), !is.na(ind), !is.na(lgdp))

# ==== Axis helpers ====
# If your indicator is 0–100, keep these; otherwise it'll auto-range around your data.
y_min <- floor(min(df$ind, na.rm = TRUE) / 5) * 5
y_max <- ceiling(max(df$ind, na.rm = TRUE) / 5) * 5
y_breaks <- seq(y_min, y_max, by = ifelse(y_max - y_min > 50, 10, 5))

# ==== Theme (white background + grid lines) ====
my_theme <- theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    panel.grid.major = element_line(color = "grey85", linewidth = 0.5),
    panel.grid.minor = element_line(color = "grey90", linewidth = 0.25),
    panel.background = element_rect(fill = "white", colour = NA)
  )

# ==== 1) Scatter: ln(GDPpc) vs Indicator ====
p_scatter <- ggplot(df, aes(x = lgdp, y = ind)) +
  geom_point(alpha = 0.8, size = 2) +
  geom_text_repel(aes(label = country), size = 3, max.overlaps = 60, box.padding = 0.3) +
  labs(
    title = paste0("Ln of GDP vs ", indicator_col),
    x = "Ln of GDP per capita (2019, USD)",
    y = indicator_col
  ) +
  scale_x_continuous(breaks = pretty_breaks(8)) +
  scale_y_continuous(breaks = y_breaks, limits = c(y_min, y_max), expand = c(0, 0)) +
  my_theme

ggsave(paste0("scatter_lgdp_", indicator_col, ".png"),
       p_scatter, width = 24, height = 18, dpi = 300, limitsize = FALSE)

# ==== 2) Add linear fit (SE shaded) ====
p_with_lm <- p_scatter +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.9, fullrange = FALSE)

ggsave(paste0("scatter_lgdp_", indicator_col, "_lm.png"),
       p_with_lm, width = 24, height = 18, dpi = 300, limitsize = FALSE)

# ==== 3) OPTIONAL: use ln(Green GDP) on x-axis if available ====
if (!all(is.na(df$lggdp))) {
  df_g <- df |> filter(!is.na(lggdp))
  
  p_g <- ggplot(df_g, aes(x = lggdp, y = ind)) +
    geom_point(alpha = 0.8, size = 2) +
    geom_text_repel(aes(label = country), size = 3, max.overlaps = 60, box.padding = 0.3) +
    labs(
      title = paste0("Ln of Green GDP vs ", indicator_col),
      x = "Ln of Green GDP per capita (2019, USD)",
      y = indicator_col
    ) +
    scale_x_continuous(breaks = pretty_breaks(8)) +
    scale_y_continuous(breaks = y_breaks, limits = c(y_min, y_max), expand = c(0, 0)) +
    my_theme
  
  ggsave(paste0("scatter_lggdp_", indicator_col, ".png"),
         p_g, width = 24, height = 18, dpi = 300, limitsize = FALSE)
  
  ggsave(paste0("scatter_lggdp_", indicator_col, "_lm.png"),
         p_g + geom_smooth(method = "lm", se = TRUE, linewidth = 0.9),
         width = 24, height = 18, dpi = 300, limitsize = FALSE)
}

# ==== 4) OPTIONAL: OECD-only version (keep if useful) ====
oecd_countries <- c(
  "Australia","Austria","Belgium","Canada","Chile","Colombia","Costa Rica",
  "Czech RepuLGGDP2019c","Denmark","Estonia","Finland","France","Germany","Greece",
  "Hungary","Iceland","Ireland","Israel","Italy","Japan","South Korea",
  "Latvia","Lithuania","Luxembourg","Mexico","Netherlands","New Zealand",
  "Norway","Poland","Portugal","Slovakia","Slovenia","Spain","Sweden",
  "Switzerland","Turkey","United Kingdom","United States"
)

df_oecd <- df |> filter(country %in% oecd_countries)

if (nrow(df_oecd) > 0) {
  y_min_oecd <- floor(min(df_oecd$ind, na.rm = TRUE) / 5) * 5
  y_max_oecd <- ceiling(max(df_oecd$ind, na.rm = TRUE) / 5) * 5
  y_brks_oecd <- seq(y_min_oecd, y_max_oecd, by = ifelse(y_max_oecd - y_min_oecd > 50, 10, 5))
  
  p_oecd <- ggplot(df_oecd, aes(x = lgdp, y = ind)) +
    geom_point(alpha = 0.8, size = 2) +
    geom_text_repel(aes(label = country), size = 3, max.overlaps = 60, box.padding = 0.3) +
    labs(
      title = paste0("Ln of GDP vs ", indicator_col, " (OECD only)"),
      x = "Ln of GDP per capita (2019, USD)",
      y = indicator_col
    ) +
    scale_x_continuous(breaks = pretty_breaks(8)) +
    scale_y_continuous(breaks = y_brks_oecd,
                       limits = c(y_min_oecd, y_max_oecd),
                       expand = c(0, 0)) +
    my_theme
  
  ggsave(paste0("scatter_lgdp_", indicator_col, "_OECD.png"),
         p_oecd, width = 24, height = 18, dpi = 300, limitsize = FALSE)
  
  ggsave(paste0("scatter_lgdp_", indicator_col, "_OECD_lm.png"),
         p_oecd + geom_smooth(method = "lm", se = TRUE, linewidth = 0.9),
         width = 24, height = 18, dpi = 300, limitsize = FALSE)
}
