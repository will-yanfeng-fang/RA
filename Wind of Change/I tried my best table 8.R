# ============================
# Country-level effects (Table 8, models 1-8)
# ============================

# ---- Packages ----
required <- c("tidyverse", "patchwork")
to_install <- setdiff(required, installed.packages()[, "Package"])
if (length(to_install)) install.packages(to_install, repos = "https://cloud.r-project.org")
invisible(lapply(required, library, character.only = TRUE))

setwd("/Users/fangxiaoling/Documents/Nathan Nunn RA/Wind of Change/")

# ---- Step 1: Table 8 coefficients ----
table8 <- tibble::tibble(
  model = 1:8,
  trade_var = c(rep("ln(export/GDP)", 4), rep("ln(export/pop)", 4)),
  b1 = c(-0.400, -0.395, -0.241, -0.219, -0.419, -0.407, -0.260, -0.227),
  se1 = c(0.180, 0.182, 0.0961, 0.0987, 0.217, 0.219, 0.122, 0.122),
  b_int = c(NA, NA, 0.248, 0.161, NA, NA, 0.185, 0.110),
  se_int = c(NA, NA, 0.143, 0.0691, NA, NA, 0.114, 0.0512),
  interaction = c(NA, NA, "Above mean GDP 1850", "Above top 25% GDP 1850",
                  NA, NA, "Above mean GDP 1850", "Above top 25% GDP 1850")
)

# ---- Step 2: Country classification (1850 GDPpc) ----
library(haven)
df <- read_dta("/Users/fangxiaoling/Documents/Nathan Nunn RA/Wind of Change/PUBLIC_DATA/COUNTRY_LEVEL_PUBLIC.dta") %>%
  zap_labels()
country_1850 <- df %>%
  filter(year == 1850) %>%
  group_by(country) %>%
  summarise(GDPpc_1850 = mean(GDPpc, na.rm = TRUE), .groups = "drop")

# thresholds
mean_gdp_1850 <- mean(country_1850$GDPpc_1850, na.rm = TRUE)
p75_gdp_1850  <- quantile(country_1850$GDPpc_1850, 0.75, na.rm = TRUE)

country_1850 <- country_1850 %>%
  mutate(
    above_mean_gdp_1850  = if_else(GDPpc_1850 >= mean_gdp_1850, 1, 0),
    above_top25_gdp_1850 = if_else(GDPpc_1850 >= p75_gdp_1850, 1, 0)
  )

# ---- Step 3: Compute effects for all models ----
all_effects <- list()

for (i in seq_len(nrow(table8))) {
  row <- table8[i, ]
  b1 <- row$b1; b_int <- row$b_int
  se1 <- row$se1; se_int <- row$se_int
  
  if (!is.na(row$interaction)) {
    dvar <- if (row$interaction == "Above mean GDP 1850") {
      "above_mean_gdp_1850"
    } else {
      "above_top25_gdp_1850"
    }
    tmp <- country_1850 %>%
      mutate(
        model = row$model,
        trade_var = row$trade_var,
        interaction = row$interaction,
        effect = b1 + b_int * .data[[dvar]],
        se = sqrt(se1^2 + (se_int * .data[[dvar]])^2), # rough approx
        ci_low = effect - 1.96 * se,
        ci_high = effect + 1.96 * se
      )
  } else {
    tmp <- country_1850 %>%
      mutate(
        model = row$model,
        trade_var = row$trade_var,
        interaction = NA,
        effect = b1,
        se = se1,
        ci_low = effect - 1.96 * se,
        ci_high = effect + 1.96 * se
      )
  }
  all_effects[[i]] <- tmp
}

effects_df <- bind_rows(all_effects)

# ---- Step 4: Plot function ----
make_plot <- function(m) {
  pd <- effects_df %>%
    filter(model == m) %>%
    distinct(country, .keep_all = TRUE) %>%
    arrange(effect) %>%
    mutate(country = factor(country, levels = unique(country)))
  
  p <- ggplot(pd, aes(x = effect, y = country)) +
    geom_point(size = 2) +
    geom_errorbarh(aes(xmin = ci_low, xmax = ci_high), height = 0.2) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    labs(
      title = paste0("Model ", m),
      subtitle = paste0(unique(pd$trade_var),
                        ifelse(all(is.na(pd$interaction)), "",
                               paste0(" × ", unique(na.omit(pd$interaction)))),
                        " (2SLS, Table 8)"),
      x = "Marginal effect of trade",
      y = "Country"
    ) +
    theme_minimal(base_size = 12)
  
  return(p)   # <- Explicit return
}

# ---- Step 5: Generate all 8 plots separately ----
for (m in 1:8) {
  p <- make_plot(m)
  fname <- paste0("Table8_country_effects_model", m, ".png")
  ggsave(fname, plot = p, width = 9, height = 7, dpi = 300)
}

# ---- Step 6 (alternative with cowplot) ----
install.packages("cowplot")   # if not installed
library(cowplot)

plots <- lapply(1:8, make_plot)

final_plot <- plot_grid(
  plots[[1]], plots[[2]],
  plots[[3]], plots[[4]],
  plots[[5]], plots[[6]],
  plots[[7]], plots[[8]],
  ncol = 2, labels = paste0("Model ", 1:8)
)

ggsave("Table8_country_effects_models1-8.png",
       plot = final_plot, width = 18, height = 40, dpi = 300)
