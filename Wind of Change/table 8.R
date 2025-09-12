# ============================
# Table 8: Trade and Economic Divergence
# ============================

# ---- Setup ----
required <- c("tidyverse", "fixest", "haven", "modelsummary")
to_install <- setdiff(required, installed.packages()[, "Package"])
if (length(to_install)) install.packages(to_install)
invisible(lapply(required, library, character.only = TRUE))

REPLICATIONroot <- "/Users/fangxiaoling/Documents/Nathan Nunn RA/Wind of Change/"
setwd(REPLICATIONroot)

# ---- 1. Load TEMP100 ----
df <- read_dta(file.path(REPLICATIONroot,"TEMP100.dta")) %>%
  zap_labels()

# ---- 2. 1850 baseline GDPpc ----
df <- df %>%
  group_by(country) %>%
  mutate(
    GDPpc_1850 = min(ifelse(year == 1850, GDPpc, NA), na.rm = TRUE),
    lGDPpc_1850 = log(GDPpc_1850),
    perc_lGDPpc_1850 = ntile(lGDPpc_1850, 100)
  ) %>%
  ungroup()

mean_gdp_1850 <- mean(df$lGDPpc_1850[df$year == 1850], na.rm = TRUE)

df <- df %>%
  mutate(
    above_mean_gdp_1850 = ifelse(!is.na(perc_lGDPpc_1850) & lGDPpc_1850 >= mean_gdp_1850, 1, 0),
    above_75_gdp_1850   = ifelse(!is.na(perc_lGDPpc_1850) & perc_lGDPpc_1850 > 75, 1, 0)
  )

# ---- 3. Interaction variables ----
df <- df %>%
  mutate(
    instr_by_above_mean_gdp_1850  = lpred_TOTAL_trad_5ys * above_mean_gdp_1850,
    trade_by_above_mean_gdp_1850  = ln_exportGDP         * above_mean_gdp_1850,
    trade2_by_above_mean_gdp_1850 = ln_exportpc          * above_mean_gdp_1850,
    
    instr_by_above_p75_gdp_1850   = lpred_TOTAL_trad_5ys * above_75_gdp_1850,
    trade_by_above_p75_gdp_1850   = ln_exportGDP         * above_75_gdp_1850,
    trade2_by_above_p75_gdp_1850  = ln_exportpc          * above_75_gdp_1850
  )


#pt4
library(ivreg)

m1 <- ivreg(
  ln_GDPpc ~ ln_exportGDP + trade_by_above_mean_gdp_1850 + factor(country) + factor(year) |
    lpred_TOTAL_trad_5ys + instr_by_above_mean_gdp_1850 + factor(country) + factor(year),
  data = df
)

m2 <- ivreg(
  ln_GDPpc ~ ln_exportGDP + trade_by_above_mean_gdp_1850 + factor(country) + factor(year) |
    lpred_TOTAL_trad_5ys + instr_by_above_mean_gdp_1850 + factor(country) + factor(year),
  data = df, weights = lpopulation_1860
)

m3 <- ivreg(
  ln_GDPpc ~ ln_exportGDP + trade_by_above_p75_gdp_1850 + factor(country) + factor(year) |
    lpred_TOTAL_trad_5ys + instr_by_above_p75_gdp_1850 + factor(country) + factor(year),
  data = df
)

m4 <- ivreg(
  ln_GDPpc ~ ln_exportGDP + trade_by_above_p75_gdp_1850 + factor(country) + factor(year) |
    lpred_TOTAL_trad_5ys + instr_by_above_p75_gdp_1850 + factor(country) + factor(year),
  data = df, weights = lpopulation_1860
)

m5 <- ivreg(
  ln_GDPpc ~ ln_exportpc + trade2_by_above_mean_gdp_1850 + factor(country) + factor(year) |
    lpred_TOTAL_trad_5ys + instr_by_above_mean_gdp_1850 + factor(country) + factor(year),
  data = df %>% filter(!is.na(ln_exportGDP))
)

m6 <- ivreg(
  ln_GDPpc ~ ln_exportpc + trade2_by_above_mean_gdp_1850 + factor(country) + factor(year) |
    lpred_TOTAL_trad_5ys + instr_by_above_mean_gdp_1850 + factor(country) + factor(year),
  data = df %>% filter(!is.na(ln_exportGDP)), weights = lpopulation_1860
)

m7 <- ivreg(
  ln_GDPpc ~ ln_exportpc + trade2_by_above_p75_gdp_1850 + factor(country) + factor(year) |
    lpred_TOTAL_trad_5ys + instr_by_above_p75_gdp_1850 + factor(country) + factor(year),
  data = df %>% filter(!is.na(ln_exportGDP))
)

m8 <- ivreg(
  ln_GDPpc ~ ln_exportpc + trade2_by_above_p75_gdp_1850 + factor(country) + factor(year) |
    lpred_TOTAL_trad_5ys + instr_by_above_p75_gdp_1850 + factor(country) + factor(year),
  data = df %>% filter(!is.na(ln_exportGDP)), weights = lpopulation_1860
)

# ---- 5. Export LaTeX table ----
# ============================
# Table 8 Export
# ============================

library(modelsummary)
library(broom)

models <- list(
  "(1)" = m1, "(2)" = m2, "(3)" = m3, "(4)" = m4,
  "(5)" = m5, "(6)" = m6, "(7)" = m7, "(8)" = m8
)

# Custom labels to match paper
coef_map <- c(
  "ln_exportGDP"                     = "log export/GDP",
  "ln_exportpc"                      = "log export/population",
  "trade_by_above_mean_gdp_1850"     = "log export/GDP × above mean GDP 1850",
  "trade_by_above_p75_gdp_1850"      = "log export/GDP × above top 25pc GDP 1850",
  "trade2_by_above_mean_gdp_1850"    = "log export/population × above mean GDP 1850",
  "trade2_by_above_p75_gdp_1850"     = "log export/population × above top 25pc GDP 1850"
)

# Additional rows like "Country dummies", "Year dummies", "F", "Observations", "Weighted"
extra_rows <- tibble::tibble(
  term = c("Country dummies", "Year dummies", "F", "Observations", "Weighted"),
  `(1)` = c("Yes", "Yes", round(summary(m1)$diagnostics[1,2], 3), nobs(m1), "No"),
  `(2)` = c("Yes", "Yes", round(summary(m2)$diagnostics[1,2], 3), nobs(m2), "Yes"),
  `(3)` = c("Yes", "Yes", round(summary(m3)$diagnostics[1,2], 3), nobs(m3), "No"),
  `(4)` = c("Yes", "Yes", round(summary(m4)$diagnostics[1,2], 3), nobs(m4), "Yes"),
  `(5)` = c("Yes", "Yes", round(summary(m5)$diagnostics[1,2], 3), nobs(m5), "No"),
  `(6)` = c("Yes", "Yes", round(summary(m6)$diagnostics[1,2], 3), nobs(m6), "Yes"),
  `(7)` = c("Yes", "Yes", round(summary(m7)$diagnostics[1,2], 3), nobs(m7), "No"),
  `(8)` = c("Yes", "Yes", round(summary(m8)$diagnostics[1,2], 3), nobs(m8), "Yes")
)

# Export to LaTeX
modelsummary(
  models,
  stars = c('*' = .10, '**' = .05, '***' = .01),
  coef_map = coef_map,
  gof_omit = "AIC|BIC|RMSE|R2|Log.Lik",
  add_rows = extra_rows,
  output = "table8.tex",
  title = "Table 8---Trade and Economic Divergence"
)

# Optional: also export CSV for debugging
msummary(
  models,
  coef_map = coef_map,
  gof_omit = "AIC|BIC|RMSE|R2|Log.Lik",
  add_rows = extra_rows,
  output = "table8.csv"
)
