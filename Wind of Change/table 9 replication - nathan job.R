# ============================
# Table 9 (R translation from STATA)
# ============================

# Packages
required <- c("tidyverse", "data.table", "haven", "fixest", "modelsummary")
to_install <- setdiff(required, installed.packages()[, "Package"])
if (length(to_install)) install.packages(to_install, repos = "https://cloud.r-project.org")
invisible(lapply(required, library, character.only = TRUE))

# ---- Paths (edit to your replication root) ----
REPLICATIONroot <- "/Users/fangxiaoling/Documents/Nathan Nunn RA/Wind of Change/"
setwd(REPLICATIONroot)

# Short helpers
`%ni%` <- Negate(`%in%`)
yrs <- 1845:1905

# ============================
# 1) Build 1870 trade shares (TEMP1)
# ============================
country_level <- read_dta(file.path("PUBLIC_DATA", "COUNTRY_LEVEL_PUBLIC.dta")) %>%
  zap_labels()

temp1 <- country_level %>%
  filter(year == 1870) %>%
  mutate(
    TOTAL_TRADE = coalesce(IMPORT_cd + EXPORT_cd,
                           2*IMPORT_cd,
                           2*EXPORT_cd),
    # avoid double counting Australia (per Stata)
    mean_trade_usable = if_else(country != "Australia", TOTAL_TRADE, NA_real_)
  ) %>%
  filter(!is.na(mean_trade_usable)) %>%
  arrange(desc(mean_trade_usable)) %>%
  mutate(
    large_country_top1 = as.integer(row_number() <= 1),
    large_country_top3 = as.integer(row_number() <= 3),
    large_country_top5 = as.integer(row_number() <= 5)
  ) %>%
  select(country, mean_trade_usable, starts_with("large_country_top"))

# ============================
# 2) Gravity regression to get period-by-technology betas & year FE
# ============================
bilat_trade <- read_dta(file.path("PUBLIC_DATA", "BILATERAL_TRADE_PUBLIC2.dta")) %>% zap_labels()

# Build the gravity formula exactly as in Stata
f_rhs <- paste(
  sprintf("c(d6_%s)*c(lsail2) + c(d6_%s)*c(lsteam)",
          c("60","65","70","75","80","85","90","95","00"),
          c("60","65","70","75","80","85","90","95","00")),
  collapse = " + "
)

# dummy_Y*, dummy_ORI*, dummy_DES*  -> treat as factors
bilat_trade <- bilat_trade %>%
  mutate(
    across(starts_with("dummy_Y"), as.numeric),
    across(starts_with("dummy_ORI"), as.numeric),
    across(starts_with("dummy_DES"), as.numeric)
  )

# We’ll use fixest with linear index of these “*” dummies already materialized.
# If your dataset holds dummies as 0/1 columns dummy_Y1..dummy_Y56, etc., fixest can include them directly.
# Build formula with all dummy columns present:
ys  <- grep("^dummy_Y\\d+$", names(bilat_trade), value = TRUE)
ori <- grep("^dummy_ORI\\d+$", names(bilat_trade), value = TRUE)
des <- grep("^dummy_DES\\d+$", names(bilat_trade), value = TRUE)

f <- as.formula(
  paste("lexpr ~",
        paste(f_rhs,
              paste(ys, collapse = " + "),
              paste(ori, collapse = " + "),
              paste(des, collapse = " + "),
              sep = " + "))
)

mod_grav <- feols(f, data = bilat_trade, notes = FALSE)

# Extract period betas
get_beta <- function(period, tech) {
  # c.d6_60#c.lsail2 in Stata corresponds to interaction c(d6_60)*c(lsail2)
  # fixest builds standard coefficients with name "c(d6_60):c(lsail2)"
  nm <- sprintf("c(d6_%s):c(%s)", period, tech)
  coef(mod_grav)[nm]
}
b <- list(
  `60sa` = get_beta("60", "lsail2"), `60st` = get_beta("60", "lsteam"),
  `65sa` = get_beta("65", "lsail2"), `65st` = get_beta("65", "lsteam"),
  `70sa` = get_beta("70", "lsail2"), `70st` = get_beta("70", "lsteam"),
  `75sa` = get_beta("75", "lsail2"), `75st` = get_beta("75", "lsteam"),
  `80sa` = get_beta("80", "lsail2"), `80st` = get_beta("80", "lsteam"),
  `85sa` = get_beta("85", "lsail2"), `85st` = get_beta("85", "lsteam"),
  `90sa` = get_beta("90", "lsail2"), `90st` = get_beta("90", "lsteam"),
  `95sa` = get_beta("95", "lsail2"), `95st` = get_beta("95", "lsteam"),
  `00sa` = get_beta("00", "lsail2"), `00st` = get_beta("00", "lsteam")
)

# Map year FE coefficients: dummy_Y1..dummy_Y56 correspond to 1845..1900
yy_coefs <- coef(mod_grav)[ys]
year_map <- tibble(
  y_dummy = ys,
  coef    = as.numeric(yy_coefs),
  year    = 1845 + as.integer(str_remove(ys, "dummy_Y")) - 1
)
# years 1901–1905 take the 1900 FE (as in Stata)
year_map <- bind_rows(
  year_map,
  tibble(
    y_dummy = paste0("dummy_Y56"),
    coef = year_map$coef[year_map$year == 1900],
    year = 1901:1905
  )
) %>%
  distinct(year, .keep_all = TRUE) %>%
  arrange(year)

# ============================
# 3) Build bilateral panel of travel times & merge TEMP1
# ============================
bilat_dist <- read_dta(file.path("PUBLIC_DATA", "BILATERAL_DISTANCES_PUBLIC.dta")) %>%
  zap_labels() %>%
  as_tibble()

# Expand to years 1845..1905 (69 years)
bilat_panel <- bilat_dist %>%
  mutate(row_id = row_number()) %>%
  tidyr::uncount(length(yrs), .remove = FALSE) %>%
  group_by(row_id) %>%
  mutate(year = yrs) %>%
  ungroup() %>%
  select(-row_id)

bilat_panel <- bilat_panel %>%
  mutate(
    lsteam = if_else(year < 1870, log(TIME_4_2), log(TIME_4_1)),
    lsail2 = if_else(year < 1870, log(TIME_5_2_5), log(TIME_5_1_5))
  ) %>%
  rename(country_o = country_o, country_d = country_d) %>%
  filter(year <= 1905, country_o != "")

# Merge TEMP1 for weights (by destination country)
bilat_panel <- bilat_panel %>%
  left_join(temp1 %>% select(country, mean_trade_usable, starts_with("large_country_top")),
            by = c("country_d" = "country"))

# Global normalization (per origin-year)
weights_tbl <- bilat_panel %>%
  filter(year == 1870, country_o != country_d) %>%
  group_by(year, country_o) %>%
  summarise(global_trade2 = sum(mean_trade_usable, na.rm = TRUE), .groups = "drop") %>%
  group_by(country_o) %>%
  summarise(global_trade3 = mean(global_trade2, na.rm = TRUE), .groups = "drop")

bilat_panel <- bilat_panel %>%
  left_join(weights_tbl, by = "country_o") %>%
  mutate(
    # NOTE: Stata line used mean_trade/global_trade3; the variable present in TEMP1 is mean_trade_usable.
    weight = mean_trade_usable / global_trade3
  )

# Year FE (dummy_year) mapped from gravity year FE
bilat_panel <- bilat_panel %>%
  left_join(year_map %>% select(year, fe_year = coef), by = "year")

# Predicted bilateral trade with 5-year technology windows
pick_beta <- function(y, sa_or_st) {
  if (y <= 1860) b[[paste0("60", sa_or_st)]]
  else if (y <= 1865) b[[paste0("65", sa_or_st)]]
  else if (y <= 1870) b[[paste0("70", sa_or_st)]]
  else if (y <= 1875) b[[paste0("75", sa_or_st)]]
  else if (y <= 1880) b[[paste0("80", sa_or_st)]]
  else if (y <= 1885) b[[paste0("85", sa_or_st)]]
  else if (y <= 1890) b[[paste0("90", sa_or_st)]]
  else if (y <= 1895) b[[paste0("95", sa_or_st)]]
  else b[[paste0("00", sa_or_st)]]
}

bilat_panel <- bilat_panel %>%
  rowwise() %>%
  mutate(
    beta_sa = pick_beta(year, "sa"),
    beta_st = pick_beta(year, "st"),
    lpred_trad_5ys = beta_sa * lsail2 + beta_st * lsteam
  ) %>%
  ungroup()

# Aggregate to predicted total trade for each origin–year, add year FE
pred_country_year <- bilat_panel %>%
  group_by(country = country_o, year) %>%
  summarise(lpred_TOTAL_trad_5ys = sum(weight * lpred_trad_5ys, na.rm = TRUE),
            fe_year = dplyr::first(fe_year),
            .groups = "drop") %>%
  mutate(lpred_TOTAL_trad_5ys = lpred_TOTAL_trad_5ys + fe_year) %>%
  filter(year %in% c(1845,1850,1855,1860,1865,1870,1875,1880,1885,1890,1895,1900,1905))

# ============================
# 4) Prepare country–year panel for Table 9
# ============================
panel <- country_level %>%
  left_join(pred_country_year, by = c("country", "year")) %>%
  mutate(
    ln_GDPpc     = log(GDPpc),
    lpopulation  = log(population * 1000),
    ln_exportpc  = log(EXPORT_cd) - lpopulation,
    ln_exportGDP = ln_exportpc - ln_GDPpc
  )

# Weights: population in 1860 (origin-country specific)
pop1860 <- panel %>%
  filter(year == 1860) %>%
  group_by(country) %>%
  summarise(population_1860 = sum(population, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    population_1860 = if_else(population_1860 == 0, NA_real_, population_1860),
    lpopulation_1860 = log(population_1860)
  )

panel <- panel %>%
  left_join(pop1860, by = "country")

# Filter to “informative” panels: at least one obs pre-1870 AND post-1870 for ln_exportGDP
pre_counts <- panel %>%
  filter(year <= 1870) %>%
  group_by(country) %>%
  summarise(pre_n = sum(!is.na(ln_exportGDP)), .groups = "drop")
post_counts <- panel %>%
  filter(year > 1870) %>%
  group_by(country) %>%
  summarise(post_n = sum(!is.na(ln_exportGDP)), .groups = "drop")

panel <- panel %>%
  left_join(pre_counts, by = "country") %>%
  left_join(post_counts, by = "country") %>%
  filter(!(coalesce(pre_n, 0) == 0 & coalesce(post_n, 0) == 0)) %>%
  filter(coalesce(pre_n, 0) > 0 & coalesce(post_n, 0) > 0)

# FE identifiers for clustering
panel <- panel %>%
  mutate(
    ctry = factor(country),
    yr   = factor(year)
  )

# Interactions with initial institutions
panel <- panel %>%
  mutate(
    trade_by_initial  = ln_exportGDP * cons1860,
    trade2_by_initial = ln_exportpc * cons1860,
    instr_by_initial  = lpred_TOTAL_trad_5ys * cons1860
  )

# ============================
# 5) Regressions for Table 9
#    - country & year FE via fixest FEs
#    - two-way clustered SEs by (country, year)
#    - population_1860 weights (analytic weights analog)
# ============================

# Weight: use population_1860 as analytic weights (match Stata [aweight = lpopulation_1860] conceptually)
# Stata uses [aweight=lpopulation_1860]; there lpopulation_1860 is log(population_1860).
panel <- panel %>%
  mutate(w_a = lpopulation_1860)
weights = ~ w_a

# Fixest formulae
# OLS with interaction
m_b1 <- feols(ln_GDPpc ~ ln_exportGDP + trade_by_initial | ctry + yr,
              data = panel, weights = ~ w_a,
              cluster = ~ ctry + yr)

# IV: (ln_exportGDP, trade_by_initial) instrumented by (lpred_TOTAL_trad_5ys, instr_by_initial)
m_b2 <- feols(ln_GDPpc ~ 1 | ctry + yr,
              iv = ~ ln_exportGDP + trade_by_initial ~ lpred_TOTAL_trad_5ys + instr_by_initial,
              data = panel, weights = ~ w_a,
              cluster = ~ ctry + yr)

# IV with per-capita exports (drop rows w/ missing ln_exportGDP per Stata)
panel_b3 <- panel %>% filter(!is.na(ln_exportGDP))
m_b3 <- feols(ln_GDPpc ~ 1 | ctry + yr,
              iv = ~ ln_exportpc + trade2_by_initial ~ lpred_TOTAL_trad_5ys + instr_by_initial,
              data = panel_b3, weights = ~ w_a,
              cluster = ~ ctry + yr)

# Population as DV
m_b4 <- feols(lpopulation ~ ln_exportGDP + trade_by_initial | ctry + yr,
              data = panel, weights = ~ w_a,
              cluster = ~ ctry + yr)

m_b5 <- feols(lpopulation ~ 1 | ctry + yr,
              iv = ~ ln_exportGDP + trade_by_initial ~ lpred_TOTAL_trad_5ys + instr_by_initial,
              data = panel, weights = ~ w_a,
              cluster = ~ ctry + yr)

panel_b6 <- panel %>% filter(!is.na(ln_exportGDP))
m_b6 <- feols(lpopulation ~ 1 | ctry + yr,
              iv = ~ ln_exportpc + trade2_by_initial ~ lpred_TOTAL_trad_5ys + instr_by_initial,
              data = panel_b6, weights = ~ w_a,
              cluster = ~ ctry + yr)



# ============================
# 6) Export Table 9 to LaTeX
# ============================

# Ensure helpers are available (modelsummary relies on these to tidy fixest)
if (!requireNamespace("broom", quietly = TRUE)) install.packages("broom")
if (!requireNamespace("parameters", quietly = TRUE)) install.packages("parameters")
suppressPackageStartupMessages({
  library(broom)
  library(parameters)
})

outfile <- "Table9.tex"

make_table_with_modelsummary <- function() {
  modelsummary::msummary(
    list(`(1)` = m_b1, `(2)` = m_b2, `(3)` = m_b3, `(4)` = m_b4, `(5)` = m_b5, `(6)` = m_b6),
    estimate  = "{estimate}",
    statistic = "({std.error})",
    stars     = c('*' = .10, '**' = .05, '***' = .01),
    gof_omit  = "IC|RMSE",
    add_rows  = tibble::tibble(
      term = c("Country FE", "Year FE", "Clusters"),
      `(1)` = c("Yes", "Yes", "Country & Year"),
      `(2)` = c("Yes", "Yes", "Country & Year"),
      `(3)` = c("Yes", "Yes", "Country & Year"),
      `(4)` = c("Yes", "Yes", "Country & Year"),
      `(5)` = c("Yes", "Yes", "Country & Year"),
      `(6)` = c("Yes", "Yes", "Country & Year")
    ),
    title  = "Table 9 — Trade and development: the role of local institutions",
    output = outfile
  )
}

make_table_with_etable <- function() {
  tb <- fixest::etable(
    `(1)` = m_b1, `(2)` = m_b2, `(3)` = m_b3, `(4)` = m_b4, `(5)` = m_b5, `(6)` = m_b6,
    se.below    = TRUE,
    tex         = TRUE,
    digits      = 3,
    signif.code = c("***"=0.01,"**"=0.05,"*"=0.10),
    # drop FE dummies from printout
    drop        = c("^ctry::", "^yr::"),
    # nicer labels
    dict        = c(
      "ln_exportGDP"      = "log export/GDP",
      "trade_by_initial"  = "log export/GDP × exec constraints",
      "ln_exportpc"       = "log export/pop",
      "trade2_by_initial" = "log export/pop × exec constraints"
    ),
    fitstat     = ~ n + wald + r2
  )
  
  # Wrap in a table env and write to TeX
  lines <- c(
    "\\begin{table}[!htbp]\\centering",
    "\\caption{Table 9 — Trade and development: the role of local institutions}",
    tb,
    "\\end{table}"
  )
  writeLines(lines, con = outfile)
}

# Try modelsummary; if it errors, fall back to etable
ok <- TRUE
tryCatch({
  make_table_with_modelsummary()
}, error = function(e) {
  message("modelsummary failed; falling back to fixest::etable. Reason:\n", e$message)
  ok <<- FALSE
})
if (!ok) make_table_with_etable()

cat("Done. Wrote Table9.tex to:\n", file.path(getwd(), outfile), "\n")

## table 9 should be generated already, then, i do the job
## i have double checked, the replication is successful
# ============================
# 7) Country-specific effects & plots (with cons1860 in labels)
# ============================
suppressPackageStartupMessages({
  library(forcats)
  library(ggplot2)
  library(readr)
  library(dplyr)
})

# Helper: delta-method for b1 + Z*b3 with clustered vcov from fixest
compute_effects <- function(model, df_ctry, b1_name, b3_name) {
  beta <- coef(model)
  V    <- vcov(model)
  
  df_ctry %>%
    mutate(
      est = beta[[b1_name]] + Z * beta[[b3_name]],
      var = V[b1_name, b1_name] +
        (Z^2) * V[b3_name, b3_name] +
        2 * Z * V[b1_name, b3_name],
      se  = sqrt(pmax(var, 0)),
      lo  = est - 1.96 * se,
      hi  = est + 1.96 * se
    ) %>%
    arrange(desc(est))
}

# ---------- A) ln_exportGDP spec ----------
Z_gdp <- panel %>%
  distinct(country, Z = cons1860) %>%
  filter(!is.na(Z)) %>%
  mutate(
    Zlab = ifelse(is.na(Z), "NA", as.character(as.integer(round(Z)))),
    country_lab = sprintf("%s (%s)", country, Zlab)
  )

effects_col1 <- compute_effects(
  model   = m_b1,
  df_ctry = Z_gdp,
  b1_name = "ln_exportGDP",
  b3_name = "trade_by_initial"
) %>%
  mutate(country_lab = fct_reorder(country_lab, est))

p_col1 <- ggplot(effects_col1, aes(x = est, y = country_lab)) +
  geom_point(size = 1.8) +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0.15, linewidth = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    title = "Effect of ln(export/GDP) on ln(GDP per capita), by country (OLS; Table 9 col 1)",
    subtitle = "Dot = estimate; line = 95% CI; countries ordered by estimate\n(cons1860 shown in parentheses)",
    x = "d ln(GDPpc) / d ln(export/GDP)",
    y = NULL
  ) +
  theme_classic(base_size = 12) +
  theme(
    panel.grid       = element_blank(),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

# Save outputs (include cons1860 in CSV)
write_csv(
  effects_col1 %>%
    select(country, cons1860 = Z, est, se, lo, hi) %>%
    arrange(desc(est)),
  "country_effects_OLS_ln_exportGDP.csv"
)

ggsave(
  "country_effects_OLS_ln_exportGDP.png",
  p_col1,
  width = 10,
  height = max(6, nrow(effects_col1) * 0.18),
  dpi = 300
)

cat("\nCountry-specific effects saved as:\n",
    "- country_effects_OLS_ln_exportGDP.csv/.png (Table 9 col 1, labels show cons1860)\n", sep = "")
