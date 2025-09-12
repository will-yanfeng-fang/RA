# ---- Setup ----
required <- c("tidyverse", "haven", "fixest", "data.table")
to_install <- setdiff(required, installed.packages()[, "Package"])
if (length(to_install)) install.packages(to_install)
invisible(lapply(required, library, character.only = TRUE))

REPLICATIONroot <- "/Users/fangxiaoling/Documents/Nathan Nunn RA/Wind of Change/"
setwd(REPLICATIONroot)

# Load raw data
country <- read_dta("PUBLIC_DATA/COUNTRY_LEVEL_PUBLIC.dta") %>% zap_labels()
bilat   <- read_dta("PUBLIC_DATA/BILATERAL_TRADE_PUBLIC2.dta") %>% zap_labels()
dist    <- read_dta("PUBLIC_DATA/BILATERAL_DISTANCES_PUBLIC.dta") %>% zap_labels()

# Gravity regression (simplified translation)
gravity <- lm(
  lexpr ~ d6_60:lsail2 + d6_60:lsteam + d6_65:lsail2 + d6_65:lsteam +
    d6_70:lsail2 + d6_70:lsteam + d6_75:lsail2 + d6_75:lsteam +
    d6_80:lsail2 + d6_80:lsteam + d6_85:lsail2 + d6_85:lsteam +
    d6_90:lsail2 + d6_90:lsteam + d6_95:lsail2 + d6_95:lsteam +
    d6_00:lsail2 + d6_00:lsteam +
    factor(year) + factor(country_o) + factor(country_d),
  data = bilat
)

coefs <- coef(gravity)

# Expand bilateral distance panel to 1845–1905
dist_expanded <- dist %>%
  group_by(country_o, country_d) %>%
  tidyr::uncount(weights = 61, .id = "t") %>%
  mutate(year = 1845 - 1 + row_number()) %>%
  ungroup() %>%
  mutate(
    lsteam = if_else(year < 1870, log(TIME_4_2), log(TIME_4_1)),
    lsail2 = if_else(year < 1870, log(TIME_5_2_5), log(TIME_5_1_5))
  )

# Predicted bilateral trade based on estimated coefficients
dist_expanded <- dist_expanded %>%
  mutate(
    lpred_trad_5ys = case_when(
      year <= 1860 ~ coefs["d6_60:lsail2"] * lsail2 + coefs["d6_60:lsteam"] * lsteam,
      year <= 1865 ~ coefs["d6_65:lsail2"] * lsail2 + coefs["d6_65:lsteam"] * lsteam,
      year <= 1870 ~ coefs["d6_70:lsail2"] * lsail2 + coefs["d6_70:lsteam"] * lsteam,
      year <= 1875 ~ coefs["d6_75:lsail2"] * lsail2 + coefs["d6_75:lsteam"] * lsteam,
      year <= 1880 ~ coefs["d6_80:lsail2"] * lsail2 + coefs["d6_80:lsteam"] * lsteam,
      year <= 1885 ~ coefs["d6_85:lsail2"] * lsail2 + coefs["d6_85:lsteam"] * lsteam,
      year <= 1890 ~ coefs["d6_90:lsail2"] * lsail2 + coefs["d6_90:lsteam"] * lsteam,
      year <= 1895 ~ coefs["d6_95:lsail2"] * lsail2 + coefs["d6_95:lsteam"] * lsteam,
      year <= 1905 ~ coefs["d6_00:lsail2"] * lsail2 + coefs["d6_00:lsteam"] * lsteam,
      TRUE ~ NA_real_
    )
  )

trade1870 <- country %>%
  filter(year == 1870) %>%
  mutate(
    TOTAL_TRADE = case_when(
      !is.na(IMPORT_cd) & !is.na(EXPORT_cd) ~ IMPORT_cd + EXPORT_cd,
      is.na(EXPORT_cd) & !is.na(IMPORT_cd) ~ 2 * IMPORT_cd,
      is.na(IMPORT_cd) & !is.na(EXPORT_cd) ~ 2 * EXPORT_cd,
      TRUE ~ NA_real_
    ),
    mean_trade_usable = ifelse(country != "Australia", TOTAL_TRADE, NA_real_)
  ) %>%
  filter(!is.na(mean_trade_usable)) %>%
  arrange(desc(mean_trade_usable)) %>%
  group_by(country) %>%
  mutate(
    global_trade2 = sum(mean_trade_usable, na.rm = TRUE),
    global_trade3 = mean(global_trade2, na.rm = TRUE),
    weight = mean_trade_usable / global_trade3
  ) %>%
  ungroup() %>%
  select(country, weight)

# Merge weights into bilateral panel
dist_expanded <- dist_expanded %>%
  left_join(trade1870, by = c("country_d" = "country"))

# Aggregate to country–year instrument
pred_trade <- dist_expanded %>%
  group_by(country_o, year) %>%
  summarise(
    lpred_TOTAL_trad_5ys = sum(weight * lpred_trad_5ys, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(country = country_o)

df <- country %>%
  left_join(pred_trade, by = c("country", "year")) %>%
  mutate(
    ln_GDPpc = log(GDPpc),
    lpopulation = log(population * 1000),
    ln_exportpc = log(EXPORT_cd) - lpopulation,
    ln_exportGDP = ln_exportpc - ln_GDPpc,
    l1urban_50pc = log(1 + urban_50pc),
    l1urban_100pc = log(1 + urban_100pc),
    SHARE_NONAGRI_EXP = SHARE_MAN_EXP + SHARE_COM_EXP,
    lSHARE_NONAGRI_EXP = log(1 + SHARE_NONAGRI_EXP)
  ) %>%
  group_by(country) %>%
  mutate(
    population_1860 = if (any(year == 1860 & !is.na(population))) {
      population[year == 1860][1]   # take that year’s value
    } else {
      NA_real_                       # no 1860 data → NA
    },
    lpopulation_1860 = ifelse(!is.na(population_1860),
                              log(population_1860), NA_real_)
  ) %>%
  ungroup()

write_dta(df, "TEMP100.dta")
