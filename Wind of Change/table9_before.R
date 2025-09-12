# ============================
# Translation of Stata code into R
# ============================

#i have to do this or i won't have BILATERAL_TRADE_PUBLIC2.dta

library(haven)     # for read_dta, write_dta
library(dplyr)     # for data wrangling
library(stringr)

BeforeREP <- "/Users/fangxiaoling/Documents/Nathan Nunn RA/Wind of Change/"
setwd(BeforeREP)

# ---- Load data ----
bilat <- read_dta("PUBLIC_DATA/BILATERAL_TRADE_PUBLIC.dta") %>%
  zap_labels()   # remove Stata labels

# ---- Generate transformed variables ----
bilat <- bilat %>%
  mutate(
    lexpr   = log(expr),
    lsteam  = if_else(year < 1870, log(TIME_4_2), log(TIME_4_1)),
    lsail2  = if_else(year < 1870, log(TIME_5_2_5), log(TIME_5_1_5)),
    ldist   = log(geo_dist),
    
    # Create 'pair' as concatenated string of countries (order-invariant)
    pair    = if_else(country_d > country_o,
                      paste0(country_d, country_o),
                      paste0(country_o, country_d)),
    
    # Period dummies
    d6_60 = if_else(year <= 1860, 1L, 0L),
    d6_65 = if_else(year <= 1865 & year > 1860, 1L, 0L),
    d6_70 = if_else(year <= 1870 & year > 1865, 1L, 0L),
    d6_75 = if_else(year <= 1875 & year > 1870, 1L, 0L),
    d6_80 = if_else(year <= 1880 & year > 1875, 1L, 0L),
    d6_85 = if_else(year <= 1885 & year > 1880, 1L, 0L),
    d6_90 = if_else(year <= 1890 & year > 1885, 1L, 0L),
    d6_95 = if_else(year <= 1895 & year > 1890, 1L, 0L),
    d6_00 = if_else(year <= 1900 & year > 1895, 1L, 0L),
    
    # Interactions with lsteam
    dd6_60lsteam  = d6_60 * lsteam,
    dd6_b65lsteam = d6_65 * lsteam,
    dd6_b70lsteam = d6_70 * lsteam,
    dd6_b75lsteam = d6_75 * lsteam,
    dd6_b80lsteam = d6_80 * lsteam,
    dd6_b85lsteam = d6_85 * lsteam,
    dd6_b90lsteam = d6_90 * lsteam,
    dd6_b95lsteam = d6_95 * lsteam,
    dd6_b00lsteam = d6_00 * lsteam,
    
    # Interactions with lsail2
    dd6_60lsail2  = d6_60 * lsail2,
    dd6_b65lsail2 = d6_65 * lsail2,
    dd6_b70lsail2 = d6_70 * lsail2,
    dd6_b75lsail2 = d6_75 * lsail2,
    dd6_b80lsail2 = d6_80 * lsail2,
    dd6_b85lsail2 = d6_85 * lsail2,
    dd6_b90lsail2 = d6_90 * lsail2,
    dd6_b95lsail2 = d6_95 * lsail2,
    dd6_b00lsail2 = d6_00 * lsail2
  )

# ---- Save out new dataset ----
write_dta(bilat, "PUBLIC_DATA/BILATERAL_TRADE_PUBLIC2.dta")
