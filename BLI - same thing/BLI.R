install.packages(c("readxl", "dplyr", "stringr", "tidyr", "janitor"))

library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(janitor)

# ==== 1) Load ====
setwd("/Users/fangxiaoling/Documents/Nathan Nunn RA/Book/BLI - same thing")
raw <- read_excel("oecd-better-life-index.xlsx") |>
  clean_names()

# Expecting columns (case-insensitive after clean_names):
# country, indicator, sub_indicator, obs_value
stopifnot(all(c("country","indicator","sub_indicator","obs_value") %in% names(raw)))

# Optional: trim whitespace & fix a common typo in "Environment"
dat <- raw |>
  mutate(
    country      = str_squish(as.character(country)),
    indicator    = str_squish(as.character(indicator)),
    sub_indicator= str_squish(as.character(sub_indicator)),
    indicator    = if_else(str_to_lower(indicator) == "enviroment", "Environment", indicator)
  ) |>
  # keep only rows with numeric values
  mutate(obs_value = suppressWarnings(as.numeric(obs_value))) |>
  filter(!is.na(obs_value))

# ==== 2) Collapse duplicates at Country × Indicator × Sub-Indicator ====
dat_c <- dat |>
  group_by(country, indicator, sub_indicator) |>
  summarize(value = mean(obs_value, na.rm = TRUE), .groups = "drop")

# ==== 3) Define which sub-indicators are "negative" (lower is better) ====
# You can add/remove items here depending on what sub-indicators exist in your file.
neg_list <- tribble(
  ~indicator,           ~sub_indicator,
  "Environment",        "Air pollution",
  "Work-Life Balance",  "Employees working very long hours",
  "Jobs",               "Long-term unemployment rate",
  "Jobs",               "Labour market insecurity",
  "Safety",             "Homicide rate",
  "Housing",            "Housing expenditure"
)

dat_c <- dat_c |>
  left_join(
    neg_list |> mutate(neg = TRUE),
    by = c("indicator","sub_indicator")
  ) |>
  mutate(neg = if_else(is.na(neg), FALSE, TRUE))

# ==== 4) Normalize each Sub-Indicator to 0–10 (min–max), flipping negatives ====
normed <- dat_c |>
  group_by(indicator, sub_indicator) |>
  mutate(
    mn = min(value, na.rm = TRUE),
    mx = max(value, na.rm = TRUE),
    rng = mx - mn,
    # handle zero-range (all countries equal) -> give neutral score 5
    score = case_when(
      rng == 0 & !neg ~ 5,
      rng == 0 &  neg ~ 5,
      neg            ~ 10 * (mx - value) / rng,
      TRUE           ~ 10 * (value - mn) / rng
    )
  ) |>
  ungroup() |>
  select(country, indicator, sub_indicator, score)

# ==== 5) Aggregate to Dimension (Indicator) scores ====
dim_scores <- normed |>
  group_by(country, indicator) |>
  summarize(dimension_score = mean(score, na.rm = TRUE), .groups = "drop")

# (Optional) See each country × dimension in wide form
dim_scores_wide <- dim_scores |>
  pivot_wider(names_from = indicator, values_from = dimension_score)

# ==== 6) Equal-weighted BLI (all dimensions weight = 1) ====
bli <- dim_scores |>
  group_by(country) |>
  summarize(
    n_dims = sum(!is.na(dimension_score)),
    BLI = mean(dimension_score, na.rm = TRUE)
  ) |>
  arrange(desc(BLI))

# ==== 7) Outputs ====
# 1) Country-level BLI
print(bli, n = 200)

# 2) Country × Dimension table (handy for exporting/plotting)
print(dim_scores_wide, n = 200)

# ==== 8) Export to CSV ====
write.csv(bli, "bli_equal_weights.csv", row.names = FALSE)
write.csv(dim_scores_wide, "bli_dimension_scores_equal_weights.csv", row.names = FALSE)

## Then, I do the same thing I did for SPI. Regress the GDPpc with this.
# ===== Packages =====
library(readxl)
library(tidyverse)
library(ggrepel)
library(scales)

# ===== Load your cleaned dataset from Excel =====
setwd("/Users/fangxiaoling/Documents/Nathan Nunn RA/Book/BLI - same thing")
bli_gdp <- read_excel("BLI_GDPpc.xlsx")

# ===== Regressions (equations shown in console, not on graphs) =====
m_lin <- lm(BLI ~ GDPpc_USD_2025, data = bli_gdp)
cat("\n=== Linear model: BLI ~ GDPpc_USD_2025 ===\n")
print(summary(m_lin))

m_log <- lm(BLI ~ logG_2025, data = bli_gdp)
cat("\n=== Linear model: BLI ~ logG_2025 ===\n")
print(summary(m_log))

# ===== Plot 1: Linear GDP (show ALL labels) =====
p_lin <- ggplot(bli_gdp, aes(x = GDPpc_USD_2025, y = BLI)) +
  geom_point(alpha = 0.75, size = 3) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.9) +
  geom_text_repel(
    aes(label = country),
    size = 3.2,
    max.overlaps = Inf,     # <- force all labels
    box.padding   = 0.5,
    point.padding = 0.3,
    force         = 2,
    force_pull    = 1,
    min.segment.length = 0,
    segment.size  = 0.2
  ) +
  scale_x_continuous(labels = label_dollar(prefix = "$"), breaks = pretty_breaks(n = 6)) +
  labs(
    title = "Better Life Index (equal-weight) vs GDP per capita, 2025",
    subtitle = "Linear scale on GDP per capita",
    x = "GDP per capita (USD, 2025)",
    y = "BLI (0–10)"
  ) +
  theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank())

ggsave("gdp_vs_bli_2025_linear.png", p_lin, width = 11, height = 8.5, dpi = 300)

# ===== Plot 2: Log GDP (show ALL labels) =====
p_log <- ggplot(bli_gdp, aes(x = logG_2025, y = BLI)) +
  geom_point(alpha = 0.75, size = 3) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.9) +
  geom_text_repel(
    aes(label = country),
    size = 3.2,
    max.overlaps = Inf,     # <- force all labels
    box.padding   = 0.5,
    point.padding = 0.3,
    force         = 2,
    force_pull    = 1,
    min.segment.length = 0,
    segment.size  = 0.2
  ) +
  labs(
    title = "Better Life Index (equal-weight) vs GDP per capita, 2025",
    subtitle = "Natural log of GDP per capita ",
    x = "Ln(GDP per capita, USD; 2025)",
    y = "BLI (0–10)"
  ) +
  theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank())

ggsave("gdp_vs_bli_2025_log.png", p_log, width = 11, height = 8.5, dpi = 300)

cat("\nSaved:\n- gdp_vs_bli_2025_linear.png\n- gdp_vs_bli_2025_log.png\n")
