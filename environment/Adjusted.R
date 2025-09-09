# ==== Packages ====
suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
})

# ==== Load files ====
gdp <- read_excel("/Users/fangxiaoling/Documents/Nathan Nunn RA/Book/environment/GDP2020.xlsx") |>
  select(country, GDP2020)

ans <- read_excel("/Users/fangxiaoling/Documents/Nathan Nunn RA/Book/environment/ANS.xlsx") |>
  select(country, ANS2020)

# ==== Merge (inner join: only countries in both) ====
merged <- gdp |>
  inner_join(ans, by = "country")

# If you want to keep all countries from GDP and fill missing ANS with NA:
# merged <- gdp |> left_join(ans, by = "country")

# ==== Quick check ====
print(head(merged, 20))

# ==== Save merged dataset ====
write.csv(merged, "/Users/fangxiaoling/Documents/Nathan Nunn RA/Book/environment/merged_GDP_ANS.csv", row.names = FALSE)

# ==== Regression: ANS2020 on GDP2020 ====
model <- lm(ANS2020 ~ GDP2020, data = merged)
summary(model)

#for those that aren't found
library(dplyr)

# Countries in ANS.xlsx but not in GDP2020.xlsx
not_in_gdp <- anti_join(ans, gdp, by = "country") |> arrange(country)
print(not_in_gdp)



# ==== Packages ====
suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(ggplot2)
  library(ggrepel)
  library(scales)
})

# ==== Load merged data ====
df <- read_csv("/Users/fangxiaoling/Documents/Nathan Nunn RA/Book/environment/merged_GDP_ANS.csv")

# ==== Add natural logs ====
df <- df |>
  mutate(
    lnGDP2020 = log(GDP2020),
    lnANS2020 = log(ANS2020)
  ) |>
  filter(!is.na(lnGDP2020), !is.na(lnANS2020))  # keep only complete cases

# ==== Theme ====
my_theme <- theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    panel.grid.major = element_line(color = "grey85", size = 0.5),
    panel.grid.minor = element_line(color = "grey90", size = 0.25),
    panel.background = element_rect(fill = "white", colour = NA)
  )

# ==== Scatter + regression line ====
p <- ggplot(df, aes(x = lnGDP2020, y = lnANS2020)) +
  geom_point(alpha = 0.8, size = 2) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.9, color = "steelblue") +
  geom_text_repel(
    aes(label = country),
    size = 3,
    max.overlaps = 60,
    box.padding = 0.3
  ) +
  labs(
    title = "Ln(ANS2020) vs Ln(GDP2020)",
    x = "Ln of GDP 2020 (USD)",
    y = "Ln of ANS 2020"
  ) +
  scale_x_continuous(breaks = pretty_breaks(8)) +
  scale_y_continuous(breaks = pretty_breaks(8)) +
  my_theme

# ==== Save ====
ggsave("scatter_lnGDP_lnANS.png", p, width = 11, height = 8.5, dpi = 300)

# ==== Regression in console ====
model <- lm(lnANS2020 ~ lnGDP2020, data = df)
summary(model)
