# ---- Load dataset ----
df <- read_dta("results/predicted_effects_table9.dta")

# ---- Create combined label: Country (cons1860) ----
df <- df %>%
  mutate(
    country_label = paste0(country, " (", cons1860, ")")
  )

# ---- Order countries by estimate ----
df <- df %>%
  arrange(eff) %>%
  mutate(country_label = factor(country_label, levels = country_label))

# ---- Plot ----
ggplot(df, aes(x = eff, y = country_label)) +
  geom_point(size = 2) +
  geom_errorbarh(aes(xmin = ci_low, xmax = ci_high), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  labs(
    title = "Effect of ln(export/GDP) on ln(GDP per capita), by country",
    subtitle = "OLS; Table 9 col 1",
    x = "d ln(GDPpc) / d ln(export/GDP)",
    y = NULL,
    caption = "Dot = estimate; line = 95% CI; countries ordered by estimate\ncons1860 shown in parentheses"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.y = element_text(size = 8),
    plot.title = element_text(face = "bold")
  ) +
  xlim(-2, 2)
