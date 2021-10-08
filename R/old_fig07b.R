# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Correlation lag between absorption index.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

# Read the data -----------------------------------------------------------

df <- read_csv(
  here("data", "clean", "simulated_daily_data_from_loess.csv"),
  col_select = c(
    bioregion_name,
    yday,
    name,
    wavelength,
    simulation,
    simulated_data
  )
) %>%
  filter(wavelength %in% c(443, 670)) %>%
  filter(name %in% c("aphy", "snap"))

df

df <- df %>%
  pivot_wider(names_from = c(name, wavelength), values_from = simulated_data) %>%
  select(-snap_670) %>%
  rename(snap = snap_443)

df

# Calculate ratio ---------------------------------------------------------

df <- df %>%
  mutate(aphy_443_670 = aphy_443 / aphy_670) %>%
  filter(between(aphy_443_670, 0, 12))

# Lag correlation ---------------------------------------------------------

## Lag correlation per bioregion ----

res <- df %>%
  group_nest(bioregion_name, simulation) %>%
  mutate(mod_ccf = map(data, ~ ccf(
    .$aphy_443_670, .$snap,
    plot = FALSE, lag.max = 30
  ))) %>%
  mutate(mod_ccf_tidy = map(mod_ccf, broom::tidy)) %>%
  unnest(mod_ccf_tidy)

res

# Find the maximum correlation

max_correlation <- res %>%
  group_by(bioregion_name, simulation) %>%
  filter(abs(acf) == max(abs(acf))) %>%
  group_by(bioregion_name) %>%
  summarise(
    mean_lag = mean(lag),
    sd_lag = sd(lag),
    n = n() # Should be equal to the number of simulations
  )

max_correlation

p1 <- res %>%
  group_by(bioregion_name, lag) %>%
  summarise(acf = mean(acf)) %>%
  ungroup() %>%
  ggplot(aes(
    x = lag,
    y = 0,
    xend = lag,
    yend = acf
  )) +
  geom_segment(color = "gray60", size = 0.5) +
  geom_vline(
    data = max_correlation,
    aes(xintercept = mean_lag),
    color = "#bf1d28",
    size = 0.5
  ) +
  geom_vline(
    data = max_correlation,
    aes(xintercept = mean_lag - sd_lag),
    color = "#bf1d28",
    size = 0.25,
    lty = 2
  ) +
  geom_vline(
    data = max_correlation,
    aes(xintercept = mean_lag + sd_lag),
    color = "#bf1d28",
    size = 0.25,
    lty = 2
  ) +
  geom_hline(yintercept = 0, size = 0.25) +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 10)) +
  labs(
    x = "Lag (days)",
    y = "Correlation"
  ) +
  facet_wrap(~bioregion_name, ncol = 1)

# Scatterplot of raw data at lag 0 ----------------------------------------

df <- read_csv(here("data","clean","merged_dataset.csv")) %>%
  filter(wavelength %in% c(443, 670))

p2 <- df %>%
  group_by(bioregion_name, wavelength, yday = lubridate::yday(date)) %>%
  summarise(across(c(snap, aphy), mean)) %>%
  ungroup() %>%
  pivot_wider(names_from = c(wavelength), values_from = c(snap, aphy)) %>%
  select(-snap_670) %>%
  rename(snap = snap_443) %>%
  mutate(aphy_443_670 = aphy_443 / aphy_670) %>%
  ggplot(aes(x = aphy_443_670, y = snap)) +
  geom_point(size = 1, color = "#393E41") +
  geom_smooth(method = "lm", color = "#bf1d28", size = 0.5) +
  labs(
    x = quote(a[phi~(443)]/a[phi~(670)]),
    y = quote(S[NAP]~(nm^{-1}))
  ) +
  facet_wrap(~bioregion_name, ncol = 1, scales = "free")

# Save plots --------------------------------------------------------------

p <- p2 + p1 +
  plot_annotation(tag_levels = "A") &
  theme(
    plot.tag = element_text(face = "bold", size = 14)
  )

ggsave(
  here("graphs", "fig07b.pdf"),
  device = cairo_pdf,
  width = 8,
  height = 6
)
