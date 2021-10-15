df <- read_csv(here("data","clean","merged_dataset.csv")) %>%
  filter(wavelength %in% c(443, 675)) %>%
  select(sample_id, bioregion_name, wavelength, season, aphy, hplcchla) %>%
  mutate(aphy_specific = aphy / hplcchla)

df

df_viz <- df %>%
  select(-aphy, -hplcchla) %>%
  pivot_wider(
    names_from = wavelength,
    values_from = aphy_specific,
    names_prefix = "wl"
  ) %>%
  mutate(ratio_443_675 = wl443 / wl675)

df_viz %>%
  ggplot(aes(x = season, y = ratio_443_675)) +
  geom_boxplot() +
  facet_wrap(~bioregion_name)

# Ratio between blue and red peaks (R) was 3.4 (± 0.61) on average in summer,
# which was significantly higher than in winter (2.2 ± 0.45) (Figure 3). --
# Churilova2017

df_viz

df_viz %>%
  group_by(season) %>%
  summarise(mean_ratio = mean(ratio_443_675))

ttest <- df_viz %>%
  filter(season %in% c("Winter", "Summer"))

t.test(ratio_443_675 ~ season, data = ttest) %>%
  report::report()


df %>%
  ggplot(aes(x = season, y = aphy_specific, fill = factor(wavelength))) +
  geom_boxplot() +
  scale_y_log10()
