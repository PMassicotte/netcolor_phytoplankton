rm(list = ls())

source(here("R", "zzz.R"))
source(here("R", "zzz_ggboxplot.R"))

# Load data ---------------------------------------------------------------

df <- read_csv(here("data", "clean", "merged_dataset.csv")) %>%
  select(sample_id, bioregion_name, wavelength, season, aphy) %>%
  filter(wavelength %in% c(443, 675)) %>%
  mutate(season = factor(season,
    levels = c("Spring", "Summer", "Autumn", "Winter")
  )) %>%
  mutate(bioregion_name = factor(
    bioregion_name,
    levels = c(
      "Scotian Shelf",
      "Northwest Atlantic Basin ocean (NAB)",
      "Labrador"
    )
  )) %>%
  mutate(bioregion_name_wrap = str_wrap_factor(bioregion_name, 20))

# df <- read_csv(here("data","clean","merged_dataset.csv")) %>%
#   filter(wavelength %in% c(443, 675)) %>%
#   select(sample_id, bioregion_name, wavelength, season, aphy, hplcchla) %>%
#   mutate(aphy_specific = aphy / hplcchla)

df

df_viz <- df %>%
  pivot_wider(
    names_from = wavelength,
    values_from = aphy,
    names_prefix = "wl"
  ) %>%
  mutate(ratio_443_675 = wl443 / wl675)

p <- df_viz %>%
  ggplot(aes(x = season, y = ratio_443_675, fill = bioregion_name)) +
  geom_boxplot(size = 0.1, outlier.size = 0.25) +
  scale_fill_manual(
    breaks = area_breaks,
    values = area_colors
  ) +
  labs(
    x = NULL,
    y = quote(a[phi](443) / a[phi](675))
  ) +
  facet_wrap(~bioregion_name_wrap, scales = "free_y") +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 10)
  )

ggsave(
  here("graphs","appendix03.pdf"),
  device = cairo_pdf,
  width = 180,
  height = 70,
  units = "mm"
)

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
  ggplot(aes(x = season, y = aphy, fill = factor(wavelength))) +
  geom_boxplot() +
  scale_y_log10()
