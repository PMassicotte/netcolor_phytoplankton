# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Relationship between PAAW and aphy*. I want to see if PAAW could
# be used as an index to eventually get information on phytoplankton cell size.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

# Prepare the data --------------------------------------------------------

aphy <- read_csv(here("data","clean","merged_dataset.csv")) %>%
  filter(wavelength %in% c(443, 675)) %>%
  select(sample_id, bioregion_name, wavelength, season, aphy, hplcchla) %>%
  mutate(aphy_specific = aphy / hplcchla) %>%
  select(-aphy, -hplcchla)

paaw <- read_csv(here("data","clean","apparent_visible_wavelength.csv")) %>%
  select(sample_id, bioregion_name, avw_aphy)

df <- inner_join(aphy, paaw, by = c("sample_id", "bioregion_name"))

df

# Relation between aphy* and PAAW -----------------------------------------

df %>%
  ggplot(aes(x = avw_aphy, y = aphy_specific)) +
  geom_point() +
  scale_y_log10() +
  geom_smooth(method = "lm") +
  ggpubr::stat_regline_equation(
    label.y.npc = 0.15,
    label.x.npc = 1,
    size = 2,
    hjust = 1
  ) +
  ggpubr::stat_regline_equation(
    aes(label = ..rr.label..),
    label.y.npc = 0.05,
    label.x.npc = 1,
    size = 2,
    hjust = 1
  ) +
  facet_grid(bioregion_name~wavelength)


# Relation between aphy* ratio and PAAW -----------------------------------

# It was shown that the aphy* 443/675 could be related to both as a function of
# cell size and irradiance (Fujiki2002).

df_viz <- df %>%
  pivot_wider(names_from = wavelength, values_from = aphy_specific, names_prefix = "wl") %>%
  mutate(aphy_ratio = wl443 / wl675)

df_viz

p <- df_viz %>%
  ggplot(aes(x = avw_aphy, y = aphy_ratio)) +
  geom_point(size = 0.5, color = "#3c3c3c") +
  labs(
    x = "Phytoplankton Apparent Absorption Wavelength (PAAW, nm)",
    y = quote(a[phi]^"*"~(443) / a[phi]^"*"~(670))
  )

ggsave(
  here::here("graphs","appendix03.pdf"),
  device = cairo_pdf,
  width = 5,
  height = 4
)

## Make an exponential model ----

# df_model <- df_viz %>%
#   group_nest() %>%
#   mutate(model = map(data, ~ nls(
#     aphy_ratio ~ a * exp(b * avw_aphy) + c * exp(d * avw_aphy),
#     data = .,
#     start = c(a = 15, b = -0.01, c = 12, d = 1)
#   ))) %>%
#   mutate(tidied = map(model, tidy)) %>%
#   mutate(augmented = map(model, augment))
#
# df_model
#
# df_model %>%
#   unnest(augmented) %>%
#   ggplot(aes(x = avw_aphy, y = aphy_ratio)) +
#   geom_point() +
#   geom_line(aes(y = .fitted), color = "red")
#
#
# tibble(
#   avw_aphy = 460:500,
#   aphy_ratio = 0.8 * exp(-2 * avw_aphy) + 10 * exp(-0.5 * avw_aphy)
# ) %>%
#   ggplot(aes(x = avw_aphy, y = aphy_ratio)) +
#   geom_point()
