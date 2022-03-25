# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Figure asked by Emmanuel.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source(here("R", "zzz.R"))

df <- read_csv(here("data", "clean", "merged_dataset.csv")) %>%
  filter(wavelength == 443) %>%
  select(sample_id, season, bioregion_name, fucox, hplcchla)

paaw <- read_csv(here("data", "clean", "apparent_visible_wavelength.csv"))

df_viz <- inner_join(df, paaw)

df_viz

p1 <- df_viz %>%
  filter(fucox > 0) %>%
  ggplot(aes(x = avw_aphy, y = fucox, color = season, shape = bioregion_name)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  annotation_logticks(sides = "bl", size = 0.25) +
  scale_color_manual(
    breaks = season_breaks,
    values = season_colors
  ) +
  scale_shape_manual(
    breaks = area_breaks,
    values = area_pch
  )

p2 <- df_viz %>%
  filter(fucox > 0) %>%
  ggplot(aes(x = hplcchla, y = fucox, color = season, shape = bioregion_name)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  annotation_logticks(sides = "bl", size = 0.25) +
  scale_color_manual(
    breaks = season_breaks,
    values = season_colors
  ) +
  scale_shape_manual(
    breaks = area_breaks,
    values = area_pch
  )

p1 / p2
