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

df_viz <- inner_join(df, paaw) %>%
  mutate(season = factor(season,
    levels = c("Spring", "Summer", "Autumn", "Winter")
  )) %>%
  mutate(bioregion_name = factor(bioregion_name,
    levels = c(
      "Scotian Shelf",
      "NAB",
      "Labrador"
    )
  ))

df_viz

p <- df_viz %>%
  filter(fucox > 0) %>%
  ggplot(aes(x = hplcchla, y = fucox)) +
  geom_point(aes(color = season, shape = bioregion_name)) +
  geom_smooth(method = "lm", color = "black", se = FALSE) +
  scale_x_log10() +
  scale_y_log10() +
  annotation_logticks(sides = "bl", size = 0.25) +
  ggpmisc::stat_poly_eq(
    aes(label = ..eq.label..),
    label.y.npc = 1,
    size = 2.5,
    family = "Montserrat"
  ) +
  ggpmisc::stat_poly_eq(
    label.y.npc = 0.93,
    aes(label = ..rr.label..),
    size = 2.5,
    family = "Montserrat"
  ) +
  scale_color_manual(
    breaks = season_breaks,
    values = season_colors
  ) +
  scale_shape_manual(
    breaks = area_breaks,
    values = area_pch
  ) +
  labs(
    x = quote("[Chl-a]" ~ (mg~m^{-3})),
    y = quote("[Fucox]" ~ (mg~m^{-3}))
  ) +
  # guides(shape = "none", ncol = 1) +
  theme(
    legend.title = element_blank(),
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.spacing.x = unit(0.1, "cm"),
    legend.spacing.y = unit(0, "cm"),
    legend.position = "top",
    legend.box = "vertical"
  )

p

ggsave(
  here("graphs", "fig03.pdf"),
  width = 5,
  height = 5,
  device = cairo_pdf
)
