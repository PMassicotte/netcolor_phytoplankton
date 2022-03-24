# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Relationships between aphy(440) and fucoxanthin.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source(here("R","zzz.R"))

df <- read_csv(here("data","clean","merged_dataset.csv")) %>%
  filter(wavelength == 440) %>%
  mutate(bioregion_name = factor(
    bioregion_name,
    levels = c(
      "Scotian Shelf",
      "Northwest Atlantic Basin ocean (NAB)",
      "Labrador"
    )
  )) %>%
  mutate(bioregion_name_wrap = str_wrap_factor(bioregion_name, 20))

df

# Plot --------------------------------------------------------------------

df

p <- df %>%
  filter(if_all(c(aphy, fucox), ~. > 0)) %>%
  ggplot(aes(x = aphy, y = fucox)) +
  geom_point(
    aes(color = season, shape = bioregion_name),
    size = 1.5,
    alpha = 0.3
  ) +
  scale_x_log10() +
  scale_y_log10() +
  annotation_logticks(sides = "bl", size = 0.1) +
  geom_smooth(method = "lm", color = "#3c3c3c", size = 0.5) +
  ggpmisc::stat_poly_eq(
    aes(label = ..eq.label..),
    label.y.npc = 0.95,
    label.x.npc = 0.05,
    size = 3,
    coef.digits = 3,
    family = "Montserrat"
  ) +
  ggpmisc::stat_poly_eq(
    label.y.npc = 0.88,
    label.x.npc = 0.05,
    aes(label = ..rr.label..),
    size = 3,
    family = "Montserrat"
  ) +
  scale_fill_manual(
    breaks = season_breaks,
    values = season_colors,
    guide = guide_legend(
      override.aes = list(size = 2, alpha = 1),
      label.theme = element_text(size = 7, family = "Montserrat Light")
    )
  ) +
  scale_shape_manual(
    breaks = area_breaks,
    values = area_pch
  ) +
  labs(
    x = quote(a[phi](440) ~ (m^{-1})),
    y = quote("Fucoxanthin" ~ (mg~m^{-3}))
  ) +
  facet_wrap(~bioregion_name_wrap + season) +
  theme(
    panel.spacing.y = unit(3, "lines"),
    legend.position = "none",
    strip.text = element_text(size = 10)
  )

ggsave(
  here("graphs","appendix03.pdf"),
  device = cairo_pdf,
  width = 180,
  height = 240,
  units = "mm"
)
