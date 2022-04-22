rm(list = ls())

source(here("R","zzz.R"))

df <- read_csv(here("data", "clean", "merged_dataset.csv")) %>%
  filter(wavelength %in% c(443, 675))

df_viz <- df %>%
  select(sample_id, bioregion_name, season, fucox, wavelength, aphy) %>%
  pivot_wider(names_from = wavelength, values_from = aphy, names_prefix = "wl_") %>%
  mutate(aphy_443_675 = wl_443 / wl_675)

df_viz %>%
  mutate(bioregion_name = factor(
  bioregion_name,
  levels = c(
    "Scotian Shelf",
    "Northwest Atlantic Basin ocean (NAB)",
    "Labrador"
  )
)) %>%
  mutate(bioregion_name_wrap = str_wrap_factor(bioregion_name, 20))


p <- df_viz %>%
  filter(fucox > 0) %>%
  ggplot(aes(x = fucox, y = aphy_443_675)) +
  geom_point(
    aes(color = season, shape = bioregion_name),
    size = 1.5,
    alpha = 0.3
  ) +
  geom_smooth(
    method = "lm",
    aes(lty = "This study"),
    se = FALSE,
    show.legend = FALSE,
    color = "black"
  ) +
  scale_x_log10() +
  scale_y_log10() +
  annotation_logticks(sides = "bl", size = 0.25) +
  ggpmisc::stat_poly_eq(
    aes(label = ..eq.label..),
    label.y.npc = 1,
    label.x = 1,
    size = 2.5,
    family = "Montserrat"
  ) +
  ggpmisc::stat_poly_eq(
    label.y.npc = 0.93,
    label.x = 1,
    aes(label = ..rr.label..),
    size = 2.5,
    family = "Montserrat"
  ) +
  scale_color_manual(
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
    x = quote("[Fucox]" ~ (mg~m^{-3})),
    y = quote(a[phi] ~ (443) / a[phi] ~ (675))
  ) +
  guides(shape = "none", ncol = 1) +
  theme(
    legend.title = element_blank(),
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.spacing.x = unit(0.1, "cm"),
    legend.spacing.y = unit(0, "cm"),
    legend.position = "top",
    legend.box = "vertical"
  )

ggsave(
  here("graphs", "appendix03.pdf"),
  width = 5,
  height = 4,
  device = cairo_pdf
)
