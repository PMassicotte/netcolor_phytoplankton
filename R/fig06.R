# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  aphy443 vs chla.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source(here("R", "zzz.R"))

df <- read_csv(here("data", "clean", "merged_dataset.csv")) %>%
  filter(wavelength %in% c(443, 675)) %>%
  select(sample_id, bioregion_name, season, wavelength, aphy, hplcchla, fucox) %>%
  mutate(bioregion_name = factor(
    bioregion_name,
    levels = c(
      "Scotian Shelf",
      "NAB",
      "Labrador"
    )
  ))

df %>%
  count(sample_id) %>%
  assertr::verify(n == 2)

# aphy443/aphy675 vs chla -------------------------------------------------

df_viz <- df %>%
  dtplyr::lazy_dt() %>%
  group_by(sample_id, bioregion_name, bioregion_name, season) %>%
  summarise(
    aphy_443_675 = aphy[wavelength == 443] / aphy[wavelength == 675],
    hplcchla = unique(hplcchla)
  ) %>%
  ungroup() %>%
  as_tibble()

df_viz

p1 <- df_viz %>%
  ggplot(aes(x = hplcchla, y = aphy_443_675)) +
  geom_point(
    aes(color = season, pch = bioregion_name),
    alpha = 0.5,
    stroke = 0.25
  ) +
  scale_x_log10() +
  scale_y_log10() +
  annotation_logticks(sides = "bl", size = 0.1) +
  geom_smooth(method = "lm", color = "#3c3c3c", size = 0.5) +
  ggpmisc::stat_poly_eq(
    aes(label = ..eq.label..),
    label.y.npc = 0.95,
    label.x.npc = 1,
    size = 3,
    coef.digits = 3,
    family = "Montserrat"
  ) +
  ggpmisc::stat_poly_eq(
    label.y.npc = 0.88,
    label.x.npc = 1,
    aes(label = ..rr.label..),
    size = 3,
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
    x = quote("[Chl-a]" ~ (mg~m^{-3})),
    y = quote(a[phi](443) / a[phi](675)),
    color = NULL
  ) +
  guides(shape = "none") +
  theme(
    legend.title = element_blank(),
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.spacing.x = unit(0.1, "cm"),
    legend.spacing.y = unit(0, "cm"),
    legend.position = "top",
    legend.box = "vertical"
  )

p2 <- p1 +
  facet_wrap(~bioregion_name) +
  theme(
    legend.position = "none",
    strip.text = element_text(
      size = 8
    )
  )

p <- p1 / p2 +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(size = 16, face = "bold"))

ggsave(
  here("graphs", "fig06.pdf"),
  device = cairo_pdf,
  width = 180,
  height = 160,
  units = "mm"
)
