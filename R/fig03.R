# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Show the aphy/anap ratio across the bioregions.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source(here("R", "zzz.R"))

absorption <- read_csv(here("data", "clean", "merged_dataset.csv")) %>%
  filter(wavelength == 443) %>%
  mutate(bioregion_name = factor(
    bioregion_name,
    levels = c(
      "Scotian Shelf",
      "Northwest Atlantic Basin ocean (NAB)",
      "Labrador"
    )
  )) %>%
  mutate(bioregion_name_wrap = str_wrap_factor(bioregion_name, 20))

# Plot --------------------------------------------------------------------

p <- absorption %>%
  filter(if_all(c(aphy, anap), ~. > 0)) %>%
  ggplot(aes(x = aphy, y = anap)) +
  geom_point(aes(color = bioregion_name), size = 0.5) +
  scale_x_log10(labels = scales::label_number()) +
  scale_y_log10(labels = scales::label_number()) +
  annotation_logticks(sides = "bl", size = 0.1) +
  geom_smooth(method = "lm", color = "#3c3c3c", size = 0.5) +
  ggpmisc::stat_poly_eq(
    aes(label = ..eq.label..),
    label.y.npc = 0.15,
    label.x.npc = 1,
    size = 3,
    coef.digits = 3,
    family = "Montserrat"
  ) +
  ggpmisc::stat_poly_eq(
    label.y.npc = 0.05,
    label.x.npc = 1,
    aes(label = ..rr.label..),
    size = 3,
    family = "Montserrat"
  ) +
  scale_color_manual(
    breaks = area_breaks,
    values = area_colors
  ) +
  labs(
    x = quote(a[phi](443)),
    y = quote(a[NAP](443))
  ) +
  facet_wrap(~bioregion_name_wrap) +
  theme(
    panel.spacing.y = unit(3, "lines"),
    legend.position = "none",
    strip.text = element_text(size = 10)
  )

ggsave(
  here("graphs", "fig03.pdf"),
  device = cairo_pdf,
  width = 180,
  height = 70,
  units = "mm"
)

