# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Compare PAAW calculated on aphy and ap absorption spectra. Given
# that ap can be measured continually in situ with autonomous vehicles, this
# would open the door to use PAAW on ap to easily get information on the
# phytoplankton community.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source(here("R","zzz.R"))

seasons <- read_csv(here("data", "clean", "merged_dataset.csv")) %>%
  select(sample_id, season) %>%
  distinct()

df <- read_csv(here("data","clean","apparent_visible_wavelength.csv")) %>%
  mutate(bioregion_name = factor(
    bioregion_name,
    levels = c(
      "Scotian Shelf",
      "Northwest Atlantic Basin ocean (NAB)",
      "Labrador"
    )
  )) %>%
  left_join(seasons, by = "sample_id")

# Plot --------------------------------------------------------------------

p <- df %>%
  ggplot(aes(x = avw_aphy, y = avw_ap)) +
  geom_point(
    aes(fill = season),
    color = "transparent",
    size = 1.5,
    stroke = 0,
    pch = 21,
    alpha = 0.3
  ) +
  labs(
    x = quote(PAAW[a[phi]]~(nm)),
    y = quote(PAAW[a[p]]~(nm)),
    fill = NULL
  ) +
  scale_fill_manual(
    breaks = c("Winter", "Spring", "Summer", "Autumn"),
    values = c("#014f86", "#40916c", "#ffcb69", "#e76f51"),
    guide = guide_legend(
      override.aes = list(size = 2, alpha = 1),
      label.theme = element_text(size = 7, family = "Montserrat Light")
    )
  ) +
  geom_smooth(
    color = "#3c3c3c",
    size = 0.5,
    alpha = 0.25,
    method = "lm"
  ) +
  ggpmisc::stat_poly_eq(
    aes(label = ..eq.label..),
    parse = TRUE,
    coef.digits = 4,
    f.digits = 5,
    p.digits = 10,
    label.x.npc = 0.05,
    family = "Montserrat",
    size = 2.5
  ) +
  ggpmisc::stat_poly_eq(
    label.x.npc = 0.05,
    label.y.npc = 0.88,
    aes(label = ..rr.label..),
    size = 2.5,
    family = "Montserrat"
  ) +
  facet_wrap(~ str_wrap_factor(bioregion_name, 20)) +
  theme(
    strip.text = element_text(size = 10)
  )

ggsave(
  here("graphs","appendix05.pdf"),
  device = cairo_pdf,
  width = 180,
  height = 70,
  units = "mm"
)
