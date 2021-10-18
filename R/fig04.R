# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:

# Beaucoup de relation aph(443)-chl ont été etablie avec quelques
# croisieres seulement, ici avec une serie temporelle de 10-20ans, ca serait
# bien de voir si ces relations tiennent toujours la route, je vais commencer a
# remplir le google doc avec ce genre de relations, e.g., Bricaud et al., 1998 &
# 2004 et on pourra ajouter une figure aph440 vs chl (bricaud utilise 440 et non
# 443).
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source(here("R","zzz.R"))

df <- read_csv(here("data", "clean", "merged_dataset.csv")) %>%
  filter(wavelength == 440)

# Add prediction from other models found in the literature ----------------

df <- df %>%
  mutate(bricaud_1998 = 0.0378 * hplcchla^0.627) %>%
  mutate(bricaud_2004 = 0.0654 * hplcchla^0.728) %>%
  mutate(devred_2006 = ((0.0839 - 0.0176) / 1.613) * (1 - exp(-1.613 * hplcchla)) + 0.0176 * hplcchla)

# Plot --------------------------------------------------------------------

p1 <- df %>%
  filter(hplcchla > 0) %>% # TODO: check the minimum plausible value
  ggplot(aes(x = hplcchla, y = aphy)) +
  geom_point(
    color = "#6c6c6c",
    size = 1,
    shape = 16,
    stroke = 0,
    alpha = 0.5
  ) +
  geom_line(aes(y = bricaud_1998, color = "Bricaud 1998"), lty = 2) +
  geom_line(aes(y = bricaud_2004, color = "Bricaud 2004"), lty = 2) +
  geom_line(aes(y = devred_2006, color = "Devred 2006"), lty = 2) +
  scale_x_log10() +
  scale_y_log10() +
  annotation_logticks(sides = "bl", size = 0.25) +
  geom_smooth(method = "lm", aes(color = "This study"), se = FALSE) +
  ggpubr::stat_regline_equation(label.y.npc = 1, size = 3) +
  ggpubr::stat_regline_equation(
    label.y.npc = 0.93,
    aes(label = ..rr.label..),
    size = 3
  ) +
  labs(
    x = quote("Chlorophyll-a" ~ (mg~m^{-3})),
    y = quote(a[phi] ~ (440) ~ (m^{-1}))
  ) +
  theme(
    legend.position = "top",
    legend.title = element_blank()
  ) +
  paletteer::scale_color_paletteer_d(
    "nbapalettes::pacers_venue",
    guide = guide_legend(
      label.position = "top",
      override.aes = list(size = 2, lty = 1),
      keywidth = unit(2, "cm")
    )
  )

p2 <- df %>%
  filter(fucox > 0) %>% # TODO: check the minimum plausible value
  mutate(bioregion_name = factor(
    bioregion_name,
    levels = c(
      "Scotian Shelf",
      "Northwest Atlantic Basin ocean (NAB)",
      "Labrador"
    )
  )) %>%
  mutate(bioregion_name_wrap = str_wrap_factor(bioregion_name, 20)) %>%
  ggplot(aes(x = fucox, y = aphy)) +
  geom_point(aes(color = bioregion_name), size = 0.25) +
  scale_x_log10() +
  scale_y_log10() +
  annotation_logticks(sides = "bl", size = 0.1) +
  geom_smooth(method = "lm", color = "#3c3c3c", size = 0.5) +
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
  scale_color_manual(
    breaks = area_breaks,
    values = area_colors
  ) +
  labs(
    x = quote("Fucoxanthin" ~ (mg~m^{-3})),
    y = quote(a[phi] ~ (440) ~ (m^{-1}))
  ) +
  facet_wrap(~bioregion_name_wrap) +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 8)
  )

p <- p1 / p2 +
  plot_layout(heights = c(1, 0.75)) +
  plot_annotation(tag_levels = "A") &
  theme(
    plot.tag = element_text(face = "bold", size = 14)
  )

ggsave(
  here::here("graphs","fig04.pdf"),
  device = cairo_pdf,
  width = 5,
  height = 6
)
