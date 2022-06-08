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

source(here("R", "zzz.R"))

df <- open_dataset(here("data", "clean", "merged_dataset")) |>
  filter(wavelength == 443) |>
  collect()

# Add prediction from other models found in the literature ----------------

df <- df |>
  mutate(bricaud_1998 = 0.0378 * hplcchla^0.627) |>
  mutate(bricaud_2004 = 0.0654 * hplcchla^0.728) |>
  mutate(devred_2006 = ((0.0839 - 0.0176) / 1.613) * (1 - exp(-1.613 * hplcchla)) + 0.0176 * hplcchla)

# Plot --------------------------------------------------------------------

df_viz <- df |>
  filter(hplcchla > 0) |>
  mutate(bioregion_name = factor(
    bioregion_name,
    levels = c(
      "Scotian Shelf",
      "NAB",
      "Labrador"
    )
  ))

p1 <- df_viz |>
  ggplot(aes(x = hplcchla, y = aphy)) +
  geom_point(
    aes(color = season, shape = bioregion_name),
    size = 1.5,
    alpha = 0.3
  ) +
  geom_line(aes(y = bricaud_1998, lty = "Bricaud 1998")) +
  geom_line(aes(y = bricaud_2004, lty = "Bricaud 2004")) +
  geom_line(aes(y = devred_2006, lty = "Devred 2006")) +
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
  scale_linetype_manual(
    breaks = c("This study", "Bricaud 1998", "Bricaud 2004", "Devred 2006"),
    values = c(1, 2, 3, 4),
    guide = guide_legend(
      override.aes = list(size = 0.5, alpha = 1),
      label.theme = element_text(size = 7, family = "Montserrat Light")
    )
  ) +
  labs(
    x = quote("[Chl-a]" ~ (mg ~ m^{-3})),
    y = quote(a[phi] ~ (443) ~ (m^{-1}))
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
  here("graphs", "fig04.pdf"),
  device = cairo_pdf,
  width = 180,
  height = 180,
  units = "mm"
)

# Model stats -------------------------------------------------------------

mod <- lm(log10(aphy) ~ log10(hplcchla), data = df_viz)
summary(mod)
