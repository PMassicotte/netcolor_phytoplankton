# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Show the aphy/anap ratio across the bioregions.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source(here("R", "zzz.R"))

absorption <- open_dataset(here("data", "clean", "merged_dataset")) |>
  filter(wavelength == 443) |>
  collect() |>
  mutate(bioregion_name = factor(
    bioregion_name,
    levels = c(
      "Scotian Shelf",
      "NAB",
      "Labrador"
    )
  )) |>
  mutate(bioregion_name_wrap = str_wrap_factor(bioregion_name, 20))

# Plot --------------------------------------------------------------------

p1 <- absorption |>
  filter(if_all(c(aphy, anap), ~ . > 0)) |>
  filter(anap <= 0.05) |> # 1 obvious outlier
  ggplot(aes(x = aphy, y = anap)) +
  geom_point(
    aes(color = season, pch = bioregion_name),
    alpha = 0.5,
    stroke = 0.25
  ) +
  scale_x_log10(labels = scales::label_number()) +
  scale_y_log10(labels = scales::label_number()) +
  annotation_logticks(sides = "bl", size = 0.25) +
  geom_smooth(
    method = "lm",
    aes(lty = "This study"),
    se = FALSE,
    show.legend = FALSE,
    color = "black"
  ) +
  ggpmisc::stat_poly_eq(
    aes(label = ..eq.label..),
    label.y.npc = 1,
    size = 2.5,
    family = "Montserrat"
  ) +
  ggpmisc::stat_poly_eq(
    aes(
      label = paste(..rr.label.., after_stat(p.value.label), sep = "*\", \"*")
    ),
    label.y.npc = 0.93,
    coef.digits = 4,
    parse = TRUE,
    family = "Montserrat",
    size = 2.5,
    small.p = TRUE
  ) +
  scale_color_manual(
    breaks = season_breaks,
    values = season_colors,
    guide = guide_legend(
      override.aes = list(size = 2, alpha = 1),
      label.theme = element_text(size = 5, family = "Montserrat Light")
    )
  ) +
  scale_shape_manual(
    breaks = area_breaks,
    values = area_pch
  ) +
  labs(
    x = quote(a[phi](443)),
    y = quote(a[NAP](443)),
    color = NULL
  ) +
  facet_wrap(~bioregion_name_wrap) +
  guides(shape = "none") +
  theme(
    panel.spacing.y = unit(3, "lines"),
    strip.text = element_text(size = 10),
    legend.justification = c(1, 0),
    legend.position = c(0.99, 0.05),
    legend.key.size = unit(0.5, "lines"),
    legend.background = element_blank()
  )

ggsave(
  here("graphs", "fig05.pdf"),
  device = cairo_pdf,
  width = 180,
  height = 80,
  units = "mm"
)

# GLM ---------------------------------------------------------------------

# Create the models for Emmanuel. He wants the outputs for the paper.

df <- absorption |>
  filter(if_all(c(aphy, anap), ~ . > 0)) |>
  filter(anap <= 0.05) |> # 1 obvious outlier
  group_nest(bioregion_name) |>
  mutate(mod_glm = map(data, ~ glm(
    anap ~ aphy,
    family = Gamma(link = "log"), data = .
  )))

df

df |>
  mutate(glm_tidy = map(mod_glm, tidy)) |>
  unnest(glm_tidy) |>
  select(-where(is.list))
