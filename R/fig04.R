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

formula <- y ~ x

p <- absorption %>%
  filter(if_all(c(aphy, anap), ~ . > 0)) %>%
  filter(anap <= 0.05) %>% # 1 obvious outlier
  ggplot(aes(x = aphy, y = anap)) +
  geom_point(
    aes(color = season, pch = bioregion_name),
    alpha = 0.5,
    stroke = 0.25
  ) +
  geom_smooth(
    method = "glm",
    se = FALSE,
    method.args = list(family = Gamma(link = "log")),
    color = "#3c3c3c",
    size = 0.5
  ) +
  ggpmisc::stat_fit_tidy(
    method = "glm",
    method.args = list(formula = formula, family = Gamma(link = "log")),
    label.x = "left",
    label.y = "top",
    family = "Montserrat",
    size = 2.5,
    aes(
      label = paste(
        "y~`=`~ italic(e)^{",
        signif(stat(Intercept_estimate), digits = 3),
        "~+~",
        signif(after_stat(x_estimate), digits = 3),
        "~x}",
        sep = ""
      )
    ),
    parse = TRUE
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
    legend.justification = c(1, 1),
    legend.position = c(0.96, 0.98),
    legend.key.size = unit(0.5, "lines"),
    legend.background = element_blank()
  )

ggsave(
  here("graphs", "fig04.pdf"),
  device = cairo_pdf,
  width = 180,
  height = 70,
  units = "mm"
)


# GLM ---------------------------------------------------------------------

# Create the models for Emmanuel. He wants the outputs for the paper.

df <- absorption %>%
  filter(if_all(c(aphy, anap), ~ . > 0)) %>%
  filter(anap <= 0.05) %>% # 1 obvious outlier
  group_nest(bioregion_name) %>%
  mutate(mod_glm = map(data, ~ glm(
    anap ~ aphy,
    family = Gamma(link = "log"), data = .
  )))

df

df %>%
  mutate(glm_tidy = map(mod_glm, tidy)) %>%
  unnest(glm_tidy) %>%
  select(-where(is.list))
