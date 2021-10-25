# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Relationship between PAAW and aphy. I want to see if PAAW could
# be used as an index to eventually get information on phytoplankton cell size.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

# Note that both aphy and aphy* ratio (443/675) give the same values. Here I
# will use aphy, but it can be compared with aphy* from other studies.

rm(list = ls())

# Prepare the data --------------------------------------------------------

aphy <- read_csv(here("data", "clean", "merged_dataset.csv")) %>%
  filter(wavelength %in% c(443, 675)) %>%
  select(sample_id, bioregion_name, wavelength, season, aphy)


paaw <- read_csv(here("data", "clean", "apparent_visible_wavelength.csv")) %>%
  select(sample_id, bioregion_name, avw_aphy)

df <- inner_join(aphy, paaw, by = c("sample_id", "bioregion_name"))

df

# Relation between aphy ratio and PAAW ------------------------------------

# It was shown that the aphy* 443/675 could be related to both as a function of
# cell size and irradiance (Fujiki2002).

df_viz <- df %>%
  pivot_wider(
    names_from = wavelength,
    values_from = aphy,
    names_prefix = "aphy_wl"
  ) %>%
  mutate(aphy_ratio = aphy_wl443 / aphy_wl675)

df_viz

formula <- y ~ x + I(x^2)

p <- df_viz %>%
  ggplot(aes(x = avw_aphy, y = aphy_ratio)) +
  geom_point(
    aes(fill = season),
    color = "transparent",
    size = 1.5,
    stroke = 0,
    pch = 21,
    alpha = 0.3
  ) +
  geom_smooth(
    formula = formula,
    color = "#3c3c3c",
    size = 0.5,
    alpha = 0.25
  ) +
  ggpmisc::stat_poly_eq(
    formula = formula,
    aes(label = ..eq.label..),
    parse = TRUE,
    coef.digits = 4,
    f.digits = 5,
    p.digits = 10,
    label.x.npc = 1,
    family = "Montserrat",
    size = 3
  ) +
  ggpmisc::stat_poly_eq(
    formula = formula,
    label.y.npc = 0.88,
    label.x.npc = 1,
    aes(label = ..rr.label..),
    size = 3,
    family = "Montserrat"
  ) +
  scale_fill_manual(
    breaks = c("Winter", "Spring", "Summer", "Autumn"),
    values = c("#014f86", "#40916c", "#ffcb69", "#e76f51"),
    guide = guide_legend(
      override.aes = list(size = 2, alpha = 1),
      label.theme = element_text(size = 8, family = "Montserrat")
    )
  ) +
  labs(
    x = "Phytoplankton Apparent Absorption Wavelength (PAAW, nm)",
    y = quote(a[phi] ~ (443) / a[phi] ~ (675))
  ) +
  # facet_wrap(~bioregion_name, ncol = 1) +
  theme(
    legend.title = element_blank(),
    legend.justification = c(0, 0),
    legend.position = c(0.02, 0.02),
    legend.key.size = unit(0.4, "cm")
  )

ggsave(
  here::here("graphs", "fig09.pdf"),
  device = cairo_pdf,
  width = 5,
  height = 4
)

## Model to predict aphy* ratio from PAAW ----

df_model <- df_viz %>%
  group_nest() %>%
  mutate(model = map(data, ~ lm(
    aphy_ratio ~ avw_aphy + I(avw_aphy^2),
    data = .
  ))) %>%
  mutate(tidied = map(model, tidy)) %>%
  mutate(augmented = map(model, augment))

df_model

df_model$model[[1]]
summary(df_model$model[[1]])

## Average by season ----

df_viz

df_viz %>%
  group_by(season) %>%
  summarise(mean_aphy_ratio = mean(aphy_ratio)) %>%
  arrange(mean_aphy_ratio)

range(df_viz$aphy_ratio)
