# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Relationship between PAAW and aphy*. I want to see if PAAW could
# be used as an index to eventually get information on phytoplankton cell size.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

# Prepare the data --------------------------------------------------------

aphy <- read_csv(here("data","clean","merged_dataset.csv")) %>%
  filter(wavelength %in% c(443, 675)) %>%
  select(sample_id, bioregion_name, wavelength, season, aphy, hplcchla) %>%
  mutate(aphy_specific = aphy / hplcchla) %>%
  select(-aphy, -hplcchla)

paaw <- read_csv(here("data","clean","apparent_visible_wavelength.csv")) %>%
  select(sample_id, bioregion_name, avw_aphy)

df <- inner_join(aphy, paaw, by = c("sample_id", "bioregion_name"))

df

# Relation between aphy* and PAAW -----------------------------------------

df %>%
  ggplot(aes(x = avw_aphy, y = aphy_specific)) +
  geom_point() +
  scale_y_log10() +
  geom_smooth(method = "lm") +
  ggpmisc::stat_poly_eq(
    aes(label = ..eq.label..),
    label.y.npc = 0.95,
    label.x.npc = 1,
    size = 2,
    coef.digits = 3,
    family = "Montserrat"
  ) +
  ggpmisc::stat_poly_eq(
    label.y.npc = 0.88,
    label.x.npc = 1,
    aes(label = ..rr.label..),
    size = 2,
    family = "Montserrat"
  ) +
  facet_grid(bioregion_name~wavelength)


# Relation between aphy* ratio and PAAW -----------------------------------

# It was shown that the aphy* 443/675 could be related to both as a function of
# cell size and irradiance (Fujiki2002).

df_viz <- df %>%
  pivot_wider(names_from = wavelength, values_from = aphy_specific, names_prefix = "wl") %>%
  mutate(aphy_ratio = wl443 / wl675)

df_viz

formula <- y ~ x + I(x^2)

p <- df_viz %>%
  ggplot(aes(x = avw_aphy, y = aphy_ratio)) +
  geom_point(size = 0.5, color = "#3c3c3c") +
  geom_smooth(
    formula = formula,
    color = "#bf1d28",
    size = 0.5
  ) +
  ggpmisc::stat_poly_eq(
    formula = formula,
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
    parse = TRUE,
    coef.digits = 4,
    f.digits = 5,
    p.digits = 10,
    label.x.npc = 1,
    family = "Montserrat",
    size = 3
  ) +
  labs(
    x = "Phytoplankton Apparent Absorption Wavelength (PAAW, nm)",
    y = quote(a[phi]^"*" ~ (443) / a[phi]^"*" ~ (675))
  )

ggsave(
  here::here("graphs","appendix02.pdf"),
  device = cairo_pdf,
  width = 6,
  height = 4
)

## Model to predict aphy* ratio from PAAW ----

df_model <- df_viz %>%
  group_nest() %>%
  mutate(model = map(data, ~ lm(aphy_ratio ~ avw_aphy + I(avw_aphy^2), data = .))) %>%
  mutate(tidied = map(model, tidy)) %>%
  mutate(augmented = map(model, augment))

df_model

df_model$model[[1]]
summary(df_model$model[[1]])
