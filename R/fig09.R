# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Check if there are any temporal variability in AVW (both aphy
# and anap).
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source(here("R","zzz.R"))

avw <- read_csv(here("data", "clean", "apparent_visible_wavelength.csv")) %>%
  filter(avw_aphy < 490)

metadata <- read_csv(here("data", "clean", "metadata.csv")) %>%
  select(sample_id, date, season, longitude, latitude, date) %>%
  mutate(yday = lubridate::yday(date))

df <- inner_join(avw, metadata, by = "sample_id")

df

# Order by season and bioregion
df <- df %>%
  mutate(season = factor(season,
    levels = c("Spring", "Summer", "Autumn", "Winter")
  )) %>%
  mutate(bioregion_name = factor(bioregion_name,
    levels = c("Scotian Shelf", "NAB", "Labrador")
  ))

df

# Average by year and make sure that at least 10 observations were used for the
# calculation.

df_viz <- df %>%
  mutate(year = lubridate::year(date)) %>%
  group_by(bioregion_name, year, season) %>%
  summarise(across(c(contains("avw")), mean), n = n()) %>%
  ungroup() %>%
  filter(n >= 10)

df_viz

# Need at least 5 points to see a temporal trend?

df_viz <- df_viz %>%
  group_by(bioregion_name, season) %>%
  filter(n() >= 5) %>%
  ungroup()

df_viz <- df_viz %>%
  filter(season %in% c("Autumn", "Spring"))

# Plot --------------------------------------------------------------------

# There are very few measurements in autumn in the Labrador sea (n = 21). That
# is why these are no plot in the Labrador/Autumn facet.

df %>%
  count(bioregion_name, season)

# How to make a weighted lm directly in ggplot2:
# https://bitbucket.org/aphalo/ggpmisc/issues/16/ggpmisc-stat_poly_eq-does-not-consider

# Remove aes(weight = n) if we do not want to use a weighted lm

p <- df_viz %>%
  ggplot(aes(x = year, y = avw_aphy)) +
  geom_point(aes(color = season, pch = bioregion_name)) +
  scale_color_manual(
    breaks = season_breaks,
    values = season_colors
  ) +
  scale_shape_manual(
    breaks = area_breaks,
    values = area_pch
  ) +
  # geom_smooth(method = "lm", formula = y ~ x, color = "#3c3c3c", size = 0.5, alpha = 0.2) +
  geom_smooth(
    aes(weight = n),
    method = "lm",
    formula = y ~ x,
    color = "#3c3c3c",
    size = 0.5,
    alpha = 0.2
  ) +
  ggpmisc::stat_poly_eq(
    aes(weight = n, label = ..eq.label..),
    formula = y ~ x,
    label.x.npc = 0.1,
    label.y.npc = 0.12,
    coef.digits = 4,
    parse = TRUE,
    family = "Montserrat",
    size = 2.5,
  ) +
  ggpmisc::stat_poly_eq(
    aes(weight = n, label = ..rr.label..),
    formula = y ~ x,
    label.x.npc = 0.1,
    label.y.npc = 0.05,
    coef.digits = 4,
    parse = TRUE,
    family = "Montserrat",
    size = 2.5,
  ) +
  labs(
    x = NULL,
    y = "PAAW (nm)"
  ) +
  facet_grid(season ~ str_wrap_factor(bioregion_name, 20), scales = "fixed") +
  theme(
    legend.position = "none",
    panel.spacing = unit(1, "lines", data = NULL),
    strip.text = element_text(size = 10)
  )

ggsave(
  here("graphs","fig09.pdf"),
  device = cairo_pdf,
  width = 180,
  height = 120,
  units = "mm"
)

# Calculate the average increase/decrease of PAAW in spring and au --------

df_viz

# TODO: See if should report these min/max in the manuscript
df_res <- df_viz %>%
  group_nest(bioregion_name, season) %>%
  mutate(model = map(data, ~lm(avw_aphy ~ year, data = .))) %>%
  mutate(augmented = map(model, augment)) %>%
  mutate(tidied = map(model, tidy))

df_res

df_res %>%
  unnest(tidied)

# Weighted lm ------------------------------------------------------------- I

# I have some comments here, I think we should 1) weight the linear regression
# by the number of data used in the mean of a given year/region 2) perhaps do a
# GLM rather than a lm and used the mean lat/lon and day of year of a given
# region/year to include errors due to the sampling strategy, i have some code
# for this if you want.

df_viz

mod <- df_viz %>%
  group_by(bioregion_name, season) %>%
  mutate(prop_n = n / sum(n)) %>%
  nest() %>%
  mutate(mod = map(data, ~ lm(
    avw_aphy ~ year,
    data = ., weights = n
  ))) %>%
  mutate(tidied = map(mod, tidy)) %>%
  mutate(glanced = map(mod, glance)) %>%
  mutate(augmented = map(mod, augment))

# These coefficients should be the same as those presented on the graph using
# the ggpmisc package

mod %>%
  unnest(glanced)

coeffs <- mod %>%
  unnest(tidied)

mod %>%
  select(bioregion_name, season, mod) %>%
  mutate(tidied = map(mod, tidy)) %>%
  unnest(tidied) %>%
  select(-mod) %>%
  gt::gt()
