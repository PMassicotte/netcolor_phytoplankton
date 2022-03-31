# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Check if there are any temporal variability in AVW (both aphy
# and anap).
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source(here("R","zzz.R"))

avw <- read_csv(here("data", "clean", "apparent_visible_wavelength.csv"))

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
  mutate(bioregion_name = factor(
    bioregion_name,
    levels = c(
      "Scotian Shelf",
      "Northwest Atlantic Basin ocean (NAB)",
      "Labrador"
    )
  ))

df

# Average by year and make sure that at least 10 observations were used for the
# calculation.

df_viz <- df %>%
  mutate(date2 = clock::date_group(date, "year")) %>%
  group_by(bioregion_name, date2, season) %>%
  summarise(across(c(contains("avw"), longitude, latitude, yday), mean), n = n()) %>%
  ungroup() %>%
  filter(n >= 10)

df_viz

# Need at least 5 points to see a temporal trend?

df_viz <- df_viz %>%
  group_by(bioregion_name, season) %>%
  filter(n() >= 5) %>%
  ungroup()

df_viz

df_viz <- df_viz %>%
  filter(season %in% c("Autumn", "Spring"))

# Plot --------------------------------------------------------------------

# There are very few measurements in autumn in the Labrador sea (n = 21). That
# is why these are no plot in the Labrador/Autumn facet.

df %>%
  count(bioregion_name, season)

p <- df_viz %>%
  filter(avw_aphy < 490) %>% # ?
  group_by(season, bioregion_name) %>%
  mutate(date_from_zero = lubridate::year(date2 - lubridate::years(2000))) %>%
  ungroup() %>%
  ggplot(aes(x = date_from_zero, y = avw_aphy)) +
  geom_point(aes(color = season, pch = bioregion_name)) +
  scale_color_manual(
    breaks = season_breaks,
    values = season_colors
  ) +
  scale_shape_manual(
    breaks = area_breaks,
    values = area_pch
  ) +
  geom_smooth(method = "lm", color = "#3c3c3c", size = 0.5, alpha = 0.2) +
  ggpmisc::stat_poly_eq(
    aes(label = ..eq.label..),
    label.y.npc = 0.12,
    label.x.npc = 0.2,
    size = 2.5,
    coef.digits = 4,
    family = "Montserrat"
  ) +
  ggpmisc::stat_poly_eq(
    label.y.npc = 0.05,
    label.x.npc = 0.2,
    aes(label = ..rr.label..),
    size = 2.5,
    family = "Montserrat"
  ) +
  labs(
    x = NULL,
    y = "Phytoplankton Apparent Absorption Wavelength (PAAW, nm)"
  ) +
  facet_grid(season ~ str_wrap_factor(bioregion_name, 20), scales = "free_y") +
  theme(
    legend.position = "none",
    panel.spacing = unit(1, "lines", data = NULL),
    strip.text = element_text(size = 10)
  )

ggsave(
  here("graphs","fig08.pdf"),
  device = cairo_pdf,
  width = 180,
  height = 120,
  units = "mm"
)

# Calculate the average increase/decrease of PAAW in spring and autumn.

df_viz

# TODO: See if should report these min/max in the manuscript
df_res <- df_viz %>%
  mutate(year = lubridate::year(date2)) %>%
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
  filter(avw_aphy < 490) %>% # ?
  mutate(date_from_zero = lubridate::year(date2 - lubridate::years(2000))) %>%
  group_by(bioregion_name, season) %>%
  mutate(prop_n = n / sum(n)) %>%
  nest() %>%
  mutate(mod = map(data, ~ lm(
    avw_aphy ~ date_from_zero,
    data = ., weights = prop_n
  ))) %>%
  mutate(mod_spatial = map(
    data,
    ~ lm(
      avw_aphy ~ date_from_zero + longitude + latitude + yday,
      data = .,
      weights = prop_n
    )
  )) %>%
  mutate(tidied = map(mod, tidy)) %>%
  mutate(glanced = map(mod, glance)) %>%
  mutate(augmented = map(mod, augment))

coeffs <- mod %>%
  unnest(tidied)

mod %>%
  unnest(augmented) %>%
  ggplot(aes(x = date_from_zero, y = avw_aphy)) +
  geom_point(aes(color = season, pch = bioregion_name)) +
  scale_color_manual(
    breaks = season_breaks,
    values = season_colors
  ) +
  scale_shape_manual(
    breaks = area_breaks,
    values = area_pch
  ) +
  geom_line(aes(y = .fitted)) +
  geom_text(
    data = coeffs %>% filter(term == "(Intercept)"),
    aes(x = 0, y = 470, label = round(estimate, digits = 2), hjust = -0.1)
  ) +
  geom_text(
    data = coeffs %>% filter(term == "date_from_zero"),
    aes(x = 0, y = 467, label = round(estimate, digits = 7), hjust = -0.1)
  ) +
  labs(
    x = NULL,
    y = "Phytoplankton Apparent Absorption Wavelength (PAAW, nm)",
    title = "Weighted lm"
  ) +
  facet_grid(season ~ str_wrap_factor(bioregion_name, 20), scales = "free_y") +
  theme(
    legend.position = "none",
    panel.spacing = unit(1, "lines", data = NULL),
    strip.text = element_text(size = 10)
  )

ggsave(
  here("graphs","fig08b.pdf"),
  device = cairo_pdf,
  width = 180,
  height = 120,
  units = "mm"
)

mod %>%
  select(bioregion_name, season, mod_spatial) %>%
  mutate(tidied = map(mod_spatial, tidy)) %>%
  unnest(tidied) %>%
  select(-mod_spatial) %>%
  gt::gt()
