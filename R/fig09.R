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
  select(sample_id, date, season)

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
  summarise(across(contains("avw"), mean), n = n()) %>%
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
  ggplot(aes(x = date2, y = avw_aphy)) +
  geom_point(aes(color = bioregion_name)) +
  scale_color_manual(
    breaks = area_breaks,
    values = area_colors
  ) +
  geom_smooth(method = "lm", color = "#3c3c3c", size = 0.5, alpha = 0.2) +
  ggpmisc::stat_poly_eq(
    aes(label = ..eq.label..),
    label.y.npc = 0.12,
    label.x.npc = 0.2,
    size = 3,
    coef.digits = 4,
    family = "Montserrat"
  ) +
  ggpmisc::stat_poly_eq(
    label.y.npc = 0.05,
    label.x.npc = 0.2,
    aes(label = ..rr.label..),
    size = 3,
    family = "Montserrat"
  ) +
  labs(
    x = NULL,
    y = "Phytoplankton Apparent Absorption Wavelength (PAAW, nm)"
  ) +
  facet_grid(season ~ str_wrap_factor(bioregion_name, 20), scales = "free_y") +
  theme(
    legend.position = "none",
    panel.spacing = unit(1, "lines", data = NULL)
  )

ggsave(
  here("graphs","fig09.pdf"),
  device = cairo_pdf,
  width = 8,
  height = 6
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
