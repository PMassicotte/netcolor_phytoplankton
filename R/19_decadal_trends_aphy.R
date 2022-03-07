# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Check if there are any temporal variability in AVW (both aphy
# and anap).
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source(here("R","zzz.R"))

absorption <- read_csv(here("data", "clean", "merged_dataset.csv")) %>%
  select(sample_id, bioregion_name, date, wavelength, season, aphy) %>%
  filter(wavelength %in% c(443, 675))

absorption

absorption <- absorption %>%
  pivot_wider(
    names_from = wavelength,
    values_from = aphy,
    names_prefix = "aphy_"
  ) %>%
  mutate(aphy_443_675_ratio = aphy_443 / aphy_675)

absorption

# Order by season and bioregion
df <- absorption %>%
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
  summarise(across(contains("aphy"), mean), n = n()) %>%
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

p1 <- df_viz %>%
  filter(aphy_443 < 0.09) %>% # ?
  ggplot(aes(x = date2, y = aphy_443)) +
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
  facet_grid(season ~ str_wrap_factor(bioregion_name, 20), scales = "free_y") +
  theme(
    legend.position = "none",
    panel.spacing = unit(1, "lines", data = NULL),
    strip.text = element_text(size = 10)
  )

p2 <- df_viz %>%
  filter(aphy_443 < 0.09) %>% # ?
  ggplot(aes(x = date2, y = aphy_443_675_ratio)) +
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
  facet_grid(season ~ str_wrap_factor(bioregion_name, 20), scales = "free_y") +
  theme(
    legend.position = "none",
    panel.spacing = unit(1, "lines", data = NULL),
    strip.text = element_text(size = 10)
  )

p <- p1 / p2

ggsave(
  here("graphs","19_decadal_trends_aphy.pdf"),
  device = cairo_pdf,
  width = 180,
  height = 220,
  units = "mm"
)
