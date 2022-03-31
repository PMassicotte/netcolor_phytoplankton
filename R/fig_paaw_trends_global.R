# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  More graphs asked by Emmanuel: could you compute the trends for
# the entire dataset and the 3 regions? I would like to see if there are no
# trends at the annual level, but significant trends at the seasonal level.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source(here("R","zzz.R"))

avw <- read_csv(here("data", "clean", "apparent_visible_wavelength.csv")) %>%
  filter(avw_aphy < 490)

metadata <- read_csv(here("data", "clean", "metadata.csv")) %>%
  select(sample_id, date, season)

df <- inner_join(avw, metadata, by = "sample_id")

df

p1 <- df %>%
  group_by(year = lubridate::year(date)) %>%
  summarise(mean_avw_aphy = mean(avw_aphy, na.rm = TRUE)) %>%
  ggplot(aes(x = year - 2000, y = mean_avw_aphy)) +
  geom_point() +
  geom_smooth(method = "lm") +
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
  )


p2 <- df %>%
  group_by(year = lubridate::year(date), bioregion_name) %>%
  summarise(mean_avw_aphy = mean(avw_aphy, na.rm = TRUE)) %>%
  ggplot(aes(x = year - 2000, y = mean_avw_aphy)) +
  geom_point() +
  geom_smooth(method = "lm") +
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
  facet_wrap(~bioregion_name)

p1 / p2

ggsave(
  here("graphs", "fig11.pdf"),
  device = cairo_pdf,
  width = 6,
  height = 6
)
