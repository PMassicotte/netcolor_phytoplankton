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

# All the data
df %>%
  ggplot(aes(x = date, y = avw_aphy)) +
  geom_point() +
  geom_smooth()

df %>%
  ggplot(aes(x = date, y = avw_aphy, shape = bioregion_name)) +
  geom_point() +
  geom_smooth() +
  scale_shape_manual(
    breaks = area_breaks,
    values = area_pch
  ) +
  facet_wrap(~bioregion_name, ncol = 1) +
  theme(
    legend.position = "none"
  )
