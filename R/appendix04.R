# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Show how the spectral shape of phytoplankton absorption change
# between 2000 and 2020.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

df <- read_csv(here(
  "data",
  "clean",
  "apparent_visible_wavelength_normalized_spectra.csv"
)) %>%
  filter(season %in% c("Spring", "Winter")) %>%
  filter(bioregion_name != "Labrador")

df

metadata <- read_csv(
  here("data", "clean", "metadata.csv"),
  col_select = c("sample_id", "date")
)

metadata

df <- df %>%
  inner_join(metadata, by = "sample_id") %>%
  mutate(year = lubridate::year(date), .after = sample_id)

df

# Select the earliest and latest years between 2000 and 2020 and calculate the
# average spectra.

df_viz <- df %>%
  group_by(bioregion_name, season) %>%
  filter(year == min(year) | year == max(year)) %>%
  group_by(bioregion_name, season, year, wavelength) %>%
  summarise(across(c(avw_aphy, aphy, normalized_aphy), mean)) %>%
  ungroup()

df_viz

df_viz %>%
  ggplot(aes(x = wavelength, y = normalized_aphy, color = factor(year), group = year)) +
  geom_line() +
  facet_grid(season ~ bioregion_name)
