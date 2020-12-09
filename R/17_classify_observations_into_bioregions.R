# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Define in which "regions" belong all the observations.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

metadata <- vroom::vroom("data/clean/absorption_with_metadata.csv")
bathymetry <- read_csv(here::here("data/clean/bathymetry.csv"))

metadata
bathymetry

metadata <- metadata %>%
  inner_join(bathymetry, by = "measurement_id") %>%
  distinct(
    measurement_id,
    latitude,
    date,
    bathymetry
  ) %>%
  mutate(yday = lubridate::yday(date))

# Define north or south position for each station -------------------------

metadata <- metadata %>%
  mutate(position = case_when(
    latitude >= 48 ~ "North",
    TRUE ~ "South"
  ), .after = latitude)

metadata

# Classify ----------------------------------------------------------------

metadata <- metadata %>%
  mutate(
    bioregion = case_when(
      bathymetry > -300 & position == "North" ~ "Region 1",
      bathymetry <= -300 & position == "North" ~ "Region 2",
      bathymetry >= -300 & position == "South" & yday <= 180 ~ "Region 3",
      bathymetry >= -300 & position == "South" & yday > 180 ~ "Region 4",
      bathymetry < -300 & position == "South"~ "Region 5",
      TRUE ~ NA_character_
    )
  ) %>%
  mutate(
    bioregion_name = case_when(
      bioregion == "Region 1" ~ "Labrador & Greenland Shelves (LGS)",
      bioregion == "Region 2" ~ "Labrador Sea Bassin (LSB)",
      bioregion == "Region 3" ~ "Scotian Shelf Spring (SSSp)",
      bioregion == "Region 4" ~ "Scotian Shelf Fall (SSFa)",
      bioregion == "Region 5" ~ "Northwest Atlantic Bassin ocean (NAB)",
      TRUE ~ NA_character_
    )
  )

metadata %>%
  filter(is.na(bioregion)) %>%
  ggplot(aes(x = yday)) +
  geom_histogram(binwidth = 5)

metadata %>%
  count(bioregion) %>%
  as_tibble() %>%
  ggplot(aes(x = n, y = bioregion)) +
  geom_col()

metadata %>%
  select(measurement_id, bioregion, bioregion_name) %>%
  write_csv(here::here("data/clean/bioregions.csv"))
