# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Define in which "regions" belong all the observations.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

metadata <- read_csv(here("data/clean/metadata.csv"))
bathymetry <- read_csv(here("data/clean/bathymetry.csv"))

metadata
bathymetry

# Only keep the variables needed for the classification
metadata <- metadata %>%
  inner_join(bathymetry, by = "sample_id") %>%
  select(sample_id, date, bathymetry, latitude)

metadata

# Extract the yday

metadata <- metadata %>%
  mutate(yday = lubridate::yday(date), .after = date)

# Define north or south position for each station -------------------------

metadata <- metadata %>%
  mutate(bioregion_position = case_when(
    latitude >= 48 ~ "North",
    TRUE ~ "South"
  ), .after = latitude)

metadata

# Classify ----------------------------------------------------------------

metadata <- metadata %>%
  mutate(
    bioregion = case_when(
      bathymetry > -300 & bioregion_position == "North" ~ "Region 1",
      bathymetry <= -300 & bioregion_position == "North" ~ "Region 2",
      bathymetry >= -300 & bioregion_position == "South" & yday <= 180 ~ "Region 3",
      bathymetry >= -300 & bioregion_position == "South" & yday > 180 ~ "Region 4",
      bathymetry < -300 & bioregion_position == "South"~ "Region 5",
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
  select(sample_id, bioregion_name, bioregion_position) %>%
  write_csv(here::here("data/clean/bioregions.csv"))

p <- metadata %>%
  count(bioregion_name) %>%
  mutate(bioregion_name = fct_reorder(bioregion_name, n)) %>%
  ggplot(aes(x = n, y = bioregion_name)) +
  geom_col() +
  labs(
    title = "Number of observations per bioregion",
    y = NULL
  ) +
  theme(
    plot.title.position = "plot"
  )

ggsave(
  here("graphs/04_number_observations_per_bioregion.pdf"),
  device = cairo_pdf,
  width = 7,
  height = 4
)

