# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Define in which "regions" belong all the observations.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

metadata <- read_csv(here("data","clean","metadata.csv"))
bathymetry <- read_csv(here("data","clean","bathymetry.csv"))

metadata
bathymetry

# Only keep the variables needed for the classification
metadata <- metadata %>%
  inner_join(bathymetry, by = "sample_id") %>%
  select(sample_id, date, bathymetry, longitude, latitude)

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
    bioregion_name = case_when(
      bioregion_position == "North" ~ "Labrador",
      bathymetry <= -600 & bioregion_position == "South" ~ "NAB",
      bathymetry > -600 & bioregion_position == "South" ~ "Scotian Shelf",
      TRUE ~ NA_character_
    )
  ) %>% # re-classify 1 problematic station
  mutate(
    bioregion_name = case_when(
      bathymetry > -600 & longitude >= -50 ~ "Labrador",
      TRUE ~ bioregion_name
    )
  )

metadata %>%
  select(sample_id, bioregion_name, bioregion_position) %>%
  write_csv(here("data","clean","bioregions.csv"))

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
  here("graphs","04_number_observations_per_bioregion.pdf"),
  device = cairo_pdf,
  width = 7,
  height = 4
)

# Histogram of the extracted bathymetry -----------------------------------

metadata %>%
  ggplot(aes(x = -bathymetry)) +
  geom_histogram() +
  scale_x_log10(breaks = scales::breaks_log(n = 6)) +
  scale_y_continuous(breaks = scales::breaks_pretty(n = 6)) +
  annotation_logticks(sides = "b", size = 0.25) +
  geom_vline(xintercept = 600, color = "red") +
  facet_wrap(~bioregion_name, ncol = 1) +
  labs(
    x = "Depth (m)",
    y = "Count",
    title = "Bathymetry at the\nsampling stations"
  )

ggsave(
  here("graphs","04_histogram_bathymetry.pdf"),
  device = cairo_pdf,
  width = 4,
  height = 5
)
