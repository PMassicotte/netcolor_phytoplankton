# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Extract bathymetry at the sampling locations.
# Data: https://download.gebco.net/
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

crs_string <-
  "+proj=lcc +lat_1=49 +lat_2=77 +lon_0=-91.52 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"

metadata <- read_csv(here("data", "clean", "metadata.csv"))

metadata

metadata <- metadata %>%
  drop_na(longitude, latitude) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

bathy <- read_stars(
  here(
    "data",
    "raw",
    "bathymetry",
    "GEBCO_2020_13_May_2021_578bee3937bb",
    "gebco_2020_n75.0_s30.0_w-100.0_e-20.0.tif"
  )
)

df <- st_extract(bathy, metadata) %>%
  as_tibble() %>%
  janitor::clean_names()

bathymetry <- df %>%
  select(bathymetry = gebco_2020_n75_0_s30_0_w_100_0_e_20_0_tif) %>%
  bind_cols(metadata, .) %>%
  as_tibble() %>%
  select(sample_id, bathymetry) %>%
  mutate(bathymetry = as.vector(bathymetry))

bathymetry

# Define depth bins -------------------------------------------------------

bathymetry <- bathymetry %>%
  mutate(bathymetry_bin = case_when(
    between(bathymetry, -300, 0) ~ "0-300 m",
    between(bathymetry, -1000, -299.999) ~ "300-1000 m",
    between(bathymetry, -10000, -999.999) ~ "1000+ m"
  )) %>%
  mutate(bathymetry_bin = fct_relevel(
    bathymetry_bin,
    c(
      "0-300 m",
      "300-1000 m",
      "1000+ m"
    )
  ))

bathymetry

# Save the data -----------------------------------------------------------

bathymetry %>%
  write_csv(here("data", "clean", "bathymetry.csv"))
