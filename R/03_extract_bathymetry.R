# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Extract bathymetry at the sampling locations.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

crs_string <-
  "+proj=lcc +lat_1=49 +lat_2=77 +lon_0=-91.52 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"

metadata <- read_csv("data/clean/metadata.csv")

metadata

metadata <- metadata %>%
  drop_na(longitude, latitude) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

bathy <-
  read_stars(
    "data/raw/bathymetry/GEBCO_2020_20_Mar_2021_6b1b8dd40371/gebco_2020_n80.0_s35.0_w-90.0_e-20.0.tif"
  )

df <- st_extract(bathy, metadata) %>%
  as_tibble() %>%
  janitor::clean_names()

bathymetry <- df %>%
  select(bathymetry = gebco_2020_n80_0_s35_0_w_90_0_e_20_0_tif) %>%
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
  write_csv(here::here("data/clean/bathymetry.csv"))

# Histogram of the extracted bathymetry -----------------------------------

p <- bathymetry %>%
  ggplot(aes(x = -bathymetry)) +
  geom_histogram(binwidth = 0.05) +
  scale_x_log10(breaks = scales::breaks_log(n = 6)) +
  scale_y_continuous(breaks = scales::breaks_pretty(n = 6)) +
  annotation_logticks(sides = "b", size = 0.25) +
  labs(
    x = "Depth (m)",
    y = "Count",
    title = "Bathymetry at the sampling stations"
  )

ggsave(
  here::here("graphs/03_histogram_bathymetry.pdf"),
  device = cairo_pdf,
  width = 6,
  height = 5
)
