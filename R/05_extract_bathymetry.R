# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Extract bathymetry at the sampling locations.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

crs_string <-
  "+proj=lcc +lat_1=49 +lat_2=77 +lon_0=-91.52 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"

metadata <- vroom::vroom("data/clean/absorption_with_metadata.csv")

metadata

metadata <- metadata %>%
  distinct(measurement_id, mission_name, longitude, latitude, position) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(crs = crs_string)

bathy <-
  read_stars(
    "data/raw/bathymetry/GEBCO_2020_01_Dec_2020_dc69d6fb1af3/gebco_2020_n70.0_s35.0_w-80.0_e-40.0.tif"
  )

df <- st_extract(bathy, metadata) %>%
  as_tibble() %>%
  janitor::clean_names()

bathymetry <- df %>%
  select(bathymetry = gebco_2020_n70_0_s35_0_w_80_0_e_40_0_tif) %>%
  bind_cols(metadata, .) %>%
  as_tibble() %>%
  select(measurement_id, bathymetry)

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
  here::here("graphs/05_histogram_bathymetry.pdf"),
  device = cairo_pdf,
  width = 6,
  height = 5
)

# Count how many observation there are in each bin ------------------------

p <- bathymetry %>%
  left_join(metadata) %>%
  count(bathymetry_bin, position) %>%
  mutate(position = fct_relevel(position, c("South", "North"))) %>%
  ggplot(aes(x = n, y = position, fill = bathymetry_bin)) +
  geom_col(position = "dodge") +
  paletteer::scale_fill_paletteer_d(
    "ggsci::default_locuszoom",
    guide = guide_legend(
      label.position = "top",
      keywidth = unit(3, "cm"),
      keyheight = unit(0.25, "cm")
    )
  ) +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 10)) +
  labs(
    x = "Number of observation",
    y = NULL,
    title = "Number of observation per position and bathymetry bin",
    subtitle = "North/south stations classified using 52.5 degree latitude."
  ) +
  theme(
    legend.position = "top",
    legend.title = element_blank()
  )

ggsave(
  here::here("graphs/06_number_observation_position_bathymetry_class.pdf"),
  device = cairo_pdf,
  width = 8,
  height = 6
)
