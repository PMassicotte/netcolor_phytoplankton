# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Calculate the distance of each station to the nearest land.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

# https://stackoverflow.com/questions/66968669/how-to-calculate-the-distance-to-the-closest-polygon-in-r-with-the-sf-package/66968905?noredirect=1#comment118377299_66968905

rm(list = ls())

source(here("R","zzz.R"))

bbox <- st_read(here("data","clean","bbox_sampling_area.json")) %>%
  st_transform(crs = 4326)

stations <- read_csv(here("data","clean","merged_dataset.csv")) %>%
  distinct(sample_id, bioregion_name, longitude, latitude) %>%
  drop_na(longitude, latitude) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

stations

ne_land <- rnaturalearth::ne_download(
  category = "physical",
  type = "land",
  returnclass = "sf",
  scale = "medium"
) %>%
  st_union() %>%
  st_crop(bbox)

# This is a workaround because some distances were calculated as 0. See the
# stackoverflow question for more information.

# st_nearest_points(stations, ne_land) %>%
#   st_length()

# Plot --------------------------------------------------------------------

p <- ggplot() +
  geom_sf(data = st_nearest_points(stations, ne_land), size = 0.1) +
  geom_sf(data = ne_land, size = 0.1) +
  geom_sf(data = stations, aes(color = bioregion_name), size = 0.25) +
  # coord_sf(xlim = c(-16, 16), ylim = c(25, 60)) +
  scale_color_manual(
    breaks = area_breaks,
    values = area_colors,
    guide = guide_legend(
      override.aes = list(size = 2),
      ncol = 3
    )
  ) +
  labs(
    title = str_wrap("Visualization of the shortest distances to the land", 40)
  ) +
  coord_sf(xlim = st_bbox(bbox)[c(1, 3)], ylim = st_bbox(bbox)[c(2, 4)]) +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    plot.title.position = "plot"
  )

# outfile <- here("graphs","16_geographical_map_stations_shortest_distances.pdf")
#
# ggsave(
#   outfile,
#   device = cairo_pdf,
#   width = 7,
#   height = 7
# )

# knitr::plot_crop(outfile)

# ggplot() +
#   geom_sf(data = st_union(ne_land)) +
#   geom_sf(data = stations) +
#   coord_sf(xlim = c(2, 5), ylim = c(50, 54))

# Calculate the distances -------------------------------------------------

stations <- stations %>%
  mutate(distance_to_shore_m = as.vector(st_distance(stations, ne_land)))

# TODO: Some distances are calculated as 0. Try to figure out why (see
# stackoverflow).

stations %>%
  filter(distance_to_shore_m == 0)

stations %>%
  as_tibble() %>%
  select(sample_id, distance_to_shore_m) %>%
  write_csv(here("data","clean","distances_to_shore.csv"))
