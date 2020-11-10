# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Map of the sampling locations.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

crs_string <-
  "+proj=lcc +lat_1=49 +lat_2=77 +lon_0=-91.52 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"

metadata <- read_csv("data/clean/metadata.csv")

metadata <- metadata %>%
  distinct(mission_name, longitude, latitude) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(crs = crs_string)

wm <- rnaturalearth::ne_states("canada", returnclass = "sf") %>%
  st_transform(crs = crs_string)

p <- metadata %>%
  ggplot() +
  geom_sf(data = wm, size = 0.1, fill = "#616368", color = "white") +
  geom_sf(size = 0.25, color = "#E92026") +
  coord_sf(
    xlim = c(-2827590, 3593626),
    ylim = c(5700000, NA)
  ) +
  labs(
    title = "Map of the sampling locations",
    subtitle = str_wrap("Only showing locations where we have a match between absorption spectra and metadata.", 70)
  ) +
  scale_x_continuous(breaks = seq(-180, 180, by = 10), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 90, by = 5), expand = c(0, 0)) +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    panel.border = element_blank(),
    axis.ticks = element_blank()
  )

ggsave(
  here::here("graphs/04_map_sampling_location.pdf"),
  device = cairo_pdf,
  width = 5.96,
  height = 5.2
)

pdftools::pdf_convert(
  here::here("graphs/04_map_sampling_location.pdf"),
  format = "png",
  filenames = here::here("graphs/04_map_sampling_location.png"),
  dpi = 300
)

# Spatial/temporal overview -----------------------------------------------

metadata <- read_csv("data/clean/metadata.csv")

df <- metadata %>%
  distinct(mission_name, date, longitude, latitude) %>%
  mutate(month = lubridate::month(date, label = TRUE, abbr = FALSE)) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(crs = crs_string)

p <- df %>%
  ggplot() +
  geom_sf(data = wm, size = 0.1, fill = "#616368", color = "white") +
  geom_sf(size = 0.25, color = "#E92026") +
  coord_sf(
    xlim = c(-2827590, 3593626),
    ylim = c(5700000, NA)
  ) +
  facet_wrap(~month) +
  labs(
    title = "Map of the sampling locations"
  ) +
  scale_x_continuous(breaks = seq(-180, 180, by = 10), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 90, by = 5), expand = c(0, 0)) +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_text(size = 6)
  )

file <- here::here("graphs/04_map_sampling_location_by_month.pdf")
ggsave(
  file,
  width = 8,
  height = 8
)

knitr::plot_crop(file)
