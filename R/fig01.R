# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Map of the sampling locations.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

crs_string <-
  "+proj=lcc +lat_1=49 +lat_2=77 +lon_0=-91.52 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"

metadata <- vroom::vroom("data/clean/absorption_with_metadata.csv")

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
  scale_x_continuous(breaks = seq(-180, 180, by = 10), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 90, by = 5), expand = c(0, 0)) +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_line(size = 0.25)
  )

ggsave(
  here::here("graphs/fig01.pdf"),
  device = cairo_pdf,
  width = 5.96,
  height = 5.2
)

knitr::plot_crop(here::here("graphs/fig01.pdf"))
