# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Map of the sampling locations.
#
# Bathymetry data: https://download.gebco.net/
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

crs_string <-
  "+proj=lcc +lat_1=49 +lat_2=77 +lon_0=-91.52 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"

metadata <- vroom::vroom("data/clean/absorption_with_metadata.csv")

bathymetry <- read_csv(here::here("data/clean/bathymetry.csv"))

metadata <- metadata %>%
  left_join(bathymetry, by = "measurement_id") %>%
  distinct(mission_name, longitude, latitude, bathymetry)


metadata <- metadata %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(crs = crs_string)

canada <- rnaturalearth::ne_states("canada", returnclass = "sf") %>%
  st_transform(crs = crs_string)

usa <- rnaturalearth::ne_states("united states of america", returnclass = "sf") %>%
  st_transform(crs = crs_string)

greenland <- rnaturalearth::ne_states("greenland", returnclass = "sf") %>%
  st_transform(crs = crs_string)

wm <- rnaturalearth::ne_countries(returnclass = "sf") %>%
  st_transform(crs = crs_string)

# Prepare bathymetry data -------------------------------------------------

bathy <- raster::raster(
  "data/raw/bathymetry/GEBCO_2020_01_Dec_2020_dc69d6fb1af3/gebco_2020_n70.0_s35.0_w-80.0_e-40.0.tif"
) %>%
  raster::sampleRegular(size = 1e5, asRaster = TRUE) %>%
  raster::projectRaster(crs = crs_string) %>%
  raster::rasterToPoints() %>%
  as_tibble() %>%
  rename(z = 3)

bathy_interpolated <- bathy %>%
  mba.surf(no.X = 600, no.Y = 600, sp = TRUE) %>%
  as.data.frame() %>%
  as_tibble() %>%
  mutate(xyz.est.z = ifelse(xyz.est.z >= 0, 0, xyz.est.z))

range(bathy_interpolated$xyz.est.z, na.rm = TRUE)

# Plot --------------------------------------------------------------------

p <- metadata %>%
  ggplot() +
  ggisoband::geom_isobands(
    data = bathy_interpolated,
    aes(xyz.est.x, xyz.est.y, fill = xyz.est.z, z = xyz.est.z),
    bins = 10,
    color = NA
  ) +
  paletteer::scale_color_paletteer_d(
    "ggsci::default_locuszoom",
    guide = guide_legend(
      label.position = "bottom",
      title = "Bathymetry class",
      title.position = "top",
      title.hjust = 0.5,
      title.theme = element_text(face = "bold", size = 8, family = "Poppins"),
      label.theme = element_text(size = 6, family = "Poppins"),
      keyheight = unit(0.25, "cm"),
      keywidth = unit(1.5, "cm"),
      byrow = TRUE,
      nrow = 1,
      override.aes = list(size = 2)
    ),
    breaks = c("0-300 m", "300-1000 m", "1000+ m")
  ) +
  paletteer::scale_fill_paletteer_c(
    "ggthemes::Blue",
    direction = -1,
    limits = c(-4000, 0),
    oob = scales::squish,
    guide = guide_legend(
      label.position = "top",
      title = "Depth (m)",
      title.position = "top",
      title.hjust = 0.5,
      title.theme = element_text(face = "bold", size = 8, family = "Poppins"),
      label.theme = element_text(size = 6, family = "Poppins"),
      keyheight = unit(0.25, "cm"),
      keywidth = unit(0.75, "cm"),
      byrow = TRUE,
      nrow = 1
    ),
    breaks = -seq(0, 3000, by = 500),
    na.value = "#B9DDF1"
  ) +
  geom_sf(
    data = usa,
    size = 0.1,
    fill = "grey75",
    color = "white"
  ) +
  geom_sf(
    data = greenland,
    size = 0.1,
    fill = "grey75",
    color = "white"
  ) +
  geom_sf(
    data = canada,
    size = 0.1,
    fill = "#616368",
    color = "white"
  ) +
  geom_sf(size = 0.25, aes(color = bathymetry_bin)) +
  geom_sf(
    data = st_graticule(
      wm,
      lat = 52.5, lon = 150
    ),
    color = "red",
    size = 0.25,
    lty = 2
  ) +
  coord_sf(
    crs = crs_string,
    xlim = c(-2827590, 3593626),
    ylim = c(5700000, 11239251)
  ) +
  scale_x_continuous(breaks = seq(-180, 180, by = 10), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 90, by = 5), expand = c(0, 0)) +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_line(size = 0.25),
    axis.title = element_blank(),
    legend.key.size = unit(1, "cm"),
    legend.key = element_rect(color = NA, fill = NA)
  )

ggsave(
  here::here("graphs/fig01.pdf"),
  device = cairo_pdf,
  width = 5.96,
  height = 5.2
)

knitr::plot_crop(here::here("graphs/fig01.pdf"))
