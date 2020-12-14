# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Map of the sampling locations.
#
# Bathymetry data: https://download.gebco.net/
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

metadata <- vroom::vroom("data/clean/absorption_with_metadata.csv") %>%
  distinct(
    measurement_id,
    mission_name,
    longitude,
    latitude,
    date,
    position
  )

bathymetry <- read_csv(here::here("data/clean/bathymetry.csv"))
bioregion <- read_csv(here::here("data/clean/bioregions.csv"))

df <- metadata %>%
  inner_join(bathymetry, by = "measurement_id") %>%
  inner_join(bioregion, by = "measurement_id")

df

# Spatial operations ------------------------------------------------------

crs_string <-
  "+proj=lcc +lat_1=49 +lat_2=77 +lon_0=-91.52 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"

crs_string <- 4326

df <- df %>%
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

# Labels for the facets ---------------------------------------------------

df

df <- df %>%
  mutate(yday = lubridate::yday(date)) %>%
  mutate(bioregion_id = parse_number(bioregion)) %>%
  group_by(bioregion) %>%
  mutate(label = glue::glue("{bioregion_name}<br><span style = 'font-size:4pt; color:grey75;'>Bathymetry: {-max(bathymetry)} - {-min(bathymetry)} meters<br>DOY: {min(yday)} - {max(yday)}</span>")) %>%
  mutate(label = fct_reorder(label, bioregion_id)) %>%
  ungroup()

df

# Plot --------------------------------------------------------------------

# tibble(
#   longitude = c(-80, -40),
#   latitude = c(35, 70)
# ) %>%
#   st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
#   st_transform(crs = crs_string) %>%
#   st_bbox()

p <- df %>%
  # filter(bioregion_id == 1) %>%
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
      label.position = "left",
      title = "Depth (m)",
      title.position = "top",
      title.hjust = 0.5,
      title.theme = element_text(face = "bold", size = 8, family = "Poppins"),
      label.theme = element_text(size = 6, family = "Open Sans"),
      keyheight = unit(0.25, "cm"),
      keywidth = unit(0.75, "cm"),
      direction = "vertical"
    ),
    breaks = -seq(0, 3000, by = 250),
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
  geom_sf(size = 0.1, color = "#FA003F") +
  geom_sf(
    data = st_graticule(
      wm,
      lat = 48, lon = 150
    ),
    color = "#FFCF00",
    size = 0.25,
    lty = 2
  ) +
  coord_sf(
    crs = 4326,
    xlim = c(-80, -40),
    ylim = c(35, 70)
  ) +
  scale_x_continuous(breaks = seq(-180, 180, by = 10), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 90, by = 5), expand = c(0, 0)) +
  theme(
    legend.title = element_blank(),
    legend.position = c(0.9, 0.1),
    legend.justification = c(1, 0),
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_line(size = 0.25),
    axis.title = element_blank(),
    legend.key.size = unit(1, "cm"),
    legend.key = element_rect(color = NA, fill = NA),
    legend.box = "vertical",
    axis.text = element_text(size = 4, color = "gray50"),
    panel.background = element_rect(fill = "#B9DDF1"),
    strip.text = element_markdown(size = 5, face = "bold", family = "Open Sans"),
    panel.spacing = unit(1, "lines")
  ) +
  facet_wrap(~label)

ggsave(
  here::here("graphs/fig01.pdf"),
  device = cairo_pdf,
  width = 8,
  height = 5.2
)

knitr::plot_crop(here::here("graphs/fig01.pdf"))

pdftools::pdf_convert(
  here::here("graphs/fig01.pdf"),
  format = "png",
  filenames = here::here("graphs/fig01.png"),
  dpi = 300
)

# ggsave(
#   here::here("graphs/fig01.png"),
#   # res = 600,
#   width = 8,
#   height = 5.2,
#   dpi = 600
# )


# ragg::agg_png(
#   here::here("graphs/fig01.png"),
#   width = 4000,
#   height = 4000,
#   res = 600
# )
# print(p)
# invisible(dev.off())
