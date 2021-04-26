# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Overview of the sample locations.
# Data source:  https://download.gebco.net/
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source("R/zzz.R")

stations <- read_csv(here("data","clean","metadata.csv"))
bioregions <- read_csv(here("data","clean","bioregions.csv"))

# crs_string <- "+proj=lcc +lat_1=49 +lat_2=77 +lon_0=-91.52 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"

bbox <- st_read(here("data","clean","bbox_sampling_area.json")) %>%
  st_transform(crs = 4326)

# crs_string <- 4326

stations_sf <- stations %>%
  inner_join(bioregions, by = "sample_id") %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

wm <- rnaturalearth::ne_countries(returnclass = "sf", scale = "large") %>%
  st_crop(c(xmin = -100, xmax = 0, ymin = 20, ymax = 90))

canada <- rnaturalearth::ne_states("canada", returnclass = "sf")
usa <- rnaturalearth::ne_states("united states of america", returnclass = "sf")
greenland <- rnaturalearth::ne_states("greenland", returnclass = "sf")

# Bathymetry --------------------------------------------------------------

bathy <- raster::raster(
  here(
    "data",
    "raw",
    "bathymetry",
    "GEBCO_2020_26_Apr_2021_edb2b42fc5c7",
    "gebco_2020_n72.0_s36.0_w-95.0_e-25.0.tif"
  )
) %>%
  raster::sampleRegular(size = 1e5, asRaster = TRUE) %>%
  # raster::projectRaster(crs = crs_string) %>%
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

p <- ggplot() +
  ggisoband::geom_isobands(
    data = bathy_interpolated,
    aes(xyz.est.x, xyz.est.y, fill = xyz.est.z, z = xyz.est.z),
    bins = 10,
    color = NA
  ) +
  scale_color_manual(
    breaks = area_breaks,
    values = area_colors,
    guide = guide_legend(
      order = 1,
      label.position = "top",
      keyheight = unit(0.1, "cm"),
      keywidth = unit(3, "cm"),
      label.theme = element_text(size = 5, family = "Poppins"),
      nrow = 3
    )
  ) +
  # ggnewscale::new_scale_fill() +
  paletteer::scale_fill_paletteer_c(
    "ggthemes::Blue",
    direction = -1,
    oob = scales::squish,
    guide = guide_colorbar(
      title = "Depth (m)",
      title.position = "top",
      title.hjust = 0.5,
      title.theme = element_text(
        size = 6,
        family = "Poppins",
        color = "black"
      ),
      label.theme = element_text(
        size = 5,
        family = "Open Sans",
        color = "black"
      ),
      barheight = unit(0.15, "cm"),
      barwidth = unit(4, "cm"),
      direction = "horizontal"
    ),
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
  geom_sf(data = stations_sf, aes(color = bioregion_name), size = 0.25, key_glyph = "rect") +
  coord_sf(xlim = st_bbox(bbox)[c(1, 3)], ylim = st_bbox(bbox)[c(2, 4)]) +
  theme(
    legend.title = element_blank(),
    legend.position = "top",
    # legend.justification = c(1, 1),
    legend.background = element_blank(),
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_line(size = 0.25),
    axis.title = element_blank(),
    legend.key.size = unit(1, "cm"),
    legend.key = element_rect(color = NA, fill = NA),
    legend.box = "horizontal",
    axis.text = element_text(size = 4, color = "gray50"),
    panel.background = element_rect(fill = "#B9DDF1"),
    strip.text = element_markdown(size = 5, face = "bold", family = "Open Sans"),
    panel.spacing = unit(1, "lines")
  )

filename <- here::here("graphs","fig01.pdf")

ggsave(
  filename,
  device = cairo_pdf,
  width = 8,
  height = 5.2
)

knitr::plot_crop(filename)
