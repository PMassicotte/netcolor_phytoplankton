# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Overview of the sample locations.
# Data source:  https://download.gebco.net/
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source(here("R", "zzz.R"))

stations <- read_csv(here("data", "clean", "metadata.csv"))
bioregions <- read_csv(here("data", "clean", "bioregions.csv"))

# crs_string <- "+proj=lcc +lat_1=49 +lat_2=77 +lon_0=-91.52 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"

bbox <- st_read(here("data", "clean", "bbox_sampling_area.json")) |>
  st_transform(crs = 4326)

# crs_string <- 4326

stations_sf <- stations |>
  inner_join(bioregions, by = "sample_id") |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

wm <- rnaturalearth::ne_countries(returnclass = "sf", scale = "large") |>
  st_crop(c(xmin = -100, xmax = 0, ymin = 20, ymax = 90))

canada <- rnaturalearth::ne_states("canada", returnclass = "sf")
usa <- rnaturalearth::ne_states("united states of america", returnclass = "sf")
greenland <- rnaturalearth::ne_states("greenland", returnclass = "sf")

# Bathymetry --------------------------------------------------------------

set.seed(2021)

bathy <- rast(
  here(
    "data",
    "raw",
    "bathymetry",
    "GEBCO_2020_13_May_2021_578bee3937bb",
    "gebco_2020_n75.0_s30.0_w-100.0_e-20.0.tif"
  )
) |>
  spatSample(size = 1e5, as.raster = TRUE, method = "regular") |>
  as.data.frame(xy = TRUE) |>
  as_tibble() |>
  rename(z = 3)

bathy_interpolated <- bathy |>
  mba.surf(no.X = 600, no.Y = 600, sp = TRUE) |>
  as.data.frame() |>
  as_tibble() |>
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
  scale_shape_manual(
    breaks = area_breaks,
    values = area_pch,
    labels = ~ str_wrap(., 20),
    guide = guide_legend(
      order = 1,
      label.position = "right",
      label.theme = element_text(
        size = 6,
        family = "Montserrat Light",
        color = "white"
      ),
      nrow = 3,
      override.aes = list(stroke = 0.5, color = "white")
    )
  ) +
  paletteer::scale_fill_paletteer_c(
    "ggthemes::Blue",
    direction = -1,
    breaks = scales::breaks_pretty(n = 5),
    oob = scales::squish,
    guide = guide_colorbar(
      title = "Depth (m)",
      title.position = "top",
      title.hjust = 0.5,
      title.theme = element_text(
        size = 6,
        family = "Montserrat",
        color = "black"
      ),
      label.theme = element_text(
        size = 5,
        family = "Montserrat",
        color = "black"
      ),
      barheight = unit(0.15, "cm"),
      barwidth = unit(3.5, "cm"),
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
  geom_sf(
    data = stations_sf,
    aes(shape = bioregion_name),
    size = 1,
    stroke = 0.2
  ) +
  geom_hline(
    yintercept = 48,
    lty = 2,
    size = 0.25,
    color = "gray25"
  ) +
  annotate(
    "text",
    x = Inf,
    y = 48.5,
    label = "48°N",
    hjust = 1,
    size = 3,
    color = "#3c3c3c"
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.08,
    height = unit(0.1, "cm"),
    line_width = 0.1,
    text_cex = 0.5,
  ) +
  annotation_north_arrow(
    location = "tr",
    which_north = "true",
    height = unit(0.9, "cm"),
    width = unit(0.9, "cm"),
    pad_x = unit(0.1, "in"),
    pad_y = unit(0.1, "in"),
    style = north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20",
      text_family = "ArcherPro Book",
      text_size = 6,
      line_width = 0.5
    )
  ) +
  coord_sf(xlim = st_bbox(bbox)[c(1, 3)], ylim = st_bbox(bbox)[c(2, 4)]) +
  theme(
    legend.title = element_blank(),
    legend.justification = c(0, 0),
    legend.position = c(0, 0.05),
    legend.background = element_blank(),
    legend.key.size = unit(0.5, "cm"),
    legend.key = element_rect(color = NA, fill = NA),
    legend.box = "vertical",
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_line(size = 0.25),
    axis.title = element_blank(),
    axis.text = element_text(size = 6, color = "gray50"),
    panel.background = element_rect(fill = "#B9DDF1"),
    strip.text = element_markdown(
      size = 5,
      face = "bold",
      family = "Montserrat Light"
    ),
    legend.spacing = unit(2.5, "cm")
  )

filename <- here("graphs", "fig01.pdf")

ggsave(
  filename,
  device = cairo_pdf,
  width = 180,
  height = 120,
  units = "mm"
)

knitr::plot_crop(filename)
