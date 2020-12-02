load("data/raw/BGP_NWA.Rdata")
load("data/raw/lalon.Rdata")


image(BGP_NWA[, 1742:1])

as.vector(BGP_NWA) %>%
  unique()

lat <- as.vector(latitude)
lon <- as.vector(longitude)

crs_string <-
  "+proj=lcc +lat_1=49 +lat_2=77 +lon_0=-91.52 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"

latlon <- tibble(lat, lon)

p <- latlon %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  ggplot() +
  geom_sf(size = 0.1, alpha = 0.1) +
  coord_sf(crs = crs_string)

ggsave(
  "~/Desktop/test.pdf",
  device = cairo_pdf
)

s <- latlon %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_as_stars()

ggplot() +
  geom_stars(data = s, downsample = 3) +
  coord_sf(crs = crs_string)

e <- raster::extent(latlon[, 2:1] %>% setNames(c("x", "y")))
r <- raster::raster(e, ncol = 190, nrow = 240, crs = "+proj=longlat +datum=WGS84")
r <- raster::rasterize(latlon[, 2:1], r, 0, fun = min)
r <- raster::projectRaster(r, crs = crs_string)
r <- st_as_stars(r)

plot(r)

test <- latlon %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  slice_sample(n = 1000)

ggplot() +
  geom_stars(data = r, color = "white") +
  geom_sf(data = test) +
  coord_sf(crs = crs_string)
