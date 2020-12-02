rm(list = ls())

non_algal_absorption_slope <- read_csv("data/clean/non_algal_absorption_slope.csv") %>%
  select(measurement_id, non_algal_absorption_slope)

metadata <- read_csv("data/clean/metadata.csv")

hplc_pigments <-
  c(
    "hplc_chla",
    "hplc_chlb",
    "hplc_chlc3",
    "hplc_fucox",
    "hplc_perid",
    "hplc_zea",
    "hplc_allox",
    "hplc_but19_total",
    "hplc_hex19_total",
    "hplc_phaeo",
    "hplc_prasinox"
  )

metadata <- metadata %>%
  select(
    measurement_id,
    depth,
    salinity:silicate,
    date,
    longitude,
    latitude,
    all_of(hplc_pigments)
  )

df <- non_algal_absorption_slope %>%
  inner_join(metadata, by = "measurement_id") %>%
  drop_na()

df

# Visualize the input data ------------------------------------------------

df %>%
  count(longitude, latitude, depth, sort = TRUE) %>%
  filter(n == max(n)) %>%
  semi_join(df, .) %>%
  janitor::remove_constant() %>%
  pivot_longer(starts_with("hplc")) %>%
  group_by(name) %>%
  arrange(date) %>%
  # filter(name == "hplc_chlc3") %>%
  filter(between(value, 0, 10)) %>%
  mutate(running_mean_value = RcppRoll::roll_median(value, n = 14, fill = NA)) %>%
  ggplot(aes(x = date, y = value)) +
  geom_point() +
  geom_line(aes(y = running_mean_value), color = "red") +
  facet_wrap(~name, scales = "free")

# Center scale ------------------------------------------------------------

df <- df %>%
  relocate(c(measurement_id, depth, longitude, latitude, date), .before = 1) %>%
  mutate(across(-c(measurement_id, depth, longitude, latitude, date), ~ as.vector(scale(.))))

df

# Perform the clustering --------------------------------------------------


df <- df %>%
  crossing(k = 1:12) %>%
  group_nest(k) %>%
  mutate(kclust = map2(data, k, function(data, k) {
    data %>%
      # select(salinity) %>%
      select(-measurement_id, -longitude, -latitude, -date) %>%
      kmeans(., k)
  })) %>%
  mutate(tidied = map(kclust, tidy)) %>%
  mutate(glanced = map(kclust, glance)) %>%
  mutate(augmented = map2(kclust, data, augment))

df %>%
  unnest(glanced) %>%
  ggplot(aes(x = k, y = tot.withinss)) +
  geom_line() +
  geom_point()

df %>%
  unnest(augmented) %>%
  count(k, .cluster) %>%
  ggplot(aes(x = n, y = .cluster)) +
  geom_col() +
  facet_wrap(~k, scales = "free_y")

p <- df %>%
  unnest(augmented) %>%
  # filter(k == 2) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(crs = "+proj=lcc +lat_1=49 +lat_2=77 +lon_0=-91.52 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") %>%
  ggplot() +
  geom_sf(aes(color = .cluster)) +
  facet_wrap(~k)

ggsave(
  "~/Desktop/test_clusters.pdf",
  device = cairo_pdf,
  width = 20,
  height = 20
)


# df %>%
#   unnest(data) %>%
#   select(-where(is.list), -k) %>%
#   pivot_longer(-c(measurement_id, longitude, latitude)) %>%
#   ggplot(aes(x = value)) +
#   geom_histogram() +
#   facet_wrap(~name, scales = "free_x")


# Temporal cluster? -------------------------------------------------------

df %>%
  unnest(augmented) %>%
  filter(k == 3) %>%
  mutate(month = lubridate::month(date, label = TRUE)) %>%
  count(.cluster, month) %>%
  ggplot(aes(x = n, y = month, fill = .cluster)) +
  geom_col()
