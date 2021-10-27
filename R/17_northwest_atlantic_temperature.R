urls <-
  c(
    "https://www.ncei.noaa.gov/data/oceans/archive/arc0092/0155889/1.1/data/0-data/DATA/temperature/csv/all/1.00/nwa_all_t13mn01.csv",
    "https://www.ncei.noaa.gov/data/oceans/archive/arc0092/0155889/1.1/data/0-data/DATA/temperature/csv/all/1.00/nwa_all_t15mn01.csv"
  )

df <- read_csv(urls, skip = 1, num_threads = 30, id = "file")

df <- df %>%
  janitor::clean_names() %>%
  pivot_longer(
    -c(file, number_comma_separated_latitude, longitude),
    names_to = "depth_m",
    values_to = "temperature",
    names_transform = list(depth_m = parse_number)
  ) %>%
  filter(depth_m == 0) %>%
  mutate(season = case_when(
    str_detect(file, "t13mn01") ~ "Winter",
    str_detect(file, "t15mn01") ~ "Summer",
    TRUE ~ NA_character_
  ))

df %>%
  ggplot(aes(x = temperature)) +
  geom_histogram() +
  facet_wrap(~season)

df %>%
  ggplot(aes(x = season, y = temperature)) +
  geom_boxplot()

df %>%
  group_by(season) %>%
  summarise(across(temperature, .fns = list(min, max), na.rm = TRUE))
