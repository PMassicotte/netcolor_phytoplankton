rm(list = ls())

source("R/zzz.R")

# Prepare the data --------------------------------------------------------

df <- fread(here::here("data/clean/merged_dataset.csv")) %>%
  as_tibble() %>%
  filter(wavelength == 443)

df %>%
  count(sample_id) %>%
  assertr::verify(n == 1)

# Temporal cycles ---------------------------------------------------------

range(df$date)

df

df %>%
  # group_by(bioregion_name, date) %>%
  # summarise(across(c(aphy), ~mean(., na.rm = TRUE))) %>%
  ggplot(aes(x = date, y = ap, group = bioregion_name)) +
  geom_point(color = "gray75", size = 0.25) +
  geom_smooth(aes(color = bioregion_name), method = "gam") +
  facet_wrap(~bioregion_name, scales = "free_y") +
  labs(
    title = "Temporal "
  ) +
  theme(
    legend.position = "none"
  )
