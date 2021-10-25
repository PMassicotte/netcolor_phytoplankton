# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Explore the metadata.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

df <- fread(here("data","clean","merged_dataset.csv")) %>%
  as_tibble() %>%
  filter(wavelength == 443)

# Number of measurements per year -----------------------------------------

df %>%
  mutate(year = lubridate::year(date)) %>%
  count(year, sort = TRUE) %>%
  ggplot(aes(x = year, y = n)) +
  geom_line() +
  geom_point(size = 6, color = "#3c3c3c") +
  geom_text(aes(label = n), color = "white", size = 2) +
  labs(
    x = NULL,
    y = "Number of measurements",
    title = "Number of measurements over the years"
  ) +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 8)) +
  scale_y_continuous(breaks = scales::breaks_pretty(n = 8)) +
  theme(
    panel.border = element_blank(),
    axis.ticks = element_blank()
  )

ggsave(
  here("graphs","06_number_measurements_per_year.pdf"),
  device = cairo_pdf,
  width = 8,
  height = 5
)

# Number of measurements per cruise ---------------------------------------

df %>%
  count(cruise_name) %>%
  mutate(cruise_name = str_wrap(cruise_name, 30)) %>%
  mutate(cruise_name = fct_reorder(cruise_name, n)) %>%
  ggplot(aes(x = n, y = cruise_name)) +
  geom_col() +
  labs(
    y = NULL,
    x = "Number of measurements"
  ) +
  theme(
    axis.text.y = element_text(size = 6)
  )

ggsave(
  here("graphs","06_number_measurements_cruise.pdf"),
  device = cairo_pdf,
  height = 7,
  width = 5
)
