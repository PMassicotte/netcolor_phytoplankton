# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Explore the metadata.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

metadata <- read_csv(here::here("data/clean/metadata.csv"))

metadata

metadata %>%
  count(mission_descriptor, sort = TRUE) %>%
  mutate(mission_descriptor = fct_reorder(mission_descriptor, n)) %>%
  ggplot(aes(x = n, y = mission_descriptor)) +
  geom_col() +
  labs(
    x = "Number of absorption spectra",
    y = "Mission",
    title = "Number of absorption spectra per mission"
  ) +
  scale_x_continuous(expand = c(0, 0), breaks = scales::breaks_pretty(n = 8)) +
  theme(
    panel.border = element_blank(),
    axis.ticks = element_blank()
  )

ggsave(
  here::here("graphs/07_number_absorption_spectra_per_mission.pdf"),
  device = cairo_pdf,
  width = 8,
  height = 8
)

metadata %>%
  count(year) %>%
  ggplot(aes(x = year, y = n)) +
  geom_line() +
  geom_point(size = 6, color = "#3c3c3c") +
  geom_text(aes(label = n), color = "white", size = 2) +
  labs(
    y = "Number of observations",
    x = NULL,
    title = "Number of absorption spectra per year"
  ) +
  scale_x_continuous(breaks = 2000:2020) +
  theme(
    panel.border = element_blank(),
    axis.ticks = element_blank()
  )

ggsave(
  here::here("graphs/07_number_absorption_spectra_per_year.pdf"),
  device = cairo_pdf,
  width = 6,
  height = 5
)

metadata %>%
  pivot_longer(-c(measurement_id:depth)) %>%
  drop_na() %>%
  count(name) %>%
  mutate(name = fct_reorder(name, n)) %>%
  ggplot(aes(x = n, y = name)) +
  geom_col() +
  labs(
    x = "Number of observation",
    y = "Variables",
    title = "Number of available observations per variable",
    subtitle = str_wrap(glue::glue("This shows how many observations are available for each variables. For example, there are {nrow(count(metadata, poc))} POC measurements available for a total of {nrow(metadata)} absorption spectra."), 90)
  ) +
  scale_x_continuous(expand = c(0, 0), breaks = scales::breaks_pretty(n = 8)) +
  theme(
    panel.border = element_blank(),
    axis.ticks = element_blank()
  )

ggsave(
  here::here("graphs/07_number_available_observations.pdf"),
  device = cairo_pdf,
  width = 8,
  height = 8
)

metadata %>%
  mutate(yday = lubridate::yday(date)) %>%
  count(yday) %>%
  ggplot(aes(x = yday, y = n)) +
  geom_line() +
  # geom_point(color = "#3c3c3c") +
  # geom_text(aes(label = n), color = "white", size = 2) +
  labs(
    y = "Number of observations",
    x = NULL,
    title = "Number of absorption spectra by day of year"
  ) +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 20)) +
  scale_y_continuous(breaks = scales::breaks_pretty(n = 10)) +
  theme(
    panel.border = element_blank(),
    axis.ticks = element_blank()
  )

ggsave(
  here::here("graphs/07_number_absorption_spectra_per_yday.pdf"),
  device = cairo_pdf,
  width = 9,
  height = 5
)


# Histogram of chla -------------------------------------------------------

p <- metadata %>%
  ggplot(aes(x = hplc_chla)) +
  geom_histogram(binwidth = 0.05) +
  scale_x_log10(breaks = scales::breaks_log()) +
  annotation_logticks(sides = "b", size = 0.25) +
  labs(
    title = "Histogram of chla from HPLC",
    x = bquote("Chl a" ~ (mgC~m^{-3})),
    y = "Number of observations"
  ) +
  theme(
    legend.title = element_blank(),
    strip.background = element_rect(fill = "#3c3c3c"),
    strip.text = element_text(color = "white", face = "bold"),
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(hjust = 0),
    legend.position = "top"
  )

ggsave(
  here::here("graphs/07_histogram_hplc_chla.pdf"),
  device = cairo_pdf,
  width = 6,
  height = 4
)
