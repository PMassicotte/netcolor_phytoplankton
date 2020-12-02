# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Explore pigment ratios.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

hplc <- read_csv(here::here("data/clean/metadata.csv"))

df_viz <- hplc %>%
  select(measurement_id, date, hplc_chla, hplc_fucox) %>%
  mutate(yday = lubridate::yday(date)) %>%
  mutate(hplc_fucox_hplc_chla_ratio = hplc_fucox / hplc_chla) %>%
  mutate(date = as.Date(paste0("2014-", yday), "%Y-%j")) %>%
  group_by(date) %>%
  summarise(hplc_fucox_hplc_chla_ratio = mean(hplc_fucox_hplc_chla_ratio, na.rm = TRUE))

df_viz

df_viz %>%
  ggplot(aes(x = date, y = hplc_fucox_hplc_chla_ratio)) +
  geom_point(color = "gray50") +
  geom_line(aes(y = rollmean(
    hplc_fucox_hplc_chla_ratio,
    k = 28,
    fill = NA
  )), color = "red") +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  scale_y_continuous(breaks = scales::breaks_pretty(n = 10)) +
  labs(
    y = "fuco / chla",
    x = NULL,
    title = "fuco/chla ratio over the time",
    subtitle = "This ratio is a proxy for the diatoms. The red line is a 28 days moving average.",
    caption =
      glue(
        "Data measured between {glue_collapse(range(hplc$date), sep = ' and ')}"
      )
  ) +
  theme(
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    plot.caption = element_text(color = "grey50", size = 6)
  )

ggsave(
  here::here("graphs/12_timeseries_fuco_chla.pdf"),
  device = cairo_pdf,
  width = 8,
  height = 6
)

# Chla --------------------------------------------------------------------

df_viz <- hplc %>%
  select(measurement_id, date, hplc_chla) %>%
  mutate(yday = lubridate::yday(date)) %>%
  mutate(date = as.Date(paste0("2014-", yday), "%Y-%j")) %>%
  group_by(date) %>%
  summarise(hplc_chla = mean(hplc_chla, na.rm = TRUE))

df_viz

p <- df_viz %>%
  ggplot(aes(x = date, y = hplc_chla)) +
  geom_point(color = "gray50") +
  geom_line(aes(y = rollmean(
    hplc_chla,
    k = 28,
    fill = NA
  )), color = "red") +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  scale_y_continuous(breaks = scales::breaks_pretty(n = 10)) +
  labs(
    y = bquote("Chla" ~ (mgC~m^{-3})),
    x = NULL,
    title = "HPLC chla by the day of the year",
    subtitle = "The red line is a 28 days moving average.",
    caption =
      glue(
        "Data measured between {glue_collapse(range(hplc$date), sep = ' and ')}"
      )
  ) +
  theme(
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    plot.caption = element_text(
      color = "grey50",
      size = 6
    )
  )

ggsave(
  here::here("graphs/12_timeseries_chla.pdf"),
  device = cairo_pdf,
  width = 8,
  height = 6
)
