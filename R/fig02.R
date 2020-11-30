# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Timeseries of chla
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

hplc <- read_csv(here::here("data/clean/metadata.csv"))

df_viz <- hplc %>%
  select(measurement_id, date, hplc_chla, hplc_fucox) %>%
  mutate(yday = lubridate::yday(date)) %>%
  mutate(date = as.Date(paste0("2014-", yday), "%Y-%j")) %>%
  group_by(date) %>%
  summarise(across(contains("hplc"),  ~mean(., na.rm = TRUE)))

df_viz

# Chla --------------------------------------------------------------------

p1 <- df_viz %>%
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
    x = NULL
  ) +
  theme(
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    plot.caption = element_text(
      color = "grey50",
      size = 6
    )
  )


# Fuco/chla ratio ---------------------------------------------------------

p2 <- df_viz %>%
  mutate(fuco_chla_ratio = hplc_fucox / hplc_chla) %>%
  ggplot(aes(x = date, y = fuco_chla_ratio)) +
  geom_point(color = "gray50") +
  geom_line(aes(y = rollmean(
    fuco_chla_ratio,
    k = 28,
    fill = NA
  )), color = "red") +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  scale_y_continuous(breaks = scales::breaks_pretty(n = 10)) +
  labs(
    y = bquote(frac("Fuco" ~ (mgC~m^{-3}), "Chla" ~ (mgC~m^{-3}))),
    x = NULL
  ) +
  theme(
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    plot.caption = element_text(
      color = "grey50",
      size = 6
    )
  )

# Combine plots -----------------------------------------------------------

p <- p1 / p2 +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(face = "bold"))

ggsave(
  here::here("graphs/fig02.pdf"),
  device = cairo_pdf,
  width = 8,
  height = 8
)
