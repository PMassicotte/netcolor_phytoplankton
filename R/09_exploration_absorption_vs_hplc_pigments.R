# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Explore some absorption proxies.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

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

absorption <- vroom::vroom(here::here("data/clean/absorption_with_metadata.csv"))

absorption <- absorption %>%
  select(
    measurement_id,
    date,
    wavelength,
    contains("absorption"),
    salinity:silicate,
    date,
    longitude,
    latitude,
    all_of(hplc_pigments)
  ) %>%
  mutate(yday = lubridate::yday(date))

absorption

absorption %>%
  count(measurement_id) %>%
  assertr::verify(n == 401)

# Absorption vs hplc chla -------------------------------------------------

df_viz <- absorption %>%
  filter(wavelength %in% c(443, 670)) %>%
  select(wavelength, contains("absorption"), hplc_chla) %>%
  pivot_longer(contains("absorption"))

df_viz

p <- df_viz %>%
  ggplot(aes(x = hplc_chla, y = value)) +
  geom_point(size = 0.5) +
  scale_x_log10() +
  scale_y_log10() +
  annotation_logticks(sides = "bl", size = 0.25) +
  facet_grid(name ~ glue("{wavelength} nm"), scales = "free") +
  geom_smooth(method = "lm", color = "#F15025") +
  labs(
    title = "Absorption vs HPLC Chl a",
    x = bquote("Absorption" ~ (m^{-1})),
    y = bquote("Chla" ~ (mgC~m^{-3}))
  ) +
  theme(
    strip.background = element_rect(fill = "#3c3c3c"),
    strip.text = element_text(color = "white", face = "bold", size = 10)
  )

ggsave(
  here::here("graphs/09_absorption_vs_hplc_chla.pdf"),
  device = cairo_pdf,
  height = 8
)


# Packaging effect --------------------------------------------------------

df_viz <- absorption %>%
  filter(wavelength %in% c(443, 670)) %>%
  select(
    measurement_id,
    date,
    yday,
    wavelength,
    phytoplankton_absorption
  ) %>%
  group_by(measurement_id, date, yday) %>%
  summarise(
    a_443_a_670_ratio =
      phytoplankton_absorption[wavelength == 443] /
        phytoplankton_absorption[wavelength == 670]
  ) %>%
  ungroup()

df_viz

df_viz %>%
  count(yday)

df_viz <- df_viz %>%
  filter(a_443_a_670_ratio >= 0) %>%
  mutate(date = as.Date(paste0("2014-", yday), "%Y-%j")) %>%
  group_by(date) %>%
  summarise(a_443_a_670_ratio = mean(a_443_a_670_ratio, na.rm = TRUE))

p <- df_viz %>%
  ggplot(aes(x = date, y = a_443_a_670_ratio)) +
  geom_point(color = "gray50") +
  geom_line(aes(y = rollmean(
    a_443_a_670_ratio,
    k = 28,
    na.pad = TRUE
  )), color = "red") +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  scale_y_continuous(breaks = scales::breaks_pretty(n = 10)) +
  labs(
    y = bquote(a[phy](443)/a[phy](670)),
    x = NULL,
    title = bquote(bold(a[phy](443)/a[phy](670)~"ratio over the time")),
    subtitle = "The red line is a 28 days moving average.",
    caption =
      glue(
        "Data measured between {glue_collapse(range(absorption$date), sep = ' and ')}"
      )
  ) +
  theme(
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    plot.caption = element_text(color = "grey50", size = 6)
  )

ggsave(
  here::here("graphs/09_timeseries_packaging_effect.pdf"),
  device = cairo_pdf,
  width = 8,
  height = 6
)

# a443 / chla over the time -----------------------------------------------

df_viz <- absorption %>%
  filter(wavelength == 443) %>%
  select(
    measurement_id,
    date,
    yday,
    wavelength,
    phytoplankton_absorption,
    hplc_chla
  ) %>%
  mutate(a_443_hplc_chla_ratio = phytoplankton_absorption / hplc_chla) %>%
  mutate(date = as.Date(paste0("2014-", yday), "%Y-%j")) %>%
  group_by(date) %>%
  summarise(a_443_hplc_chla_ratio = mean(a_443_hplc_chla_ratio, na.rm = TRUE))

df_viz %>%
  ggplot(aes(x = date, y = a_443_hplc_chla_ratio)) +
  geom_point(color = "gray50") +
  geom_line(aes(y = rollmean(
    a_443_hplc_chla_ratio,
    k = 28,
    na.pad = TRUE
  )), color = "red") +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  scale_y_continuous(breaks = scales::breaks_pretty(n = 10)) +
  labs(
    y = bquote(a[phy](443)/Chl~italic(a)),
    x = NULL,
    title = bquote(bold(a[phy](443)/Chl~italic(a)~"ratio over the time")),
    subtitle = "The red line is a 28 days moving average.",
    caption =
      glue(
        "Data measured between {glue_collapse(range(absorption$date), sep = ' and ')}"
        )
  ) +
  theme(
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    plot.caption = element_text(color = "grey50", size = 6)
  )

ggsave(
  here::here("graphs/09_timeseries_aphy443_chla.pdf"),
  device = cairo_pdf,
  width = 8,
  height = 6
)


# Non-algal vs phyto overtime ---------------------------------------------

min_wl <- 440
max_wl <- 490

df_viz <- absorption %>%
  filter(wavelength %in% c(min_wl, max_wl)) %>%
  select(
    measurement_id,
    wavelength,
    phytoplankton_absorption,
    non_algal_absorption
  ) %>%
  group_by(measurement_id) %>%
  summarise(
    a_det_440_a_det_490_ratio =
      non_algal_absorption[wavelength == min_wl] /
      non_algal_absorption[wavelength == max_wl],
    a_phy_440_a_phy_490_ratio =
      phytoplankton_absorption[wavelength == min_wl] /
      phytoplankton_absorption[wavelength == max_wl]
  ) %>%
  ungroup()

df_viz

# Get the date for each measurement
df_viz <- absorption %>%
  distinct(measurement_id, date) %>%
  right_join(df_viz)

df_viz %>%
  pivot_longer(contains("ratio")) %>%
  ggplot(aes(x = value)) +
  geom_histogram() +
  facet_wrap(~name)

p1 <- df_viz %>%
  filter(across(contains("ratio"), ~ between(.x, 0, 4))) %>%
  ggplot(aes(x = a_det_440_a_det_490_ratio, y = a_phy_440_a_phy_490_ratio)) +
  geom_point(size = 1) +
  # geom_hex() +
  labs(
    title = "Scatterplot of non-algal and phytoplankton absorption",
    x = bquote(frac(a[nap](440), a[nap](490))),
    y = bquote(frac(a[phi](440), a[phi](490)))
  ) +
  theme(
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    plot.caption = element_text(color = "grey50", size = 6)
  )

p2 <- p1 +
  facet_wrap(~lubridate::month(date, label = TRUE), scales = "free")

p <- p1 / p2 +
  plot_annotation(tag_levels = "A")

ggsave(
  here::here("graphs/09_adet_ratio_vs_aphy_ratio.pdf"),
  device = cairo_pdf,
  width = 9,
  height = 11
)
