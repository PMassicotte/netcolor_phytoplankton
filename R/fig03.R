# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Timeseries of absorption data
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

absorption <- vroom::vroom(here::here("data/clean/absorption_with_metadata.csv"))

df_viz <- absorption %>%
  filter(wavelength %in% c(443)) %>%
  select(measurement_id, wavelength, date, contains("absorption"), hplc_chla) %>%
  mutate(specific_phytoplankton_absorption = phytoplankton_absorption / hplc_chla) %>%
  select(-hplc_chla) %>%
  mutate(yday = lubridate::yday(date)) %>%
  mutate(date = as.Date(paste0("2014-", yday), "%Y-%j")) %>%
  group_by(wavelength, date) %>%
  summarise(across(contains("absorption"), mean)) %>%
  ungroup()

# Plot of a_nap -----------------------------------------------------------

p1 <- df_viz %>%
  ggplot(aes(x = date, y = non_algal_absorption)) +
  geom_point(
    color = "gray50"
  ) +
  geom_line(aes(y = rollmean(
    non_algal_absorption,
    k = 28,
    na.pad = TRUE
  )),
  color = "red",
  size = 0.5
  ) +
  labs(
    x = NULL,
    y = quote(a[NAP]*(443) ~ (m^{-1}))
  ) +
  scale_x_date(
    date_labels = "%b",
    date_breaks = "1 month",
    expand = expansion(mult = c(0.05, 0.01))
  )


# Plot of a_phi -----------------------------------------------------------

p2 <- df_viz %>%
  ggplot(aes(x = date, y = phytoplankton_absorption)) +
  geom_point(
    color = "gray50"
  ) +
  geom_line(aes(y = rollmean(
    phytoplankton_absorption,
    k = 28,
    na.pad = TRUE
  )),
  color = "red",
  size = 0.5
  ) +
  labs(
    x = NULL,
    y = quote(a[phi](443) ~ (m^{-1}))
  ) +
  scale_x_date(
    date_labels = "%b",
    date_breaks = "1 month",
    expand = expansion(mult = c(0.05, 0.01))
  )

# Plot of a*_phi ----------------------------------------------------------


p3 <- df_viz %>%
  ggplot(aes(x = date, y = specific_phytoplankton_absorption)) +
  geom_point(
    color = "gray50"
  ) +
  geom_line(aes(y = rollmean(
    specific_phytoplankton_absorption,
    k = 28,
    na.pad = TRUE
  )),
  color = "red",
  size = 0.5
  ) +
  labs(
    x = NULL,
    y = quote(a[phi]^"*"*(443) ~ (m^{-1}))
  ) +
  scale_x_date(
    date_labels = "%b",
    date_breaks = "1 month",
    expand = expansion(mult = c(0.05, 0.01))
  )

# Combine plots -----------------------------------------------------------

p <- p1 / p2 / p3 +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(face = "bold"))

ggsave(
  here::here("graphs/fig03.pdf"),
  device = cairo_pdf,
  width = 8,
  height = 10
)
