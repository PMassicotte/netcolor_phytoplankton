rm(list = ls())

absorption <-
  vroom::vroom(here::here("data/clean/absorption_with_metadata.csv")) %>%
  select(
    mission,
    measurement_id,
    wavelength,
    phytoplankton_absorption,
    hplc_chla
  ) %>%
  filter(wavelength %in% c(443, 675))

absorption

df <- absorption %>%
  mutate(specific_phytoplankton_absorption = phytoplankton_absorption / hplc_chla)

# Plot --------------------------------------------------------------------

p <- df %>%
  ggplot(aes(x = specific_phytoplankton_absorption)) +
  geom_histogram(binwidth = 0.05) +
  facet_wrap(~glue("{wavelength} nm")) +
  scale_x_log10() +
  annotation_logticks(sides = "b", size = 0.25) +
  labs(
    title = "Phytoplankton specific absorption",
    x = bquote(Phytoplankton~specific~absorption~(m^2~"[mg Chla]"^{-1})),
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
  here::here("graphs/12_histogram_specific_absorption.pdf"),
  device = cairo_pdf,
  width = 9,
  height = 5
)

# Spectra examples --------------------------------------------------------

df <- vroom::vroom(here::here("data/clean/absorption_with_metadata.csv")) %>%
  filter(mission == "AZMP Fall 2015")

df

df <- df %>%
  select(measurement_id, wavelength, phytoplankton_absorption, hplc_chla) %>%
  mutate(specific_phytoplankton_absorption = phytoplankton_absorption / hplc_chla)


# Plot --------------------------------------------------------------------

p <- df %>%
  pivot_longer(contains("absorption")) %>%
  ggplot(aes(x = wavelength, y = value, color = name)) +
  geom_line() +
  facet_wrap(~measurement_id) +
  labs(
    title = "Phytoplankton asbsorption spectra (AZMP Fall 2015)",
    x = "Wavelength (nm)",
    y = bquote(Absorption ~ (m^{-1}))
  ) +
  paletteer::scale_color_paletteer_d(
    "ggthemes::wsj_colors6",
    guide = guide_legend(override.aes = list(size = 2))
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
  here::here("graphs/12_absorption_azmp_fall_2015.pdf"),
  device = cairo_pdf,
  width = 10,
  height = 10
)
