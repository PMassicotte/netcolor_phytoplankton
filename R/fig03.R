# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Fig. 3. Mean specific absorption for Scotian Shelf, LGS, NAB et
# LSB (solid lines) + Spring and Fall Scotian Shelf (dashed line) – Those would
# be spectra 400-700nm
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source(here("R","zzz.R"))

df <- vroom::vroom(here("data","clean","merged_dataset.csv"))

df

# Calculate the average absorption spectra by bioregion -------------------

df_viz <- df %>%
  filter(between(wavelength, 400, 700)) %>%
  group_by(bioregion_name, wavelength) %>%
  summarise(mean_aphy_specific = mean(aphy_specific, na.rm = TRUE)) %>%
  ungroup()

df_viz

# Normalize the average spectra -------------------------------------------

df_viz <- df_viz %>%
  group_by(bioregion_name) %>%
  mutate(
    mean_aphy_specific_normalized =
      mean_aphy_specific / pracma::trapz(wavelength, mean_aphy_specific)
  ) %>%
  ungroup()

df %>%
  distinct(bioregion_name)

# Plot the average absorption spectra -------------------------------------

p1 <- df_viz %>%
  ggplot(aes(
    x = wavelength,
    y = mean_aphy_specific,
    color = bioregion_name
  )) +
  geom_line() +
  scale_color_manual(
    breaks = area_breaks,
    values = area_colors
  )+
  labs(
    x = "Wavelength (nm)",
    y = quote(bar(a)[phi]^"*"~(m^{-1}))
  ) +
  theme(
    legend.title = element_blank(),
    legend.justification = c(1, 1),
    legend.position = c(0.99, 0.99),
    axis.title.x = element_blank()
  )

p2 <- df_viz %>%
  ggplot(aes(
    x = wavelength,
    y = mean_aphy_specific_normalized,
    color = bioregion_name
  )) +
  geom_line() +
  scale_color_manual(
    breaks = area_breaks,
    values = area_colors
  )+
  labs(
    x = "Wavelength (nm)",
    y = quote(Normalized~bar(a)[phi]^"*"~(nm^{-1}))
  ) +
  theme(
    legend.position = "none"
  )

p <- p1 / p2 +
  plot_annotation(tag_levels = "A") &
  theme(
    plot.tag = element_text(face = "bold", size = 14)
  )

ggsave(
  here("graphs/fig03.pdf"),
  device = cairo_pdf,
  width = 7.19,
  height = 8
)
