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

# Plot the average absorption spectra -------------------------------------

p <- df_viz %>%
  ggplot(aes(x = wavelength, y = mean_aphy_specific, color = bioregion_name)) +
  geom_line() +
  scale_color_manual(
    breaks = area_breaks,
    values = area_colors
  ) +
  labs(
    x = "Wavelength (nm)",
    y = quote(bar(a)[phi]^"*"~(m^{-1}))
  ) +
  theme(
    legend.title = element_blank(),
    legend.justification = c(1, 1),
    legend.position = c(0.9, 0.9)
  )

ggsave(
  here("graphs/fig03.pdf"),
  device = cairo_pdf,
  width = 7.19,
  height = 5.21
)
