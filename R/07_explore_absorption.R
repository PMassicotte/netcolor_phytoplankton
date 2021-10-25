# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Explore absorption spectra.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source(here("R","zzz.R"))

absorption <- fread(here("data","clean","merged_dataset.csv")) %>%
  as_tibble()

# Plot aphy ---------------------------------------------------------------

p <- absorption %>%
  ggplot(aes(x = wavelength, y = aphy, group = sample_id, color = bioregion_name)) +
  geom_line(size = 0.1) +
  scale_color_manual(
    breaks = area_breaks,
    values = area_colors
  ) +
  labs(
    x = "Wavelength (nm)",
    y = quote(a[phy] ~ (m^{-1}))
  ) +
  facet_wrap(~bioregion_name, scales = "free_y", ncol = 1) +
  theme(
    legend.position = "none"
  )

ggsave(
  here("graphs","07_aphy_spectral_profiles_per_bioregion.pdf"),
  device = cairo_pdf,
  width = 4,
  height = 5
)

# Plot aphy specific ------------------------------------------------------

p <- absorption %>%
  ggplot(aes(x = wavelength, y = aphy_specific, group = sample_id, color = bioregion_name)) +
  geom_line(size = 0.1) +
  scale_color_manual(
    breaks = area_breaks,
    values = area_colors
  ) +
  labs(
    x = "Wavelength (nm)",
    y = quote(a[phy]^"*" ~ (m^{-1}))
  ) +
  facet_wrap(~bioregion_name, scales = "free_y", ncol = 1) +
  theme(
    legend.position = "none"
  )

ggsave(
  here("graphs","07_aphy_specific_spectral_profiles_per_bioregion.pdf"),
  device = cairo_pdf,
  width = 4,
  height = 5
)

# Plot anap ---------------------------------------------------------------

p <- absorption %>%
  ggplot(aes(x = wavelength, y = anap, group = sample_id, color = bioregion_name)) +
  geom_line(size = 0.1) +
  scale_color_manual(
    breaks = area_breaks,
    values = area_colors
  ) +
  labs(
    x = "Wavelength (nm)",
    y = quote(a[nap] ~ (m^{-1}))
  ) +
  facet_wrap(~bioregion_name, scales = "free_y", ncol = 1) +
  theme(
    legend.position = "none"
  )

ggsave(
  here("graphs","07_anap_spectral_profiles_per_bioregion.pdf"),
  device = cairo_pdf,
  width = 4,
  height = 5
)

# Plot ap -----------------------------------------------------------------

p <- absorption %>%
  ggplot(aes(x = wavelength, y = ap, group = sample_id, color = bioregion_name)) +
  geom_line(size = 0.1) +
  scale_color_manual(
    breaks = area_breaks,
    values = area_colors
  ) +
  labs(
    x = "Wavelength (nm)",
    y = quote(a[p] ~ (m^{-1}))
  ) +
  facet_wrap(~bioregion_name, scales = "free_y", ncol = 1) +
  theme(
    legend.position = "none"
  )

ggsave(
  here("graphs","07_ap_spectral_profiles_per_bioregion.pdf"),
  device = cairo_pdf,
  width = 4,
  height = 5
)

absorption

# Sina plot ---------------------------------------------------------------

# Let's compare aphy specific

p <- absorption %>%
  filter(wavelength %in% c(443)) %>%
  ggplot(aes(x = str_wrap(bioregion_name, 20), y = aphy_specific)) +
  ggforce::geom_sina(aes(color = bioregion_name), size = 0.5) +
  scale_y_log10() +
  annotation_logticks(sides = "l", size = 0.1) +
  scale_color_manual(
    breaks = area_breaks,
    values = area_colors
  ) +
  labs(
    x = NULL,
    y = quote(a[phi]^"*" ~ (443)~(m^{-1}))
  ) +
  theme(legend.position = "none")

ggsave(
  here("graphs","07_sina_plot_aphy_specific_443nm.pdf"),
  device = cairo_pdf,
  width = 7,
  height = 5
)
