# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Explore absorption spectra.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source("R/zzz.R")

absorption <- fread(here("data/clean/merged_dataset.csv")) %>%
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
  facet_wrap(~bioregion_name, scales = "free_y") +
  theme(
    legend.position = "none"
  )

ggsave(
  here("graphs/07_aphy_spectral_profiles_per_bioregion.pdf"),
  device = cairo_pdf,
  width = 8,
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
  facet_wrap(~bioregion_name, scales = "free_y") +
  theme(
    legend.position = "none"
  )

ggsave(
  here("graphs/07_aphy_specific_spectral_profiles_per_bioregion.pdf"),
  device = cairo_pdf,
  width = 8,
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
  facet_wrap(~bioregion_name, scales = "free_y") +
  theme(
    legend.position = "none"
  )

ggsave(
  here("graphs/07_anap_spectral_profiles_per_bioregion.pdf"),
  device = cairo_pdf,
  width = 8,
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
  facet_wrap(~bioregion_name, scales = "free_y") +
  theme(
    legend.position = "none"
  )

ggsave(
  here("graphs/07_ap_spectral_profiles_per_bioregion.pdf"),
  device = cairo_pdf,
  width = 8,
  height = 5
)


absorption
