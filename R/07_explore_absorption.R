# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Explore absorption spectra.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source("R/zzz.R")

absorption <- fread(here("data/clean/absorption.csv")) %>%
  as_tibble()

bioregions <- read_csv(here("data/clean/bioregions.csv"))

absorption %>%
  distinct(sample_id)

absorption <- absorption %>%
  inner_join(bioregions, by = "sample_id")

# Plot aphy ---------------------------------------------------------------

set.seed(2021)

p <- absorption %>%
  # group_nest(sample_id) %>%
  # slice_sample(n = 500) %>%
  # unnest(data) %>%
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
  here("graphs/07_aph_spectral_profiles_per_bioregion.pdf"),
  device = cairo_pdf,
  width = 8,
  height = 5
)
