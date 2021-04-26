# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Fig. 3. Mean anap for Scotian Shelf, LGS, NAB et LSB (solid
# lines) + Spring and Fall Scotian Shelf (dashed line) – Those would be spectra
# 400-700nm
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source(here("R","zzz.R"))

df <- vroom::vroom(here("data","clean","merged_dataset.csv"))

df

# Calculate the average absorption spectra by bioregion -------------------

df_viz <- df %>%
  filter(between(wavelength, 400, 700)) %>%
  group_by(bioregion_name, wavelength) %>%
  summarise(mean_anap = mean(anap, na.rm = TRUE), n = n()) %>%
  ungroup()

df_viz

# Plot the average absorption spectra -------------------------------------

p <- df_viz %>%
  ggplot(aes(x = wavelength, y = mean_anap, color = bioregion_name)) +
  geom_line() +
  scale_color_manual(
    breaks = area_breaks,
    values = area_colors
  ) +
  labs(
    x = "Wavelength (nm)",
    y = quote(bar(a)[NAP]^"*"~(m^{-1}))
  ) +
  theme(
    legend.title = element_blank(),
    legend.justification = c(1, 1),
    legend.position = c(0.9, 0.9)
  )

ggsave(
  here("graphs/fig04.pdf"),
  device = cairo_pdf,
  width = 7.19,
  height = 5.21
)

# There are clear bumps in spectra around 530 nm. Check the raw data to find out
# where it comes from.

df %>%
  filter(between(wavelength, 500, 600)) %>%
  group_nest(sample_id) %>%
  sample_n(90) %>%
  unnest(data) %>%
  ggplot(aes(x = wavelength, y = anap, group = sample_id)) +
  geom_line(size = 0.1) +
  facet_wrap(~bioregion_name, scales = "free")
