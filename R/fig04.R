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

# Normalize the average spectra -------------------------------------------

df_viz <- df_viz %>%
  group_by(bioregion_name) %>%
  mutate(
    mean_anap_normalized =
      mean_anap / pracma::trapz(wavelength, mean_anap)
  ) %>%
  ungroup()

df_viz

# Plot the average absorption spectra -------------------------------------

p1 <- df_viz %>%
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
    legend.position = c(0.99, 0.99),
    axis.title.x = element_blank()
  )

p2 <- df_viz %>%
  ggplot(aes(
    x = wavelength,
    y = mean_anap_normalized,
    color = bioregion_name
  )) +
  geom_line() +
  scale_color_manual(
    breaks = area_breaks,
    values = area_colors
  )+
  labs(
    x = "Wavelength (nm)",
    y = quote(Normalized~bar(a)[NAP]^"*"~(nm^{-1}))
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
  here("graphs/fig04.pdf"),
  device = cairo_pdf,
  width = 7.19,
  height = 8
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
