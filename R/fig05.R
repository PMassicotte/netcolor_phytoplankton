# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Seasonal variability of phytoplankton specific absorption.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

df <- vroom::vroom(here("data","clean","merged_dataset.csv")) %>%
  select(sample_id, date, bioregion_name, wavelength, aphy_specific) %>%
  filter(wavelength %in% c(443, 670))

df

# Seasonal variability ----------------------------------------------------

# Variations saisonniere de a*p(443) et a*p(670) pour les 4 biomes, on peut
# avoir 4 panneaux (un pour chaque biome)

df_viz <- df %>%
  mutate(month = lubridate::month(date, label = TRUE), .after = date) %>%
  mutate(wavelength = glue("{wavelength} nm"))

p <- df_viz %>%
  ggplot(aes(x = month, y = aphy_specific, fill = wavelength)) +
  geom_boxplot(size = 0.1, outlier.size = 0.25, color = "gray50") +
  # scale_y_log10() +
  labs(
    x = NULL,
    y = quote(a[phi]^"*"~(m^{-1}))
  ) +
  facet_wrap(~bioregion_name, scales = "free") +
  paletteer::scale_fill_paletteer_d(
    "ggsci::default_locuszoom"
  ) +
  theme(
    legend.title = element_blank(),
    legend.position = "top"
  )

ggsave(
  here("graphs/fig05.pdf"),
  device = cairo_pdf,
  width = 10,
  height = 6
)
