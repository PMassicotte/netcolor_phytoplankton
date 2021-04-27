# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Seasonal variability of non-algal absorption and snap.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source(here("R","zzz.R"))

df <- vroom::vroom(here("data","clean","merged_dataset.csv")) %>%
  select(sample_id, date, bioregion_name, wavelength, anap, snap) %>%
  filter(wavelength %in% c(443))

df

# Seasonal variability ----------------------------------------------------

# Figure 6 : Variations saisonnières de a_det(443) et Slope pour les 4 biomes

df_viz <- df %>%
  mutate(month = lubridate::month(date, label = TRUE), .after = date) %>%
  mutate(wavelength = glue("{wavelength} nm"))

p1 <- df_viz %>%
  ggplot(aes(x = month, y = anap, fill = bioregion_name)) +
  geom_boxplot(size = 0.1, outlier.size = 0.25, color = "gray50") +
  labs(
    x = NULL,
    y = quote(a[NAP]~(m^{-1}))
  ) +
  facet_wrap(~bioregion_name, scales = "free") +
  scale_fill_manual(
    breaks = area_breaks,
    values = area_colors
  ) +
  theme(
    legend.position = "none"
  )

p2 <- df_viz %>%
  ggplot(aes(x = month, y = snap, fill = bioregion_name)) +
  geom_boxplot(size = 0.1, outlier.size = 0.25, color = "gray50") +
  labs(
    x = NULL,
    y = quote(S[NAP]~(nm^{-1}))
  ) +
  facet_wrap(~bioregion_name, scales = "free") +
  scale_fill_manual(
    breaks = area_breaks,
    values = area_colors
  ) +
  theme(
    legend.position = "none"
  )

p <- p1 / p2 +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(size = 14, face = "bold"))

ggsave(
  here("graphs/fig06.pdf"),
  device = cairo_pdf,
  width = 10,
  height = 10
)
