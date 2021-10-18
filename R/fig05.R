# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  The observed nonlinearity between Chl_a and the ratio of
# phytoplankton absorption aph (443)/aph (670) indicating the packaging effect
# and changes in the intracellular composition of pigments.
#
# Vishnu et al., Seasonal Variability in Bio-Optical Properties along the
# Coastal Waters off Cochin.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source(here("R","zzz.R"))

df <- read_csv(here("data", "clean", "merged_dataset.csv")) %>%
  filter(wavelength %in% c(443, 670)) %>%
  select(sample_id, bioregion_name, wavelength, aphy, hplcchla, fucox) %>%
  mutate(bioregion_name = factor(
    bioregion_name,
    levels = c(
      "Scotian Shelf",
      "Northwest Atlantic Basin ocean (NAB)",
      "Labrador"
    )
  )) %>%
  mutate(bioregion_name_wrap = str_wrap_factor(bioregion_name, 20))

df %>%
  count(sample_id) %>%
  assertr::verify(n == 2)

# aphy443/aphy670 vs chla -------------------------------------------------

df_viz <- df %>%
  dtplyr::lazy_dt() %>%
  group_by(sample_id, bioregion_name, bioregion_name_wrap) %>%
  summarise(
    aphy_443_670 = aphy[wavelength == 443] / aphy[wavelength == 670],
    hplcchla = unique(hplcchla)
  ) %>%
  ungroup() %>%
  as_tibble()

df_viz

p <- df_viz %>%
  ggplot(aes(x = hplcchla, y = aphy_443_670)) +
  geom_point(aes(color = bioregion_name), size = 0.5) +
  scale_x_log10() +
  scale_y_log10() +
  annotation_logticks(sides = "bl", size = 0.25) +
  geom_smooth(method = "lm", color = "#3c3c3c", size = 0.5) +
  ggpubr::stat_regline_equation(
    label.x.npc = 0.6,
    label.y.npc = 1,
    size = 3,
    vjust = 1
  ) +
  ggpubr::stat_regline_equation(
    label.x.npc = 0.6,
    label.y.npc = 0.90,
    aes(label = ..rr.label..),
    size = 3,
    vjust = 1
  ) +
  scale_color_manual(
    breaks = area_breaks,
    values = area_colors
  ) +
  labs(
    x = quote("Chlorophyll-a" ~ (mg ~ m^{-3})),
    y = quote(a[phi](443) / a[phi](670))
  ) +
  facet_wrap(~bioregion_name_wrap) +
  theme(
    panel.spacing.y = unit(3, "lines"),
    legend.position = "none"
  )

ggsave(
  here("graphs", "fig05.pdf"),
  device = cairo_pdf,
  width = 8,
  height = 3
)

# Does chla correlates more at 443 or 670 nm? -----------------------------

df %>%
  select(sample_id, wavelength, aphy, hplcchla) %>%
  ggplot(aes(x = hplcchla, y = aphy)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  geom_smooth(method = "lm") +
  ggpubr::stat_regline_equation(
    label.x.npc = 0,
    label.y.npc = 1,
    size = 3,
    vjust = 1
  ) +
  ggpubr::stat_regline_equation(
    label.x.npc = 0,
    label.y.npc = 0.90,
    aes(label = ..rr.label..),
    size = 3,
    vjust = 1
  ) +
  facet_wrap(~wavelength, scales = "free")

# Numbers for the paper
df %>%
  pivot_wider(names_from = wavelength, values_from = aphy, names_prefix = "wl") %>%
  select(hplcchla, fucox, wl443, wl670) %>%
  mutate(across(everything(), log10)) %>%
  # replace infinite values with NA
  mutate(across(everything(), ~ifelse(is.infinite(.), NA, .))) %>%
  correlate(use = "pairwise.complete.obs") %>%
  focus(starts_with("wl"))

