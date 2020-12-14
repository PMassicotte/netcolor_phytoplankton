# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:

# Beaucoup de relation aph(443)-chl ont été etablie avec quelques
# croisieres seulement, ici avec une serie temporelle de 10-20ans, ca serait
# bien de voir si ces relations tiennent toujours la route, je vais commencer a
# remplir le google doc avec ce genre de relations, e.g., Bricaud et al., 1998 &
# 2004 et on pourra ajouter une figure aph440 vs chl (bricaud utilise 440 et non
# 443).
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

# Prepare the data --------------------------------------------------------

df <- fread(here::here("data/clean/absorption_with_metadata.csv")) %>%
  as_tibble() %>%
  filter(wavelength == 440) %>%
  select(
    measurement_id,
    date,
    wavelength,
    contains("absorption"),
    hplc_chla
  )

df

bioregion <- read_csv(here::here("data/clean/bioregions.csv")) %>%
  mutate(bioregion_id = parse_number(bioregion)) %>%
  mutate(bioregion_name = fct_reorder(bioregion_name, bioregion_id))

df <- df %>%
  inner_join(bioregion, by = "measurement_id")

df

# Add prediction from other models found in the literature ----------------

df <- df %>%
  mutate(bricaud_1998 = 0.0378 * hplc_chla^0.627) %>%
  mutate(bricaud_2004 = 0.0654 * hplc_chla^0.728) %>%
  mutate(devred_2006 = ((0.0839 - 0.0176) / 1.613) * (1 - exp(-1.613 * hplc_chla)) + 0.0176 * hplc_chla)

# Plot --------------------------------------------------------------------

p <- df %>%
  ggplot(aes(x = hplc_chla, y = phytoplankton_absorption)) +
  geom_point(color = "grey60", size = 0.5) +
  geom_line(aes(y = bricaud_1998, color = "Bricaud 1998")) +
  geom_line(aes(y = bricaud_2004, color = "Bricaud 2004")) +
  geom_line(aes(y = devred_2006, color = "Devred 2006")) +
  scale_x_log10() +
  scale_y_log10() +
  annotation_logticks(sides = "bl", size = 0.25) +
  geom_smooth(method = "lm") +
  labs(
    x = quote("Chla" ~ (mgC~m^{-3})),
    y = quote(a[phi] ~ (440) ~ (m^{-1}))
  ) +
  theme(
    legend.position = "top",
    legend.title = element_blank()
  ) +
  paletteer::scale_color_paletteer_d(
    "ggthemes::wsj_rgby",
    guide = guide_legend(
      label.position = "top",
      override.aes = list(size = 2),
      keywidth = unit(3, "cm")
    )
  )

ggsave(
  here::here("graphs/fig02.pdf"),
  device = cairo_pdf,
  width = 5,
  height = 4
)

pdftools::pdf_convert(
  pdf = here::here("graphs/fig02.pdf"),
  filenames = here::here("graphs/fig02.png"),
  dpi = 600
)
