# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Show averaged absorption by bioregion.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source(here("R", "zzz.R"))

df <- read_csv(here("data", "clean", "merged_dataset.csv")) %>%
  select(
    sample_id,
    date,
    bioregion_name,
    wavelength,
    ap,
    aphy,
    aphy_specific,
    anap
  ) %>%
  filter(between(wavelength, 400, 700))

df

df %>%
  dtplyr::lazy_dt() %>%
  count(sample_id, wavelength) %>%
  as_tibble() %>%
  assertr::verify(n == 1)

df_viz <- df %>%
  group_by(bioregion_name, wavelength) %>%
  summarise(across(c(ap, aphy, aphy_specific, anap), mean)) %>%
  ungroup() %>%
  pivot_longer(c(ap, aphy, aphy_specific, anap),
    names_to = "absorption_type",
    values_to = "absorption"
  )

# Plot --------------------------------------------------------------------

facet_labels <- c(
  "ap" = "a[P]~(m^{-1})",
  "anap" = "a[NAP]~(m^{-1})",
  "aphy" = "a[phi]~(m^{-1})",
  "aphy_specific" = "a[phi]^'*'~(m^2~mg^{-1})"
)

facet_labels <- fct_inorder(facet_labels)

set.seed(123)

df_all <- df %>%
  pivot_longer(c(ap, aphy, aphy_specific, anap),
    names_to = "absorption_type",
    values_to = "absorption"
  ) %>%
  nest_by(bioregion_name, sample_id, absorption_type) %>%
  group_by(bioregion_name, absorption_type) %>%
  slice_sample(n = 100) %>%
  ungroup() %>%
  unnest(data) %>%
  mutate(absorption_type_label = facet_labels[absorption_type])

p <- df_viz %>%
  mutate(absorption_type_label = facet_labels[absorption_type]) %>%
  ggplot(aes(
    x = wavelength,
    y = absorption,
    group = bioregion_name,
    color = bioregion_name
  )) +
  geom_line(
    data = df_all,
    aes(group = sample_id),
    size = 0.1,
    alpha = 0.1
  ) +
  geom_line() +
  scale_color_manual(
    breaks = area_breaks,
    values = area_colors,
    labels = ~ str_wrap(., width = 20),
    guide = guide_legend(
      override.aes = list(size = 1),
      label.theme = element_text(size = 7, family = "Montserrat Light")
    )
  ) +
  labs(
    x = "Wavelength (nm)",
    y = "Absorption (or specific absorption)",
    color = NULL
  ) +
  facet_wrap(
    ~absorption_type_label,
    scales = "free_y",
    labeller = labeller(absorption_type_label = label_parsed)
  ) +
  theme(
    legend.justification = c(1, 1),
    legend.position = c(0.45, 1),
    legend.background = element_blank(),
    legend.key = element_blank(),
    strip.text = element_text(size = 10)
  )

ggsave(
  here("graphs", "fig03.pdf"),
  device = cairo_pdf,
  width = 180,
  height = 120,
  units = "mm"
)
