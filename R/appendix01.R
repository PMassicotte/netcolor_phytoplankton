# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Histograms showing the range of absorption at 443 nm.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

df <- read_csv(here("data", "clean", "merged_dataset.csv")) %>%
  filter(wavelength == 443) %>%
  select(
    sample_id,
    date,
    bioregion_name,
    wavelength,
    ap,
    aphy,
    aphy_specific,
    anap
  )

df %>%
  summarise(across(ap:anap, .fns = list(min = min, max = max))) %>%
  pivot_longer(everything(),
    names_to = c("variable", "type"),
    names_pattern = "(.*)_(.*)"
  ) %>%
  pivot_wider(names_from = variable, values_from = value)

facet_labels <- c(
  "ap" = "a[P]~(443)~(m^{-1})",
  "anap" = "a[NAP]~(443)~(m^{-1})",
  "aphy" = "a[phi]~(443)~(m^{-1})",
  "aphy_specific" = "a[phi]^'*'~(443)~(m^2~mg^{-1})"
)

facet_labels <- fct_inorder(facet_labels)

df_viz <- df %>%
  pivot_longer(c(ap, aphy, aphy_specific, anap)) %>%
  mutate(absorption_type_label = facet_labels[name])

# Calculate mean and some quantiles to display on the histograms

df_labels <- df_viz %>%
  group_by(absorption_type_label) %>%
  summarise(
    mean = mean(value),
    min = min(value),
    max = max(value)
  ) %>%
  mutate(across(where(is.numeric), round, digits = 4)) %>%
  mutate(label = glue("{mean} ({format(min, scientific = FALSE)} - {max})"))

df_labels

p <- df_viz %>%
  ggplot(aes(x = value, y = after_stat(density))) +
  geom_histogram(fill = "#6c6c6c") +
  geom_text(
    data = df_labels,
    aes(label = label),
    inherit.aes = FALSE,
    x = Inf,
    y = Inf,
    size = 3,
    hjust = 1,
    vjust = 3
  ) +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 5)) +
  labs(
    x = NULL,
    y = "Density"
  ) +
  facet_wrap(
    ~absorption_type_label,
    scales = "free",
    labeller = labeller(absorption_type_label = label_parsed)
  ) +
  theme(strip.text = element_text(size = 10))

ggsave(
  here("graphs", "appendix01.pdf"),
  device = cairo_pdf,
  width = 7,
  height = 5
)
