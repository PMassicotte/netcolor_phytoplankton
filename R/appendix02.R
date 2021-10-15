# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Histograms of chla and fuco pigments.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

df <- read_csv(here("data", "clean", "merged_dataset.csv")) %>%
  filter(wavelength == 443) %>%
  select(
    sample_id,
    hplcchla,
    fucox
  )

df

df_viz <- df %>%
  pivot_longer(c(hplcchla, fucox),
    names_to = "pigment",
    values_to = "concentration"
  ) %>%
  filter(concentration > 0) %>%
  mutate(pigment = case_when(
    pigment == "hplcchla" ~ "Chlorophyll-a",
    pigment == "fucox" ~ "Fucoxanthin",
    TRUE ~ NA_character_
  )) %>%
  mutate(pigment = fct_inorder(pigment))

df_labels <- df_viz %>%
  group_by(pigment) %>%
  summarise(
    mean = mean(concentration),
    min = min(concentration),
    max = max(concentration)
  ) %>%
  mutate(across(where(is.numeric), round, digits = 3)) %>%
  mutate(label = glue("{mean} ({min} - {max})"))

p <- df_viz %>%
  ggplot(aes(x = concentration, y = after_stat(density))) +
  geom_histogram(fill = "#6c6c6c") +
  scale_x_log10() +
  annotation_logticks(sides = "b", size = 0.1) +
  geom_text(
    data = df_labels,
    aes(label = label),
    inherit.aes = FALSE,
    x = Inf,
    y = Inf,
    size = 3,
    hjust = 1.1,
    vjust = 3
  ) +
  labs(
    x = quote("Pigment concentration" ~ (mg ~ m^{-3})),
    y = "Density"
  ) +
  facet_wrap(~pigment, scales = "free"
  )

ggsave(
  here("graphs", "appendix02.pdf"),
  device = cairo_pdf,
  width = 7,
  height = 2.5
)
