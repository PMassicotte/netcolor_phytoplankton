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
    hplcchla,
    aphy,
    aphy_specific,
    ap,
    anap,
    fucox
  )

gghisto <- function(df, var, label) {

  df_labels <- df %>%
    filter({{var}} > 0) %>%
    summarise(
      mean = mean({{var}}),
      min = min({{var}}),
      max = max({{var}})
    ) %>%
    mutate(across(where(is.numeric), round, digits = 4)) %>%
    mutate(label = glue("{mean} ({format(min, scientific = FALSE)} - {max})"))

  df %>%
    filter({{var}} > 0) %>%
    ggplot(aes(x = {{var}}, y = after_stat(density))) +
    geom_histogram(fill = "#6c6c6c") +
    geom_text(
      data = df_labels,
      aes(label = label),
      inherit.aes = FALSE,
      x = -Inf,
      y = Inf,
      size = 2.5,
      hjust = -0.1,
      vjust = 3
    ) +
    scale_x_log10(labels = scales::label_number(), expand = expansion(mult = c(0.01, 0.01))) +
    annotation_logticks(sides = "b", size = 0.1) +
    labs(
      x = parse(text = label),
      y = "Density"
    )
}

p1 <- gghisto(df, aphy, "a[phi]~(443)~(m^{-1})")
p2 <- gghisto(df, hplcchla, "Chlorophyll-italic(a)~(mg~m^{-3})")
p3 <- gghisto(df, aphy_specific, "a[phi]^'*'~(443)~(m^2~mg^{-1})")

p4 <- gghisto(df, ap, "a[p]~(443)~(m^{-1})")
p5 <- gghisto(df, anap, "a[NAP]~(443)~(m^{-1})")
p6 <- gghisto(df, fucox, "Fucoxanthin~(mg~m^{-3})")

p <- p1 + p2 + p3 + p4 + p5 + p6 +
  plot_layout(byrow = FALSE, ncol = 2) +
  plot_annotation(tag_levels = "A") &
  theme(
    plot.tag = element_text(face = "bold", size = 14)
  )

ggsave(
  here("graphs","appendix02.pdf"),
  device = cairo_pdf,
  width = 180,
  height = 180,
  units = "mm"
)
