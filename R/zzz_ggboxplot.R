ggboxlpot <- function(df, season, y, bioregion_name, strip.text = element_blank(), ylab) {
  p <- ggplot(df, aes(x = {{ season }}, y = {{ y }}, fill = {{ bioregion_name }})) +
    geom_boxplot(size = 0.25, outlier.size = 0.25) +
    scale_fill_manual(
      breaks = area_breaks,
      values = area_colors
    ) +
    scale_y_log10(labels = scales::label_number()) +
    annotation_logticks(sides = "l", size = 0.1) +
    labs(
      x = NULL,
      y = parse(text = ylab)
    ) +
    facet_wrap(~ bioregion_name_wrap, scales = "free_x", ncol = 3) +
    theme(
      legend.position = "none",
      strip.text = strip.text
    )

  return(p)
}
