ggboxlpot <- function(df, season, y, strip.text = element_blank(), ylab) {
  p <- ggplot(df, aes(x = {{ season }}, y = {{ y }}, fill = {{ season }})) +
    geom_boxplot(size = 0.1, outlier.size = 0.25) +
    scale_fill_manual(
      breaks = season_breaks,
      values = season_colors
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
