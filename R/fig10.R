# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Same as figure 9, but for aphy, anap and snap variables. This is
# a temporary figure made to answer the reviewer's comments.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source(here("R", "zzz.R"))

df <- open_dataset(here("data", "clean", "merged_dataset")) |>
  filter(wavelength == 443) |>
  select(
    sample_id,
    bioregion_name,
    wavelength,
    season,
    date,
    aphy,
    anap,
    snap,
    hplcchla
  ) |>
  collect()

df

df <- df |>
  mutate(season = factor(season,
    levels = c("Spring", "Summer", "Autumn", "Winter")
  )) |>
  mutate(bioregion_name = factor(bioregion_name,
    levels = c("Scotian Shelf", "NAB", "Labrador")
  ))

df_viz <- df |>
  mutate(year = lubridate::year(date)) |>
  group_by(bioregion_name, year, season) |>
  summarise(across(c(aphy, anap, snap, hplcchla), mean), n = n()) |>
  ungroup() |>
  filter(n >= 10)

df_viz <- df_viz |>
  group_by(bioregion_name, season) |>
  filter(n() >= 5) |>
  ungroup()

df_viz <- df_viz |>
  filter(season %in% c("Autumn", "Spring"))

make_plot <- function(df, var, ylab, free_y = FALSE) {
  scale_y <- ifelse(free_y, "free_y", "fixed")

  df %>%
    ggplot(aes(x = year, y = {{ var }})) +
    geom_point(aes(color = season, pch = bioregion_name)) +
    scale_color_manual(
      breaks = season_breaks,
      values = season_colors
    ) +
    scale_shape_manual(
      breaks = area_breaks,
      values = area_pch
    ) +
    geom_smooth(
      aes(weight = n),
      method = "lm",
      formula = y ~ x,
      color = "#3c3c3c",
      size = 0.5,
      alpha = 0.2
    ) +
    ggpmisc::stat_poly_eq(
      aes(weight = n, label = ..eq.label..),
      formula = y ~ x,
      label.x.npc = 0.1,
      label.y.npc = 0.12,
      coef.digits = 4,
      parse = TRUE,
      family = "Montserrat",
      size = 2.5,
    ) +
    ggpmisc::stat_poly_eq(
      aes(
        weight = n,
        label = paste(..rr.label.., after_stat(p.value.label), sep = "*\", \"*")
      ),
      formula = y ~ x,
      label.x.npc = 0.1,
      label.y.npc = 0.05,
      coef.digits = 4,
      parse = TRUE,
      family = "Montserrat",
      size = 2.5,
      small.p = TRUE
    ) +
    labs(
      x = NULL,
      y = parse(text = ylab)
    ) +
    facet_grid(season ~ str_wrap_factor(bioregion_name, 20), scales = scale_y) +
    theme(
      legend.position = "none",
      panel.spacing = unit(1, "lines", data = NULL),
      strip.text = element_text(size = 10)
    )
}

p1 <- make_plot(df_viz, aphy, "a[phi]~(443)~(m^{-1})")
p2 <- make_plot(df_viz, anap, "a[NAP]~(443)~(m^{-1})")
p3 <- make_plot(df_viz, snap, "s[NAP]~(443)~(nm^{-1})")
p4 <- make_plot(df_viz, anap / aphy, "a[NAP]~(443) / a[phi]~(443)")
p5 <- make_plot(df_viz, hplcchla, "'[Chl-a]'~(mg~m^{-3})", free_y = TRUE)

ggsave(
  here("graphs/fig10a.pdf"),
  p1,
  device = cairo_pdf,
  width = 7,
  height = 4.7
)

ggsave(
  here("graphs/fig10b.pdf"),
  p2,
  device = cairo_pdf,
  width = 7,
  height = 4.7
)

ggsave(
  here("graphs/fig10c.pdf"),
  p3,
  device = cairo_pdf,
  width = 7,
  height = 4.7
)

ggsave(
  here("graphs/fig10d.pdf"),
  p4,
  device = cairo_pdf,
  width = 7,
  height = 4.7
)

ggsave(
  here("graphs/fig10e.pdf"),
  p5,
  device = cairo_pdf,
  width = 7,
  height = 4.7
)
