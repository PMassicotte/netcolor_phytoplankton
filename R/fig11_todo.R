rm(list = ls())

source(here("R", "zzz.R"))

df <- read_csv(here("data", "clean", "merged_dataset.csv")) %>%
  filter(wavelength == 443) %>%
  mutate(season = factor(season,
    levels = c("Spring", "Summer", "Autumn", "Winter")
  )) %>%
  mutate(bioregion_name = factor(
    bioregion_name,
    levels = c(
      "Scotian Shelf",
      "Northwest Atlantic Bassin ocean (NAB)",
      "Labrador"
    )
  ))

df %>%
  count(sample_id, wavelength) %>%
  assertr::verify(n == 1)


# Plot --------------------------------------------------------------------

p <- df %>%
  ggplot(aes(x = anap, y = aphy, color = bioregion_name, fill = bioregion_name)) +
  geom_point(size = 1, shape = 16, stroke = 1, alpha = 0.25) +
  scale_x_log10() +
  scale_y_log10() +
  annotation_logticks(sides = "bl", size = 0.1) +
  geom_smooth(method = "lm", alpha = 0.25, size = 0.5) +
  ggpubr::stat_regline_equation(
    label.y.npc = 0.2,
    label.x.npc = 0.6,
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~~")),
    size = 2
  ) +
  scale_color_manual(
    breaks = area_breaks,
    values = area_colors
  ) +
  scale_fill_manual(
    breaks = area_breaks,
    values = area_colors
  ) +
  facet_wrap(~season, scales = "free") +
  theme(
    legend.position = "none"
  )

ggsave(
  here("graphs","fig11.pdf"),
  device = cairo_pdf,
  height = 6,
  width = 8
)
