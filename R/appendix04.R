# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Show some relationships with PAAW.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

df <- read_csv(here("data","clean","merged_dataset.csv")) %>%
  filter(wavelength == 443)

paaw <- read_csv(here("data","clean","apparent_visible_wavelength.csv"))

df <- df %>%
  inner_join(paaw, by = c("sample_id", "bioregion_name"))

df

# Plot --------------------------------------------------------------------

p1 <- df %>%
  ggplot(aes(x = avw_aphy, y = hplcchla)) +
  geom_point(size = 1, color = "#4c4c4c") +
  scale_x_continuous(breaks = scales::breaks_pretty()) +
  scale_y_log10() +
  annotation_logticks(sides = "l", size = 0.1) +
  geom_smooth(method = "lm", color = "red", size = 0.5) +
  labs(
    x = NULL,
    y = quote(Chlorophyll-italic(a)~(mg~m^{-3}))
  ) +
  ggpmisc::stat_poly_eq(
    aes(label = ..eq.label..),
    parse = TRUE,
    coef.digits = 4,
    f.digits = 5,
    p.digits = 10,
    label.x.npc = 0.05,
    family = "Montserrat",
    size = 3
  ) +
  ggpmisc::stat_poly_eq(
    label.x.npc = 0.05,
    label.y.npc = 0.88,
    aes(label = ..rr.label..),
    size = 3,
    family = "Montserrat"
  )

p2 <- df %>%
  ggplot(aes(x = avw_aphy, y = aphy_specific)) +
  geom_point(size = 1, color = "#4c4c4c") +
  scale_x_continuous(breaks = scales::breaks_pretty()) +
  scale_y_log10() +
  annotation_logticks(sides = "l", size = 0.1) +
  geom_smooth(method = "lm", color = "red", size = 0.5) +
  labs(
    x = "Phytoplankton Apparent Absorption Wavelength (PAAW, nm)",
    y = quote(a[phi]^'*'~(443)~(m^2~mg^{-1}))
  ) +
  ggpmisc::stat_poly_eq(
    aes(label = ..eq.label..),
    parse = TRUE,
    coef.digits = 4,
    f.digits = 5,
    p.digits = 10,
    label.x.npc = 0.05,
    label.y.npc = 0.12,
    family = "Montserrat",
    size = 3
  ) +
  ggpmisc::stat_poly_eq(
    label.x.npc = 0.05,
    label.y.npc = 0.03,
    aes(label = ..rr.label..),
    size = 3,
    family = "Montserrat"
  )

p <- p1 / p2 +
  plot_annotation(tag_levels = "A") &
  theme(
    plot.tag = element_text(face = "bold")
  )

ggsave(
  here("graphs","appendix04.pdf"),
  device = cairo_pdf,
  width = 6,
  height = 6
)
