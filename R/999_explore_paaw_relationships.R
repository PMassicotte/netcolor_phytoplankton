paaw <- read_csv(here("data","clean","apparent_visible_wavelength.csv"))

hplc <- read_csv(here("data","clean","hplc.csv"))

df <- inner_join(paaw, hplc, by = "sample_id")

df

df %>%
  filter(hplcchla > 0) %>%
  ggplot(aes(x = avw_aphy, y = hplcchla)) +
  geom_point() +
  scale_y_log10() +
  annotation_logticks(sides = "l", size = 0.25) +
  geom_smooth(method = "lm") +
  ggpmisc::stat_poly_eq(
    aes(label = ..eq.label..),
    label.y.npc = 0.95,
    label.x.npc = 0.05,
    size = 3,
    coef.digits = 3,
    family = "Montserrat"
  ) +
  ggpmisc::stat_poly_eq(
    label.y.npc = 0.88,
    label.x.npc = 0.05,
    aes(label = ..rr.label..),
    size = 3,
    family = "Montserrat"
  )
