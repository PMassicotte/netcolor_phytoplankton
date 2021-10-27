df <- read_csv(here("data","clean","merged_dataset.csv")) %>%
  filter(wavelength == 443)

paaw <- read_csv(here("data","clean","apparent_visible_wavelength.csv"))

df <- df %>%
  inner_join(paaw, by = c("sample_id", "bioregion_name"))

df

df %>%
  filter(perid > 0) %>%
  ggplot(aes(x = avw_aphy, y = perid)) +
  geom_point() +
  scale_y_log10()

df %>%
  # filter(perid > 0) %>%
  ggplot(aes(x = avw_aphy, y = fucox)) +
  geom_point() +
  scale_y_log10() +
  scale_x_log10()

df %>%
  select(sample_id, aphy, fucox, hplcchla, avw_aphy) %>%
  pivot_longer(c(fucox, hplcchla)) %>%
  mutate(value = na_if(value, 0)) %>%
  ggplot(aes(x = aphy, y = value)) +
  geom_point() +
  ggpmisc::stat_poly_eq(
    aes(label = ..eq.label..),
    parse = TRUE,
    coef.digits = 4,
    f.digits = 5,
    p.digits = 10,
    label.x.npc = 0,
    family = "Montserrat",
    size = 3
  ) +
  ggpmisc::stat_poly_eq(
    label.y.npc = 0.88,
    label.x.npc = 0,
    aes(label = ..rr.label..),
    size = 3,
    family = "Montserrat"
  ) +
  scale_x_log10() +
  scale_y_log10() +
  facet_wrap(~name)

df %>%
  select(sample_id, aphy, fucox, hplcchla, avw_aphy) %>%
  pivot_longer(c(fucox, hplcchla)) %>%
  mutate(value = na_if(value, 0)) %>%
  ggplot(aes(x = avw_aphy, y = value)) +
  geom_point() +
  ggpmisc::stat_poly_eq(
    aes(label = ..eq.label..),
    parse = TRUE,
    coef.digits = 4,
    f.digits = 5,
    p.digits = 10,
    label.x.npc = 0,
    family = "Montserrat",
    size = 3
  ) +
  ggpmisc::stat_poly_eq(
    label.y.npc = 0.88,
    label.x.npc = 0,
    aes(label = ..rr.label..),
    size = 3,
    family = "Montserrat"
  ) +
  # scale_x_log10() +
  scale_y_log10() +
  facet_wrap(~name)
