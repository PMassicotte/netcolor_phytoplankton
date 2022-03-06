paaw <- read_csv(here("data", "clean", "apparent_visible_wavelength.csv"))

df <- read_csv(here("data", "clean", "merged_dataset.csv")) %>%
  filter(wavelength == 443) %>%
  left_join(paaw)

df

p1 <- df %>%
  ggplot(aes(x = avw_aphy, y = aphy_specific)) +
  geom_point(
    aes(fill = season),
    color = "transparent",
    size = 1.5,
    stroke = 0,
    pch = 21,
    alpha = 0.3
  ) +
  scale_fill_manual(
    breaks = c("Winter", "Spring", "Summer", "Autumn"),
    values = c("#014f86", "#40916c", "#ffcb69", "#e76f51"),
    guide = guide_legend(
      override.aes = list(size = 2, alpha = 1),
      label.theme = element_text(size = 7, family = "Montserrat Light")
    )
  )

p2 <- df %>%
  ggplot(aes(x = hplcchla, y = aphy_specific)) +
  geom_point(
    aes(fill = season),
    color = "transparent",
    size = 1.5,
    stroke = 0,
    pch = 21,
    alpha = 0.3
  ) +
  scale_fill_manual(
    breaks = c("Winter", "Spring", "Summer", "Autumn"),
    values = c("#014f86", "#40916c", "#ffcb69", "#e76f51"),
    guide = guide_legend(
      override.aes = list(size = 2, alpha = 1),
      label.theme = element_text(size = 7, family = "Montserrat Light")
    )
  )

p1 / p2
