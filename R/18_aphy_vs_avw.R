absorption <- read_csv(here("data", "clean", "absorption.csv")) %>%
  filter(wavelength == 443)

avw <- read_csv(here("data", "clean", "apparent_visible_wavelength.csv"))

df <- inner_join(absorption, avw, by = "sample_id")

p <- df %>%
  ggplot(aes(x = avw_ap, y = aphy)) +
  geom_point(size = 0.5) +
  scale_y_log10() +
  annotation_logticks(sides = "l") +
  geom_smooth(method = "lm", size = 0.5) +
  labs(
    y = "aphy443"
  )

ggsave(
  here("graphs", "18_aphy443_vs_avw.pdf"),
  device = cairo_pdf,
  width = 120,
  height = 80,
  units = "mm"
)

mod1 <- lm(log10(aphy) ~ avw_ap, data = df)
summary(mod1)
