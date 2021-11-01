# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Compare aphy spectra autumn vs winter in the Labrador Sea.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

df <- read_csv(here("data","clean","merged_dataset.csv"))

df

# Plot --------------------------------------------------------------------

p <- df %>%
  filter(bioregion_name == "Labrador") %>%
  filter(season %in% c("Autumn", "Winter")) %>%
  filter(between(wavelength, 400, 700)) %>%
  ggplot(aes(x = wavelength, y = aphy, group = sample_id)) +
  geom_line(size = 0.2, color = "#3c3c3c") +
  labs(
    x = "Wavelength (nm)",
    y = quote(a[phi]~(lambda)~(m^{-1}))
  ) +
  facet_wrap(~season, ncol = 1) +
  theme(
    strip.text = element_text(size = 10),
    panel.spacing.y = unit(0.75, "cm")
  )

ggsave(
  here("graphs","appendix02.pdf"),
  device = cairo_pdf,
  width = 80,
  height = 90,
  units = "mm"
)
