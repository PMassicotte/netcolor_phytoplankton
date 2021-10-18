# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Show that the shape of normalized aphy and aphy* are the same
# and therefore should leads to the same calculated value of AVW.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

aphy <- read_csv(here("data","clean","merged_dataset.csv")) %>%
  select(sample_id, bioregion_name, wavelength, aphy, aphy_specific) %>%
  filter(between(wavelength, 400, 700))

aphy

df <- aphy %>%
  filter(sample_id == 479274)

df

p1 <- df %>%
  pivot_longer(contains("aphy")) %>%
  ggplot(aes(x = wavelength, y = value, color = name)) +
  geom_line() +
  labs(
    title = "Aphy and aphy specific"
  )

p2 <- df %>%
  pivot_longer(contains("aphy")) %>%
  group_by(name) %>%
  mutate(value = value / pracma::trapz(wavelength, value)) %>%
  ggplot(aes(x = wavelength, y = value, color = name)) +
  geom_line() +
  facet_wrap(~name) +
  labs(
    title = "Normalized spectra",
    subtitle = "Both curves have the exact same shape."
  )

p <- p1 / p2

ggsave(
  here("graphs","14_compare_avw_from_aphy_vs_aphy_specific.pdf"),
  device = cairo_pdf,
  width = 8,
  height = 6
)

# Calculate AVW -----------------------------------------------------------

# Calculate AVW for both aphy and aphy* non-normalized spectra.

df %>%
  summarise(across(contains("aphy"), .fns = list("avw" = ~sum(.) / sum(. / wavelength))))
