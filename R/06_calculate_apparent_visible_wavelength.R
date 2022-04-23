# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Apparent Visible Wavelength (AVW) calculation as described in:
# Vandermeulen et al., “150 Shades of Green.”
#
# Note: I have tried with aphy specific, and the results are the same as with
# aphy.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source(here("R", "zzz.R"))

aphy <- read_csv(here("data","clean","merged_dataset.csv")) %>%
  select(sample_id, bioregion_name, wavelength, aphy, ap) %>%
  filter(between(wavelength, 400, 700))

aphy

# Calculate AVW -----------------------------------------------------------

df <- aphy %>%
  filter(sample_id == "479274") %>%
  mutate(normalized_aphy = aphy / pracma::trapz(wavelength, aphy))

df %>%
  pivot_longer(contains("aphy")) %>%
  ggplot(aes(x = wavelength, y = value, color = name)) +
  geom_line() +
  facet_wrap(~name, scales = "free_y")

df <- df %>%
  mutate(across(contains("aphy"), .fns = list("avw" = ~sum(.) / sum(. / wavelength))))

# No influence of spectra normalization (it was expected!)
df %>%
  distinct(aphy_avw, normalized_aphy_avw)

# Lets calculate it for all the absorption spectra
df <- aphy %>%
  group_by(sample_id, bioregion_name) %>%
  summarise(across(c(aphy, ap), ~sum(.) / sum(. / wavelength), .names = "avw_{.col}")) %>%
  ungroup()

df

write_csv(df, here("data", "clean", "apparent_visible_wavelength.csv"))
