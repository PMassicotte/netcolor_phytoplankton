# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Some stats to be included in the manuscript.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

# Relation between paaw and fucox -----------------------------------------

fucox <- read_csv(here("data", "clean", "merged_dataset.csv")) %>%
  filter(wavelength == 443) %>%
  select(sample_id, fucox)

paaw <- read_csv(here("data", "clean", "apparent_visible_wavelength.csv"))

df <- fucox %>%
  inner_join(paaw, by = "sample_id") %>%
  filter(fucox > 0)

mod <- lm(log10(fucox) ~ avw_aphy, data = df)

summary(mod)
