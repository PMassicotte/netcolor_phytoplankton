# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Decadal trends of chla globally and for each bioregion. These
# stats are mostly for a table in the paper.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

df <- open_dataset(here("data", "clean", "merged_dataset")) |>
  filter(wavelength == 443) |>
  select(
    sample_id,
    bioregion_name,
    wavelength,
    date,
    aphy,
    anap,
    snap,
    hplcchla
  ) |>
  collect()

df

# Global models
df |>
  mutate(year = lubridate::year(date)) |>
  group_by(year) |>
  summarise(across(c(aphy, anap, snap, hplcchla), mean), n = n()) |>
  ungroup() |>
  filter(n >= 10) |>
  nest(data = everything()) |>
  mutate(mod = map(data, ~lm(.$hplcchla ~ .$year, weights = .$n, data = .))) |>
  mutate(coeff = map(mod, broom::tidy)) |>
  unnest(coeff)

# Global models by bioregions
df |>
  mutate(year = lubridate::year(date)) |>
  group_by(bioregion_name, year) |>
  summarise(across(c(aphy, anap, snap, hplcchla), mean), n = n()) |>
  ungroup() |>
  filter(n >= 10) |>
  group_nest(bioregion_name) |>
  mutate(mod = map(data, ~lm(.$hplcchla ~ .$year, weights = .$n, data = .))) |>
  mutate(coeff = map(mod, broom::tidy)) |>
  unnest(coeff)
