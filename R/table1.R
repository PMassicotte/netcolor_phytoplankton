

avw <-
  read_csv(here("data", "clean", "apparent_visible_wavelength.csv"))

options(scipen = 999)

read_csv(here("data", "clean", "merged_dataset.csv")) %>%
  filter(wavelength == 440) %>%
  select(
    sample_id,
    bioregion_name,
    season,
    hplcchla,
    fucox,
    aphy,
    aphy_specific,
    anap,
    snap
  ) %>%
  pivot_longer(hplcchla:snap) %>%
  group_by(bioregion_name, season, name) %>%
  summarise(
    mean_value = mean(value, na.rm = TRUE),
    min_value = min(value, na.rm = TRUE),
    max_value = max(value, na.rm = TRUE),
    n = n()
  ) %>%
  mutate(across(where(is.numeric), ~ round(.x, digits = 4))) %>%
  mutate(label = paste0(mean_value, "\n", "(", min_value, "-", max_value, ")")) %>%
  select(-contains("_value")) %>%
  pivot_wider(names_from = name, values_from = label) %>%
  group_by(bioregion_name) %>%
  gt::gt()
