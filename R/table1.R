

avw <-
  read_csv(here("data", "clean", "apparent_visible_wavelength.csv")) %>%
  select(sample_id, bioregion_name, avw_aphy)

options(scipen = 999)

read_csv(here("data", "clean", "merged_dataset.csv")) %>%
  filter(wavelength == 440) %>%
  left_join(avw, by = c("sample_id", "bioregion_name")) %>%
  select(
    sample_id,
    bioregion_name,
    season,
    hplcchla,
    fucox,
    aphy,
    aphy_specific,
    anap,
    snap,
    avw_aphy
  ) %>%
  pivot_longer(hplcchla:avw_aphy) %>%
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
  gt::gt() %>%
  gt::gtsave(here("manuscript", "table1_by_region_season.rtf"))


read_csv(here("data", "clean", "merged_dataset.csv")) %>%
  filter(wavelength == 440) %>%
  left_join(avw, by = c("sample_id", "bioregion_name")) %>%
  select(
    sample_id,
    bioregion_name,
    season,
    hplcchla,
    fucox,
    aphy,
    aphy_specific,
    anap,
    snap,
    avw_aphy
  ) %>%
  pivot_longer(hplcchla:avw_aphy) %>%
  group_by(name) %>%
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
  gt::gt() %>%
  gt::gtsave(here("manuscript", "table1_full.rtf"))
