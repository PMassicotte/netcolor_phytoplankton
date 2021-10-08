# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Simulate daily absorption data. Based on fitted loess, I will
# generate daily data of aphy, anap and snap. This is needed because we need
# data with regular temporal step to perform correlation lag between variables.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

df <- read_csv(
  here("data", "clean", "merged_dataset.csv"),
  col_select = c(
    sample_id,
    date,
    bioregion_name,
    wavelength,
    anap,
    aphy,
    aphy_specific,
    snap
  )
) %>%
  filter(wavelength %in% c(443, 670))

# Visualize the raw data --------------------------------------------------

df %>%
  filter(wavelength == 443) %>%
  ggplot(aes(x = aphy_specific, y = anap)) +
  geom_point() +
  # scale_x_log10() +
  # scale_y_log10() +
  facet_wrap(~bioregion_name, ncol = 1, scales = "free") +
  geom_smooth(method = "lm")

# Format the date for visualization ---------------------------------------

df_viz <- df %>%
  mutate(yday = lubridate::yday(date)) %>%
  mutate(date = as.Date(paste0("2014-", yday), "%Y-%j")) %>%
  group_by(bioregion_name, date, yday, wavelength) %>%
  summarise(across(c(anap, aphy, aphy_specific, snap), ~mean(., na.rm = TRUE))) %>%
  ungroup()

df_viz

df_viz %>%
  count(bioregion_name, wavelength)

# Create a loess model for each variable ----------------------------------

df_viz

df_viz <- df_viz %>%
  pivot_longer(c(anap, aphy, aphy_specific, snap)) %>%
  group_nest(bioregion_name, name, wavelength) %>%
  mutate(mod = map(data, ~ loess(value ~ yday,
    data = .
  )))

## Add predictions ----

df_viz <- df_viz %>%
  mutate(new_yday = map(data, ~ tibble(yday = seq(
    min(.$yday), max(.$yday),
    by = 1
  )))) %>%
  mutate(augmented = map2(
    mod,
    new_yday,
    ~ broom::augment(.x, newdata = .y, se = TRUE)
  )) %>%
  unnest(augmented) %>%
  select(-where(is.list))

df_viz

df_viz <- df_viz %>%
  rename(
    predicted_loess = .fitted,
    predicted_loess_standard_error = .se.fit
  )

## Add confidence interval and generate random number ----

z_value <- 2.58 # 99%

df_viz <- df_viz %>%
  mutate(low = predicted_loess - z_value * predicted_loess_standard_error) %>%
  mutate(high = predicted_loess + z_value * predicted_loess_standard_error)

# Simulations -------------------------------------------------------------

## Generate random uniform data within the confidence interval ----

res <- df_viz %>%
  group_nest(bioregion_name, name, wavelength) %>%
  expand_grid(simulation = 1:1000) %>%
  mutate(simulated_data = pbmcapply::pbmclapply(data, function(df) {
    res <- df %>%
      mutate(r = map2_dbl(low, high, ~ runif(1, ..1, ..2))) %>%
      pull(r)

    return(res)
  }, mc.cores = 30, mc.set.seed = TRUE)) %>%
  unnest(c(data, simulated_data))

res

res %>%
  distinct(name)

fwrite(res, here("data","clean","simulated_daily_data_from_loess.csv"))
