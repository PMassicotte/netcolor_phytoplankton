# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Correlation lag between absorption index.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

df <- vroom::vroom(here("data", "clean", "merged_dataset.csv")) %>%
  select(sample_id, date, bioregion_name, wavelength, anap, aphy_specific, snap) %>%
  filter(wavelength %in% c(443))

# Visualize the raw data --------------------------------------------------

df %>%
  ggplot(aes(x = aphy_specific, y = anap)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  facet_wrap(~bioregion_name, ncol = 1, scales = "free") +
  geom_smooth(method = "lm")

# Format the date for visualization ---------------------------------------

df_viz <- df %>%
  mutate(yday = lubridate::yday(date)) %>%
  mutate(date = as.Date(paste0("2014-", yday), "%Y-%j")) %>%
  group_by(bioregion_name, date, yday, wavelength) %>%
  summarise(across(c(anap, aphy_specific, snap), ~mean(., na.rm = TRUE))) %>%
  ungroup()

df_viz

# Create a loess model for each variable ----------------------------------

df_viz

df_viz <- df_viz %>%
  pivot_longer(c(anap, aphy_specific, snap)) %>%
  group_nest(bioregion_name, name) %>%
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

## Add confidence interval and generate random number ----

z_value <- 2.58 # 99%

df_viz <- df_viz %>%
  mutate(low = .fitted - z_value * .se.fit) %>%
  mutate(high = .fitted + z_value * .se.fit)

# Simulations -------------------------------------------------------------

## Generate random uniform data within the confidence interval ----

res <- df_viz %>%
  group_nest(bioregion_name, name) %>%
  expand_grid(simulation = 1:1000) %>%
  mutate(simulated_data = pbmcapply::pbmclapply(data, function(df) {

    res <- df %>%
      mutate(r = map2_dbl(low, high, ~runif(1, ..1, ..2))) %>%
      pull(r)


    return(res)
  }, mc.cores = 30, mc.set.seed = TRUE)) %>%
  unnest(c(data, simulated_data))

## Appendix figure ----

# Make a figure to explain the simulation process

set.seed(2021)

df_appendix <- res %>%
  filter(bioregion_name == "Northwest Atlantic Bassin ocean (NAB)") %>%
  filter(name %in% c("aphy_specific", "anap")) %>%
  group_nest(simulation) %>%
  sample_n(3) %>%
  unnest(data) %>%
  mutate(date = as.Date(paste0("2014-", yday), format = "%Y-%j"))

df_appendix

# Create labels for the facets

df_appendix <- df_appendix %>%
  mutate(name = case_when(
    str_detect(name, "anap") ~ "bold(a[NAP~(443)])",
    str_detect(name, "aphy_specific") ~ "bold(a[phi~(443)]^'*')",
    TRUE ~ name
  ))

p1 <- df_appendix %>%
  ggplot(aes(x = date, y = .fitted)) +
  geom_point(aes(y = simulated_data), size = 0.25, color = "gray50") +
  geom_line(aes(y = high), color = "#bf1d28", lty = 2) +
  geom_line(aes(y = low), color = "#bf1d28", lty = 2) +
  geom_line(color = "#393E41") +
  scale_x_date(date_breaks = "2 months", date_labels = "%b") +
  facet_grid(
    name ~ glue("Simulation {simulation}"),
    scales = "free",
    labeller = labeller(name = label_parsed)
  ) +
  labs(
    x = NULL,
    y = quote(Absorption~(m^{-1}))
  )

df_appendix <- res %>%
  semi_join(df_appendix, by = c("bioregion_name", "simulation")) %>%
  select(bioregion_name, name, yday, simulation, simulated_data) %>%
  pivot_wider(names_from = name, values_from = simulated_data)

df_appendix

p2 <- df_appendix %>%
  filter(anap > 0) %>%
  ggplot(aes(x = aphy_specific, y = anap)) +
  geom_point(color = "#393E41", size = 1) +
  geom_smooth(method = "lm", color = "#bf1d28", size = 0.5) +
  facet_wrap(~glue("Simulation {simulation}")) +
  labs(
    x = quote(a[phi~(443)]^"*"~(m^{-1})),
    y = quote(a[NAP~(443)]~(m^{-1}))
  )

p <- p1 / p2 +
  plot_annotation(tag_levels = "A") +
  plot_layout(heights = c(1, 0.5)) &
  theme(plot.tag = element_text(face = "bold", size = 14))

ggsave(
  here("graphs","appendix1.pdf"),
  device = cairo_pdf,
  width = 10,
  height = 8
)

# Lag correlation ---------------------------------------------------------

res

## Lag correlation per bioregion ----

res2 <- res %>%
  select(bioregion_name, yday, name, simulation, simulated_data) %>%
  pivot_wider(names_from = name, values_from = simulated_data) %>%
  group_nest(bioregion_name, simulation) %>%
  mutate(anap_aphy_specific_ccf_mod = map(data, ~ ccf(
    .$anap, .$aphy_specific,
    plot = FALSE, lag.max = 30
  ))) %>%
  mutate(anap_aphy_specific_ccf_tidy = map(anap_aphy_specific_ccf_mod, broom::tidy)) %>%
  unnest(anap_aphy_specific_ccf_tidy)

res2

# Find the maximum correlation

max_correlation <- res2 %>%
  group_by(bioregion_name, simulation) %>%
  filter(abs(acf) == max(abs(acf))) %>%
  group_by(bioregion_name) %>%
  summarise(
    mean_lag = mean(lag),
    sd_lag = sd(lag),
    n = n()
  )

max_correlation

p <- res2 %>%
  group_by(bioregion_name, lag) %>%
  summarise(acf = mean(acf)) %>%
  ungroup() %>%
  ggplot(aes(
    x = lag,
    y = 0,
    xend = lag,
    yend = acf
  )) +
  geom_segment(color = "gray60", size = 0.25) +
  geom_vline(
    data = max_correlation,
    aes(xintercept = mean_lag),
    color = "#bf1d28",
    size = 0.5
  ) +
  geom_vline(
    data = max_correlation,
    aes(xintercept = mean_lag - sd_lag),
    color = "#bf1d28",
    size = 0.25,
    lty = 2
  ) +
  geom_vline(
    data = max_correlation,
    aes(xintercept = mean_lag + sd_lag),
    color = "#bf1d28",
    size = 0.25,
    lty = 2
  ) +
  geom_hline(yintercept = 0, size = 0.25) +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 10)) +
  labs(
    x = "Lag (days)",
    y = "Correlation"
  ) +
  facet_wrap(~bioregion_name, ncol = 1)

ggsave(
  here("graphs", "fig07.pdf"),
  device = cairo_pdf,
  width = 4,
  height = 6
)
