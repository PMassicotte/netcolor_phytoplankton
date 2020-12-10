# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:
#
# Mail from Emmanuel:
#
# aNAP(443) semble tres correle a chl (r2 a calculer), et moins a
# a(443), il faudrait tester les correlations avec un lag de 0 a 8 jours et voir
# ce que cela donne. Il serait interessant de voir ce qui explique les
# variations de aNAP(443). Peut etre faire des tests anova avec chl-a, fuco,
# temperature (si on a) et nitrate… la c’est un peu exploratoire, je ne sais pas
# trop ce qu’on va trouver.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

# Prepare the data --------------------------------------------------------

df <- fread(here::here("data/clean/absorption_with_metadata.csv")) %>%
  as_tibble() %>%
  filter(wavelength == 443) %>%
  select(
    measurement_id,
    date,
    wavelength,
    contains("absorption"),
    hplc_chla
  )

df

df <- df %>%
  mutate(specific_phytoplankton_absorption = phytoplankton_absorption / hplc_chla)

snap <- read_csv(here::here("data/clean/non_algal_absorption_slope.csv")) %>%
  select(measurement_id, non_algal_absorption_slope)

bioregion <- read_csv(here::here("data/clean/bioregions.csv")) %>%
  mutate(bioregion_id = parse_number(bioregion)) %>%
  mutate(bioregion_name = fct_reorder(bioregion_name, bioregion_id))

df <- df %>%
  inner_join(bioregion, by = "measurement_id") %>%
  inner_join(snap, by = "measurement_id")

df

# Only two regions have enough data for seasonal series
df <- df %>%
  filter(str_detect(bioregion_name, "SSSp|SSFa"))

df

# Day of year mean --------------------------------------------------------

df_viz <- df %>%
  mutate(yday = lubridate::yday(date)) %>%
  mutate(date = as.Date(paste0("2014-", yday), "%Y-%j")) %>%
  group_by(date, bioregion_name) %>%
  summarise(across(
    c(contains("absorption"), "hplc_chla"),
    ~ mean(., na.rm = TRUE)
  )) %>%
  ungroup()

df_viz

# Rolling mean ------------------------------------------------------------

df_viz <- df_viz %>%
  group_by(bioregion_name) %>%
  mutate(across(
    c(contains("absorption"), "hplc_chla"),
    ~ rollmean(., k = 8, fill = NA, align = "center"),
    .names = "rollmean_{.col}"
  )) %>%
  ungroup()

df_viz

# Scatter plots -----------------------------------------------------------

p <- df_viz %>%
  ggplot(aes(x = rollmean_non_algal_absorption, y = rollmean_hplc_chla)) +
  geom_point() +
  facet_wrap(~bioregion_name, scales = "free", ncol = 2, dir = "v") +
  # scale_x_log10() +
  # scale_y_log10() +
  geom_smooth(method = "lm")

p

# Crosscorrelation --------------------------------------------------------

df_viz <- df_viz %>%
  drop_na(rollmean_non_algal_absorption, rollmean_hplc_chla) %>%
  group_nest(bioregion_name) %>%
  mutate(mod_ccf = map(
    data,
    ~ ccf(
      .$rollmean_non_algal_absorption,
      .$rollmean_hplc_chla,
      plot = FALSE,
      lag.max = 30
    )
  )) %>%
  mutate(fortified = map(mod_ccf, fortify))

df_viz

# Correlation plots -------------------------------------------------------

p <- df_viz %>%
  unnest(fortified) %>%
  janitor::clean_names() %>%
  ggplot(aes(x = lag, y = 0)) +
  geom_segment(aes(xend = lag, yend = acf), color = "#3c3c3c") +
  facet_wrap(~bioregion_name) +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 10)) +
  scale_y_continuous(breaks = scales::breaks_pretty(n = 10)) +
  labs(
    title = "Autocorrelation between aphy(443) and chla",
    y = "Correlation",
    x = "Lag (days)"
  )

ggsave(
  here::here("graphs/18_autocorrelation_aphy443_chla.pdf"),
  device = cairo_pdf,
  width = 8.55,
  height = 4.34
)

ggsave(
  here::here("graphs/18_autocorrelation_aphy443_chla.png"),
  width = 8.55,
  height = 4.34,
  dpi = 600
)

# tt <- df_viz %>%
#   filter(str_detect(bioregion_name, "SSFa")) %>%
#   unnest(data)
#
# cor(tt$rollmean_non_algal_absorption, tt$rollmean_hplc_chla)
