# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:

# Je me serais attendu a plus de « correlation » entre chl-a et a*ph. En hiver,
# ce sont les diatomées qui dominent ainsi que pendant les blooms (faible a*ph),
# mais au printemps, il y a un melange de population et on voit que a*ph se
# situe entre les deux extremes (i.e., high chl, large cell et faible a*ph, d’un
# cote et low chl, small cells et a*ph plus grand). On peut detaillee la
# discussion dans le papier.
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

# Relation between chla and a*phy -----------------------------------------

p <- df %>%
  ggplot(aes(x = hplc_chla, y = specific_phytoplankton_absorption)) +
  geom_point(color = "#3c3c3c", size = 1) +
  scale_x_log10() +
  scale_y_log10() +
  annotation_logticks(sides = "bl", size = 0.25) +
  facet_wrap(~bioregion_name, ncol = 2) +
  labs(
    y = quote(a[phi]^"*" ~ (443) ~ (m^{-1})),
    x = bquote("Chla" ~ (mgC~m^{-3}))
  ) +
  geom_smooth(method = "lm")

ggsave(
  here::here("graphs/19_scatterplot_chla_specific_absorption_phytoplankton.pdf"),
  device = cairo_pdf,
  width = 7,
  height = 8
)

ggsave(
  here::here("graphs/19_scatterplot_chla_specific_absorption_phytoplankton.png"),
  width = 7,
  height = 8,
  dpi = 600
)
