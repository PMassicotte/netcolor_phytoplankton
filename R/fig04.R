# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Yearly variations in chla, a*_phy, a_nap and s_nap.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

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

# Boxplot over the years --------------------------------------------------

# Use boxplot to have a feeling how the data vary over the years
df_viz <- df %>%
  mutate(year = lubridate::year(date)) %>%
  mutate(year = fct_reorder(as.character(year), year)) %>%
  add_count(year) %>%
  filter(n > 1)

df_viz

df_viz %>%
  ggplot(aes(x = year, y = hplc_chla)) +
  geom_boxplot(size = 0.25) +
  labs(
    title = "Temporal variation of Chla\nusing boxplot",
    y = bquote("Chla" ~ (mgC~m^{-3})),
    x = NULL
  ) +
  facet_wrap(~bioregion_name, ncol = 1, scales = "free_y")

# Weighted mean -----------------------------------------------------------

df_viz <- df %>%
  mutate(year = lubridate::year(date)) %>%
  filter(year > 2004) %>%
  group_by(year, bioregion_name) %>%
  mutate(n = n()) %>%
  summarise(across(
    c(contains("absorption"), "hplc_chla"),
    list(mean = mean, sd = sd),
    na.rm = TRUE,
    .names = "{.fn}_{.col}"
  )) %>%
  ungroup()

df_viz

# Chla --------------------------------------------------------------------

p1 <- df_viz %>%
  filter(year > 2004) %>%
  ggplot(
    aes(
      x = year,
      y = mean_hplc_chla
    )
  ) +
  geom_pointrange(
    aes(
      ymin = mean_hplc_chla - sd_hplc_chla,
      ymax = mean_hplc_chla + sd_hplc_chla
    ),
    show.legend = FALSE,
    fill = "#3c3c3c",
    color = "gray75",
    shape = 20,
    fatten = 5,
    size = 0.5
  ) +
  geom_line() +
  scale_y_continuous(breaks = scales::breaks_pretty(n = 4)) +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 8)) +
  facet_wrap(~bioregion_name, scales = "free_y", ncol = 1) +
  labs(
    y = bquote("Chla" ~ (mgC~m^{-3})),
    x = NULL
  )

# Combine plots -----------------------------------------------------------

# p <- p1 + p2 +
#   plot_annotation(tag_levels = "A") &
#   theme(plot.tag = element_text(face = "bold"))

ggsave(
  plot = p1,
  here::here("graphs/fig04.pdf"),
  device = cairo_pdf,
  width = 5,
  height = 10
)

# Convert to png ----------------------------------------------------------

pdftools::pdf_convert(
  pdf = here::here("graphs/fig04.pdf"),
  filenames = here::here("graphs/fig04.png"),
  dpi = 300
)


