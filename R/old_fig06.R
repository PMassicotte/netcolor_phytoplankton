# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Seasonal variability of non-algal absorption and snap.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

df <- read_csv(here("data", "clean", "merged_dataset.csv")) %>%
  select(sample_id, date, bioregion_name, wavelength, anap) %>%
  filter(wavelength %in% c(443))

df %>%
  ggplot(aes(x = date, y = anap, color = factor(wavelength))) +
  geom_point() +
  facet_wrap(~bioregion_name)

df %>%
  distinct(bioregion_name)

# How many observations per yday?

df %>%
  mutate(yday = lubridate::yday(date)) %>%
  count(bioregion_name, wavelength, yday) %>%
  filter(wavelength == 443) %>%
  ggplot(aes(x = yday, y = n)) +
  geom_point() +
  facet_wrap(~bioregion_name, scales = "free_y")

# Format the date for visualization ---------------------------------------

df_viz <- df %>%
  mutate(yday = lubridate::yday(date)) %>%
  mutate(date = as.Date(paste0("2014-", yday), "%Y-%j")) %>%
  group_by(bioregion_name, date, yday, wavelength) %>%
  summarise(anap = mean(anap, na.rm = TRUE)) %>%
  ungroup()

df_viz

df_viz %>%
  count(bioregion_name, wavelength)

# Plot the seasonal cycles ------------------------------------------------

p <- df_viz %>%
  ggplot(aes(
    x = date,
    y = anap
  )) +
  geom_point(size = 1, color = "#393E41") +
  geom_smooth(
    method = "loess",
    show.legend = FALSE,
    color = "#bf1d28",
    size = 0.5,
    level = 0.99
  ) +
  facet_wrap(~ bioregion_name, scales = "free_y", ncol = 1) +
  labs(
    x = NULL,
    y = quote(a[NAP~(443)]~(m^{-1}))
  ) +
  theme(
    legend.position = "none"
  )

ggsave(
  here("graphs","fig06.pdf"),
  device = cairo_pdf,
  width = 4,
  height = 6
)
