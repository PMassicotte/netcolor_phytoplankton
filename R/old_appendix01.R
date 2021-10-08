# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Make a figure to explain the simulation process used to produce
# regular daily data.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

# Read the data -----------------------------------------------------------

df <- read_csv(here("data/clean/simulated_daily_data_from_loess.csv")) %>%
  filter(wavelength == 443) %>%
  filter(bioregion_name == "Northwest Atlantic Bassin ocean (NAB)") %>%
  filter(name %in% c("aphy_specific", "anap"))

# Randomly choose 3 simulations -------------------------------------------

set.seed(2021)

df_appendix <- df %>%
  group_nest(simulation) %>%
  sample_n(3) %>%
  unnest(data) %>%
  mutate(date = as.Date(paste0("2014-", yday), format = "%Y-%j"))

df_appendix

# Panel A: Simulated daily data -------------------------------------------

# Create labels for the facets

df_appendix <- df_appendix %>%
  mutate(name = case_when(
    str_detect(name, "anap") ~ "bold(a[NAP~(443)])",
    str_detect(name, "aphy_specific") ~ "bold(a[phi~(443)]^'*')",
    TRUE ~ name
  ))

p1 <- df_appendix %>%
  ggplot(aes(x = date, y = predicted_loess)) +
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

# Panel B: Scatterplots of simulated data ---------------------------------

df_appendix <- df %>%
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

# Panel C: Histograms of correlation coefficients -------------------------

df_viz <- read_csv(here("data/clean/simulated_daily_data_from_loess.csv")) %>%
  filter(wavelength == 443) %>%
  filter(name %in% c("aphy_specific", "anap")) %>%
  select(bioregion_name, yday, name, simulation, simulated_data) %>%
  pivot_wider(names_from = name, values_from = simulated_data)

p3 <- df_viz %>%
  group_by(bioregion_name, simulation) %>%
  summarise(r = cor(anap, aphy_specific)) %>%
  ungroup() %>%
  ggplot(aes(x = r)) +
  geom_histogram(binwidth = 0.005) +
  facet_wrap(~bioregion_name, nrow = 1) +
  labs(
    x = "Correlation coefficient (r)",
    y = "Count"
  )

# Combine plots -----------------------------------------------------------

p <- p1 / p2 / p3 +
  plot_annotation(tag_levels = "A") +
  plot_layout(heights = c(1, 0.5, 0.5)) &
  theme(plot.tag = element_text(face = "bold", size = 14))

ggsave(
  here("graphs","appendix1.pdf"),
  device = cairo_pdf,
  width = 10,
  height = 9
)
