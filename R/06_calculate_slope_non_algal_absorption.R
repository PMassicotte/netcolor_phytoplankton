# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Calculate the slope of the non-algal absorption spectra.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

df <- data.table::fread("data/clean/absorption_with_metadata.csv") %>%
  as_tibble()

df


# Cut wavelengths ---------------------------------------------------------

# The fit was done for data between 380 and 730 nm, excluding the 400–480 and
# 620–710 nm ranges to avoid any residual pigment absorption that might still
# have been present after sodium hypochlorite treatment. (Babin 2003)

df <- df %>%
  filter(between(wavelength, 380, 730)) %>%
  filter(!between(wavelength, 400, 480)) %>%
  filter(!between(wavelength, 620, 710))

df

# Fit an exponential model ------------------------------------------------

fit_exponential <- function(data) {

  reference_wl <- 500

  a0 <- data$non_algal_absorption[data$wavelength == reference_wl]

  model <- nls(
    non_algal_absorption ~ a0 * exp(-s * (wavelength - reference_wl)) + k,
    start = c(a0 = a0, s = 0.002, k = 0),
    data = data
  )

  return(model)
}

df <- df %>%
  group_by(measurement_id, foldername) %>%
  nest() %>%
  mutate(id = cur_group_id())

df <- df %>%
  ungroup() %>%
  mutate(model = map(data, possibly(fit_exponential, otherwise = NA)))

df <- df %>%
  ungroup() %>%
  mutate(cc = map_chr(model, class)) %>%
  filter(cc != "logical")

df2 <- df %>%
  mutate(pred = map2(data, model, modelr::add_predictions))

# Plot some models --------------------------------------------------------

set.seed(2020)

p <- df2 %>%
  slice_sample(n= 49) %>%
  unnest(pred) %>%
  ggplot(aes(x = wavelength, y = non_algal_absorption)) +
  geom_point(size = 0.1) +
  geom_line(aes(y = pred), color = "red") +
  facet_wrap(~measurement_id, scales = "free_y") +
  scale_y_continuous(breaks = scales::breaks_pretty(n = 4)) +
  labs(
    title = "Examples of fitted non-algal absorption spectra",
    x = "Wavelength (nm)",
    y = bquote("Non-algal absorption" ~ (m^{-1}))
  ) +
  theme(
    legend.title = element_blank(),
    strip.background = element_rect(fill = "#3c3c3c"),
    strip.text = element_text(color = "white", face = "bold"),
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(hjust = 0),
    legend.position = "top"
  )

ggsave(
  here::here("graphs/06_non_algal_absorption_spectra_vs_fitted.pdf"),
  device = cairo_pdf,
  width = 12,
  height = 7
)

# Filter out bad models ---------------------------------------------------

df2 <- df2 %>%
  mutate(model_r2 = map_dbl(pred, ~ cor(.$non_algal_absorption, .$pred)^2))

df2

df2 %>%
  ggplot(aes(x = model_r2)) +
  geom_histogram()

# Example of a bad model
df2 %>%
  filter(model_r2 == min(model_r2)) %>%
  unnest(pred) %>%
  ggplot(aes(x = non_algal_absorption, y = pred)) +
  geom_point()

# Keep only "good" models
df2 <- df2 %>%
  filter(model_r2 >= 0.90)

df2 <- df2 %>%
  mutate(non_algal_absorption_slope = map_dbl(model, ~ coef(.)[2]))

df2 %>%
  select(!where(is.list)) %>%
  write_csv("data/clean/non_algal_absorption_slope.csv")
