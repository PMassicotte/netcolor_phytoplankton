# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Calculate the slope of the non-algal absorption spectra.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

absorption <- data.table::fread("data/clean/absorption.csv") %>%
  as_tibble()

absorption

# Cut wavelengths ---------------------------------------------------------

# The fit was done for data between 380 and 730 nm, excluding the 400–480 and
# 620–710 nm ranges to avoid any residual pigment absorption that might still
# have been present after sodium hypochlorite treatment (Babin 2003).

df <- absorption %>%
  filter(between(wavelength, 380, 730)) %>%
  filter(!between(wavelength, 400, 480)) %>%
  filter(!between(wavelength, 620, 710))

df

# Fit an exponential model ------------------------------------------------

fit_exponential <- function(data) {

  reference_wl <- 500

  a0 <- data$anap[data$wavelength == reference_wl]

  model <- nls(
    anap ~ a0 * exp(-s * (wavelength - reference_wl)) + k,
    start = c(a0 = a0, s = 0.002, k = 0),
    data = data
  )

  return(model)
}

df <- df %>%
  group_by(sample_id) %>%
  nest() %>%
  mutate(id = cur_group_id())

df <- df %>%
  ungroup() %>%
  mutate(model = map(data, possibly(fit_exponential, otherwise = NA)))

df <- df %>%
  ungroup() %>%
  mutate(cc = map_chr(model, class)) %>%
  filter(cc != "logical")

df <- df %>%
  mutate(pred = map2(data, model, modelr::add_predictions))

# Plot some models --------------------------------------------------------

set.seed(2020)

df_model <- df %>%
  slice_sample(n= 49) %>%
  select(sample_id, data_model = data, model) %>%
  mutate(pred = map(model, ~predict(., newdata = list(wavelength = 380:730)))) %>%
  mutate(wl = list(380:730))

df_viz <- df_model %>%
  unnest(data_model)

df_pred <- df_model %>%
  unnest(c(wl, pred))

p <- df_viz %>%
  ggplot(aes(x = wavelength, y = anap)) +
  geom_point(size = 0.1) +
  geom_line(data = df_pred, aes(x = wl, y = pred), color = "red") +
  facet_wrap(~sample_id, scales = "free_y") +
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
  here::here("graphs/02_non_algal_absorption_spectra_vs_fitted.pdf"),
  device = cairo_pdf,
  width = 12,
  height = 7
)

# Filter out bad models ---------------------------------------------------

df <- df %>%
  mutate(model_r2 = map_dbl(pred, ~ cor(.$anap, .$pred)^2))

df

df %>%
  ggplot(aes(x = model_r2)) +
  geom_histogram()

# Example of a bad model
df %>%
  filter(model_r2 == min(model_r2)) %>%
  unnest(pred) %>%
  ggplot(aes(x = anap, y = pred)) +
  geom_point()

# Keep only "good" models
df <- df %>%
  filter(model_r2 >= 0.90)

df <- df %>%
  mutate(snap = map_dbl(model, ~ coef(.)[2]))

df %>%
  select(sample_id, snap) %>%
  write_csv("data/clean/non_algal_absorption_slope.csv")
