# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Compare our a*_phy with the parametric version of Bricaud 1995.
#
# Bricaud, A., Babin, M., Morel, A., & Claustre, H. (1995). Variability in the
# chlorophyll-specific absorption coefficients of natural phytoplankton:
# Analysis and parameterization. Journal of Geophysical Research, 100(C7),
# 13321. https://doi.org/10.1029/95JC00463
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

# Extract parameters from Bricaud 1995 ------------------------------------

aphy_bricaud_1995 <-
  tabulizer::extract_tables(
    here::here("data/raw/table2_bricaud1995.pdf"),
    pages = 1,
    output = "data.frame"
  ) %>%
  .[[1]] %>%
  as_tibble() %>%
  janitor::clean_names()

aphy_bricaud_1995

setDT(aphy_bricaud_1995)

aphy_bricaud_1995 <- melt(
  aphy_bricaud_1995,
  measure.vars = patterns("^a_nm", "^a$|^a_1$", "^b$|^b_1$", "^r_2"),
  value.name = c("wavelength", "a", "b", "r2")
) %>%
  as_tibble() %>%
  select(-variable)

aphy_bricaud_1995

# aphy_bricaud_1995 %>%
#   mutate(a_phy = a * 10^(-b)) %>%
#   ggplot(aes(x = wavelength, y = a_phy)) +
#   geom_line() +
#   scale_x_continuous(breaks = scales::breaks_pretty(n = 20))

# Calculate a*_phy from our data ------------------------------------------

absorption <- vroom::vroom(here::here("data/clean/absorption_with_metadata.csv"))

absorption <- absorption %>%
  select(
    measurement_id,
    wavelength,
    phytoplankton_absorption,
    hplc_chla
  ) %>%
  mutate(specific_phytoplankton_absorption = phytoplankton_absorption / hplc_chla)

absorption <- absorption %>%
  inner_join(aphy_bricaud_1995) %>%
  mutate(specific_phytoplankton_absorption_bricaud = a * hplc_chla^-b)

# Plot --------------------------------------------------------------------

set.seed(20201125)

label <- c(
  "specific_phytoplankton_absorption" = "Measured specific absorption",
  "specific_phytoplankton_absorption_bricaud" = "Parameterized specific absorption from Bricaud 1995"
)

p <- absorption %>%
  group_nest(measurement_id) %>%
  slice_sample(n = 49) %>%
  unnest(data) %>%
  arrange(measurement_id, wavelength) %>%
  pivot_longer(contains("specific")) %>%
  ggplot(aes(x = wavelength, y = value, color = name)) +
  geom_line(key_glyph = "rect") +
  facet_wrap(~measurement_id, scales = "free_y") +
  labs(
    title = "Comparing specific phytoplankton absorption",
    caption = str_wrap("Parameterization from Bricaud, A., Babin, M., Morel, A., & Claustre, H. (1995). Variability in the chlorophyll-specific absorption coefficients of natural phytoplankton: Analysis and parameterization. Journal of Geophysical Research, 100(C7), 13321. https://doi.org/10.1029/95JC00463", 200),
    subtitle = "Showing 49 randomly chosen samples from our dataset.",
    x = "Wavelength (nm)",
    y = quote(a[phi]^"*" ~ (m^{-1}))
  ) +
  paletteer::scale_color_paletteer_d(
    "yarrr::google",
    labels = label,
    guide = guide_legend(
      label.position = "top",
      keyheight = unit(0.2, "cm"),
      keywidth = unit(10, "cm"),
      label.theme = element_text(size = 10, face = "bold")
    )
  ) +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    plot.caption = element_text(size = 8, color = "gray50", hjust = 0)
  )

ggsave(
  here::here("graphs/13_specific_phytoplanton_absorption_spectra_vs_fitted.pdf"),
  device = cairo_pdf,
  width = 12,
  height = 8
)


# Reproduce figure 1 ------------------------------------------------------

# Try to reproduce the figure 1 in Bricaud 1995

absorption <- vroom::vroom(here::here("data/clean/absorption_with_metadata.csv"))

absorption <- absorption %>%
  select(
    measurement_id,
    depth,
    longitude,
    latitude,
    wavelength,
    phytoplankton_absorption,
    hplc_chla
  ) %>%
  mutate(specific_phytoplankton_absorption = phytoplankton_absorption / hplc_chla)

df_viz <- absorption %>%
  group_nest(wavelength) %>%
  mutate(mod = map(
    data,
    ~ minpack.lm::nlsLM(
      specific_phytoplankton_absorption ~ a * hplc_chla^-b,
      data = .,
      start = list(a = 0.01, b = 0.1),
      control = minpack.lm::nls.lm.control(maxiter = 1e3)
    )
  ))

df_viz <- df_viz %>%
  mutate(tidied = map(mod, broom::tidy)) %>%
  mutate(pred = map2(data, mod, modelr::add_predictions)) %>%
  mutate(glanced = map(mod, broom::glance))

p <- df_viz %>%
  filter(wavelength %in% c(412, 443, 490, 565, 675)) %>%
  unnest(pred) %>%
  ggplot(aes(x = hplc_chla, y = specific_phytoplankton_absorption)) +
  geom_point(aes(color = latitude), size = 0.5, alpha = 0.7) +
  geom_line(aes(y = pred), color = "red") +
  facet_wrap(~glue("{wavelength} nm"), scales = "free_y") +
  scale_x_log10() +
  annotation_logticks(sides = "b", size = 0.1) +
  scale_color_viridis_c() +
  labs(
    title = "Replicate of figure 1 in Bricaud 1995"
  )

ggsave(
  here::here("graphs/13_specific_phytoplanton_absorption_spectra_vs_chla.pdf"),
  device = cairo_pdf
)

# Reproduce their figure 3
df_viz %>%
  unnest(tidied) %>%
  filter(between(wavelength, 400, 700)) %>%
  ggplot(aes(x = wavelength, y = estimate, color = term)) +
  geom_line() +
  facet_wrap(~term, scales = "free")

# Reproduce their figure 4
absorption %>%
  filter(wavelength == 443) %>%
  filter(between(phytoplankton_absorption, 0, 0.6)) %>%
  filter(between(hplc_chla, 0, 25)) %>%
  ggplot(aes(x = hplc_chla, y = phytoplankton_absorption)) +
  geom_point()

