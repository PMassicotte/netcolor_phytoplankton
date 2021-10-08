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

source("R/zzz.R")

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


# Open data ---------------------------------------------------------------

absorption <- fread(here::here("data/clean/merged_dataset.csv")) %>%
  as_tibble() %>%
  select(
    sample_id,
    bioregion_name,
    wavelength,
    aphy,
    aphy_specific,
    hplcchla
  )

absorption

# Calculate a*_phy from our data ------------------------------------------

df_viz <- absorption %>%
  inner_join(aphy_bricaud_1995, by = "wavelength") %>%
  mutate(specific_phytoplankton_absorption_bricaud = a * hplcchla^-b)

# Plot --------------------------------------------------------------------

set.seed(20201125)

label <- c(
  "aphy_specific" = "Measured specific absorption",
  "specific_phytoplankton_absorption_bricaud" = "Parameterized specific absorption from Bricaud 1995"
)

p <- df_viz %>%
  group_nest(sample_id) %>%
  slice_sample(n = 49) %>%
  unnest(data) %>%
  arrange(sample_id, wavelength) %>%
  pivot_longer(contains("specific")) %>%
  ggplot(aes(x = wavelength, y = value, color = name)) +
  geom_line(key_glyph = "rect") +
  facet_wrap(~sample_id, scales = "free_y") +
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
  here::here("graphs/12_specific_phytoplankton_absorption_spectra_vs_fitted.pdf"),
  device = cairo_pdf,
  width = 12,
  height = 8
)

# Bricaud 1995 figure 1 ---------------------------------------------------

# Try to reproduce the figure 1 in Bricaud 1995

df_viz <- absorption %>%
  filter(wavelength %in% c(412, 443, 490, 565, 675))

# Add Bricaud coefficients, so we can compare our models with theirs.

df_viz <- df_viz %>%
  inner_join(aphy_bricaud_1995, by = "wavelength")

df_viz <- df_viz %>%
  filter(hplcchla > 0 & aphy_specific > 0) %>%
  group_nest(bioregion_name, wavelength, a, b) %>%
  mutate(mod = map(
    data,
    ~ minpack.lm::nlsLM(
      aphy_specific ~ a * hplcchla^-b,
      data = .,
      start = list(a = 0.01, b = 0.1),
      control = minpack.lm::nls.lm.control(maxiter = 1e3)
    )
  ))

df_viz

df_viz <- df_viz %>%
  mutate(tidied = map(mod, broom::tidy)) %>%
  mutate(augmented = map(mod, broom::augment)) %>%
  # mutate(pred = map2(data, mod, modelr::add_predictions)) %>%
  mutate(glanced = map(mod, broom::glance))

df_viz

df_viz <- df_viz %>%
  select(-data, -mod, -tidied, -glanced) %>%
  unnest(augmented) %>%
  mutate(aphy_specific_bricaud = a * hplcchla^(-b))

p <-  df_viz %>%
  ggplot(aes(x = hplcchla, y = aphy_specific)) +
  geom_point(color = "gray60", size = 0.5, alpha = 0.7) +
  geom_line(aes(y = .fitted), color = "red") +
  geom_line(aes(y = aphy_specific_bricaud), color = "blue") +
  facet_grid(str_wrap(bioregion_name, 20)~glue("{wavelength} nm"), scales = "free_y") +
  scale_x_log10() +
  annotation_logticks(sides = "b", size = 0.1) +
  scale_color_manual(
    breaks = area_breaks,
    values = area_colors
  ) +
  labs(
    y = quote(a[phi]^"*" ~ (m^{-1})),
    x = quote("Chla" ~ (mg~m^{-3})),
    subtitle = 'The <span style="color:blue">blue lines</span> are derived using the parameters in Bricaud whereas the <span style="color:red">red lines</span> are derived<br>from fitting a model using our data.'
  ) +
  theme(
    legend.position = "none",
    plot.subtitle = element_markdown(
      family = "Montserrat Alternates",
      lineheight = 1.25
    )
  )

ggsave(
  here::here(
    "graphs/12_specific_phytoplankton_absorption_vs_chla_bricaud_1995_figure01.pdf"
  ),
  device = cairo_pdf,
  width = 10,
  height = 10
)
