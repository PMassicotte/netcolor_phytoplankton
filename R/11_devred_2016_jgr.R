# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Explore and try to understand the method of Emmanuel that aims
# to decompose absorption spectra into two populations.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

absorption <- read_csv(here::here("data/clean/merged_dataset.csv")) %>%
  select(sample_id, wavelength, aphy, hplcchla) %>%
  filter(between(wavelength, 400, 700)) %>%
  filter(hplcchla <= 20)

absorption

# Fit the model -----------------------------------------------------------

# These models are used to find a "representative" slope (S) between
# phytoplankton absorption and chla.

absorption <- absorption %>%
  group_nest(wavelength) %>%
  mutate(mod_s = map(
    data,
    ~ minpack.lm::nlsLM(
      aphy ~ (as1 - as2) / s * (1. - exp(-s * hplcchla)) + as2 *
        hplcchla,
      data = .,
      start = list(as1 = 0.1, as2 = 0.05, s = 1)
    )
  ))

absorption

# Broom it! ---------------------------------------------------------------

absorption <- absorption %>%
  mutate(glanced = map(mod_s, glance)) %>%
  mutate(tidied = map(mod_s, tidy)) %>%
  mutate(augmented = map(mod_s, augment))

absorption

# Look at some fits -------------------------------------------------------

p <- absorption %>%
  filter(wavelength %in% seq(300, 700, by = 10)) %>%
  unnest(augmented) %>%
  ggplot(aes(x = hplcchla, y = aphy)) +
  geom_point(size = 0.25) +
  geom_line(aes(y = .fitted), color = "#F15025") +
  scale_x_log10() +
  scale_y_log10() +
  annotation_logticks(size = 0.2, long = unit(1, "mm"), short = unit(0.5, "mm")) +
  facet_wrap(~glue("{wavelength} nm"), scales = "free") +
  labs(
    title = "Relationships between Chla and Aphy",
    subtitle = str_wrap("The red line represent the model using equation 4 from Devred (2006) in JGR.", 120),
    y = bquote("Phytoplankton absorption" ~ (m^{-1})),
    x = bquote("HPLC Chl a" ~ (mgC~m^{-3}))
  ) +
  theme(
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    # plot.subtitle = element_text(size = 6),
    strip.background = element_rect(fill = "#3c3c3c"),
    strip.text = element_text(face = "bold", color = "white")
  )

ggsave(
  here::here("graphs/11_hplc_chla_vs_phyto_absorption.pdf"),
  device = cairo_pdf,
  height = 8,
  width = 10
)

# S as a function of wavelengths ------------------------------------------

p <- absorption %>%
  unnest(tidied) %>%
  filter(term == "s") %>%
  ggplot(aes(x = wavelength, y = estimate)) +
  geom_line(lineend = "round") +
  geom_vline(xintercept = 443, lty = 2, color = "#F15025") +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 16)) +
  labs(
    x = "Wavelength (nm)",
    y = bquote(S ~ (nm^{-1})),
    title = "S as a function of wavelengths",
    subtitle = str_wrap("The slope was calculated between chla and phtyplankton absorption using equation 4 from Devred (2006) in JGR. The vertical red line shows the value at 443 nm.", 100)
  ) +
  theme(
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    plot.subtitle = element_text(size = 6)
  )

ggsave(
  here::here("graphs/11_s_vs_wavelength.pdf"),
  device = cairo_pdf,
  width = 6,
  height = 4
)

# Calculate as1 and as2 ---------------------------------------------------

# With the S's previously calculated, use a model with only two parameters, as1
# and as2.

absorption <- absorption %>%
  select(wavelength:mod_s, tidied) %>%
  unnest(tidied) %>%
  filter(term == "s") %>%
  select(-c(std.error:p.value)) %>%
  mutate(average_s = mean(estimate[between(wavelength, 400, 500)])) %>%
  mutate(mod_as1_as2 = map2(
    data,
    average_s,
    ~ minpack.lm::nlsLM(
      aphy ~ (as1 - as2) / ..2 * (1. - exp(-..2 * hplcchla)) + as2 *
        hplcchla,
      data = ..1,
      start = list(as1 = 0.08, as2 = 0.04)
    )
  ))

# Broom it again! ---------------------------------------------------------

absorption <- absorption %>%
  select(-term, -estimate) %>%
  mutate(tidied = map(mod_as1_as2, tidy, conf.int = TRUE, conf.level = 0.99)) %>%
  mutate(glanced = map(mod_as1_as2, glance)) %>%
  mutate(augmented = map(mod_as1_as2, augment))

absorption

# Plot as1 and as2 --------------------------------------------------------

df_viz <- absorption %>%
  unnest(tidied)

range(df_viz$estimate)

p <- df_viz %>%
  ggplot(aes(x = wavelength, y = estimate)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
    fill = "gray75",
    alpha = 0.5
  ) +
  geom_line(size = 0.5, lineend = "round") +
  facet_wrap(~term, scales = "free", ncol = 1) +
  scale_y_continuous(breaks = scales::breaks_pretty(n = 6)) +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 6)) +
  labs(
    title = "Absorption spectra for the two populations",
    subtitle = str_wrap(glue("The partitioning has been done using an average S value of {round(unique(absorption$average_s), digits = 2)}. The shaded area shows the 99% confidence interval."), 100),
    x = "Wavelength (nm)",
    y = "Specific absorption aoefficients"
  ) +
  theme(
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    strip.background = element_rect(fill = "#3c3c3c"),
    strip.text = element_text(face = "bold", color = "white", size = 14)
  )

ggsave(
  here::here("graphs/11_as1_as2_vs_wavelength.pdf"),
  device = cairo_pdf,
  width = 8,
  height = 8
)
