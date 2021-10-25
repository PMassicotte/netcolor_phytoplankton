# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Implement the idea from Emanuel to explore the linearity of
# fuxo on absorption.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

# HPLC data ---------------------------------------------------------------

## Calculate the relative contribution of fucox ----

hplc <- read_csv(here("data","clean","hplc.csv")) %>%
  select(-abs_vol_l) %>%
  filter(depth <= 2)

hplc

hplc_clean <- hplc %>%
  group_by(sample_id, depth) %>%
  mutate(percent_fucox = fucox / sum(c_across(chlb:zea), na.rm = TRUE)) %>%
  select(-c(chlb:zea)) %>%
  drop_na(percent_fucox) %>%
  ungroup() %>%
  assertr::verify(between(percent_fucox, 0, 1))

p <- hplc_clean %>%
  ggplot(aes(x = percent_fucox)) +
  geom_histogram(binwidth = 0.005) +
  scale_x_continuous(
    name = NULL,
    labels = scales::label_percent()
  ) +
  labs(
    title = "Percentage of fucox in each sample"
  )

ggsave(
  here("graphs","13_boxplot_percentage_fucoxanthin.pdf"),
  device = cairo_pdf,
  width = 8.16,
  height = 5.21
)

## Bin into 5% classes ----

hplc_clean <- hplc_clean %>%
  mutate(
    percent_fucox_bin =
      santoku::chop(percent_fucox,
        breaks = seq(0, 1, by = 0.05)
      )
  )

p <- hplc_clean %>%
  count(percent_fucox_bin) %>%
  ggplot(aes(x = n, y = percent_fucox_bin)) +
  geom_col() +
  labs(
    title = "Number of observations in each fucox bin",
    x = "Number of observations",
    y = "Bins"
  )

ggsave(
  here("graphs","13_barplot_number_observations_bining_fucoxanthin.pdf"),
  device = cairo_pdf,
  width = 8.16,
  height = 5.21
)

# Merge the fucox classes with all the data -------------------------------

df <- read_csv(here("data","clean","absorption.csv")) %>%
  inner_join(hplc_clean)

df

# Mean phytoplankton specific absorption per fucox bin -------------------

df_viz <- df %>%
  group_by(percent_fucox_bin, wavelength) %>%
  summarise(mean_aphy_specific = mean(aphy_specific)) %>%
  ungroup()

df_viz

p <- df_viz %>%
  ggplot(aes(
    x = wavelength,
    y = mean_aphy_specific,
    color = percent_fucox_bin
  )) +
  geom_line() +
  labs(
    title = "Mean phytoplankton specific absorption",
    subtitle = "Each color represents a bin of fucoxanthin in percentage.",
    x = "Wavelengths (nm)",
    y = quote(a[phi]^"*"~(m^{-1}))
  ) +
  paletteer::scale_color_paletteer_d(
    "ggsci::default_nejm",
    guide = guide_legend(
      label.position = "top",
      nrow = 1,
      override.aes = list(size = 2),
      keywidth = unit(2, "cm")
    )
  ) +
  theme(
    legend.position = "top",
    legend.title = element_blank()
  )

ggsave(
  here(
    "graphs",
    "13_mean_specific_phytoplankton_absorption_bining_fucoxanthin.pdf"
  ),
  device = cairo_pdf,
  width = 8.16,
  height = 5.21
)

# Absorption vs fuco at each wavelength -----------------------------------

# Pour chaque longueur d’onde, tu fais une régression du pourcentage de fuco en
# fonction de a_p (ou a*_p) et on plot le r2 en fonction de lambda.

df

## Visualize some of the relationships ----

p <- df %>%
  filter(wavelength %in% seq(400, 700, by = 20)) %>%
  filter(percent_fucox > 0) %>%
  ggplot(aes(x = percent_fucox, y = aphy_specific)) +
  geom_point(size = 0.25) +
  geom_smooth(method = "lm") +
  facet_wrap(~glue("{wavelength} nm"), scales = "free_y") +
  scale_x_continuous(labels = scales::label_percent()) +
  labs(
    x = "Percentage of fucoxanthin",
    y = quote(a[phi]^"*"~(m^{-1}))
  )

ggsave(
  here("graphs","13_scatterplot_percent_fucoxanthin_aphy_specific.pdf"),
  device = cairo_pdf,
  width = 10,
  height = 8
)

## Calculate the determination coefficient ----

df_viz <- df %>%
  group_nest(wavelength) %>%
  mutate(mod = map(data, ~lm(aphy_specific ~ percent_fucox, data = .))) %>%
  mutate(glanced = map(mod, broom::glance))

p <- df_viz %>%
  unnest(glanced) %>%
  ggplot(aes(x = wavelength, y = r.squared)) +
  geom_line(size = 1) +
  labs(
    x = "Wavelength (nm)",
    y = quote(R^2)
  )

ggsave(
  here("graphs","13_r2_percent_fucoxanthin_vs_aphy_specific.pdf"),
  device = cairo_pdf,
  width = 8.16,
  height = 5.21
)

# Mean fucox vs mean aphy -------------------------------------------------

hplc <- read_csv(here("data","clean","hplc.csv")) %>%
  select(sample_id, depth, fucox)

hplc

df

df_viz <- df %>%
  inner_join(hplc, by = c("sample_id", "depth"))

df_viz

df_viz <- df_viz %>%
  group_by(wavelength, percent_fucox_bin) %>%
  summarise(across(c(contains("aphy"), fucox), .fns = list("mean" = mean))) %>%
  ungroup()

df_viz

df_viz %>%
  filter(wavelength %in% seq(400, 700, by = 20)) %>%
  ggplot(aes(x = aphy_mean, y = fucox_mean)) +
  geom_point(aes(color = percent_fucox_bin)) +
  geom_smooth(method = "lm") +
  facet_wrap(~wavelength, scales = "free")
