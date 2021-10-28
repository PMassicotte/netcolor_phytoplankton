# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Apparent Visible Wavelength (AVW) calculation as described in:
# Vandermeulen et al., “150 Shades of Green.”
#
# Note: I have tried with aphy specific, and the results are the same as with
# aphy.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source(here("R","zzz.R"))

aphy <- read_csv(here("data","clean","merged_dataset.csv")) %>%
  select(sample_id, bioregion_name, wavelength, aphy, ap) %>%
  filter(between(wavelength, 400, 700))

aphy

# Calculate AVW -----------------------------------------------------------

df <- aphy %>%
  filter(sample_id == "479274") %>%
  mutate(normalized_aphy = aphy / pracma::trapz(wavelength, aphy))

df %>%
  pivot_longer(contains("aphy")) %>%
  ggplot(aes(x = wavelength, y = value, color = name)) +
  geom_line() +
  facet_wrap(~name, scales = "free_y")

df <- df %>%
  mutate(across(contains("aphy"), .fns = list("avw" = ~sum(.) / sum(. / wavelength))))

# No influence of spectra normalization (it was expected!)
df %>%
  distinct(aphy_avw, normalized_aphy_avw)

# Lets calculate it for all the absorption spectra
df <- aphy %>%
  group_by(sample_id, bioregion_name) %>%
  summarise(across(c(aphy, ap), ~sum(.) / sum(. / wavelength), .names = "avw_{.col}")) %>%
  ungroup()

df

write_csv(df, here("data","clean","apparent_visible_wavelength.csv"))

# Visualization of AVW ----------------------------------------------------

df %>%
  ggplot(aes(x = avw_aphy)) +
  geom_histogram(binwidth = 1) +
  facet_wrap(~bioregion_name)

p <- df %>%
  mutate(bioregion_name = fct_reorder(bioregion_name, avw_aphy)) %>%
  ggplot(aes(x = bioregion_name, y = avw_aphy, fill = bioregion_name)) +
  geom_boxplot(size = 0.1) +
  scale_fill_manual(breaks = area_breaks, values = area_colors) +
  scale_x_discrete(labels = function(x) str_wrap(x, 20)) +
  labs(
    x = NULL,
    y = "AWV (nm)"
  ) +
  theme(
    legend.position = "none"
  )

ggsave(
  here("graphs","14_boxplot_avw_by_bioregions.pdf"),
  device = cairo_pdf,
  width = 7.19,
  height = 5.21
)

# Visualize spectra base on calculated AVW values -------------------------

df_viz <- df %>%
  dtplyr::lazy_dt() %>%
  inner_join(aphy, by = c("sample_id", "bioregion_name")) %>%
  group_by(sample_id) %>%
  mutate(across(c(aphy, ap),
    ~ . / pracma::trapz(wavelength, .),
    .names = "normalized_{.col}"
  )) %>%
  ungroup() %>%
  as_tibble()

# source("https://gist.githubusercontent.com/friendly/67a7df339aa999e2bcfcfec88311abfc/raw/761a7688fba3668a84b2dfe42a655a1b246ca193/wavelength_to_rgb.R")

p <- df_viz %>%
  filter(between(wavelength, 400, 700)) %>%
  # mutate(color = map_chr(round(avw_aphy), wavelength_to_rgb)) %>%
  ggplot(aes(x = wavelength, y = normalized_aphy, color = avw_aphy, group = sample_id)) +
  geom_line(size = 0.1) +
  # scale_color_identity() +
  paletteer::scale_color_paletteer_c("pals::kovesi.linear_bgyw_15_100_c68", direction = -1) +
  facet_wrap(~bioregion_name, ncol = 1) +
  labs(
    x = "Wavelength (nm)",
    y = quote(Normalized~a[phi]~(nm^-1)),
    color = "AVW (nm)"
  )

ggsave(
  here("graphs","14_normalized_aphy_avw_colored.pdf"),
  device = cairo_pdf,
  width = 6,
  height = 9
)

# Compare the spectra with the highest difference
df_viz %>%
  filter(avw_aphy == min(avw_aphy) | avw_aphy == max(avw_aphy)) %>%
  ggplot(aes(
    x = wavelength,
    y = normalized_aphy,
    group = sample_id,
    color = factor(sample_id)
  )) +
  geom_line() +
  geom_vline(aes(xintercept = avw_aphy, color = factor(sample_id)),
    lty = 2,
    size = 0.5
  ) +
  labs(
    x = "Wavelength (nm)",
    y = quote(Normalized~a[phi]~(nm^-1)),
    title = "Spectra with the highest AVW difference"
  ) +
  paletteer::scale_colour_paletteer_d("ggthemes::wsj_colors6") +
  theme(
    legend.position = "none"
  )

ggsave(
  here("graphs","14_aphy_spectra_highest_avw_difference.pdf"),
  device = cairo_pdf,
  width = 7.19,
  height = 5.21
)

# Relationship between AVW and chla ---------------------------------------

hplc <- read_csv(here("data","clean","hplc.csv"))

df_viz <- df %>%
  inner_join(hplc, by = "sample_id")

df_viz

p <- df_viz %>%
  ggplot(aes(x = avw_aphy, y = hplcchla)) +
  geom_point(aes(color = bioregion_name), size = 1) +
  scale_y_log10() +
  annotation_logticks(sides = "l", size = 0.1) +
  geom_smooth(method = "lm") +
  scale_color_manual(
    breaks = area_breaks,
    values = area_colors,
    guide = guide_legend(
      override.aes = list(size = 2)
    )
  ) +
  labs(
    x = "Apparent visible wavelength (nm)",
    y = quote(Chla~(mg~m^{-3})),
    title = "Relationship between AVW and chla"
  ) +
  theme(

    legend.title = element_blank()
  )

ggsave(
  here("graphs","14_scatterplot_chla_vs_avw.pdf"),
  device = cairo_pdf,
  width = 9,
  height = 5.21
)
