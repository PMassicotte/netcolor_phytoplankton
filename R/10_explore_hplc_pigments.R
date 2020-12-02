# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Explore if the bloom can be detected with the pheopigments.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

metadata <- read_csv(here::here("data/clean/metadata.csv"))

metadata

# HPLC_CHLB, HPLC_CHLC3, HPLC_FUCOX, HPLC_PERID, HPLC_ZEA, HPLC_ALLOX, HPLC_BUT19, HPLC_HEX19, HPLC_BUT19 et HPLC_PRASINOX

hplc_pigments <-
  c(
    "hplc_chlb",
    "hplc_chlc3",
    "hplc_fucox",
    "hplc_perid",
    "hplc_zea",
    "hplc_allox",
    "hplc_but19_total",
    "hplc_hex19_total",
    "hplc_phaeo",
    "hplc_prasinox"
  )

df_viz <- metadata %>%
  select(date, longitude, latitude, all_of(hplc_pigments)) %>%
  pivot_longer(starts_with("hplc"),
    names_to = "pigment",
    values_to = "concentration"
  )

df_viz

# Plot --------------------------------------------------------------------

p <- df_viz %>%
  # mutate(concentration = ifelse(concentration == 0, concentration + 0.001, concentration)) %>%
  mutate(latitude_bin = santoku::chop_equally(
    latitude,
    groups = 4,
    labels = santoku::lbl_intervals(raw = TRUE)
  )) %>%
  mutate(month = lubridate::month(date, label = TRUE, abbr = TRUE)) %>%
  ggplot(aes(x = month, y = concentration)) +
  geom_boxplot(
    size = 0.25,
    outlier.size = 0.5,
    fill = "gray85"
  ) +
  scale_y_log10() +
  annotation_logticks(sides = "l", size = 0.1) +
  facet_grid(pigment ~ latitude_bin, scales = "free") +
  labs(
    title = "HPLC pigment concentration by latitudes",
    x = NULL,
    y = "Concentration"
  ) +
  theme(
    strip.text.y = element_text(size = 6),
    strip.text = element_text(color = "white", face = "bold"),
    strip.background = element_rect(fill = "#3c3c3c")
  )

ggsave(
  here::here("graphs/10_boxplots_hplc_pigments_by_latitudes.pdf"),
  device = cairo_pdf,
  width = 12,
  height = 10
)

# Another plot ------------------------------------------------------------

df_viz

df_viz %>%
  filter(concentration > 0) %>%
  filter(str_detect(pigment, "phaeo")) %>%
  mutate(month = lubridate::month(date, label = TRUE)) %>%
  add_count(month) %>%
  mutate(rank = dense_rank(desc(n))) %>%
  filter(rank <= 12) %>%
  ggplot(aes(x = factor(month), y = concentration)) +
  geom_boxplot() +
  scale_y_log10()
