# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Boxplot showing the seasonal evolution of selected variables.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source(here("R", "zzz.R"))
source(here("R", "zzz_ggboxplot.R"))

# Load data ---------------------------------------------------------------

df <- read_csv(here("data", "clean", "merged_dataset.csv")) %>%
  filter(wavelength == 443) %>%
  mutate(season = factor(season,
    levels = c("Spring", "Summer", "Autumn", "Winter")
  )) %>%
  mutate(bioregion_name = factor(
    bioregion_name,
    levels = c(
      "Scotian Shelf",
      "Northwest Atlantic Basin ocean (NAB)",
      "Labrador"
    )
  )) %>%
  mutate(bioregion_name_wrap = str_wrap_factor(bioregion_name, 20))

df %>%
  count(sample_id, wavelength) %>%
  assertr::verify(n == 1)

# Range
df %>%
  filter(snap > 0.001) %>%
  pull(snap) %>%
  range() %>%
  round(digits = 3)

# Fig 7 Boxplots on absorption --------------------------------------------

p1 <- ggboxlpot(df,
  season,
  aphy,
  bioregion_name,
  strip.text = element_text(size = 10),
  ylab = "a[phi]~(443)~(m^{-1})"
)

p2 <- ggboxlpot(df,
  season,
  hplcchla,
  bioregion_name,
  strip.text = element_blank(),
  ylab = "Chlorophyll-italic(a)~(mg~m^{-3})"
)

p3 <- ggboxlpot(df,
  season,
  aphy_specific,
  bioregion_name,
  strip.text = element_blank(),
  ylab = "a[phi]^'*'~(443)~(m^{2}~mg^{-1})"
)

p <- p1 + p2 + p3 +
  plot_layout(ncol = 1) +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(size = 16, face = "bold"))

ggsave(
  here("graphs","fig06.pdf"),
  device = cairo_pdf,
  width = 190,
  height = 180,
  units = "mm"
)

# p2 <- ggboxlpot(df,
#   season,
#   fucox,
#   bioregion_name,
#   strip.text = element_blank(),
#   ylab = "Fucoxanthin~(mg~m^{-3})"
# )
#
# p <- p1 / p2 +
#   plot_annotation(tag_levels = "A") &
#   theme(plot.tag = element_text(size = 16, face = "bold"))
#
# ggsave(
#   here("graphs","fig07.pdf"),
#   device = cairo_pdf,
#   width = 8,
#   height = 6
# )
