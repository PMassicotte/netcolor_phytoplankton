# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Boxplot of apparent visible wavelength (AVW) by bioregion and
# season.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source(here("R", "zzz.R"))
source(here("R", "zzz_ggboxplot.R"))

# Data --------------------------------------------------------------------

avw <- read_csv(here("data", "clean", "apparent_visible_wavelength.csv"))

avw

bioregions <- read_csv(here("data", "clean", "bioregions.csv"))

metadata <- read_csv(here("data", "clean", "metadata.csv")) %>%
  select(sample_id, date, season) %>%
  inner_join(bioregions, by = "sample_id")

metadata

df <- avw %>%
  inner_join(metadata, by = c("sample_id", "bioregion_name"))

df

df <- df %>%
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
  ))

# Boxplot AVW -------------------------------------------------------------

p1 <- df %>%
  ggplot(aes(x = season, y = avw_aphy, fill = season)) +
  geom_boxplot(size = 0.1, outlier.size = 0.25) +
  scale_fill_manual(
    breaks = season_breaks,
    values = season_colors
  ) +
  scale_y_continuous(
    labels = scales::label_number(accuracy = 1),
    breaks = scales::breaks_pretty()
  ) +
  labs(
    x = NULL,
    y = str_wrap("Phytoplankton Apparent Absorption Wavelength (PAAW, nm)", 40)
  ) +
  facet_wrap(~ str_wrap_factor(bioregion_name, 20),
    scales = "free_x",
    ncol = 3
  ) +
  theme(
    legend.position = "none",
    panel.grid.minor.y = element_blank(),
    strip.text = element_text(size = 10)
  )

ggsave(
  here("graphs","fig09.pdf"),
  device = cairo_pdf,
  width = 180,
  height = 75,
  units = "mm"
)

df

# Average by season excluding Labrador data (because the seasonal pattern is not
# the same as in Scotian Shelf and NAB).

df %>%
  filter(bioregion_name != "Labrador") %>%
  group_by(season) %>%
  summarise(across(avw_aphy, .fns = list(mean = mean, sd = sd), na.rm = TRUE))
