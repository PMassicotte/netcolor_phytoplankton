# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Overview of the sampling and bathymetry.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source(here("R", "zzz.R"))

stations <- read_csv(here("data", "clean", "metadata.csv"))
bioregions <- read_csv(here("data", "clean", "bioregions.csv"))
station_bathymetry <- read_csv(here("data", "clean", "bathymetry.csv"))

df <- stations %>%
  select(sample_id, date, season) %>%
  inner_join(bioregions, by = "sample_id") %>%
  inner_join(station_bathymetry, by = "sample_id") %>%
  mutate(bioregion_name = factor(
    bioregion_name,
    levels = c(
      "Scotian Shelf",
      "NAB",
      "Labrador"
    )
  ))

df

df %>%
  count(sample_id) %>%
  assertr::verify(n == 1)

# How many observation per season

df %>%
  count(season)

# Number of observations per bioregion/year -------------------------------

df_viz <- df %>%
  mutate(date_month = clock::date_group(date, "year"), .after = date) %>%
  count(bioregion_name, date_month)

df_viz

p1 <- df_viz %>%
  ggplot(aes(x = date_month, y = bioregion_name)) +
  geom_point(aes(size = n)) +
  geom_text(aes(label = n), color = "white", size = 3) +
  scale_x_date(breaks = scales::breaks_pretty(n = 10)) +
  scale_y_discrete(labels = ~ str_wrap(., width = 15)) +
  scale_size(range = c(4, 10)) +
  labs(
    x = "Sampling year",
    y = NULL
  ) +
  theme(
    legend.position = "none"
  )

# Number of observations per bioregion/month ------------------------------

df

p2 <- df %>%
  count(bioregion_name, season) %>%
  mutate(
    month = case_when(
      season == "Spring" ~ "Mar, Apr, May",
      season == "Summer" ~ "Jun, Jul, Aug",
      season == "Autumn" ~ "Sep, Oct, Nov",
      season == "Winter" ~ "Dec, Jan, Feb",
      TRUE ~ NA_character_
    )
  ) %>%
  mutate(
    label = glue::glue(
      "{season}<br><span style = 'font-size:6pt; color:grey50'>({month})</span>"
    )
  ) %>%
  mutate(season = factor(season,
    levels =
      c(
        "Winter",
        "Spring",
        "Summer",
        "Autumn"
      )
  )) %>%
  mutate(label = fct_reorder(label, as.numeric(season))) %>%
  ggplot(aes(x = n, y = label)) +
  geom_col(position = "dodge", fill = "gray75") +
  geom_text(
    aes(label = n),
    size = 2.5,
    position = position_dodge(width = 0.9),
    hjust = -0.5
  ) +
  scale_x_continuous(
    breaks = scales::pretty_breaks(),
    expand = expansion(mult = c(0, 0.2))
  ) +
  labs(
    x = "Number of observations",
    y = NULL
  ) +
  facet_wrap(~str_wrap(bioregion_name, 20)) +
  theme(
    legend.position = "none",
    axis.text.y = element_markdown(),
    strip.text = element_text(size = 10)
  )

p2

# Bathymetry --------------------------------------------------------------

df

p3 <- df %>%
  ggplot(aes(x = -bathymetry, y = bioregion_name)) +
  ggbeeswarm::geom_quasirandom(size = 0.25, groupOnX = FALSE) +
  scale_x_log10() +
  scale_y_discrete(labels = ~ str_wrap(., width = 15)) +
  annotation_logticks(sides = "b", size = 0.1) +
  labs(
    x = "Bathymetry (m)",
    y = NULL
  ) +
  theme(
    legend.position = "none"
  )

# Combine plots -----------------------------------------------------------

p <- p1 / p2 / p3 +
  plot_annotation(tag_levels = "A") &
  theme(
    plot.tag = element_text(face = "bold")
  )

ggsave(
  here("graphs", "appendix01.pdf"),
  device = cairo_pdf,
  width = 180,
  height = 180,
  units = "mm"
)
