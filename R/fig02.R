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
  inner_join(station_bathymetry, by = "sample_id")

df

df %>%
  count(sample_id) %>%
  assertr::verify(n == 1)

# Number of observations per bioregion/year -------------------------------

df_viz <- df %>%
  mutate(date_month = clock::date_group(date, "year"), .after = date) %>%
  count(bioregion_name, date_month)

df_viz

p1 <- df_viz %>%
  ggplot(aes(x = date_month, y = bioregion_name, color = bioregion_name)) +
  geom_point(aes(size = n)) +
  geom_text(aes(label = n), color = "white", size = 3) +
  scale_x_date(breaks = scales::breaks_pretty(n = 10)) +
  scale_y_discrete(labels = ~ str_wrap(., width = 15)) +
  scale_color_manual(
    breaks = area_breaks,
    values = area_colors,
    guide = guide_legend(
      order = 1,
      label.position = "top",
      keyheight = unit(0.1, "cm"),
      keywidth = unit(3, "cm"),
      label.theme = element_text(size = 5, family = "Poppins"),
      nrow = 3
    )
  ) +
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
  ggplot(aes(x = n, y = label, fill = bioregion_name)) +
  geom_col(position = "dodge") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 11)) +
  scale_fill_manual(
    breaks = area_breaks,
    values = area_colors
  ) +
  labs(
    x = "Number of observations",
    y = NULL
  ) +
  theme(
    legend.position = "none",
    axis.text.y = element_markdown()
  )

p2

# Bathymetry --------------------------------------------------------------

df

p3 <- df %>%
  ggplot(aes(x = -bathymetry, y = bioregion_name, color = bioregion_name)) +
  ggbeeswarm::geom_quasirandom(size = 0.25, groupOnX = FALSE) +
  scale_x_log10() +
  scale_y_discrete(labels = ~ str_wrap(., width = 15)) +
  annotation_logticks(sides = "b", size = 0.1) +
  scale_color_manual(
    breaks = area_breaks,
    values = area_colors
  ) +
  labs(
    x = "Bathymetry (m)",
    y = NULL
  ) +
  theme(
    legend.position = "none"
  )

# Combine plots -----------------------------------------------------------

p <- p1 / p2 / p3 +
  # plot_layout(widths = c(1, 5, 5)) +
  plot_annotation(tag_levels = "A") &
  theme(
    plot.tag = element_text(face = "bold")
  )

ggsave(
  here("graphs", "fig02.pdf"),
  device = cairo_pdf,
  width = 8,
  height = 8
)
