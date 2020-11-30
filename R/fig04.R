# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Temporal variability of the spectral slope of non-algal
# absorption.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

non_algal_absorption_slope <-
  read_csv("data/clean/non_algal_absorption_slope.csv") %>%
  select(-c(id, cc, model_r2))

metadata <- read_csv("data/clean/metadata.csv")

metadata <- metadata %>%
  select(
    measurement_id,
    date,
    depth,
    longitude,
    latitude
  )

df <- non_algal_absorption_slope %>%
  inner_join(metadata, by = "measurement_id") %>%
  mutate(yday = lubridate::yday(date))

df

df %>%
  count(yday, sort = TRUE)

# Histogram of S_NAP ------------------------------------------------------

df %>%
  ggplot(aes(x = non_algal_absorption_slope)) +
  geom_histogram(binwidth = 0.0005) +
  geom_vline(
    xintercept = 0.0123,
    color = "red",
    lty = 2
  ) +
  annotate("text",
    x = 0.0126,
    y = Inf,
    label = "Average value of 0.0123\nin Babin 2003",
    vjust = 1,
    hjust = 0
  )


# Histogram of depth measurements -----------------------------------------

#TODO: Should we keep only surface measurements?
df %>%
  ggplot(aes(x = depth)) +
  geom_histogram(binwidth = 0.5)

# Sina plot ---------------------------------------------------------------

p1 <- df %>%
  ggplot(aes(
    x = lubridate::month(date, label = TRUE, abbr = TRUE),
    y = non_algal_absorption_slope
  )) +
  ggforce::geom_sina(
    aes(color = latitude),
    size = 0.5
  ) +
  scale_x_discrete(expand = expansion(mult = c(0.05, 0.05))) +
  labs(
    x = NULL,
    y = quote(S[NAP] ~ (nm^{-1})),
    color = "Latitude"
  ) +
  scale_color_viridis_c(
    option = "B",
    guide = guide_colorbar(
      barwidth = unit(10, "cm"),
      barheight = unit(0.25, "cm"),
      title.position = "top",
      title.hjust = 0.5
    )
  ) +
  theme(legend.position = "top")

# Rolling mean plot -------------------------------------------------------

df_viz <- df %>%
  group_by(yday) %>%
  summarise(
    mean_non_algal_absorption_slope = mean(non_algal_absorption_slope),
    sd_non_algal_absorption_slope = sd(non_algal_absorption_slope),
    n = n()
  ) %>%
  mutate(date = as.Date(paste0("2014-", yday), "%Y-%j"))

df_viz

p2 <- df_viz %>%
  ggplot(aes(x = date, y = mean_non_algal_absorption_slope)) +
  geom_point(color = "gray50") +
  geom_line(aes(y = rollmean(
    mean_non_algal_absorption_slope,
    k = 28,
    na.pad = TRUE
  )),
  color = "red",
  size = 0.5
  ) +
  labs(
    y = quote(S[NAP] ~ (nm^{-1})),
    x = NULL
  ) +
  scale_x_date(
    date_labels = "%b",
    date_breaks = "1 month",
    expand = expansion(mult = c(0.05, 0.01))
  ) +
  scale_y_continuous(breaks = scales::breaks_pretty(n = 5)) +
  theme(
    panel.grid.minor.x = element_blank()
  )

# Combine plots -----------------------------------------------------------

p <- p1 / p2 +
  plot_annotation(
    tag_levels = "A"
  ) &
  theme(plot.tag = element_text(face = "bold"))

ggsave(
  here::here("graphs/fig04.pdf"),
  device = cairo_pdf,
  width = 6,
  height = 8
)
