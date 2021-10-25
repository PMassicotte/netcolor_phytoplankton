# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Seasonal patterns of chla, a*_phy, a_nap and s_nap.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source(here("R","zzz.R"))

# Prepare the data --------------------------------------------------------

df <- fread(here("data","clean","merged_dataset.csv")) %>%
  as_tibble() %>%
  filter(wavelength == 443)

df %>%
  count(sample_id) %>%
  assertr::verify(n == 1)

# Visualize how many observations do we have for seasonal cycles ----------

p <- df %>%
  mutate(yday = lubridate::yday(date)) %>%
  mutate(date = as.Date(paste0("2014-", yday), "%Y-%j")) %>%
  count(bioregion_name, date) %>%
  complete(bioregion_name, date) %>%
  ggplot(aes(x = date, y = n, color = bioregion_name)) +
  geom_point(size = 0.5) +
  scale_color_manual(
    breaks = area_breaks,
    values = area_colors
  ) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b") +
  labs(
    x = NULL,
    y = "Number of observations",
    title = str_wrap("Number of observations for each days of the year", 30)
  ) +
  facet_wrap(~bioregion_name, ncol = 1) +
  theme(
    legend.position = "none"
  )

ggsave(
  here("graphs","08_number_observations_per_yday_bioregion.pdf"),
  device = cairo_pdf,
  width = 4,
  height = 5
)

# Average by the day of the year ------------------------------------------

df_viz <- df %>%
  mutate(yday = lubridate::yday(date)) %>%
  mutate(date = as.Date(paste0("2014-", yday), "%Y-%j")) %>%
  group_by(date, bioregion_name) %>%
  mutate(n = n()) %>%
  summarise(across(
    c(anap, ap, aphy, aphy_specific, snap, hplcchla),
    ~ mean(., na.rm = TRUE)
  )) %>%
  ungroup()

df_viz

# Rolling mean ------------------------------------------------------------

# 8 days rolling mean

df_viz <- df_viz %>%
  group_by(bioregion_name) %>%
  arrange(date) %>%
  mutate(across(
    c(anap, ap, aphy, aphy_specific, snap, hplcchla),
    ~ rollmean(., k = 8, fill = NA, align = "center"),
    .names = "rollmean_{.col}"
  )) %>%
  ungroup()

df_viz

# Select regions ----------------------------------------------------------

df_viz %>%
  count(bioregion_name) %>%
  mutate(bioregion_name = fct_reorder(bioregion_name, n)) %>%
  ggplot(aes(x = n, y = bioregion_name)) +
  geom_col() +
  labs(
    title = "Number of averaged yday observations",
    x = "Number of observations",
    y = NULL
  )

# Only two regions have enough data for seasonal series

df_viz <- df_viz %>%
  filter(str_detect(bioregion_name, "SSSp|SSFa"))

df_viz

# Chla --------------------------------------------------------------------

p1 <- df_viz %>%
  ggplot(aes(x = date, y = hplcchla)) +
  geom_point(color = "gray60") +
  geom_line(aes(y = rollmean_hplcchla), color = "red") +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  scale_y_continuous(breaks = scales::breaks_pretty(n = 6)) +
  labs(
    y = quote("Chla" ~ (mgC~m^{-3})),
    x = NULL
  ) +
  facet_wrap(~bioregion_name, scales = "free_x", nrow = 1) +
  theme(
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    plot.caption = element_text(
      color = "grey50",
      size = 6
    )
  )

# Specific absorption -----------------------------------------------------

p2 <- df_viz %>%
  ggplot(aes(x = date, y = aphy_specific)) +
  geom_point(color = "gray60") +
  geom_line(aes(y = rollmean_aphy_specific), color = "red") +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  scale_y_continuous(breaks = scales::breaks_pretty(n = 6)) +
  labs(
    y = quote(a[phi]^"*" ~ (443) ~ (m^{-1})),
    x = NULL
  ) +
  facet_wrap(~bioregion_name, scales = "free_x", nrow = 1) +
  theme(
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    plot.caption = element_text(
      color = "grey50",
      size = 6
    )
  )

# Non-algal absorption ----------------------------------------------------

p3 <- df_viz %>%
  ggplot(aes(x = date, y = anap)) +
  geom_point(color = "gray60") +
  geom_line(aes(y = rollmean_anap), color = "red") +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  scale_y_continuous(breaks = scales::breaks_pretty(n = 6)) +
  labs(
    y = quote(a[NAP] ~ (443) ~ (m^{-1})),
    x = NULL
  ) +
  facet_wrap(~bioregion_name, scales = "free_x", nrow = 1) +
  theme(
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    plot.caption = element_text(
      color = "grey50",
      size = 6
    )
  )

# Slope of non-algal absorption -------------------------------------------

p4 <- df_viz %>%
  ggplot(aes(x = date, y = snap)) +
  geom_point(color = "gray60") +
  geom_line(aes(y = rollmean_snap), color = "red") +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  scale_y_continuous(breaks = scales::breaks_pretty(n = 6)) +
  labs(
    y = quote(S[NAP] ~ (nm^{-1})),
    x = NULL
  ) +
  facet_wrap(~bioregion_name, scales = "free_x", nrow = 1) +
  theme(
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    plot.caption = element_text(
      color = "grey50",
      size = 6
    )
  )

# Boxplot by month and bioregion ------------------------------------------

p <- df %>%
  ggplot(aes(x = bioregion_position, y = aphy_specific, fill = bioregion_name)) +
  geom_boxplot(size = 0.1, outlier.size = 1, color = "gray75") +
  scale_y_log10() +
  scale_fill_manual(
    breaks = area_breaks,
    values = area_colors
  ) +
  labs(
    x = NULL,
    y = quote(a[phi]^"*"~(443)~(m^{-1}))
  ) +
  facet_wrap(~lubridate::month(date, label = TRUE, abbr = FALSE), ncol = 3) +
  theme(
    legend.title = element_blank(),
    legend.position = "top"
  )

ggsave(
  here("graphs","08_boxplot_aphy_specific_by_month_bioregion.pdf"),
  device = cairo_pdf,
  width = 8,
  height = 8
)

# Boxplot specific absorption ---------------------------------------------

p <- df %>%
  mutate(month = lubridate::month(date, label = TRUE, abbr = TRUE)) %>%
  ggplot(aes(x = month, y = aphy_specific, fill = bioregion_name)) +
  geom_boxplot(size = 0.1, outlier.size = 1, color = "gray75") +
  scale_y_log10() +
  scale_fill_manual(
    breaks = area_breaks,
    values = area_colors,
    guide = guide_legend(
      nrow = 2
    )
  ) +
  labs(
    x = NULL,
    y = quote(a[phi]^"*"~(443)~(m^{-1}))
  ) +
  facet_wrap(~bioregion_name, ncol = 2) +
  theme(
    legend.position = "none"
  )

ggsave(
  here("graphs","08_boxplot_aphy_specific_by_month_bioregion2.pdf"),
  device = cairo_pdf,
  width = 10,
  height = 8
)

# Boxplot hex19 -----------------------------------------------------------

p <- df %>%
  mutate(month = lubridate::month(date, label = TRUE, abbr = TRUE)) %>%
  ggplot(aes(x = month, y = hex19, fill = bioregion_name)) +
  geom_boxplot(size = 0.1, outlier.size = 1, color = "gray75") +
  scale_y_log10() +
  scale_fill_manual(
    breaks = area_breaks,
    values = area_colors
  ) +
  labs(
    x = NULL
  ) +
  facet_wrap(~bioregion_name, ncol = 2) +
  theme(
    legend.position = "none"
  )

ggsave(
  here("graphs","08_boxplot_hex19_by_month_bioregion.pdf"),
  device = cairo_pdf,
  width = 10,
  height = 8
)
