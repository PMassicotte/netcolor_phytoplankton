# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Seasonal patterns of chla, a*_phy, a_nap and s_nap.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

# Prepare the data --------------------------------------------------------

df <- fread(here::here("data/clean/absorption_with_metadata.csv")) %>%
  as_tibble() %>%
  filter(wavelength == 443) %>%
  select(
    measurement_id,
    date,
    wavelength,
    contains("absorption"),
    hplc_chla
  )

df

df <- df %>%
  mutate(specific_phytoplankton_absorption = phytoplankton_absorption / hplc_chla)

snap <- read_csv(here::here("data/clean/non_algal_absorption_slope.csv")) %>%
  select(measurement_id, non_algal_absorption_slope)

bioregion <- read_csv(here::here("data/clean/bioregions.csv")) %>%
  mutate(bioregion_id = parse_number(bioregion)) %>%
  mutate(bioregion_name = fct_reorder(bioregion_name, bioregion_id))

df <- df %>%
  inner_join(bioregion, by = "measurement_id") %>%
  inner_join(snap, by = "measurement_id")

df

# Weighted mean -----------------------------------------------------------

df_viz <- df %>%
  mutate(yday = lubridate::yday(date)) %>%
  mutate(date = as.Date(paste0("2014-", yday), "%Y-%j")) %>%
  group_by(date, bioregion_name) %>%
  mutate(n = n()) %>%
  summarise(across(
    c(contains("absorption"), "hplc_chla"),
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
    c(contains("absorption"), "hplc_chla"),
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
  ggplot(aes(x = date, y = hplc_chla)) +
  geom_point(color = "gray60") +
  geom_line(aes(y = rollmean_hplc_chla), color = "red") +
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
  ggplot(aes(x = date, y = phytoplankton_absorption)) +
  geom_point(color = "gray60") +
  geom_line(aes(y = rollmean_phytoplankton_absorption), color = "red") +
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
  ggplot(aes(x = date, y = non_algal_absorption)) +
  geom_point(color = "gray60") +
  geom_line(aes(y = rollmean_non_algal_absorption), color = "red") +
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
  ggplot(aes(x = date, y = non_algal_absorption_slope)) +
  geom_point(color = "gray60") +
  geom_line(aes(y = rollmean_non_algal_absorption_slope), color = "red") +
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

# Combine plots -----------------------------------------------------------

p <- p1 / p2 / p3 / p4 +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(face = "bold"))

ggsave(
  here::here("graphs/fig03.pdf"),
  device = cairo_pdf,
  width = 8,
  height = 12
)

pdftools::pdf_convert(
  pdf = here::here("graphs/fig03.pdf"),
  filenames = here::here("graphs/fig03.png"),
  dpi = 300
)

