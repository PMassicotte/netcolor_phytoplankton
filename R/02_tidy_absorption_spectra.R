# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Clean the raw absorption spectra given by Emmanuel Devred.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

# Read the data -----------------------------------------------------------

df <- read_csv("data/clean/list_of_valid_absorption_measurements.csv")

df <- df %>%
  rowwise() %>%
  mutate(data = list(data.table::fread(
    filepath,
    header = FALSE,
    col.names = c("wavelength", "absorption")
  ))) %>%
  unnest(data) %>%
  as_tibble()

df %>%
  filter(wavelength == 750) %>%
  count(measurement_id, foldername, sort = TRUE)

# Verify that we have 401 wavelengths per spectra
df %>%
  count(measurement_id, foldername, type, sort = TRUE) %>%
  assertr::verify(n == 401)

# Calculate ANAP ----------------------------------------------------------

df <- df %>%
  select(-filename, -filepath) %>%
  pivot_wider(names_from = type, values_from = absorption) %>%
  janitor::clean_names() %>%
  mutate(phytoplankton_absorption = total_absorption - non_algal_absorption)

df


# Remove spectra with a least 1 negative value between 350 and 400 --------

df <- df %>%
  group_by(measurement_id, foldername) %>%
  filter(!any((
    total_absorption < 0 |
      non_algal_absorption < 0 |
      phytoplankton_absorption < 0
  ) & between(wavelength, 350, 400))) %>%
  ungroup()


# Remove outliers ---------------------------------------------------------

df_viz <- df %>%
  filter(wavelength %in% c(443, 488)) %>%
  select(measurement_id, foldername, wavelength, phytoplankton_absorption) %>%
  pivot_wider(
    names_from = wavelength,
    values_from = phytoplankton_absorption,
    names_prefix = "nm_"
  )

df_viz %>%
  ggplot(aes(x = nm_443, y = nm_488)) +
  geom_point()

mod <- lm(nm_488 ~ nm_443, data = df_viz)

# https://stats.stackexchange.com/questions/164099/removing-outliers-based-on-cooks-distance-in-r-language
df_viz <- df_viz %>%
  mutate(outlier = ifelse(cooks.distance(mod) < 4 / nrow(.), "keep", "delete")) %>%
  filter(outlier == "keep")

# Much better
df_viz %>%
  ggplot(aes(x = nm_443, y = nm_488)) +
  geom_point()

df <- df %>%
  semi_join(df_viz)

# Plot --------------------------------------------------------------------

plot_absorption_spectra <- function(df) {

  p <- df %>%
    ggplot(aes(
      x = wavelength,
      y = absorption,
      color = type,
      group = measurement_id
    )) +
    geom_line(size = 0.25) +
    facet_grid(foldername ~ type, scales = "free") +
    paletteer::scale_color_paletteer_d("yarrr::google") +
    labs(y = bquote("Absorption"~(m^{-1})),
         x = "Wavelength (nm)",
         title = "Total, Non-algal and Phytoplankton absorption") +
    theme(
      legend.position = "none",
      legend.title = element_blank(),
      strip.background = element_rect(fill = "#3c3c3c"),
      strip.text = element_text(color = "white", face = "bold"),
      panel.border = element_blank(),
      axis.ticks = element_blank(),
      plot.title = element_text(hjust = 0)
    )

  return(p)
}

df_viz <- df %>%
  pivot_longer(contains("absorption"),
    names_to = "type",
    values_to = "absorption"
  ) %>%
  mutate(type = str_replace_all(type, "_", " ")) %>%
  mutate(type = str_to_title(type)) %>%
  mutate(type = factor(
    type,
    levels = c(
      "Total Absorption",
      "Non Algal Absorption",
      "Phytoplankton Absorption"
    )
  ))

df_viz <- df_viz %>%
  group_nest(foldername, keep = TRUE) %>%
  # slice(86) %>%
  mutate(p = map(data, plot_absorption_spectra))

pdf(here::here("graphs/02_absorption_spectra.pdf"), width = 10, height = 4)

walk(df_viz$p, print)

dev.off()

# Export the clean data ---------------------------------------------------

df %>%
  count(foldername, measurement_id) %>%
  assertr::verify(n == 401)

df %>%
  group_by(foldername, measurement_id) %>%
  group_walk( ~ data.table::fwrite(
    .x,
    file = glue::glue(
      "data/clean/absorption/{.y$foldername}_{.y$measurement_id}.csv"
    )
  ),
  .keep = TRUE)

