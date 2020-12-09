# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Match absorption data with metadata.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())


# Metadata ----------------------------------------------------------------

metadata <-
  map_df(
    c(
      "data/raw/batch1/ID_HPLC_data.csv",
      "data/raw/batch2/Historical_data_newSampleIDs_created_20201110_FIXED.csv"
    ),
    read_csv
  ) %>%
  janitor::clean_names() %>%
  rename(measurement_id = sample_id)

# There are duplicated measurement_id in the metadata. I will remove them.

metadata %>%
  janitor::get_dupes(measurement_id) %>%
  write_csv(here::here("data/clean/duplicated_metadata_entries.csv"))

metadata <- metadata %>%
  group_by(measurement_id) %>%
  filter(n() == 1) %>%
  ungroup()

metadata <- metadata %>%
  mutate(date = lubridate::make_date(year, month, day), .after = station)

metadata

# Complete and filter hplc data -------------------------------------------

# If hplc_chla is measured, then all NA pigments should be set to 0 (and not NA)
metadata <- metadata %>%
  rowwise() %>%
  mutate(across(
    c(starts_with("hplc"), -hplc_chla),
    ~ ifelse(!is.na(hplc_chla) &
      is.na(.x), 0, .x)
  ))

# Count how many hplc pigments are present and keep only observations with at
# least 6 measured hplc.
metadata <- metadata %>%
  mutate(n_pigment = sum(c_across(starts_with("hplc")) > 0, na.rm = TRUE)) %>%
  ungroup()

p <- metadata %>%
  ggplot(aes(x = n_pigment)) +
  geom_histogram(binwidth = 1) +
  labs(
    title = "Number of HPLC pigments per station",
    subtitle = "The red line shows the threshold of 6 pigments to filter out the data.",
    x = "Number of HPLC pigments",
    y = "Count"
  ) +
  geom_vline(xintercept = 6, lty = 2, color = "red") +
  theme(
    panel.border = element_blank(),
    axis.ticks = element_blank()
  )

ggsave(
  here::here("graphs/04_histogram_number_hplc_pigments_per_observation.pdf"),
  device = cairo_pdf,
  width = 7.44,
  height = 4.31
)

# Filter to keep observations with at least 6 hplc pigments

metadata <- metadata %>%
  filter(n_pigment >= 6) %>%
  select(-n_pigment)

# Regroup some hplc pigments ----------------------------------------------

metadata <- metadata %>%
  # select(matches("but|hex")) %>%
  rowwise() %>%
  mutate(hplc_but19_total = sum(c_across(contains("but")), na.rm = TRUE)) %>%
  mutate(hplc_hex19_total = sum(c_across(contains("hex")), na.rm = TRUE)) %>%
  ungroup() %>%
  select(-hplc_but19, -hplc_hex19, -hplc_butlike, -hplc_hexlike, -hplc_hexlike2)

metadata

# Merge with absorption data ----------------------------------------------

files <- fs::dir_ls("data/clean/absorption/")

absorption <- map_df(files,
  data.table::fread,
  colClasses = c(measurement_id = "character")
) %>%
  as_tibble()

absorption <- absorption %>%
  # distinct(measurement_id) %>%
  mutate(measurement_id = parse_number(measurement_id))

metadata %>%
  anti_join(absorption)

absorption %>%
  anti_join(metadata)

absorption <- absorption %>%
  inner_join(metadata)

absorption %>%
  data.table::fwrite(here::here("data/clean/absorption_with_metadata.csv"))

# metadata <- metadata %>%
#   semi_join(absorption)

metadata %>%
  write_csv("data/clean/metadata.csv")

# Plot the spectra for which there are metadata ---------------------------

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

df_viz <- absorption %>%
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

pdf(
  here::here("graphs/04_absorption_spectra_that_have_matching_metadata.pdf"),
  width = 10,
  height = 4
)

walk(df_viz$p, print)

dev.off()
