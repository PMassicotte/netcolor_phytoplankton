# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Combine all the data into a single file.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

metadata <- read_csv("data/clean/metadata.csv")
absorption <- read_csv("data/clean/absorption.csv")
bathymetry <- read_csv("data/clean/bathymetry.csv")
bioregions <- read_csv("data/clean/bioregions.csv")
hplc <- read_csv("data/clean/hplc.csv")
snap <- read_csv("data/clean/non_algal_absorption_slope.csv")

df <- metadata %>%
  inner_join(absorption) %>%
  inner_join(bathymetry) %>%
  inner_join(bioregions) %>%
  inner_join(hplc) %>%
  inner_join(snap)

df %>%
  count(sample_id, sort = TRUE) %>%
  assertr::verify(n == 400)

p <- df %>%
  filter(wavelength == 443) %>%
  count(depth, sort = TRUE) %>%
  mutate(depth = fct_rev(factor(depth))) %>%
  drop_na() %>%
  ggplot(aes(x = n, y = depth)) +
  geom_col() +
  labs(
    x = "Number of measurements",
    y = "Depth (m)"
  )

ggsave(
  here("graphs/05_barplot_measurement_depth.pdf"),
  device = cairo_pdf,
  width = 6,
  height = 4
)

# Based on the barplot, I will only keep measurements made between 0-2 meters in
# the water column.

df <- df %>%
  filter(depth <= 2)

df %>%
  distinct(sample_id)

fwrite(df, here("data/clean/merged_dataset.csv"))
