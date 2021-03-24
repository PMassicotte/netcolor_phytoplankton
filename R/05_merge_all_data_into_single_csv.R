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

fwrite(df, here("data/clean/merged_dataset.csv"))
