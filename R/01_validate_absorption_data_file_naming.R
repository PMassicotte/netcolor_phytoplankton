# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Explore the absorption files to identify problematic
# measurements.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

files <- fs::dir_ls(here::here("data/raw/"), glob = "*.ASC", recurse = TRUE,)
files <- files[!str_detect(files, regex("copy|test|blank|blk|\\_b", ignore_case = TRUE))]

df <- files %>%
  enframe(name = NULL, value = "filepath")

df

df <- df %>%
  mutate(filename = fs::path_file(filepath), .after = filepath) %>%
  mutate(foldername = basename(dirname(filepath)), .after = filename)

df

# We do not want to use the files in the root DATA folder
df <- df %>%
  filter(foldername != "DATA")

# Extract the measurement id ----------------------------------------------

df <- df %>%
  mutate(
    measurement_id = fs::path_ext_remove(filename),
    .after = filename
  )

df

df <- df %>%
  mutate(measurement_id = str_remove(measurement_id, "[:alpha:]$"))

# Keep only files that finish with a number or with P ---------------------

df <- df %>%
  filter(str_detect(filename, regex("\\d\\.ASC$|\\dP\\.ASC$", ignore_case = TRUE)))

# Categorize the type of measurements -------------------------------------

df <- df %>%
  mutate(
    type = case_when(
      str_detect(filename, regex("\\d\\.ASC$", ignore_case = TRUE)) ~ "Total absorption",
      str_detect(filename, regex("\\dP\\.ASC$", ignore_case = TRUE)) ~ "Non-algal absorption",
      TRUE ~ NA_character_
    )
  )

df %>%
  count(type)

df %>%
  filter(is.na(type))

# Check that all measurement_id have 2 measurements -----------------------

# Should have 2 files per measurement, 1 for total and 1 for particulate
# absorption.

problematic_files <- df %>%
  filter(!str_detect(measurement_id, regex("blank|blk", ignore_case = TRUE))) %>%
  group_nest(foldername, measurement_id) %>%
  mutate(n_files = map_int(data, nrow)) %>%
  filter(n_files != 2) %>%
  arrange(desc(n_files))

problematic_files

# problematic_files %>%
#   select(-data) %>%
#   write_csv("~/Desktop/problematic_files.csv")

# Export a clean list of exploitable files --------------------------------

df %>%
  anti_join(problematic_files) %>%
  write_csv(here::here("data/clean/list_of_valid_absorption_measurements.csv"))

df %>%
  anti_join(problematic_files) %>%
  distinct(measurement_id, foldername) %>%
  write_csv(here::here("data/clean/list_of_valid_absorption_measurements_metadata.csv"))

# Good! We have the same number of particulate and total absorption spectra
df %>%
  anti_join(problematic_files) %>%
  count(type) %>%
  assertr::verify(length(unique(n)) == 1)
