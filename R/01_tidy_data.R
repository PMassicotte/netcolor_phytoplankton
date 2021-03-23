# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Tidy HPLC and absorption data.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source("R/read_hplc.R")
source("R/read_metadata.R")
source("R/read_absorption.R")

# List the folder in which all the data is --------------------------------

folders <- fs::dir_ls(
  here("data/raw/HPLC_Absorption_Final/"),
  type = "directory"
)

folders <- folders %>%
  enframe(name = NULL, value = "folder")

# Find which sheet should be used in each excel file ----------------------

hplc_files <- fs::dir_ls(here("data/raw/HPLC_Absorption_Final/"),
  recurse = TRUE,
  type = "file",
  regexp = "hplc.*\\.xls",
  ignore.case = TRUE
)

df <- hplc_files %>%
  enframe(name = NULL, value = "hplc_file") %>%
  mutate(hplc_sheet = map(hplc_file, readxl::excel_sheets)) %>%
  unnest(hplc_sheet)

df %>%
  distinct(hplc_sheet) %>%
  pull()

df <- df %>%
  filter(str_detect(hplc_sheet, regex("pigment|jbedits", ignore_case = TRUE))) %>%
  add_count(hplc_file) %>%
  group_by(hplc_file) %>%
  filter(n == 1 | (n == 2 & str_detect(hplc_sheet, regex("jbedits", ignore_case = TRUE)))) %>%
  ungroup() %>%
  select(-n) %>%
  add_count() %>%
  assertr::verify(n == 61) %>%
  select(-n)

df

df <- df %>%
  mutate(folder = dirname(hplc_file), .before = hplc_file) %>%
  inner_join(folders, by = "folder")

df

# Add absorption files ----------------------------------------------------

df <- df %>%
  mutate(anap_file = map_chr(
    folder,
    ~ fs::dir_ls(
      .,
      type = "file",
      regexp = "absorption_detritus",
      ignore.case = TRUE
    )
  )) %>%
  mutate(ap_file = map_chr(
    folder,
    ~ fs::dir_ls(
      .,
      type = "file",
      regexp = "absorption_particulate",
      ignore.case = TRUE
    )
  )) %>%
  mutate(aphy_file = map_chr(
    folder,
    ~ fs::dir_ls(
      .,
      type = "file",
      regexp = "absorption_phytoplankton",
      ignore.case = TRUE
    )
  ))

# Read the data -----------------------------------------------------------

df <- df %>%
  mutate(metadata = map(anap_file, read_metadata)) %>%
  mutate(hplc = map2(hplc_files, hplc_sheet, read_hplc)) %>%
  mutate(anap = map(anap_file, read_absorption, absorption_type = "anap")) %>%
  mutate(ap = map(ap_file, read_absorption, absorption_type = "ap")) %>%
  mutate(aphy = map(aphy_file, read_absorption, absorption_type = "aphy")) %>%
  mutate(absorption = pmap(list(anap, ap), inner_join)) %>%
  mutate(absorption = pmap(list(absorption, aphy), inner_join)) %>%
  select(-anap, -ap, -aphy)

df

# Metadata ----------------------------------------------------------------

metadata <- df %>%
  select(metadata, absorption) %>%
  mutate(
    absorption = map(
      absorption,
      ~ distinct(., sample_id, date, longitude, latitude)
    )
  ) %>%
  unnest(everything()) %>%
  relocate(sample_id, .before = 1) %>%
  relocate(c(date, cruise_name, cruise_number, longitude, latitude),
    .after = sample_id
  )

# Remove non unique sample_id

metadata <- metadata %>%
  add_count(sample_id, sort = TRUE) %>%
  filter(n == 1) %>%
  select(-n)

# HPLC --------------------------------------------------------------------

hplc <- df %>%
  select(hplc, hplc_file) %>%
  unnest(everything()) %>%
  rename(sample_id = id)

# Some HPLC `sample_id` were not numeric (ex.: FL002), I removed them from the
# data.

hplc %>%
  filter(!str_starts(sample_id, "\\d"))

hplc <- hplc %>%
  filter(str_starts(sample_id, "\\d")) %>%
  mutate(sample_id = parse_integer(sample_id)) %>%
  relocate(contains("chl"), .after = depth)

hplc %>%
  add_count(sample_id, depth) %>%
  assertr::verify(n == 1)

# There are two hplc chla variables (hplchla and hplcchla). I will merge both
# into a single column.

hplc %>%
  select(starts_with("hplc"))

hplc <- hplc %>%
  rowwise() %>%
  mutate(hplchla = sum(hplchla, hplcchla, na.rm = TRUE)) %>%
  select(-hplcchla) %>%
  rename(hplcchla = hplchla)

# Absorption --------------------------------------------------------------

absorption <- df %>%
  select(absorption) %>%
  unnest(everything()) %>%
  select(-longitude, -latitude) %>%
  relocate(sample_id,
    date,
    depth,
    pressure,
    wavelength,
    ap,
    aphy,
    anap,
    .before = 1
  )

# Remove duplicated sample_id

absorption <- absorption %>%
  dtplyr::lazy_dt() %>%
  add_count(sample_id, wavelength) %>%
  filter(n == 1) %>%
  as_tibble() %>%
  select(-n)

# Should have 400 wavelengths per sample_id

absorption %>%
  count(sample_id, sort = TRUE) %>%
  assertr::verify(n == 400)

# Calculate specific phyto absorption -------------------------------------

absorption
hplc

absorption <- hplc %>%
  select(sample_id, hplcchla) %>%
  left_join(absorption, ., by = "sample_id") %>%
  mutate(aphy_specific = aphy / hplcchla, .after = aphy) %>%
  select(-hplcchla)

absorption

# Should have 400 wavelengths per sample_id

absorption %>%
  count(sample_id, sort = TRUE) %>%
  assertr::verify(n == 400)

# Export ------------------------------------------------------------------

write_csv(metadata, here("data/clean/metadata.csv"))

write_csv(hplc, here("data/clean/hplc.csv"))

data.table::fwrite(absorption, here("data/clean/absorption.csv"))
