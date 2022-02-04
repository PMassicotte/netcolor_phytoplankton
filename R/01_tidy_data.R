# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Tidy HPLC and absorption data.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source(here("R","read_hplc.R"))
source(here("R","read_metadata.R"))
source(here("R","read_absorption.R"))

# List the folder in which all the data is --------------------------------

folders <- fs::dir_ls(here("data", "raw", "HPLC_Absorption_Final"),
  type = "directory"
)

folders <- folders %>%
  enframe(name = NULL, value = "folder")

# Find which sheet should be used in each excel file ----------------------

hplc_files <- fs::dir_ls(
  here("data", "raw", "HPLC_Absorption_Final"),
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
  ungroup()

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
  mutate(absorption = map(
    absorption,
    ~ distinct(., sample_id, date, longitude, latitude)
  )) %>%
  unnest(everything()) %>%
  relocate(sample_id, .before = 1) %>%
  relocate(c(date, cruise_name, cruise_number, longitude, latitude),
    .after = sample_id
  ) %>%
  mutate(
    month = lubridate::month(date, label = TRUE),
    .after = date
  ) %>%
  mutate(
    season = fct_collapse(
      .f = month,
      Spring = c("Mar", "Apr", "May"),
      Summer = c("Jun", "Jul", "Aug"),
      Autumn = c("Sep", "Oct", "Nov"),
      Winter = c("Dec", "Jan", "Feb")
    ),
    .after = month
  )

metadata

# Remove duplicates

metadata <- metadata %>%
  distinct(sample_id, .keep_all = TRUE)

# Remove measurements without geographic coordinate or date

metadata <- metadata %>%
  filter(!if_any(c(date, longitude, latitude), is.na))

# HPLC --------------------------------------------------------------------

hplc <- df %>%
  select(hplc) %>%
  unnest(everything())

# Some HPLC `sample_id` were not numeric (ex.: FL002), I removed them from the
# data.

hplc %>%
  filter(!str_starts(sample_id, "\\d"))

hplc <- hplc %>%
  filter(str_starts(sample_id, "\\d")) %>%
  mutate(sample_id = parse_integer(sample_id)) %>%
  relocate(contains("chl"), .after = depth) %>%
  distinct() # There are duplicated data...

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

# but19 should be the sum of but19 + butlike

hplc %>%
  select(contains("but"))

hplc <- hplc %>%
  rowwise() %>%
  mutate(but19 = sum(c_across(contains("but")), na.rm = TRUE)) %>%
  select(-butlike)

# hex19 should be the sum of hex19 + hexlike + hexlike2

hplc %>%
  select(contains("hex"))

hplc <- hplc %>%
  rowwise() %>%
  mutate(hex19 = sum(c_across(contains("hex")), na.rm = TRUE)) %>%
  select(-hexlike2, -hexlike)

# Only keep sample with metadata

hplc <- hplc %>%
  semi_join(metadata, by = "sample_id")

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
  distinct()

# Should have 400 wavelengths per sample_id

absorption <- absorption %>%
  add_count(sample_id) %>%
  filter(n == 400) %>%
  select(-n)

# Remove spectra with any value <= 0 between 350 and 400 nm

absorption <- absorption %>%
  group_by(sample_id) %>%
  filter(!any((ap <= 0 | aphy <= 0 | anap <= 0) & between(wavelength, 350, 400))) %>%
  ungroup()

absorption %>%
  count(sample_id, sort = TRUE) %>%
  assertr::verify(n == 400)

# Remove spectra where aphy(440) < aphy(410)

aphy_pigment_extraction_problem <- absorption %>%
  filter(wavelength %in% c(410, 440)) %>%
  select(sample_id, wavelength, aphy) %>%
  pivot_wider(
    names_from = wavelength,
    values_from = aphy,
    names_prefix = "wl"
  ) %>%
  filter(wl440 < wl410)

absorption <- absorption %>%
  anti_join(aphy_pigment_extraction_problem, by = "sample_id")

absorption

# Using 2 times SD

outlier <- absorption %>%
  filter(wavelength == 443) %>%
  mutate(across(
    c(anap, aphy),
    .fns = list(mean = mean, sd = sd),
    na.rm = TRUE
  ))

absorption %>%
  filter(wavelength == 443) %>%
  ggplot(aes(x = anap)) +
  geom_histogram() +
  geom_vline(xintercept = outlier$anap_mean, color = "red") +
  geom_vline(
    xintercept = outlier$anap_mean + 2 * outlier$anap_sd,
    color = "red"
  ) +
  geom_vline(
    xintercept = outlier$anap_mean - 2 * outlier$anap_sd,
    color = "red"
  )

# Use 2 times standard deviation

outlier <- outlier %>%
  filter(between(anap, anap_mean - 2 * anap_sd, anap_mean + 2 * anap_sd)) %>%
  filter(between(aphy, aphy_mean - 2 * aphy_sd, aphy_mean + 2 * aphy_sd))

# After discussion with Emmanuel, it was decided that we will not remove
# observations that are 2*SD off the mean.

# absorption <- absorption %>%
#   semi_join(outlier, by = "sample_id")

absorption %>%
  filter(wavelength == 443) %>%
  ggplot(aes(x = anap)) +
  geom_histogram()

absorption %>%
  filter(wavelength == 443) %>%
  ggplot(aes(x = aphy)) +
  geom_histogram()

# Remove spectra with a lot of 0. This is happening when the detection limit is
# reached. Set all values of a spectra to NAs (either ap, aphy or anap) has more
# than 50 values <= 0 between 600 and 700.

# absorption %>%
#   filter(sample_id == 476288) %>%
#   ggplot(aes(x = wavelength)) +
#   geom_line(aes(y = anap, color = "anap")) +
#   geom_line(aes(y = aphy, color = "aphy"))
#
# absorption %>%
#   filter(sample_id == 236035) %>%
#   ggplot(aes(x = wavelength)) +
#   geom_line(aes(y = anap, color = "anap")) +
#   geom_line(aes(y = aphy, color = "aphy"))
#
# absorption <- absorption %>%
#   group_by(sample_id) %>%
#   mutate(anap_detection = sum(anap[between(wavelength, 600, 700)] <= 0)) %>%
#   ungroup() %>%
#   mutate(anap = ifelse(anap_detection >= 50, NA_real_, anap)) %>%
#   select(-anap_detection)
#
# # 271 spectra were completely set to NA
# absorption %>%
#   group_by(sample_id) %>%
#   filter(if_any(c(ap, aphy, anap), ~ all(is.na(.))))


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

absorption <- absorption %>%
  semi_join(metadata, by = "sample_id")

# Export ------------------------------------------------------------------

write_csv(metadata, here("data", "clean", "metadata.csv"))
write_csv(hplc, here("data", "clean", "hplc.csv"))
fwrite(absorption, here("data", "clean", "absorption.csv"))
