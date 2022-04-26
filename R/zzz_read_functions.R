# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Function to read all the raw data (HPLC, absorption and
# metadata).
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

read_metadata <- function(file) {
  df <- read_lines(file, skip = 2, n_max = 10) %>%
    str_squish() %>%
    enframe(name = NULL, value = "tmp") %>%
    separate(tmp, c("name", "value"), sep = "=") %>%
    mutate(across(everything(), str_squish)) %>%
    mutate(across(everything(), ~ str_remove_all(., "'"))) %>%
    pivot_wider(names_from = name, values_from = value) %>%
    janitor::clean_names() %>%
    mutate(across(contains("date"), anytime::anytime))

  return(df)
}

read_hplc <- function(hplc_file, hplc_sheet) {
  n <- readxl::read_excel(hplc_file, hplc_sheet, .name_repair = "minimal") %>%
    janitor::clean_names() %>%
    rowid_to_column() %>%
    filter(if_any(everything(), ~ str_detect(., "DEPTH"))) %>%
    pull(rowid)

  df <- readxl::read_excel(
    path = hplc_file,
    sheet = hplc_sheet,
    skip = n,
    col_types = "text"
  ) %>%
    rename_with(tolower, everything()) %>%
    janitor::remove_empty(which = "rows") %>%
    janitor::clean_names() %>%
    rename_with(~"sample_id", any_of(c("id", "sampleid"))) %>%
    mutate(across(-sample_id, parse_number))


  return(df)
}

read_absorption <- function(file, absorption_type) {
  df <- data.table::fread(file, skip = 17, na.strings = c("", "-999")) %>%
    as_tibble() %>%
    pivot_longer(
      matches("^\\d{3}$"),
      names_to = "wavelength",
      values_to = absorption_type,
      names_transform = list(wavelength = parse_number)
    ) %>%
    janitor::clean_names() %>%
    mutate(date = as.Date(date, "%b %d %Y"))


  return(df)
}
