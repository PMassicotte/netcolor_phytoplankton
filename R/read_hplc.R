read_hplc <- function(hplc_file, hplc_sheet) {
  n <- readxl::read_excel(hplc_file, hplc_sheet) %>%
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
