read_metadata <- function(file) {

  # file <- "data/raw/HPLC_Absorption_Final/AMU2019-001 Lab sea/AMU2019001_Absorption_Detritus.csv"

  df <- read_lines(file, skip = 2, n_max = 10) %>%
    str_squish() %>%
    enframe(name = NULL, value = "tmp") %>%
    separate(tmp, c("name", "value"), sep = "=") %>%
    mutate(across(everything(), str_squish)) %>%
    mutate(across(everything(), ~str_remove_all(., "'"))) %>%
    pivot_wider(names_from = name, values_from = value) %>%
    janitor::clean_names() %>%
    mutate(across(contains("date"), anytime::anytime))

  return(df)
}
