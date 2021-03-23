read_absorption <- function(file, absorption_type) {

  # file <- "data/raw/HPLC_Absorption_Final/AMU2019-001 Lab sea/AMU2019001_Absorption_Detritus.csv"

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
