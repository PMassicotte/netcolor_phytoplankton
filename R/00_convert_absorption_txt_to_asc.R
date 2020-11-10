# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Convert absorption spectra provided in .txt format into .asc
# format with no headers. I am converting so all the absorption spectra have the
# same format.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

files <-
  fs::dir_ls(here::here("data/raw/batch2/"),
    recurse = TRUE,
    glob = "*.txt"
  )

size <- fs::file_size(files) %>%
  as.numeric()

files <- files[size > 11000]
files <-
  files[!str_detect(files, regex("copy|test|blank|blk|\\_b", ignore_case = TRUE))]

# Create the output directory where all converted files will be written.
outdir <- here::here("data/raw/batch2/acs/")
if (fs::dir_exists(outdir)) {
  fs::dir_delete(outdir)
}

fs::dir_create(here::here("data/raw/batch2/acs/"))

# Create a list of input and output files
df <- files %>%
  enframe(name = NULL, value = "inputfile") %>%
  mutate(outputfile = fs::path(
    outdir,
    basename(dirname(inputfile)),
    fs::path_ext_set(basename(inputfile), ".ASC")
  ))

# Read absorption spectra
df <- df %>%
  mutate(walk(outputfile, ~ fs::dir_create(dirname(..1)))) %>%
  mutate(data = map(
    inputfile,
    data.table::fread,
    skip = 1,
    col.names = c("wavelength", "absorption")
  ))


# Filter to keep only wavelengths at 1 nm increment
df <- df %>%
  mutate(data = map(data, ~ filter(., wavelength %in% 350:750)))

# Write files
walk2(df$outputfile, df$data, ~ data.table::fwrite(..2, ..1, col.names = FALSE))

