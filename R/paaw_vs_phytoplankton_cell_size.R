# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Calculate the PAAW (or AVW) from the normalized absorption
# spectra derived from two classes of phytoplankton cell size. Data from table 3
# in Ciotti, Áurea M., Marlon R. Lewis, and John J. Cullen. “Assessment of the
# Relationships between Dominant Cell Size in Natural Phytoplankton Communities
# and the Spectral Shape of the Absorption Coefficient.” Limnology and
# Oceanography 47, no. 2 (March 2002): 404–17.
# https://doi.org/10.4319/lo.2002.47.2.0404.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

file <- here("data","raw","table3_ciotti2002.pdf")

df <- tabulizer::extract_tables(file, pages = 1, output = "data.frame")[[1]]

setDT(df)

df <- melt(
  df,
  measure = patterns("(^l)", "(^Micro)", "(^Pico)"),
  value.name = c("wavelength", "pico", "micro")
) %>%
  as_tibble() %>%
  select(-variable)

df

df <- df %>%
  pivot_longer(-wavelength,
    names_to = "phyto_cell_size_class",
    values_to = "normalized_absorption",
    values_drop_na = TRUE
  )

df

# Calculate PAAW ----------------------------------------------------------

paaw <- df %>%
  group_by(phyto_cell_size_class) %>%
  summarise(
    paaw = sum(normalized_absorption) / sum(normalized_absorption / wavelength),
    absorption = max(normalized_absorption),
    wavelength = wavelength[which.max(normalized_absorption)]
  )

# Interesting!
paaw

# Plot the averaged spectra along with their calculated PAAW --------------

p <- df %>%
  ggplot(aes(x = wavelength, y = normalized_absorption, color = phyto_cell_size_class)) +
  geom_line() +
  geom_text(
    data = paaw,
    aes(
      x = wavelength,
      y = absorption,
      label = glue("PAAW: {round(paaw)} nm")
    ),
    show.legend = FALSE,
    size = 2,
    hjust = -0.2
  ) +
  scale_color_manual(
    breaks = c("micro", "pico"),
    labels = c(parse(text = "bar(a)[micro]('> 20 um')"), parse(text = "bar(a)[pico]('< 2 um')")),
    values = c("red", "blue")
  ) +
  labs(
    x = "Wavelength (nm)",
    y = "Normalized absorption spectra",
    subtitle = str_wrap(
      "Normalized absorption for the smallest and biggest average microplankton cell sizes.",
      70
    )
  ) +
  theme(
    legend.title = element_blank(),
    legend.justification = c(1, 1),
    legend.position = c(0.95, 0.95)
  )

ggsave(
  here("graphs/appendix04.pdf"),
  device = cairo_pdf,
  width = 6,
  height = 4
)

