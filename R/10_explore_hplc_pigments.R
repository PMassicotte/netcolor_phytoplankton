# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Explore HPLC data.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

hplc <- fread(here("data/clean/merged_dataset.csv")) %>%
  as_tibble() %>%
  filter(wavelength == 443) %>%
  filter(depth <= 2)

hplc

# Monthly average
df_viz <- hplc %>%
  select(bioregion_name, date, fucox, hex19, but19, perid, hplcphae) %>%
  mutate(month = lubridate::month(date, label = TRUE)) %>%
  pivot_longer(fucox:hplcphae,
    names_to = "pigment",
    values_to = "concentration"
  )

df_viz <- df_viz %>%
  group_by(bioregion_name, month, pigment) %>%
  summarise(average_concentration = mean(concentration, na.rm = TRUE)) %>%
  group_by(bioregion_name, month) %>%
  mutate(total_average_concentration = sum(average_concentration, na.rm = TRUE)) %>%
  mutate(relative_average_concentration = average_concentration / total_average_concentration)

# Plot --------------------------------------------------------------------

p <- df_viz %>%
  mutate(pigment = fct_reorder(pigment, relative_average_concentration)) %>%
  ggplot(aes(x = month, y = relative_average_concentration)) +
  geom_col(aes(fill = pigment)) +
  paletteer::scale_fill_paletteer_d(
    "ggsci::default_nejm",
    guide = guide_legend(
      ncol = 1,
      keyheight = unit(0.5, "cm"),
      label.theme = element_text(size = 8)
    )
  ) +
  scale_y_continuous(labels = scales::label_percent()) +
  labs(
    x = NULL,
    y = "Average contribution"
  ) +
  facet_wrap(~bioregion_name, scales = "free_y", ncol = 1) +
  theme(
    legend.direction = "horizontal",
    legend.title = element_blank()
  )

ggsave(
  here("graphs/10_hplc_month_bioregion.pdf"),
  device = cairo_pdf,
  width = 6,
  height = 7
)
