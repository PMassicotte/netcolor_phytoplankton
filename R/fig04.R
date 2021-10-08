# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:

# Beaucoup de relation aph(443)-chl ont été etablie avec quelques
# croisieres seulement, ici avec une serie temporelle de 10-20ans, ca serait
# bien de voir si ces relations tiennent toujours la route, je vais commencer a
# remplir le google doc avec ce genre de relations, e.g., Bricaud et al., 1998 &
# 2004 et on pourra ajouter une figure aph440 vs chl (bricaud utilise 440 et non
# 443).
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

# Prepare the data --------------------------------------------------------

absorption <- fread(here::here("data","clean","absorption.csv")) %>%
  as_tibble() %>%
  filter(wavelength == 440) %>%
  select(
    sample_id,
    date,
    wavelength,
    aphy,
    aphy_specific
  ) %>%
  drop_na()

absorption

bioregion <- read_csv(here::here("data","clean","bioregions.csv"))

hplc <- read_csv(here::here("data","clean","hplc.csv")) %>%
  select(sample_id, hplcchla) %>%
  drop_na()

df <- absorption %>%
  inner_join(bioregion, by = "sample_id") %>%
  inner_join(hplc, by = "sample_id")

df

# Add prediction from other models found in the literature ----------------

df <- df %>%
  mutate(bricaud_1998 = 0.0378 * hplcchla^0.627) %>%
  mutate(bricaud_2004 = 0.0654 * hplcchla^0.728) %>%
  mutate(devred_2006 = ((0.0839 - 0.0176) / 1.613) * (1 - exp(-1.613 * hplcchla)) + 0.0176 * hplcchla)

# Plot --------------------------------------------------------------------

p <- df %>%
  filter(hplcchla > 0) %>% # TODO: check the minimum plausible value
  ggplot(aes(x = hplcchla, y = aphy)) +
  geom_point(color = "#3c3c3c", size = 0.75, shape = 16, stroke = 0, alpha = 0.25) +
  geom_line(aes(y = bricaud_1998, color = "Bricaud 1998"), lty = 2) +
  geom_line(aes(y = bricaud_2004, color = "Bricaud 2004"), lty = 2) +
  geom_line(aes(y = devred_2006, color = "Devred 2006"), lty = 2) +
  scale_x_log10() +
  scale_y_log10() +
  annotation_logticks(sides = "bl", size = 0.25) +
  geom_smooth(method = "lm", aes(color = "This study"), se = FALSE) +
  ggpubr::stat_regline_equation(label.y.npc = 1, size = 3) +
  ggpubr::stat_regline_equation(label.y.npc = 0.93, aes(label = ..rr.label..), size = 3) +
  labs(
    x = quote("Chla" ~ (mgC~m^{-3})),
    y = quote(a[phi] ~ (440) ~ (m^{-1}))
  ) +
  theme(
    legend.position = "top",
    legend.title = element_blank()
  ) +
  paletteer::scale_color_paletteer_d(
    "ggsci::default_locuszoom",
    guide = guide_legend(
      label.position = "top",
      override.aes = list(size = 2, lty = 1),
      keywidth = unit(2, "cm")
    )
  )

ggsave(
  here::here("graphs","fig04.pdf"),
  device = cairo_pdf,
  width = 5,
  height = 4
)
