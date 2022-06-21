# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION: Je souhaiterai mettre le tableau 2 a jours, j’ai remplie une
# bonne partie avec les information dans les figures 10 a-c mais j’aurais besoin
# de plus d’information, vois le tableau ci-dessous.
#
# J’aurais besoin des pentes/p-value et R2 pour :
#
# - le jeu de donnee entier pour aphy, anap, snap et anap/aphy
#
# - chaque region individuelle, SS, NBA et LS pour aphy, anap, snap et
# anap/aphy
#
# - chaque region/saison pour anap/aphy"
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source(here("R", "zzz.R"))

df <- open_dataset(here("data", "clean", "merged_dataset")) |>
  filter(wavelength == 443) |>
  select(
    sample_id,
    bioregion_name,
    wavelength,
    season,
    date,
    aphy,
    anap,
    snap
  ) |>
  collect()

df

# Let's calculate the average absorption/snap for each bioregion for each year
# regardless of the the season.

df_viz <- df |>
  mutate(year = lubridate::year(date)) |>
  group_by(year, bioregion_name) |>
  summarise(across(
    c(aphy:snap),
    .fns = list(mean = mean, sd = sd),
    na.rm = TRUE
  ), n = n()) |>
  ungroup()

df_viz

plot_fun <- function(df, mean_var, sd_var) {
  df %>%
    ggplot(aes(x = year, y = {{ mean_var }})) +
    geom_point() +
    geom_pointrange(aes(
      ymin = {{ mean_var }} - {{ sd_var }},
      ymax = {{ mean_var }} + {{ sd_var }}
    )) +
    geom_smooth(
      aes(weight = n),
      method = "lm",
      formula = y ~ x,
      color = "#3c3c3c",
      size = 0.5,
      alpha = 0.2
    ) +
    ggpmisc::stat_poly_eq(
      aes(weight = n, label = ..eq.label..),
      formula = y ~ x,
      label.x.npc = 0.1,
      label.y.npc = 0.12,
      coef.digits = 4,
      parse = TRUE,
      family = "Montserrat",
      size = 2.5,
    ) +
    ggpmisc::stat_poly_eq(
      aes(
        weight = n,
        label = paste(..rr.label.., after_stat(p.value.label), sep = "*\", \"*")
      ),
      formula = y ~ x,
      label.x.npc = 0.1,
      label.y.npc = 0.05,
      coef.digits = 4,
      parse = TRUE,
      family = "Montserrat",
      size = 2.5,
      small.p = TRUE
    ) +
    labs(x = NULL) +
    facet_grid(~bioregion_name) +
    theme(
      legend.position = "none",
      panel.spacing = unit(1, "lines", data = NULL),
      strip.text = element_text(size = 10)
    )
}

p1 <- plot_fun(df_viz, aphy_mean, aphy_sd)
p2 <- plot_fun(df_viz, anap_mean, anap_sd)
p3 <- plot_fun(df_viz, snap_mean, snap_sd)
p4 <- plot_fun(df_viz, anap_mean / aphy_mean, anap_sd / aphy_sd)

p1 + p2 + p3 + p4
