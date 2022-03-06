# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Show the aphy/anap ratio across the bioregions.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source(here("R", "zzz.R"))

absorption <- read_csv(here("data", "clean", "merged_dataset.csv")) %>%
  filter(wavelength == 443) %>%
  mutate(bioregion_name = factor(
    bioregion_name,
    levels = c(
      "Scotian Shelf",
      "Northwest Atlantic Basin ocean (NAB)",
      "Labrador"
    )
  )) %>%
  mutate(bioregion_name_wrap = str_wrap_factor(bioregion_name, 20))

# Plot --------------------------------------------------------------------

formula <- y ~ x

p <- absorption %>%
  filter(if_all(c(aphy, anap), ~ . > 0)) %>%
  ggplot(aes(x = aphy, y = anap)) +
  geom_point(aes(color = bioregion_name), size = 0.5) +
  geom_smooth(
    method = "glm",
    se = FALSE,
    method.args = list(family = Gamma(link = "log")),
    color = "#3c3c3c",
    size = 0.5
  ) +
  ggpmisc::stat_fit_tidy(
    method = "glm",
    method.args = list(formula = formula, family = Gamma(link = "log")),
    label.x = "left",
    label.y = "top",
    family = "Montserrat",
    size = 2.5,
    aes(
      label = paste(
        "y~`=`~ italic(e)^{",
        signif(stat(Intercept_estimate), digits = 3),
        "~+~",
        signif(after_stat(x_estimate), digits = 3),
        "~x}",
        sep = ""
      )
    ),
    parse = TRUE
  ) +
  scale_color_manual(
    breaks = area_breaks,
    values = area_colors
  ) +
  labs(
    x = quote(a[phi](443)),
    y = quote(a[NAP](443))
  ) +
  facet_wrap(~bioregion_name_wrap) +
  theme(
    panel.spacing.y = unit(3, "lines"),
    legend.position = "none",
    strip.text = element_text(size = 10)
  )

ggsave(
  here("graphs", "fig03.pdf"),
  device = cairo_pdf,
  width = 180,
  height = 70,
  units = "mm"
)


# GLM ---------------------------------------------------------------------
# easystats::install_suggested()
library(easystats)

df <- absorption %>%
  filter(if_all(c(aphy, anap), ~. > 0)) %>%
  filter(bioregion_name == "Scotian Shelf")

df

df %>%
  glm(anap ~ aphy, data = ., family = "gaussian") %>%
  summary()

df %>%
  glm(log(anap) ~ aphy, data = ., family = "gaussian") %>%
  summary()

mod <-df %>%
  glm(anap ~ aphy, data = ., family = Gamma(link = "log"))

summary(mod)

mod %>%
  augment(type.predict = c("response")) %>%
  ggplot(aes(x = aphy, y = anap)) +
  geom_point() +
  geom_line(aes(y = .fitted))

tidy(mod, exponentiate = TRUE)
report(mod)
