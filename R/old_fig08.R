# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  PCA using the apparent visible wavelength and HPLC pigments.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

# Pigments ----------------------------------------------------------------

hplc <- read_csv(here("data", "clean", "merged_dataset.csv")) %>%
  filter(wavelength == 443) %>%
  filter(depth <= 2)

# %>%
#   select(
#     sample_id,
#     bioregion_name,
#     date,
#     fucox,
#     hex19,
#     but19,
#     perid,
#     hplcphae
#   )

hplc

# Apparent visible wavelength ---------------------------------------------

avw <- read_csv(here("data/clean/apparent_visible_wavelength.csv"))

# Join --------------------------------------------------------------------

df <- inner_join(avw, hplc, by = c("sample_id", "bioregion_name"))

# Explore correlations ----------------------------------------------------

x <- df %>%
  select(avw, chlb:zea) %>%
  correlate()

df_corrr <- focus(x, avw) %>%
  arrange(desc(avw)) %>%
  top_n(10, wt = avw) %>%
  mutate(term = fct_reorder(term, avw))

df_corrr

# Visualize the top correlated nutrients

df %>%
  select(avw, df_corrr$term) %>%
  # mutate(across(-avw, ~log(. + 1))) %>%
  ggpairs(lower = list(continuous = wrap(
    "points",
    alpha = 0.3, size = 0.1
  ))) +
  labs(title = "Top 10 nutrients correlated to AWD")

ggsave(
  filename = here("graphs","fig08_nutrient_correlation_to_avw.pdf"),
  device = cairo_pdf,
  width = 9,
  height = 9
)

# PCA ---------------------------------------------------------------------

df_pca <- df %>%
  select(avw, df_corrr$term, -hplcchlc) %>%
  # mutate(across(-avw, ~log(. + 1))) %>%
  drop_na()
plot(relaimpo::calc.relimp(slm1))
mod <- lm(avw ~ ., data = df_pca)
summary(mod)
car::vif(mod)

slm1 <- MASS::stepAIC(mod)
summary(slm1)
car::vif(slm1)
GGally::ggcoef(slm1, exclude_intercept = TRUE)

# Looks like this gives the same result as the stepAIC() function
forward.sel(df_pca$avw, df_pca %>% select(-avw))

pca <- df_pca %>%
  select(avw, names(slm1$coefficients)[-1], -hplcchla) %>%
  # summarise(across(everything(), ~sum(. == 0)))
  # filter(!if_any(everything(), ~. == 0)) %>%
  prcomp(center = TRUE, scale. = TRUE)

summary(pca)
plot(pca)
biplot(pca)

p <- autoplot(
  pca,
  loadings = TRUE,
  loadings.label = TRUE,
  loadings.label.repel = TRUE,
  label = FALSE,
  size = 0.25
)

p

ggsave(
  here("graphs/fig08_pca_avw_nutrients.pdf"),
  device = cairo_pdf,
  width = 6,
  height = 6
)

# Explore relation of AWD and Fucox ---------------------------------------

mod <- lm(log10(fucox + 1) ~ avw, data = df)
summary(mod)

p <- df %>%
  ggplot(aes(x = avw, y = fucox)) +
  geom_point(color = "#393E41", size = 0.5) +
  geom_smooth(color = "#bf1d28", size = 0.5) +
  labs(
    x = "Apparent visible wavelength (nm)",
    y = "Fucoxanthin"
  )

ggsave(
  here("graphs/fig08.pdf"),
  device = cairo_pdf,
  width = 7.11,
  height = 5.21
)

