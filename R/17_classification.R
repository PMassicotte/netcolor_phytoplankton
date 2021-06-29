# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Classification of bioregions using pigments and AVW.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

# Pigments ----------------------------------------------------------------

hplc <- vroom::vroom(here("data", "clean", "merged_dataset.csv")) %>%
  filter(wavelength == 443) %>%
  filter(depth <= 2)

hplc

# Apparent visible wavelength ---------------------------------------------

avw <- read_csv(here("data/clean/apparent_visible_wavelength.csv"))

# Join --------------------------------------------------------------------

df <- inner_join(avw, hplc, by = c("sample_id", "bioregion_name"))

# Select model variables --------------------------------------------------

df_mod <- df %>%
  select(
    bioregion_name,
    longitude,
    latitude,
    avw,
    anap,
    ap,
    anap,
    fucox,
    but19,
    hplcphae,
    perid,
    hex19,
    zea,
    snap,
    diadinox
  ) %>%
  mutate(bioregion_name = factor(bioregion_name))

df_mod

# Classes are imbalanced, will deal with that later in the recipe.

df_mod %>%
  count(bioregion_name)

# Classification tree -----------------------------------------------------

set.seed(2021)

df_split <- initial_split(df_mod, strata = bioregion_name)
df_training <- training(df_split)
df_testing <- testing(df_split)

set.seed(2021)

validation_splits <- mc_cv(df_training, strata = bioregion_name)
validation_splits

## Define the recipe ----

tree_rec <- recipe(bioregion_name ~ ., data = df_training) %>%
  update_role(longitude, latitude, new_role = "id") %>%
  step_corr(all_predictors()) %>%
  step_nzv(all_predictors()) %>%
  step_zv(all_predictors()) %>%
  step_smote(bioregion_name)

prep(tree_rec)

prep(tree_rec) %>%
  juice()

# Not balanced
df_training %>%
  count(bioregion_name)

# Now it is balanced!
prep(tree_rec) %>%
  juice() %>%
  count(bioregion_name)

prep(tree_rec) %>%
  juice() %>%
  ggpairs()

## Define the random forest model ----

tree_spec <- rand_forest(trees = 1000) %>%
  set_mode("classification") %>%
  set_engine("ranger")

## Define the workflow ----

tree_wf <- workflow() %>%
  add_recipe(tree_rec) %>%
  add_model(tree_spec)

tree_wf

## Fit all the resamples ----

bioregion_res <- fit_resamples(
  tree_wf,
  resamples = validation_splits,
  control = control_resamples(save_pred = TRUE)
)

bioregion_res

bioregion_res %>%
  conf_mat_resampled()

bioregion_res %>%
  collect_metrics()

bioregion_res %>%
  show_best(metric = "roc_auc")

bioregion_res %>%
  collect_predictions() %>%
  conf_mat(bioregion_name, .pred_class)

bioregion_res %>%
  collect_predictions() %>%
  group_by(id) %>%
  roc_curve(bioregion_name, 2:4) %>%
  autoplot()

## Select the best regression tree ----

final_tree <- finalize_workflow(
  tree_wf,
  bioregion_res %>% select_best(metric = "roc_auc")
  )

final_tree

## Calculate the VIP ----

# Set the seed, because it is calculated by random permutations
set.seed(2021)

tree_spec %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(
    bioregion_name ~ .,
    data = juice(tree_rec %>% prep()) %>% select(-c(longitude, latitude))
  ) %>%
  vip::vip() +
  labs(
    title = "Variable importance of the RF classification",
    subtitle = "Ranks of the variables used in the randomforest model"
  ) +
  theme(
    panel.border = element_blank(),
    axis.ticks = element_blank()
  )

ggsave(
  here("graphs","17_variable_importance_plot_random_forest.pdf"),
  device = cairo_pdf,
  width = 7.95,
  height = 5.18
)

## Fit the best model on the test data ----

final_fit <- last_fit(final_tree, df_split)

# No sign of overfitting when comparing the training and test model

final_fit %>%
  collect_metrics()

bioregion_res %>%
  collect_metrics()

final_fit %>%
  collect_predictions()

final_fit %>%
  collect_predictions() %>%
  conf_mat(bioregion_name, .pred_class) %>%
  autoplot()

# Geographic map showing the prediction accuracy --------------------------

# https://youtu.be/0WCmLYvfHMw?t=2248

bioregion_pred <- bioregion_res %>%
  collect_predictions() %>%
  mutate(correct = bioregion_name == .pred_class) %>%
  left_join(df_mod %>% mutate(.row = row_number()))

bioregion_pred

p <- bioregion_pred %>%
  group_by(bioregion_name, longitude, latitude) %>%
  summarise(mean_correct = mean(as.integer(correct)), n = n()) %>%
  ungroup() %>%
  ggplot(aes(x = longitude, y = latitude, color = mean_correct)) +
  geom_point(alpha = 0.7) +
  scale_color_viridis_c(labels = scales::label_percent()) +
  facet_wrap(~bioregion_name)

ggsave(
  here("graphs","17_map_random_forest_prediction_accuracy.pdf"),
  device = cairo_pdf
)
