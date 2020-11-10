rm(list = ls())

non_algal_absorption_slope <- read_csv("data/clean/non_algal_absorption_slope.csv")
metadata <- read_csv("data/clean/metadata.csv")

hplc_pigments <-
  c(
    "hplc_chlb",
    "hplc_chlc3",
    "hplc_fucox",
    "hplc_perid",
    "hplc_zea",
    "hplc_allox",
    "hplc_but19_total",
    "hplc_hex19_total",
    "hplc_phaeo",
    "hplc_prasinox"
  )

metadata <- metadata %>%
  select(measurement_id, salinity:silicate, date, longitude, latitude, all_of(hplc_pigments))

df <- non_algal_absorption_slope %>%
  inner_join(metadata, by = "measurement_id") %>%
  mutate(yday = lubridate::yday(date))

df

df %>%
  count(yday, sort = TRUE)

# Timeseries of the non-algal absorption slope ----------------------------

df_viz <- df %>%
  group_by(yday) %>%
  summarise(
    mean_non_algal_absorption_slope = mean(non_algal_absorption_slope),
    sd_non_algal_absorption_slope = sd(non_algal_absorption_slope),
    n = n()
  ) %>%
  mutate(date = as.Date(paste0("2014-", yday), "%Y-%j"))

df_viz

p <- df_viz %>%
  ggplot(aes(x = date, y = mean_non_algal_absorption_slope)) +
  geom_point(color = "gray50") +
  geom_line(aes(y = rollmean(
    mean_non_algal_absorption_slope,
    k = 28,
    na.pad = TRUE
  )), color = "red") +
  labs(
    y = bquote("Non-algal absorption slope"~(nm^{-1})),
    x = NULL,
    title = str_wrap("Average of non-algal absorption slope for each day of the year", 45),
    subtitle = str_wrap("The red line is a 28 days moving average. The slopes were calculated between 350-750 nm.", 80)
  ) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  scale_y_continuous(breaks = scales::breaks_pretty(n = 10)) +
  theme(
    panel.border = element_blank(),
    axis.ticks = element_blank()
  )

ggsave(
  here::here("graphs/07_non_algal_absorption_slope_vs_yday.pdf"),
  device = cairo_pdf,
  width = 7,
  height = 6
)

# Multivariate analysis ---------------------------------------------------

#TODO: learnr::run_tutorial("pca_recipes", package = "learntidymodels")

library(factoextra)
# http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/118-principal-component-analysis-in-r-prcomp-vs-princomp/

df

skim_df <- df %>%
  # select(starts_with("hplc")) %>%
  skimr::skim()

p <- skim_df %>%
  mutate(skim_variable = fct_reorder(skim_variable, complete_rate)) %>%
  ggplot(aes(x = complete_rate, y = skim_variable)) +
  geom_col() +
  scale_x_continuous(labels = scales::label_percent()) +
  labs(
    x = "Complete rate",
    y = "Variables",
    title = "Overview of the available variables for statistical analysis"
  )

ggsave(
  here::here("graphs/07_variable_complete_rate.pdf"),
  device = cairo_pdf,
  height = 10,
  width = 10
)

var <- skim_df %>%
  filter(complete_rate >= 0.75) %>%
  pull(skim_variable)

df_viz <- df %>%
  select(any_of(var))

rda1 <- df_viz %>%
  select(non_algal_absorption_slope, salinity:silicate, starts_with("hplc")) %>%
  drop_na() %>%
  prcomp(scale. = TRUE, center = TRUE)

summary(rda1)

biplot(rda1)
fviz_eig(rda1)
fviz_pca_ind(
  rda1,
  col.ind = "cos2",
  # Color by the quality of representation
  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
  repel = FALSE
)


fviz_pca_biplot(rda1,
  repel = FALSE,
  col.var = "red",
  # Variables color
  col.ind = "gray75"
) # Individuals color)

fviz_pca_var(
  rda1,
  col.var = "contrib",
  # Color by contributions to the PC
  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
  repel = T # Avoid text overlapping
)

ggsave(
  "~/Desktop/test_pca.pdf",
  device = cairo_pdf,
  width = 8,
  height = 8
)
