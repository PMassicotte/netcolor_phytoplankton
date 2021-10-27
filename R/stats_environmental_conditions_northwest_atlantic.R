# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Some information on the environmental condition in the Northwest
# Atlantic.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

daylength <- tibble(
  date = seq(as.Date("2016-01-01"), as.Date("2016-12-31"), by = "day"),
  month = lubridate::month(date, label = TRUE),
  daylength = geosphere::daylength(48, date)
) %>%
  mutate(
    season = fct_collapse(
      .f = month,
      Spring = c("Mar", "Apr", "May"),
      Summer = c("Jun", "Jul", "Aug"),
      Autumn = c("Sep", "Oct", "Nov"),
      Winter = c("Dec", "Jan", "Feb")
    ),
    .after = month
  )

daylength

# Average by season
daylength %>%
  group_by(season) %>%
  summarise(mean_daylength = mean(daylength))
