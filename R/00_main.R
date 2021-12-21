# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# FILE:         main.R
#
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Project aiming to look at a large database of absorption
# measurements (total, phyto and anap).
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

# Setup -------------------------------------------------------------------

library(tidyverse)
library(ggpmthemes)
library(sf)
library(zoo)
library(vegan)
library(broom)
library(glue)
library(patchwork)
library(data.table)
library(stars)
library(MBA)
library(ggtext)
library(ggfortify) # Fortify ACF and CCF objects
library(corrr)
library(here)
library(GGally)
library(tidymodels)
library(themis)
library(ggtext)
library(terra)
library(ggspatial)
library(ggridges)

# library(furrr)
# plan(multiprocess(workers = availableCores() - 1))

# Set default ggplot2 font size and font family
# devtools::install_github("PMassicotte/ggpmthemes")
theme_set(theme_light_modified(base_family = "Montserrat", base_size = 10))

theme_update(
  strip.background = element_blank(),
  strip.text = element_text(face = "bold", size = 14),
  panel.border = element_blank(),
  axis.ticks = element_blank()
)

doParallel::registerDoParallel(cores = parallel::detectCores() - 1)

# Cleaning scripts --------------------------------------------------------

source(here("R","01_tidy_data.R"))
source(here("R","02_calculate_slope_non_algal_absorption.R"))
source(here("R","03_extract_bathymetry.R"))
source(here("R","04_classify_observations_into_bioregions.R"))
source(here("R","05_merge_all_data_into_single_csv.R"))

# Visualization -----------------------------------------------------------

source(here("R","06_visualize_metadata.R"))
source(here("R","07_explore_absorption.R"))
source(here("R","08_seasonal_cycles.R"))

# source(here("R","09_temporal_cycles.R"))

source(here("R","10_explore_hplc_pigments.R"))
source(here("R","11_devred_2016_jgr.R"))
source(here("R","12_specific_absorption_phyto_bricaud_1995.R"))
source(here("R","13_fucoxanthin_bining.R"))
source(here("R","14_apparent_visible_wavelength.R"))
source(here("R","14a_aphy_vs_aphy_specific_on_apparent_visible_wavelength.R"))
source(here("R","15_calculate_station_distance_to_shore.R"))
source(here("R","16_simulate_daily_data_from_loess.R"))

# Figures for the manuscript ----------------------------------------------

source(here("R","fig01.R"))
source(here("R","fig02.R"))
source(here("R","fig03.R"))
source(here("R","fig04.R"))
source(here("R","fig05.R"))
source(here("R","fig06.R"))
source(here("R","fig07.R"))
source(here("R","fig08.R"))
source(here("R","fig09.R"))
source(here("R","fig10.R"))
source(here("R","appendix01.R"))
source(here("R","appendix02.R"))
source(here("R","appendix03.R"))
source(here("R","appendix04.R"))
source(here("R","appendix05.R"))
