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
library(ggcorrplot)
library(here)

# library(furrr)
# plan(multiprocess(workers = availableCores() - 1))

# Set default ggplot2 font size and font family
# devtools::install_github("PMassicotte/ggpmthemes")
theme_set(theme_poppins(base_size = 10))
theme_update(
  panel.border = element_blank(),
  axis.ticks = element_blank(),
  strip.background = element_rect(fill = "#3c3c3c"),
  strip.text = element_text(color = "white", face = "bold")
)

# Cleaning scripts --------------------------------------------------------

source("R/01_tidy_data.R")
source("R/02_calculate_slope_non_algal_absorption.R")
source("R/03_extract_bathymetry.R")
source("R/04_classify_observations_into_bioregions.R")
source("R/05_merge_all_data_into_single_csv.R")

# Visualization -----------------------------------------------------------

source("R/06_visualize_metadata.R")
source("R/07_explore_absorption.R")
source("R/08_seasonal_cycles.R")


# source("R/06_map_sampling_locations.R")
# source("R/07_visualize_metadata.R")
# source("R/08_calculate_slope_non_algal_absorption.R")
# source("R/09_exploration.R")
# source("R/10_explore_hplc_pigments.R")
# source("R/11_exploration_absorption_vs_hplc_pigments.R")
# source("R/12_timeseries_hplc_pigments.R")
# source("R/13_devred_2016_jgr.R")
# source("R/14_visualize_phytoplankton_specific_absorption.R")
# source("R/15_specific_absorption_phyto_bricaud_1995.R")


