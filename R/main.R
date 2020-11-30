# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# FILE:         main.R
#
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Project aiming to look at a large database of absorption
# meausrments (total + phyto).
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

source("R/00_convert_absorption_txt_to_asc.R")
source("R/01_validate_absorption_data_file_naming.R")
source("R/02_tidy_absorption_spectra.R")
source("R/03_merge_absorption_metadata.R")

# Visualization -----------------------------------------------------------

source("R/04_map_sampling_locations.R")
source("R/05_visualize_metadata.R")
source("R/06_calculate_slope_non_algal_absorption.R")
source("R/07_exploration.R")
source("R/08_explore_hplc_pigments.R")
source("R/09_exploration_absorption_vs_hplc_pigments.R")
source("R/10_timeseries_hplc_pigments.R")
source("R/11_devred_2016_jgr.R")
source("R/12_visualize_phytoplankton_specific_absorption.R")
source("R/13_temporal_variability_s_nap.R")
source("R/14_specific_absorption_phyto_bricaud_1995.R")
