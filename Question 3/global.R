# ============================================================
# global.R
#
# Purpose:
#   Prepare the shared shiny environment
# ============================================================

# Check whether required packages are installed
required_pkgs <- c("pharmaverseadam", "tidyverse", "ggplot2", "shiny")
missing_pkgs <- setdiff(required_pkgs, rownames(installed.packages()))
if (length(missing_pkgs) > 0) install.packages(missing_pkgs, dependencies = TRUE)

suppressPackageStartupMessages({
  library(pharmaverseadam)
  library(tidyverse)
  library(ggplot2)
  library(shiny)
})

# Load helper functions from the R code directory
r_files <- list.files("functions", pattern = "\\.R$", full.names = TRUE)
invisible(lapply(r_files, source))

# Load the ADAE dataset
adae <- pharmaverseadam::adae

# Define the display order for adverse event severity
sev_levels <- c("MILD", "MODERATE", "SEVERE")

# Pre-compute treatment arm choices for the checkbox input
actarm_choices <- get_actarm_choices(adae)

