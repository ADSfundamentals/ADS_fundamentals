### Title:    Example Data Processing
### Author:   Kyle M. Lang
### Created:  2023-08-17
### Modified: 2025-09-15

rm(list = ls(all = TRUE))

library(dplyr)

# Define variables pointing to the relevant directories
codeDir <- "code"
dataDir <- "data"

# Use the here::here() function to resolve relative file paths
source(here::here(codeDir, "supportFunctions.R"))

# Read the dataset
diabetes0 <- readRDS(here::here(dataDir, "diabetes.rds"))

# Process the data and save the processed dataset
diabetes2 <- slice_sample(diabetes0, n = 200)

saveRDS(diabetes2, here::here(dataDir, "diabetes_sampled.rds"))
