### Title:    Example Data Processing
### Author:   Kyle M. Lang
### Created:  2023-08-17
### Modified: 2025-09-15

rm(list = ls(all = TRUE))

library(dplyr)

# Define variables pointing to the relevant directories
codeDir <- "code"
inDir   <- "data/raw"
outDir  <- "data/processed"

# Use the here::here() function to resolve relative file paths
source(here::here(codeDir, "99_supportFunctions.R"))

# Read the dataset
diabetes0 <- readRDS(here::here(inDir, "diabetes.rds"))

# Process the data and save the processed dataset
diabetes2 <- slice_sample(diabetes0, n = 200)

saveRDS(diabetes2, here::here(outDir, "diabetes_sampled.rds"))
