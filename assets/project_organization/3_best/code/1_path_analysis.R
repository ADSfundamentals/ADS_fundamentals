### Title:    Example Path Analysis
### Author:   Kyle M. Lang
### Created:  2023-08-17
### Modified: 2025-09-15

rm(list = ls(all = TRUE))

library(dplyr)
library(lavaan)
library(tidySEM)

dataDir <- "data/processed"
codeDir <- "code"
outDir  <- "output"
figDir  <- "figures"

source(here::here(codeDir, "99_supportFunctions.R"))

# Read the dataset
diabetes <- readRDS(here::here(dataDir, "diabetes_sampled.rds"))

# Define the path model
mod1 <- '
## Define the structural relations:
bp + glu ~ age + male
'

# Estimate the path model and save the fitted lavaan object
out <- diabetes %>% 
  mutate(male = ifelse(sex == "male", 1, 0)) %>% 
  sem(mod1, data = ., fixed.x = FALSE)

saveRDS(out, here::here(outDir, "path_model.rds"))

# Summarize the results
summary(out, rsquare = TRUE)

# Create a path diagram
prepare_graph(out)

l <- matrix(c("age", "male", "bp", "glu"), ncol = 2)
p <- prepare_graph(out, rect_width = 1, rect_height = 1, variance_diameter = 0.5, layout = l) 
e <- edges(p)

e[2, 6] <- "left"
e[6, 5:6] <- "right"

edges(p) <- e

png(here::here(figDir, "path_diagram.png"))

plot(p)

dev.off()
