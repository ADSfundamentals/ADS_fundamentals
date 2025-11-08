### Title:    Example Path Analysis
### Author:   Kyle M. Lang
### Created:  2023-08-17
### Modified: 2025-09-15

# We're already in trouble:
# The following command will fail unless we set our working directory correctly.
# To do so, we should create an RStudio project.
source("supportFunctions.R")

library(dplyr)
library(lavaan)
library(tidySEM)

# Read the dataset
diabetes <- readRDS("diabetes.rds")

# Process the data and save the processed dataset
diabetes2 <- slice_sample(diabetes, n = 200)

saveRDS(diabetes2, "diabetes_sampled.rds")

# Define the path model
mod1 <- '
## Define the structural relations:
bp + glu ~ age + male
'

# Estimate the path model and save the fitted lavaan object
out <- diabetes2 %>% 
  mutate(male = ifelse(sex == "male", 1, 0)) %>% 
  sem(mod1, data = ., fixed.x = FALSE)

saveRDS(out, "path_model.rds")

# Summarize the results
summary(out, rsquare = TRUE)

# Create a path diagram and save it as a PNG
prepare_graph(out)

l <- matrix(c("age", "male", "bp", "glu"), ncol = 2)
p <- prepare_graph(out, rect_width = 1, rect_height = 1, variance_diameter = 0.5, layout = l) 
e <- edges(p)

e[2, 6] <- "left"
e[6, 5:6] <- "right"

edges(p) <- e

png("path_diagram.png")

plot(p)

dev.off()
