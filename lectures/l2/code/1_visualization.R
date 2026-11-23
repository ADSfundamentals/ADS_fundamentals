### Title:    Visualization with GGPlot
### Author:   Kyle M. Lang
### Created:  2026-11-13
### Modified: 2026-11-23

rm(list = ls(all = TRUE))

library(ggplot2)
library(dplyr)
library(gridExtra)

dataDir <- "data"

diabetes <- readRDS(here::here(dataDir, "diabetes.rds"))
titanic <- readRDS(here::here(dataDir, "titanic.rds"))
bfi <- readRDS(here::here(dataDir, "bfi.rds"))

## Create a numeric version of the survival variable:
titanic <- mutate(titanic, surviveNum = ifelse(survived == "yes", 1, 0))

### GGPlot uses the "grammar of graphics" and "tidy data" to build up a figure
### from modular components.

### ----------------------------------------------------------------------------

# We start by calling the ggplot() function.
(p1 <- ggplot(data = diabetes, mapping = aes(x = bmi, y = glu)))

# To plot the data, we define some geometry via an appropriate "geom".
p1 + geom_point()
p1 + geom_line()
p1 + geom_rug()

# We can also combine different geoms into a single figure
p1 + geom_point() + geom_line() + geom_rug()

# We can use different flavors of geom for different types of data
p2 <- ggplot(diabetes, aes(tc))

p2 + geom_histogram()
p2 + geom_density()
p2 + geom_boxplot()

p3 <- ggplot(diabetes, aes(sex, bmi))

p3 + geom_boxplot()
p3 + geom_violin()

p4 <- ggplot(bfi, aes(education, age))

p4 + geom_point()
p4 + geom_jitter()

## We can also add statistical summaries of the data
p1 + geom_point() + geom_smooth()
p1 + geom_point() + geom_smooth(method = "lm")

### ----------------------------------------------------------------------------

### We can modify the styling of a plot in two distinct ways.

# Style options outside of aes() affect the entire plot
p5 <- ggplot(titanic, aes(age, survived))
p5 + geom_jitter(color = "blue", size = 3, height = 0.1)

# Style options defined inside aes() vary as functions of specific variables
p6_1 <- ggplot(titanic, aes(age, surviveNum, color = sex))
p6_1 +
  geom_jitter(size = 3, height = 0.1) +
  geom_smooth()

p6_2 <- ggplot(titanic, aes(age, surviveNum))

p6_2 +
  geom_jitter(aes(color = sex), size = 3, height = 0.1) +
  geom_smooth()

p6_2 +
  geom_jitter(size = 3, height = 0.1) +
  geom_smooth(aes(color = sex))

p6_2 +
  geom_jitter(aes(color = class), size = 3, height = 0.1) +
  geom_smooth(aes(color = sex))

p6_2 +
  geom_jitter(aes(shape = class), size = 3, height = 0.1) +
  geom_smooth(aes(color = sex))

p6_1 +
  geom_jitter(aes(shape = class), size = 3, height = 0.1) +
  geom_smooth()

### ----------------------------------------------------------------------------

### The theme defines all of the non-data ink in a plot

# We can apply several pre-baked themes to adjust a plot's overall appearance
(p1_1 <- p1 + geom_point())
p1_1 + theme_classic()
p1_1 + theme_minimal()
p1_1 + theme_bw()

# We can also modify individual theme elements
p1_1 +
  theme_classic() +
  theme(
    axis.title = element_text(
      size = 16,
      family = "serif",
      face = "bold",
      color = "blue"
    ),
    aspect.ratio = 1
  )

### ----------------------------------------------------------------------------

### Faceting allow us to make arrays of conditional plots

# Use facet_wrap() to condition plots on 'sex'
p7 <- ggplot(titanic, aes(age, surviveNum, color = class)) +
  geom_jitter(height = 0.05) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE)

p7 + facet_wrap(vars(sex))

# Use facet_grid() to condition plots on both 'sex' and 'class'
p8 <- ggplot(titanic, aes(age, surviveNum)) +
  geom_jitter(height = 0.05) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE)

p8 + facet_grid(vars(sex), vars(class))

### If we want to paste several different plots into a single figure (without
### faceting), we can use the utilities in the 'gridExtra' package.

grid.arrange(
  p1 + geom_point(),
  p3 + geom_boxplot(),
  p4 + geom_jitter(),
  p8 + facet_grid(vars(sex), vars(class)),
  ncol = 2
)
