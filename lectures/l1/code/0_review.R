### Title:    Review of Prerequisite Concepts
### Author:   Kyle M. Lang
### Created:  2026-11-13
### Modified: 2026-11-16

rm(list = ls(all = TRUE))

# Load the readr package
library(readr)

# Load the iris dataset
data(iris)

### ----------------------------------------------------------------------------

head(iris)
summary(iris)

dim(iris)
ncol(iris)
nrow(iris)

### ----------------------------------------------------------------------------

class(iris)
is.data.frame(iris)
is.matrix(iris)
is.list(iris)

### ----------------------------------------------------------------------------

iris[, 1]
iris[, "Sepal.Length"]
iris$Sepal.Length
iris[["Sepal.Length"]]
iris["Sepal.Length"]

iris[, 2:3]
iris[, c("Sepal.Width", "Petal.Length")]

### ----------------------------------------------------------------------------

levels(iris$Species)
mean(iris$Species)
table(iris$Species)
table(iris$Sepal.Length)

### ----------------------------------------------------------------------------

arrange(iris, Sepal.Length)

### ----------------------------------------------------------------------------

mean(boys$age)

boys <- readRDS("data/boys.rds")
cars <- read_csv("data/mtcars.csv")

x <- boys[1:10, "age"]
x
y <- 2
x[y:y^2]

### ----------------------------------------------------------------------------

hist(boys$age)
plot(y = cars$hp, x = cars$disp)
boxplot(Sepal.Width ~ Species, data = iris)
