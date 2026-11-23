### Title:    Lecture 2 Demonstration
### Author:   Kyle M. Lang
### Created:  2026-11-13
### Modified: 2026-11-23

rm(list = ls(all = TRUE))

library(dplyr)
library(magrittr)

dataDir <- "data"

### User-Defined Functions -----------------------------------------------------

### We can define our own functions using the function() function.

# Define a function
square <- function(x) {
  out <- x^2
  out
}

# Use the function
square(5)

# One-line functions don't need braces
square <- function(x) x^2
square(5)

# Function arguments are not strictly typed.
square(1:5)
square(pi)
square(TRUE)
square("bob") # But we can only push things so far

# Functions can take multiple arguments
mod <- function(x, y) x %% y
mod(10, 3)

# Sometimes it's useful to specify arguments as a structured list.
getLsBeta <- function(datList) {
  X <- datList$X
  y <- datList$y

  solve(crossprod(X)) %*% t(X) %*% y
}

X <- matrix(runif(500), ncol = 5)
datList <- list(y = X %*% rep(0.5, 5), X = X)

getLsBeta(datList = datList)

### ----------------------------------------------------------------------------

### Functions are first-class objects in R.

# Initialized, but unevaluated, functions are special kinds of objects.
class(getLsBeta)
typeof(getLsBeta)

## After evaluation, functions are simply equivalent to the objects they return.
class(getLsBeta(datList))
typeof(getLsBeta(datList))

### We can use functions as arguments to other operations and functions.

fun1 <- function(x, y) x + y

# What will this command return?
fun1(fun1(1, 1), 1)
fun1(1, 1) |> fun1(1)

# Why would we care?
s2 <- var(runif(100))
x <- rnorm(100, 0, sqrt(s2))

X[1:10, ]

c(1, 3, 6:9, 12)

rnorm(100) |> square() |> mean()

### Tidyverse Data Transformation ----------------------------------------------

bfi <- readRDS(here::here(dataDir, "bfi.rds"))

### The dplyr::mutate() function is tidyverse's general-purpose data
### manipulation function.

# Use dplyr::mutate() to create the gender factor
bfi <- mutate(bfi, gender = factor(gender, labels = c("male", "female")))

# Mean center age
bfi <- mutate(bfi, age_mc = age - mean(age))
head(bfi)

# Standardize age and convert education into a factor
bfi <- mutate(
  bfi,
  age_std = scale(age)[, 1],
  education = factor(
    education,
    labels = c(
      "some high school",
      "high school graduate",
      "some college",
      "college graduate",
      "graduate degree"
    )
  )
)
str(bfi)

### ----------------------------------------------------------------------------

### To project an operation across a range of variables, we need to use the
### across() function.

# Create square-rooted versions of the agreeableness and extraversion items
bfi <- mutate(
  bfi,
  across(
    starts_with(c("A", "E"), ignore.case = FALSE),
    sqrt,
    .names = "{.col}_sqrt"
  )
)

head(bfi)

### We can use the case_when() function inside dplyr::mutate() to build new
### variables from conditional logic

# Use case_when() inside of mutate() to create a new factor called
# 'gendered_maturity' according to the following logic
# - gendered_maturity = "boy" when age < 18 and gender = "male"
# - gendered_maturity = "girl" when age < 18 and gender = "female"
# - gendered_maturity = "man" when age >= 18 and gender = "male"
# - gendered_maturity = "woman" when age >= 18 and gender = "female"

bfi <- mutate(
  bfi,
  gendered_maturity = case_when(
    age < 18 & gender == "male" ~ "boy",
    age < 18 & gender == "female" ~ "girl",
    age >= 18 & gender == "male" ~ "man",
    age >= 18 & gender == "female" ~ "woman",
    TRUE ~ NA
  ),
  gendered_maturity = as.factor(gendered_maturity)
)

str(bfi)
levels(bfi$gendered_maturity)
bfi %$% table(minor = age < 18, gender, gendered_maturity)
