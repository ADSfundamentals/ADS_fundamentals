### Title:    Lecture 1 Demonstration
### Author:   Kyle M. Lang
### Created:  2026-11-13
### Modified: 2026-11-16

rm(list = ls(all = TRUE))

# Load necessary packages
library(dplyr)
library(magrittr)
library(psych)

# Load the 'bfi' data from the 'psych' package
data(bfi, package = "psych")

# Check the documentation
?bfi

# High-level summaries
head(bfi)
summary(bfi)
str(bfi)

### Preparation ----------------------------------------------------------------

names(bfi.keys) <- c("agree", "consc", "extra", "neuro", "open")
scores <- scoreVeryFast(keys = bfi.keys, items = bfi)

bfi <- data.frame(bfi, scores)

### Tidyverse Subsetting -------------------------------------------------------

### We can use the dplyr::filter() function to subset rows or a data frame based
### on logical conditions

# Filter on a simple condition
tmp <- filter(bfi, gender == 1)

dim(tmp)
summary(tmp)

# Filter on a composite condition
tmp <- filter(bfi, age < 50, gender == 1, education >= 4)

dim(tmp)
summary(tmp)

### The dplyr::select() function is a very flexible tool for selecting columns
### from a data frame

# Select the first 10 columns
tmp <- select(bfi, 1:10)
head(tmp)

# Exclude the first 10 columns
tmp <- select(bfi, -(1:10))
head(tmp)

# Be careful with negative signs!
select(bfi, -1:10) |> head()
select(bfi, -10:-1) |> head()

-(1:10)
-10:-1
-1:10

# Select the 'gender', 'age', and 'education' columns
tmp <- select(bfi, gender, age, education)
head(tmp)

# Exclude the 'gender' and 'education' columns
tmp <- select(bfi, -gender, -education)
head(tmp)

# Select all columns between "C1" and "O5"
tmp <- select(bfi, C1:O5)
head(tmp)

# Select all variables whose names begin with "A" or "E"
tmp <- select(bfi, starts_with(c("A", "E"), ignore.case = FALSE))
head(tmp)

### Pipes ----------------------------------------------------------------------

### The pipe operator, |> or %>%, allows us to compose functions and chain
### together multiple data processing steps into single intuitive "pipelines"

### Say we want to compose the following series of functions
### 1. Select the five scale scores
### 2. Calculate the covariance matrix of these scores
### 3. Extract the variances from this covariance matrix
### 4. Convert these variances into standard deviations

# We could do these operations with a series of nested functions
sqrt(
  diag(
    cov(
      bfi[c("agree", "consc", "extra", "neuro", "open")],
      use = "pairwise"
    )
  )
)

# We could also save intermediate objects along the way
tmp <- select(bfi, agree, consc, extra, neuro, open)
tmp <- cov(tmp, use = "pairwise")
tmp <- diag(tmp)
sqrt(tmp)

# A pipeline makes the workflow clearer
bfi |>
  select(agree, consc, extra, neuro, open) |>
  cov(use = "pairwise") |>
  diag() |>
  sqrt()

### ----------------------------------------------------------------------------

### Pipes work by passing the output from the upstream function into the
### downstream function as the first argument.

# Equivalent commands
mean(bfi$age)
bfi$age |> mean()

var(bfi$A1, na.rm = TRUE)
bfi$A1 |> var(na.rm = TRUE)

# We have a problem if the input data are not the first argument.
bfi |> lm(extra ~ age)

# We can use the '_'/'.' character as a placeholder.
bfi |> lm(extra ~ age, data = _)
bfi %>% lm(extra ~ age, data = .)

# The exposition pipe, %$%, provides an alternative solution.
bfi %$% lm(extra ~ age)

### Loops ----------------------------------------------------------------------

# Sum the numbers from 1 to 100
val <- 0
for (i in 1:100) {
  val <- val + i
}

val

# Compute the mean of every column in the 'mtcars' data
means <- rep(0, ncol(mtcars))
for (j in 1:ncol(mtcars)) {
  means[j] <- mean(mtcars[, j])
}

means

# Regress each variable in the 'mtcars' dataset onto all others
fits <- list()
for (v in 1:ncol(mtcars)) {
  fits[[v]] <- lm(mtcars[, v] ~ as.matrix(mtcars[, -v]))
}

fits

# Summarize the previous results
summaries <- list()
for (bob in 1:length(fits)) {
  summaries[[bob]] <- summary(fits[[bob]])
}

summaries

# A more direct version of the above
for (f in fits) {
  summary(f) |> print()
}

### ----------------------------------------------------------------------------

### Loops are often one of the least efficient solutions in R

n <- 1e8

t0 <- system.time({
  val0 <- 0
  for (i in 1:n) {
    val0 <- val0 + i
  }
})

t1 <- system.time(
  val1 <- sum(1:n)
)

# Same answer
val0 - val1

# But many times slower
t0
t1

# The appropriate way to get variable means:
colMeans(mtcars)

# A more elegant way to summarize the list of fits from above
lapply(fits, summary)

### Apply Functions ------------------------------------------------------------

### Base R apply functions let us broadcast univariate operations across the
### elements of vector-like data objects

x <- bfi[1:10, 1:5]
l1 <- list(a = rnorm(10), b = runif(10), c = 1:10)

# The basic model: apply()
apply(x, 1, max)
apply(x, 2, mean)
apply(x, 1:2, sqrt)

# For lists: lapply()
lapply(l1, sd)
lapply(l1, range)
lapply(x, var)

# Simplified: sapply()
sapply(l1, sd)
sapply(l1, range)
sapply(x, var)
