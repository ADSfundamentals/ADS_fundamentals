### Title:    Lecture 2 Demonstration
### Author:   Kyle M. Lang
### Created:  2026-11-13
### Modified: 2026-11-13

rm(list = ls(all = TRUE))

###-Functions----------------------------------------------------------------###

### We can define our own functions using the function() function (how many
### times do you think I can use the word "function" in a single sentence?)

## Define a function
square <- function(x) {
  out <- x^2
  out
}

## Use the function
square(5)

## We don't need an explicit return() statement. R functions will return any
## object that is 'printed' as the last command inside the function (e.g.,
## 'out' in the above example)

## One-line functions don't need braces
square <- function(x) x^2
square(5)

## Function arguments are not strictly typed. R will try to work with whatever
## you provide as input.
square(1:5)
square(pi)
square(TRUE)
square("bob") # But one can only try so hard

## Functions can take multiple arguments
mod <- function(x, y) x %% y
mod(10, 3)

## Sometimes it's useful to specify a list of arguments that we unpack inside
## the function
getLsBeta <- function(datList) {
  X <- datList$X
  y <- datList$y

  solve(crossprod(X)) %*% t(X) %*% y
}

X <- matrix(runif(500), ncol = 5)
datList <- list(y = X %*% rep(0.5, 5), X = X)

getLsBeta(datList = datList)

################################################################################
## PRACTICE PROBLEM 5.1
##
## Write a 'hello world' function
##
## HINT: You can use the cat() function to echo a string to stdout
##
################################################################################

### Functions are first-class objects in R. We can treat them like any other R
### object.

## R views an initialized, but unevaluated, function as a special object with
## type "closure"
class(getLsBeta)
typeof(getLsBeta)


## After evaluation, functions are simply equivalent to the objects they return.
class(getLsBeta(datList))
typeof(getLsBeta(datList))

### We can use functions as arguments to other operations and functions.

fun1 <- function(x, y) x + y

## What will this command return?
fun1(1, fun1(1, 1))


## Why would we care?
s2 <- var(runif(100))
x <- rnorm(100, 0, sqrt(s2))

X[1:10, ]

c(1, 3, 6:9, 12)

rnorm(100) %>% square() %>% mean()

###-Tidyverse Data Transformation--------------------------------------------###

### The dplyr::mutate() function is the tidyverse function for computing new
### variables

## Use dplyr::mutate() to create the gender factor
bfi <- mutate(bfi, gender = factor(gender, labels = c("male", "female")))

## Mean center age
bfi <- mutate(bfi, age_mc = age - mean(age))
head(bfi)

## Standardize age and convert education into a factor
bfi <- mutate(
  bfi,
  age_std = scale(age)[, 1],
  education = factor(
    education,
    labels = c("some high school", "high school graduate", "some college", "college graduate", "graduate degree")
  )
)
str(bfi)

################################################################################
## PRACTICE PROBLEM 2.1
##
## Use dplyr::mutate() to add three new variables to the 'bfi' data
## 1. rootAge = The square root of 'age'
## 2. permEdu = A random permutation of the 'education' factor
## 3. compEdu = A logical vector that contains TRUE for every row where
##              'permEdu' = 'education' and FALSE otherwise
##
## HINTS:
## - You can use the sample() function to permute the contents of an R vector
## - You can use the tidyr::replace_na() function to replace missing values with
##   some non-missing value
##
################################################################################

### To project an operation across a range of variables, we need to use the
### across() function.

## Create mean scores for the five personality scales
tmp <- mutate(
  bfi,
  agree = rowMeans(across(A1:A5), na.rm = TRUE),
  consc = rowMeans(across(C1:C5), na.rm = TRUE),
  extra = rowMeans(across(E1:E5), na.rm = TRUE),
  neuro = rowMeans(across(N1:N5), na.rm = TRUE),
  open = rowMeans(across(O1:O5), na.rm = TRUE)
)
head(tmp)

### In these particular data, some of the items are reverse coded, so we need to
### flip their scaling before we compute scale scores. While we could do this
### ourselves (e.g., using the dplyr::recode() function), the 'psych' package
### has a very convenient scoreItems() function that we can use to make our
### lives much easier.

## The psych::scoreItems() function requires a list of item keys to describe the
## coding. Thankfully, the 'bfi' data ship with pre-defined keys
bfi.keys

## Let's give the scales some more succinct names
names(bfi.keys) <- c("agree", "consc", "extra", "neuro", "open")

## Now we can create the mean scores:
tmp <- scoreItems(keys = bfi.keys, items = bfi, impute = "none")

## The scoreItems() function returns a bunch of extra stuff
ls(tmp)

## When we print the returned object, we see some nice summary stats
tmp

## If we just want the scores with no fluff, we can use psych::scoreVeryFast()
scores <- scoreVeryFast(keys = bfi.keys, items = bfi)

head(scores)

## Add the scale scores onto our data
bfi <- data.frame(bfi, scores)

head(bfi)

## Create squarerooted versions of the agreeableness and extraversion items
bfi <- mutate(bfi, across(starts_with(c("A", "E"), ignore.case = FALSE), sqrt, .names = "{.col}_sqrt"))

head(bfi)

################################################################################
## PRACTICE PROBLEM 2.2
##
## Use the dplyr::mutate() function to create standardized versions of the five
## scales scores we just created.
##
################################################################################

### We can use the case_when() function inside dplyr::mutate() to build new
### variables from conditional logic

## Use case_when() inside of mutate() to create a new factor called
## 'gendered_maturity' according to the following logic
## - gendered_maturity = "boy" when age < 18 and gender = "male"
## - gendered_maturity = "girl" when age < 18 and gender = "female"
## - gendered_maturity = "man" when age >= 18 and gender = "male"
## - gendered_maturity = "woman" when age >= 18 and gender = "female"

bfi <- mutate(
  bfi,
  gendered_maturity = case_when(
    age < 18 & gender == "male" ~ "boy",
    age < 18 & gender == "female" ~ "girl",
    age >= 18 & gender == "male" ~ "man",
    age >= 18 & gender == "female" ~ "woman",
    TRUE ~ NA_character_
  ),
  gendered_maturity = as.factor(gendered_maturity)
)
str(bfi)
levels(bfi$gendered_maturity)
with(bfi, table(minor = age < 18, gender, gendered_maturity))

### We can use the dplyr::rename() and dplyr::rename_with() functions to easily
### rename columns in a data frame

## Rename a few of the variables in our bfi data
bfi <- rename(bfi, sex = gender, edu = education, gm = gendered_maturity)
head(bfi)

## Convert all scale item names to lower case
bfi <- rename_with(bfi, .fn = tolower, .cols = matches("\\d$"))
head(bfi)

## Save the modified data for later use
bfi |>
  select(-contains("_")) |>
  saveRDS(here::here(dataDir, "bfi.rds"))

################################################################################
## PRACTICE PROBLEM 2.3
##
## Use dplyr::mutate() and case_when() to create a new factor called 'col_grad'
## that satisfies the following logic
## - col_grad = "no" when 'edu' is "some high school", "high school graduate",
##   or "some college"
## - col_grad = "yes" when 'edu' is "college graduate" or "graduate degree"
## - col_grad = is missing, otherwise
##
################################################################################

################################################################################
## PRACTICE PROBLEM 2.4
##
## NOTE: The following problem statement uses these abbreviations
##       - O = Openness to Experience ('open')
##       - E = Extraversion ('extra')
##
## Use dplyr::mutate() and case_when() to create a new factor called 'type'
## that satisfies the following logic
## - type = "adventurous" when O is higher than the mean of O and E is higher
##          than the mean of E
## - type = "inquisitive" when O is higher than the mean of O and E is lower
##          than or equal to the mean of E
## - type = "quiet" when O is lower than or equal to the mean of O and E is
##          lower than or equal to the mean of E
## - type = "chatty" when O is lower than or equal to the mean of O and E is
##          higher than the mean of E
##
################################################################################

### --------------------------------------------------------------------------------------------------------------------

library(ggplot2)
library(dplyr)
library(magrittr)
library(gridExtra)

dataDir <- "data"
figDir <- "figures"

diabetes <- readRDS(here::here(dataDir, "diabetes.rds"))
titanic <- readRDS(here::here(dataDir, "titanic.rds"))
bfi <- readRDS(here::here(dataDir, "bfi.rds"))

## Create a numeric version of the survival variable:
titanic <- mutate(titanic, surviveNum = ifelse(survived == "yes", 1, 0))

###-GGPlot-------------------------------------------------------------------###

### Base R graphics are fine for quick-and-dirty visualizations (e.g., for
### checking assumptions), but for publication quality graphics, we probably
### want to use GGPlot.

### GGPlot uses the "grammar of graphics" and "tidy data" to build up a figure
### from modular components

## We start by calling the ggplot() function. We must define a dataset and some
## aesthetic via the aes() function.
(p1 <- ggplot(data = diabetes, mapping = aes(x = bmi, y = glu)))

## We need to define some geometry via an appropriate "geom" to actually plot
## the data
p1 + geom_point()
p1 + geom_line()
p1 + geom_rug()

## We can also combine different geoms into a single figure
p1 + geom_point() + geom_line() + geom_rug()

################################################################################
## PRACTICE PROBLEM 4.2
##
## Use GGPlot and the 'diabetes' data to create an empty plot of total
## cholesterol, 'tc', (on the y-axis) against 'age' (on the x-axis).
## - Don't add any geoms yet.
## - Assign the resulting plot object to a variable in your environment.
##
################################################################################

## We can use different flavors of geom for different types of data
p2 <- ggplot(diabetes, aes(tc))

p2 + geom_histogram()
p2 + geom_density()
p2 + geom_boxplot()

p3 <- ggplot(diabetes, aes(sex, bmi))

p3 + geom_boxplot()
p3 + geom_violin()

p4 <- ggplot(bfi, aes(edu, age))

p4 + geom_point()
p4 + geom_jitter()

## We can also add statistical summaries of the data
p1 + geom_point() + geom_smooth()
p1 + geom_point() + geom_smooth(method = "lm")

### We can modify the styling of a plot in two distinct ways.

## Changing style options outside of the aes() function applies the styling to
## the entire plot
p5 <- ggplot(titanic, aes(age, surviveFac))
p5 + geom_jitter(color = "blue", size = 3, height = 0.1)

## We can also apply styles as a function of variables by defining the style
## within the aes() function.
p6.1 <- ggplot(titanic, aes(age, surviveNum, color = sex))
p6.1 + geom_jitter(size = 3, height = 0.1) + geom_smooth()

p6.2 <- ggplot(titanic, aes(age, surviveNum))

p6.2 + geom_jitter(aes(color = sex), size = 3, height = 0.1) + geom_smooth()

p6.2 + geom_jitter(size = 3, height = 0.1) + geom_smooth(aes(color = sex))

p6.2 + geom_jitter(aes(color = class), size = 3, height = 0.1) + geom_smooth(aes(color = sex))

p6.2 + geom_jitter(aes(shape = class), size = 3, height = 0.1) + geom_smooth(aes(color = sex))

p6.1 + geom_jitter(aes(shape = class), size = 3, height = 0.1) + geom_smooth()

################################################################################
## PRACTICE PROBLEM 4.3
##
## Augment the plot you created in 4.2 to create a scatterplot.
## - Map the size of the points to 'bmi'
## - Assign the resulting plot object to a variable in your environment.
##
################################################################################

################################################################################
## PRACTICE PROBLEM 4.4
##
## Augment the plot you created in 4.3 by adding RUG lines to both the x-axis
## and y-axis.
## - Map the color of the RUG lines to 'glu'
## - Assign the resulting plot object to a variable in your environment.
##
################################################################################

################################################################################
## PRACTICE PROBLEM 4.5
##
## Augment the plot you created in 4.4 by adding linear regression lines.
## - Add separate lines for males and females.
## - Differentiate the regression lines by giving them different line types.
## - Do not include the SE bands.
## - Assign the resulting plot object to a variable in your environment.
##
################################################################################

### The theme defines all of the non-data ink in a plot

## We can apply several pre-baked themes to adjust a plot's overall appearance
(p1.1 <- p1 + geom_point())
p1.1 + theme_classic()
p1.1 + theme_minimal()
p1.1 + theme_bw()

## We can also modify individual theme elements
p1.1 +
  theme_classic() +
  theme(axis.title = element_text(size = 16, family = "serif", face = "bold", color = "blue"), aspect.ratio = 1)

################################################################################
## PRACTICE PROBLEM 4.6
##
## Modify the plot that you created in 4.5 by adjusting the theme.
## - Change the global theme to the "classic" theme.
## - Convert all text to 14-point, serif font.
##
################################################################################

### Faceting allow us to make arrays of conditional plots

## Use facet_wrap() to condition plots on 'sex'
(p7 <- ggplot(titanic, aes(age, surviveNum, color = class)) +
  geom_jitter(height = 0.05) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE))

p7 + facet_wrap(vars(sex))

## Use facet_grid() to condition plots on both 'sex' and 'class'
(p8 <- ggplot(titanic, aes(age, surviveNum)) +
  geom_jitter(height = 0.05) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE))

p8 + facet_grid(vars(sex), vars(class))

################################################################################
## PRACTICE PROBLEM 4.7
##
## Use the 'titanic' data, GGPlot, and faceting to create conditional
## histograms of 'age' conditioned on 'survived'.
## - Adjust the number of bins to optimize the clarity of the visualization.
## - Overlay kernel density plots on each histogram.
## - Do you think this figure is a more effective visualization than the
##   conditional boxplots you created in (4.1)? Why or why not?
##
## HINT: You can get ggplot to scale your histogram in proportions, rather than
##       counts, by specifying the argument "y = after_stat(density)" for the y
##       aesthetic in an appropriate geom.
##
################################################################################

### If we want to paste several different plots into a single figure (without
### faceting), we can use the utilities in the 'gridExtra' package.

grid.arrange(
  p1 + geom_point(),
  p3 + geom_boxplot(),
  p4 + geom_jitter(),
  p8 + facet_grid(vars(sex), vars(class)),
  ncol = 2
)

################################################################################
## PRACTICE PROBLEM 4.8
##
## Use ggplot() and grid.arrange() to create the two plots described below and
## organize the plots into a 1x2 array (i.e., 1 row and 2 columns).
##
## Using the diabetes data, create two plots. Both plots should begin with a
## histogram of blood glucose level ('glu').
## - In the first plot, overlay the kernel density plot for 'glu' as a blue line.
## - In the second plot, overlay the theoretical normal density as a red line.
##
## HINTS:
## - You can calculate the values for the normal density with the dnorm()
##   function.
## - Don't forget to define the appropriate mean and SD.
##
################################################################################

###-Saving Graphics----------------------------------------------------------###

### To save a graphic that we've created in R, we simply redirect the graphical
### output to a file using an appropriate function.

## Save as PDF
pdf(here::here(figDir, "example_plot.pdf"))

p7 + facet_wrap(vars(sex))

dev.off()

## Save as JPEG
jpeg(here::here(figDir, "example_plot.jpg"))

p7 + facet_wrap(vars(sex))

dev.off()

## Save as PNG
png(here::here(figDir, "example_plot.png"))

p7 + facet_wrap(vars(sex))

dev.off()

## With PDF documents we can save multiple figures to a single file.
pdf(here::here(figDir, "example_plot2.pdf"))

p6.1 + geom_jitter(size = 3, height = 0.1) + geom_smooth()
p7 + facet_wrap(vars(sex))
p8 + facet_grid(vars(sex), vars(class))

dev.off()

################################################################################
## PRACTICE PROBLEM 4.9
##
## Save the figure that you created in (4.7) as a JPEG
## - Adjust the size to 10cm X 10cm
## - Set the resolution to 800
## - Save the image to the "figures" directory
##
################################################################################

### With any format, we can save multiple figures, but we need to add the '%d'
### token to the file name, so the file names will have an index.

## For PDF files, we also need to set 'onefile = FALSE'
pdf(here::here(figDir, "example_plot2_%d.pdf"), onefile = FALSE)

p6.1 + geom_jitter(size = 3, height = 0.1) + geom_smooth()
p7 + facet_wrap(vars(sex))
p8 + facet_grid(vars(sex), vars(class))

dev.off()

## Multiple PNG files
png(here::here(figDir, "example_plot2_%d.png"))

p6.1 + geom_jitter(size = 3, height = 0.1) + geom_smooth()
p7 + facet_wrap(vars(sex))
p8 + facet_grid(vars(sex), vars(class))

dev.off()

## Multiple JPEG files
jpeg(here::here(figDir, "example_plot2_%d.jpg"))

p6.1 + geom_jitter(size = 3, height = 0.1) + geom_smooth()
p7 + facet_wrap(vars(sex))
p8 + facet_grid(vars(sex), vars(class))

dev.off()

################################################################################
## PRACTICE PROBLEM 4.10
##
## (a) Save the five figures you created in (4.2 - 4.6) to a single PDF file.
## (b) Save the five figures you created in (4.2 - 4.6) to a separate PNG files.
##     - Save both the PDF and the PNG files to the "figures" directory
##
################################################################################
