### Title:    Lecture 5 Examples
### Author:   Kyle M. Lang
### Created:  2021-12-17
### Modified: 2025-12-05

library(openxlsx)
library(dplyr)
library(magrittr)
library(ggplot2)

dataDir <- "data"
fn      <- "utmb_2017.csv"

utmb <- read.csv(here::here(dataDir, fn))
penguin <- read.xlsx(here::here(dataDir, "penguin_counts.xlsx"))
crossing <- read.csv(here::here(dataDir, "nyc-east-river-bicycle-counts.csv"), row.names = 1)

dat1 <- data.frame(Sex = factor(grepl("H$", utmb$category), labels = c("Female", "Male")),
                   Finish = factor(utmb$time != " ", labels = c("No", "Yes"))
                   )

saveRDS(dat1, paste0(dataDir, "utmb_finish_2017.rds"))

###--------------------------------------------------------------------------###

head(penguin)
str(penguin)

penguin %$% unique(common.name)

penguin %<>% mutate(site = factor(site.name),
                    species = factor(common.name),
                    type = factor(count.type)
                    )

str(penguin)

filter(penguin, type == "chicks") %>%
  ggplot(aes(year, count, color = species)) +
  geom_point() +
  geom_smooth()

filter(penguin, type == "chicks") %>%
  ggplot(aes(year, log(count), color = species)) +
  geom_point() +
  geom_smooth()

hist(count ~ type, data = penguin)

###--------------------------------------------------------------------------###

head(crossing)
colnames(crossing) <- c(
  "date",
  "day",
  "high_temp",
  "low_temp",
  "precip",
  "brooklyn",
  "manhattan",
  "williamsburg", 
  "queensboro",
  "total"
)

hist(crossing$brooklyn)
hist(crossing$manhattan)
hist(crossing$williamsburg)
hist(crossing$queensboro)

ggplot(crossing, aes(high_temp, brooklyn)) + 
  geom_point() +
  geom_smooth()

ggplot(crossing, aes(high_temp, manhattan)) + 
  geom_point() +
  geom_smooth()

ggplot(crossing, aes(high_temp, williamsburg)) + 
  geom_point() +
  geom_smooth()

ggplot(crossing, aes(high_temp, queensboro)) + 
  geom_point() +
  geom_smooth()

ggplot(crossing, aes(high_temp, log(brooklyn))) +
  geom_point() +
  geom_smooth()

ggplot(crossing, aes(high_temp, log(manhattan))) + 
  geom_point() +
  geom_smooth()

ggplot(crossing, aes(high_temp, log(williamsburg))) + 
  geom_point() +
  geom_smooth()

ggplot(crossing, aes(high_temp, log(queensboro))) + 
  geom_point() +
  geom_smooth()

###--------------------------------------------------------------------------###

n <- 500

x <- runif(n, 0, 3)
x <- rnorm(n)
y <- rpois(n, exp(x))

dat <- data.frame(x, y, eta = exp(x))

ggplot(dat, aes(x, y)) + 
 geom_point() +
 geom_smooth()

ggplot(dat, aes(eta, y)) + 
 geom_point() +
 geom_smooth()

x <- seq(-2, 3, length.out = 10000)
y <- 3 * exp(x)

dat2 <- data.frame(x, y)

ggplot(dat2, aes(x, y)) +
  geom_line(color = "blue", linewidth = 1) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  theme_minimal() +
  scale_x_continuous(expand = c(0, 0)) +
  ylab(expression(hat(mu))) +
  ggtitle("Raw Conditional Mean") +
  theme(text = element_text(family = "courier"), 
        plot.title = element_text(hjust = 0.5)
       )

ggplot(dat2, aes(x, log(y))) +
  geom_line(color = "blue", linewidth = 1) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  theme_minimal() +
  scale_x_continuous(expand = c(0, 0)) +
  ylab(expression(paste("g(", hat(mu), ")")))

ggplot(dat2, aes(x, y)) +
  geom_line()

ggplot(dat2, aes(x, log(y))) +
  geom_line()

x <- 3 + seq(-5, 5, length.out = 10000)
y <- plogis(x - 3)

dat2 <- data.frame(x, y)

ggplot(dat2, aes(x, y)) +
  geom_line(color = "blue", linewidth = 1) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  theme_minimal() +
  scale_x_continuous(expand = c(0, 0)) +
  ylab(expression(hat(mu)))

ggplot(dat2, aes(x, qlogis(y))) +
  geom_line(color = "blue", linewidth = 1) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  theme_minimal() +
  scale_x_continuous(expand = c(0, 0)) +
  ylab(expression(paste("g(", hat(mu), ")")))


ggplot(dat2, aes(x, qlogis(y))) +
  geom_line() +
  scale_x_continuous(expand = c(0, 0))


plot(x)
plot(log(x))

#----------------------------------------------------------------------------------------------------------------------#

x <- 0:6
n0 <- 5
n1 <- 2
y <- n0 * (1 + n1)^x
y2 <- log(n0) + log(1 + n1) * x

out <- lm(log(y) ~ x)

coef(out) |> exp()

y2 - log(y)

405 / 3
135 / 3
45 / 3

plot(x, y)
lines(x, y2)

plot(x, log(y))

dat <- data.frame(x, y)

filter(dat, x == 0)
filter(dat, x == 1)
filter(dat, x == 2)
filter(dat, x == 3)
filter(dat, x == 4)

ggplot(dat, aes(x, y)) +
  geom_line() +
  xlab("Time") +
  ylab("Cases")

ggplot(dat, aes(x, log(y))) +
  geom_line() +
  xlab("Time") +
  ylab("ln(Cases)")

