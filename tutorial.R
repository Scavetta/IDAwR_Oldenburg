# IDAwR Workshop in Oldenburg
# Rick Scavetta
# 09.12.2019

# Clear workspace
rm(list = ls())

# Load packages
library(tidyverse)

# Plant Growth data I, wide format ----
# Not using tidyverse functions:

# Import data:
PG_wide <- read.delim("http://www.scavetta.academy/IDAwR/data/PlantGrowth_Wide.txt")
class(PG_wide)

# examine:
# head(), tail()
str(PG_wide)
class(PG_wide)

# 1 column data.frames become vectors
PG_wide[,2]
PG_wide[2]

# Descriptive stats:
fivenum(PG_wide$ctrl)
with(PG_wide, fivenum(ctrl))
summary(PG_wide)

# Plotting: "dot plot"
# maybe, but not here:
plot(PG_wide) # SPLOM - SCATTER PLOT MATRIX
with(PG_wide, plot(ctrl, trt1))

# Plotting: "Q-Q plot"
qqnorm(PG_wide$ctrl, main = "ctrl")
qqline(PG_wide$ctrl, col = "red")

qqnorm(PG_wide$trt1, main = "trt1")
qqline(PG_wide$trt1, col = "red")

qqnorm(PG_wide$trt2, main = "trt2")
qqline(PG_wide$trt2, col = "red")

xx <- rnorm(10)
qqnorm(xx, main = "rnorm")
qqline(xx, col = "blue")

# Calculate a statistical test y ~ x
# Parametric
with(PG_wide, t.test(ctrl, trt1))
with(PG_wide, t.test(ctrl, trt2))

# Non-parmetric
with(PG_wide, wilcox.test(ctrl, trt1))
with(PG_wide, wilcox.test(ctrl, trt2))

# Plant Growth data II, long format ----
# Import data, using readr:
PG_wide <- read_tsv("http://www.scavetta.academy/IDAwR/data/PlantGrowth_Wide.txt")

# This makes by default a tibble
# Does NOT by default convert character to factor!

# examine:
glimpse(PG_wide)
class(PG_wide)
attributes(PG_wide)
typeof(PG_wide) # A list where every element is a vector of the same length
PG_wide # print only the first 10 lines to console, i.e. print(PG_wide)

# And... tibbles don't convert to vectors!
PG_wide[,2]
PG_wide[2]
# i.e. data frame in, data frame out!

# Another example of a "wide" data frame:
# Create a new play dataset to work on:
PlayData <- data.frame(type = rep(c("A", "B"), each = 2),
                       time = 1:2,
                       height = seq(10, 40, 10),
                       width = seq(50, 80, 10))

# Make the wide, messy data tidy
# old: gather()
# new: pivot_longer()
# i.e. take column heading into a factor variable
# shift + ctrl/cmd + m

# Specify ID vars (exclude with -)
PlayData %>% 
  pivot_longer(-c(type, time), names_to = "key", values_to = "value") -> PlayData_t

# Specify MEASURE vars
PlayData %>% 
  pivot_longer(c(height, width), names_to = "key", values_to = "value")

# Exercise: tidy up the PG_wide data frame
PG_long <- pivot_longer(PG_wide, everything(), names_to = "group", values_to = "weight")

# Let's return to some of the tasks from earlier, using the tidy data:

# Descriptive stats, use dplyr functions (later on)

# Plotting: "dot plot"
# boxplot(weight ~ group, data = PG_long)
# stripchart(weight ~ group, data = PG_long)
PG_long %>% 
  ggplot(aes(group, weight)) +
  # geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.7) +
  stat_summary(fun.data = mean_sdl, 
               fun.args = list(mult = 1), 
               color = "red")

# Plotting: "Q-Q plot" (i.e. 3 Q-Q plots)
PG_long %>% 
  ggplot(aes(sample = weight)) +
  geom_qq() +
  geom_qq_line(color = "red") +
  facet_grid(. ~ group)

# Calculate a statistical test y ~ x e.g. ANOVA
summary(aov(weight ~ group, data = PG_long))

# Explictly match where the data goes
PG_long %>% 
  aov(weight ~ group, data = .) %>% 
  summary()

# Implicit positional matching
PG_long %>% 
  aov(formula = weight ~ group) %>% 
  summary()

# Bonus plotting exercise: how to plot ctrl against
# trt1 & trt2 in one command? i.e. 2 Scatter plots
# NOTE: This is just a plotting challenge, 
# it's NOT appropriate here!
PG_wide %>% 
  pivot_longer(-ctrl) %>% 
  ggplot(aes(ctrl, value)) +
  geom_point() +
  facet_grid(. ~ name)


# Example with dplyr ----
# using PlayData_t
# Scenario 1: compute across height and width
# Aggregration: mean
# True aggregration
PlayData_t %>% 
  group_by(type, time) %>%
  summarise(avg = mean(value)) %>% 
  ungroup()

# Pretend Transformation
PlayData_t %>% 
  group_by(type, time) %>%
  mutate(avg = mean(value))

# Transformation: standardize where height = 1
PlayData_t %>% 
  group_by(type, time) %>% 
  mutate(stand = value/value[key == "height"])

# Scenario 2: compute across time 1 & time 2
# Aggregration: mean
PlayData_t %>% 
  group_by(type, key)  %>%
  summarise(avg = mean(value))

# Transformation: standardize where time = 1
PlayData_t %>% 
  group_by(type, key) %>% 
  mutate(stand = value/value[time == 1])

# Scenario 3: compute across type A & B
# Aggregration: mean
# Transformation: standardize where type = A

# Filter
# Arrange
# Select

# two lowest values:
PlayData_t %>% 
  arrange(value) %>% 
  slice(1:2)
  
PlayData_t %>% 
  top_n(-2, value)

# filter with logical vectors, combine with a ,
PlayData_t %>% 
  filter(type == "A", time == 2)

# select specific columns
PlayData_t %>% 
  select(type, value)

# exclude a column
PlayData_t %>% 
  select(-type)

# Ways of assembling text:
# The glue way:
library(glue)
glue("the value is {mean(1:10)}.")

# The old way:
paste0("the value is ", mean(1:10), ".")


