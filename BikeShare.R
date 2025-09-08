library(tidyverse)
library(tidymodels)
library(vroom)
library(patchwork)

# Read in data and check variable types #
train <- vroom('C:\\Users\\cjmsp\\Desktop\\Stat348\\BikeShare\\train.csv')
glimpse(train)

# Change variable types #
train <- train |>
  mutate(
  season = as.factor(season),
  holiday = as.factor(holiday),
  workingday = as.factor(workingday),
  weather = as.factor(weather),
  humidity = as.integer(humidity),
  casual = as.integer(casual),
  registered = as.integer(registered),
  count = as.integer(count)
)


# EDA #
GGally::ggpairs(train)
skimr::skim(train)
ggplot(data = train, aes(x=humidity)) + # View Density of Humidity
  geom_density()

plot1 <- ggplot(data = train, aes(x = datetime, y = count, color = season)) + 
  geom_point() +
  labs(title = "Number of Bike Rentals over Time")
plot2 <- ggplot(data = train, aes(x = weather)) +
  geom_bar() +
  labs(title = "Number of Bike Rentals for Different Weather Conditions")
plot3 <- ggplot(data = train, aes(x = temp, y = count, color = weather)) +
  geom_point() +
  scale_color_manual(values = c("red", "blue", "green", "black")) +
  geom_smooth(method = lm, formula = y ~ x, se = FALSE) +
  labs(title = "Scatterplot of Temperature and Bike Rentals, Stratified by Weather")
plot4 <- ggplot(data = train, aes(x = temp, y = count, color = workingday)) +
  geom_point() +
  scale_color_manual(values = c('red', 'blue')) +
  labs(title = "Scatterplot of Temperature and Bike Rentals, Stratified by Workingday")
(plot1 + plot2) / (plot3 + plot4)

# No missing data, pretty even amount of observations for each season #
# Weather: only one observation with category 4 weather, better weather <- more obs #
# Temperature in celcius #
# atemp (feels like temperature) generally appears to be around temp +4
# Humidity Generally pretty high. Q1: 47 Q3: 77 
# Frequent periodic gaps of no bike rentals across time
