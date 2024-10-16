rm(list=ls()) # remove all variables stored previously
library(Hmisc) # import

data <- read.csv("C:/Users/priya/Downloads/archive (8)/COVID19_line_list_data.csv")
describe(data) # Hmisc command

#cleaned up death column
data$death_dummy <- as.integer(data$death != 0)
# death rate
sum(data$death_dummy) / nrow(data)

#AGE
# claim: people who die are older
dead = subset(data, death_dummy ==1)
alive= subset(data, death_dummy ==0)
mean(dead$age, na.rm = TRUE)
mean(alive$age, na.rm = TRUE)

# is this statistically significant?
t.test(alive$age, dead$age, alternative="two.sided",conf.level = 0.95)
# normally, if p-value < 0.05, we reject null hypothesis
#here, p-value ~ 0, so we reject null hypothesis and 
# conclude that this is statistically significant


# GENDER
# claim: gender has no effect
men = subset(data, gender == "male")
women= subset(data, gender == "female")
mean(men$death_dummy, na.rm = TRUE) # 8.5%!
mean(women$death_dummy, na.rm = TRUE) #3.7%

# is this statistically significant?
t.test(men$death_dummy, women$death_dummy, alternative="two.sided",conf.level = 0.95)
# 99% confidence: men have from 0.8% to 8.8% higher chance
# of dying.
# p-value = 0.002 < 0.05, so this is statistically 
# significant

# Install the ggplot2 package if you don't have it already
install.packages("ggplot2")

# Load the ggplot2 library
library(ggplot2)
# Create a density plot for the age distribution
ggplot(data, aes(x = age, fill = as.factor(death_dummy))) +
  geom_density(alpha = 0.5) +
  labs(title = "Age Distribution: Alive vs Deceased", x = "Age", fill = "Death Status") +
  scale_fill_manual(values = c("0" = "blue", "1" = "red"), labels = c("Alive", "Deceased")) +
  theme_minimal()

ggplot(data, aes(x = age, y = death_dummy)) +
  geom_jitter(aes(color = as.factor(death_dummy)), width = 0.2, height = 0.05, alpha = 0.6) +
  scale_y_continuous(breaks = c(0, 1), labels = c("Alive", "Deceased")) +
  scale_color_manual(values = c("0" = "blue", "1" = "red"), labels = c("Alive", "Deceased")) +
  labs(title = "Scatter Plot of Age vs Death Status", x = "Age", y = "Death Status", color = "Outcome") +
  theme_minimal()
# Create a boxplot comparing age distribution between alive and deceased individuals
ggplot(data, aes(x = as.factor(death_dummy), y = age, fill = as.factor(death_dummy))) +
  geom_boxplot() +
  labs(title = "Age Distribution by Death Status", x = "Death Status", y = "Age") +
  scale_fill_manual(values = c("0" = "blue", "1" = "red"), labels = c("Alive", "Deceased")) +
  theme_minimal() +
  scale_x_discrete(labels = c("Alive", "Deceased"))

# Create a summary table for heatmap
age_gender_death <- data %>%
  group_by(age_group, gender) %>%
  summarize(death_rate = mean(death_dummy, na.rm = TRUE))

# Heatmap of death rates by age group and gender
ggplot(age_gender_death, aes(x = age_group, y = gender, fill = death_rate)) +
  geom_tile(color = "white") +
  labs(title = "Heatmap of Death Rates by Age Group and Gender", x = "Age Group", y = "Gender", fill = "Death Rate") +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_minimal()

ggplot(data, aes(x = age, fill = as.factor(death_dummy))) +
  geom_histogram(binwidth = 5, position = "identity", alpha = 0.5) +
  labs(title = "Histogram of Age Distribution: Alive vs Deceased", x = "Age", y = "Count", fill = "Death Status") +
  scale_fill_manual(values = c("0" = "blue", "1" = "red"), labels = c("Alive", "Deceased")) +
  theme_minimal()

# Load necessary libraries
library(ggplot2)
library(dplyr)

# Summarize death status
death_status <- data.frame(
  Status = c("Alive", "Deceased"),
  Count = c(sum(data$death_dummy == 0, na.rm = TRUE), sum(data$death_dummy == 1, na.rm = TRUE))
)

# Create the pie chart
ggplot(death_status, aes(x = "", y = Count, fill = Status)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Pie Chart of Death Status (Alive vs. Deceased)") +
  theme_void() +  # Removes unnecessary axis labels
  scale_fill_manual(values = c("Alive" = "blue", "Deceased" = "red"))
library(dplyr)


# Create a summarized data frame for death status
death_status <- data.frame(
  Status = c("Alive", "Deceased"),
  Count = c(sum(data$death_dummy == 0, na.rm = TRUE), sum(data$death_dummy == 1, na.rm = TRUE))
)

# Create the pie chart
ggplot(death_status, aes(x = "", y = Count, fill = Status)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Pie Chart of Death Status (Alive vs. Deceased)") +
  theme_void() + # Remove axis labels for a clean pie chart
  scale_fill_manual(values = c("Alive" = "blue", "Deceased" = "red"))
# Convert date column to Date format
data$date <- as.Date(data$date, format = "%Y-%m-%d")








