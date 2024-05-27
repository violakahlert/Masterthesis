library(lubridate)
library(dplyr)
library(gridExtra)
library(janitor)
library(effectsize)
library(ggplot2)

#load csv files 
setwd("D:/backup 12.01/viola/OneDrive/Documents/PCPT Master/Masterthesis")
df_neurotic <- read.csv("2neurotic_offensive_analysis.csv")
df_neuroticism <- read.csv("2neuroticism_offensive_analysis.csv")

#check the frequency of offensive and not offensive tweets
df_neurotic%>%tabyl(predominant_label)
df_neuroticism%>%tabyl(predominant_label)

#create table for the offensive variables with the number of offensive and not offensive tweets per keyword
neurotic <- c(295403, 131624)
neuroticism <- c(30577, 2705)

#combine them in a matrix 
offensive_data <- matrix(c(neurotic, neuroticism), nrow=2, byrow=TRUE)

#test chi-square and cramers v
chisq.test(x = offensive_data)
cramers_v(x = offensive_data)

#ANALYSIS OVER TIME

# Convert 'date' and 'acc_created' to Date objects
df_neurotic$date <- as.Date(df_neurotic$date)
df_neurotic$acc_created <- as.Date(df_neurotic$acc_created)
df_neuroticism$date <- as.Date(df_neuroticism$date)
df_neuroticism$acc_created <- as.Date(df_neuroticism$acc_created)

# Add a column to indicate the keyword
df_neurotic$keyword <- 'neurotic'
df_neuroticism$keyword <- 'neuroticism'

# Combine datasets
combined_df <- bind_rows(df_neurotic, df_neuroticism)

# Convert 'predominant_label' to a binary variable
combined_df <- combined_df %>%
  mutate(offensive = ifelse(predominant_label == 'offensive', 1, 0))

# Create a column for the interval (number of 30-day intervals since the start of the dataset)
combined_df <- combined_df %>%
  arrange(date) %>%
  mutate(interval = as.numeric(difftime(date, min(date), units = "days")) %/% 30)

# Ensure 'keyword' is a factor
combined_df$keyword <- as.factor(combined_df$keyword)

# Fit the linear regression model
model <- lm(offensive ~ keyword * interval, data = combined_df)

# Print the summary of the regression
summary(model)

# Plotting
ggplot(combined_df, aes(x = interval, y = offensive, color = keyword)) +
  geom_point(alpha = 0.5) + # Scatter plot points
  geom_smooth(method = "lm", se = FALSE, fullrange = TRUE) + # Regression lines
  labs(title = "Likelihood of Offensive Content Over Time",
       x = "30-Day Intervals",
       y = "Offensive Content Likelihood",
       color = "Keyword") +
  theme_minimal()



# Diagnostic Plots
# Residuals vs Fitted Values Plot
plot(model, which = 1)

# Q-Q Plot of Residuals
plot(model, which = 2)

# Histogram of Residuals
residuals <- resid(model)
hist(residuals, breaks = 50, main = "Histogram of Residuals", xlab = "Residuals")

# Durbin-Watson Test for independence
library(lmtest)
dwtest(model)