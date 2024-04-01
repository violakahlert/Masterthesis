install.packages('effectsize')
install.packages('lubridate')
library(lubridate)
library(dplyr)
library(gridExtra)
library(janitor)
library(effectsize)

#load csv files 
setwd("D:/backup 12.01/viola/OneDrive/Documents/PCPT Master/Masterthesis")
df_neurotic <- read.csv("neurotic_offensive_analysis.csv")
df_neuroticism <- read.csv("neuroticism_offensive_analysis.csv")

#check the frequency of offensive and not offensive tweets
df_neurotic%>%tabyl(predominant_label)
df_neuroticism%>%tabyl(predominant_label)

#create for the offensive variables with the number of offensive and not offensive tweets per keyword
neurotic <- c(402977, 194653)
neuroticism <- c(30804, 2731)

#combine them in a matrix 
offensive_data <- matrix(c(neurotic, neuroticism), nrow=2, byrow=TRUE)

#test chi-square and cramers v
chisq.test(x = offensive_data)
cramers_v(x = offensive_data)

#ANALYSIS OVER TIME

#In year 2015
year_2015 <- 2015  

# Filtering for df_neurotic
df_neurotic_year_2015 <- df_neurotic %>% 
  filter(year(date) == year_2015)

# Filtering for df_neuroticism
df_neuroticism_year_2015 <- df_neuroticism %>%
  filter(year(date) == year_2015)

#check the frequency of offensive and not offensive tweets
df_neurotic_year_2015%>%tabyl(predominant_label)
df_neuroticism_year_2015%>%tabyl(predominant_label)

#create for the offensive variables with the number of offensive and not offensive tweets per keyword
neurotic_2015 <- c(48023, 14952)
neuroticism_2015 <- c(3842, 210)

#combine them in a matrix 
offensive_data_2015 <- matrix(c(neurotic_2015, neuroticism_2015), nrow=2, byrow=TRUE)

#test chi-square and cramers v
chisq.test(x = offensive_data_2015)
cramers_v(x = offensive_data_2015)

#In year 2016
year_2016 <- 2016  

# Filtering for df_neurotic
df_neurotic_year_2016 <- df_neurotic %>% 
  filter(year(date) == year_2016)

# Filtering for df_neuroticism
df_neuroticism_year_2016 <- df_neuroticism %>%
  filter(year(date) == year_2016)

#check the frequency of offensive and not offensive tweets
df_neurotic_year_2016%>%tabyl(predominant_label)
df_neuroticism_year_2016%>%tabyl(predominant_label)

#create for the offensive variables with the number of offensive and not offensive tweets per keyword
neurotic_2016 <- c(38191, 15108)
neuroticism_2016 <- c(3267, 132)

#combine them in a matrix 
offensive_data_2016 <- matrix(c(neurotic_2016, neuroticism_2016), nrow=2, byrow=TRUE)

#test chi-square and cramers v
chisq.test(x = offensive_data_2016)
cramers_v(x = offensive_data_2016)

#In year 2017
year_2017 <- 2017  

# Filtering for df_neurotic
df_neurotic_year_2017 <- df_neurotic %>% 
  filter(year(date) == year_2017)

# Filtering for df_neuroticism
df_neuroticism_year_2017 <- df_neuroticism %>%
  filter(year(date) == year_2017)

#check the frequency of offensive and not offensive tweets
df_neurotic_year_2017%>%tabyl(predominant_label)
df_neuroticism_year_2017%>%tabyl(predominant_label)

#create for the offensive variables with the number of offensive and not offensive tweets per keyword
neurotic_2017 <- c(41249, 16625)
neuroticism_2017 <- c(3829, 289)

#combine them in a matrix 
offensive_data_2017 <- matrix(c(neurotic_2017, neuroticism_2017), nrow=2, byrow=TRUE)

#test chi-square and cramers v
chisq.test(x = offensive_data_2017)
cramers_v(x = offensive_data_2017)

#In year 2018
year_2018 <- 2018  

# Filtering for df_neurotic
df_neurotic_year_2018 <- df_neurotic %>% 
  filter(year(date) == year_2018)

# Filtering for df_neuroticism
df_neuroticism_year_2018 <- df_neuroticism %>%
  filter(year(date) == year_2018)

#check the frequency of offensive and not offensive tweets
df_neurotic_year_2018%>%tabyl(predominant_label)
df_neuroticism_year_2018%>%tabyl(predominant_label)

#create for the offensive variables with the number of offensive and not offensive tweets per keyword
neurotic_2018 <- c(37746, 17721)
neuroticism_2018 <- c(4215, 4215)

#combine them in a matrix 
offensive_data_2018 <- matrix(c(neurotic_2018, neuroticism_2018), nrow=2, byrow=TRUE)

#test chi-square and cramers v
chisq.test(x = offensive_data_2018)
cramers_v(x = offensive_data_2018)

#In year 2019
year_2019 <- 2019  

# Filtering for df_neurotic
df_neurotic_year_2019 <- df_neurotic %>% 
  filter(year(date) == year_2019)

# Filtering for df_neuroticism
df_neuroticism_year_2019 <- df_neuroticism %>%
  filter(year(date) == year_2019)

#check the frequency of offensive and not offensive tweets
df_neurotic_year_2019%>%tabyl(predominant_label)
df_neuroticism_year_2019%>%tabyl(predominant_label)

#create for the offensive variables with the number of offensive and not offensive tweets per keyword
neurotic_2019 <- c(41767, 20723)
neuroticism_2019 <- c(4455, 459)

#combine them in a matrix 
offensive_data_2019 <- matrix(c(neurotic_2019, neuroticism_2019), nrow=2, byrow=TRUE)

#test chi-square and cramers v
chisq.test(x = offensive_data_2019)
cramers_v(x = offensive_data_2019)

#In year 2020
year_2020 <- 2020 

# Filtering for df_neurotic
df_neurotic_year_2020 <- df_neurotic %>% 
  filter(year(date) == year_2020)

# Filtering for df_neuroticism
df_neuroticism_year_2020 <- df_neuroticism %>%
  filter(year(date) == year_2020)

#check the frequency of offensive and not offensive tweets
df_neurotic_year_2020%>%tabyl(predominant_label)
df_neuroticism_year_2020%>%tabyl(predominant_label)

#create for the offensive variables with the number of offensive and not offensive tweets per keyword
neurotic_2020 <- c(53260, 28096)
neuroticism_2020 <- c(5995, 679)

#combine them in a matrix 
offensive_data_2020 <- matrix(c(neurotic_2020, neuroticism_2020), nrow=2, byrow=TRUE)

#test chi-square and cramers v
chisq.test(x = offensive_data_2020)
cramers_v(x = offensive_data_2020)

#In year 2021
year_2021 <- 2021 

# Filtering for df_neurotic
df_neurotic_year_2021 <- df_neurotic %>% 
  filter(year(date) == year_2021)

# Filtering for df_neuroticism
df_neuroticism_year_2021 <- df_neuroticism %>%
  filter(year(date) == year_2021)

#check the frequency of offensive and not offensive tweets
df_neurotic_year_2021%>%tabyl(predominant_label)
df_neuroticism_year_2021%>%tabyl(predominant_label)

#create for the offensive variables with the number of offensive and not offensive tweets per keyword
neurotic_2021 <- c(53480, 28630)
neuroticism_2021 <- c(5177, 592)

#combine them in a matrix 
offensive_data_2021 <- matrix(c(neurotic_2021, neuroticism_2021), nrow=2, byrow=TRUE)

#test chi-square and cramers v
chisq.test(x = offensive_data_2021)
cramers_v(x = offensive_data_2021)


#In year 2022
year_2022 <- 2022 

# Filtering for df_neurotic
df_neurotic_year_2022 <- df_neurotic %>% 
  filter(year(date) == year_2022)

# Filtering for df_neuroticism
df_neuroticism_year_2022 <- df_neuroticism %>%
  filter(year(date) == year_2022)

#check the frequency of offensive and not offensive tweets
df_neurotic_year_2022%>%tabyl(predominant_label)
df_neuroticism_year_2022%>%tabyl(predominant_label)

#create for the offensive variables with the number of offensive and not offensive tweets per keyword
neurotic_2022 <- c(67986, 40962)
neuroticism_2022 <- c(20, 2)

#combine them in a matrix 
offensive_data_2022 <- matrix(c(neurotic_2022, neuroticism_2022), nrow=2, byrow=TRUE)

#test chi-square and cramers v
chisq.test(x = offensive_data_2022)
cramers_v(x = offensive_data_2022)

#In year 2023
year_2023 <- 2023

# Filtering for df_neurotic
df_neurotic_year_2023 <- df_neurotic %>% 
  filter(year(date) == year_2023)

# Filtering for df_neuroticism
df_neuroticism_year_2023 <- df_neuroticism %>%
  filter(year(date) == year_2023)

#check the frequency of offensive and not offensive tweets
df_neurotic_year_2023%>%tabyl(predominant_label)
df_neuroticism_year_2023%>%tabyl(predominant_label)

#create for the offensive variables with the number of offensive and not offensive tweets per keyword
neurotic_2023 <- c(21275, 11836)
neuroticism_2023 <- c(4, 0)

#combine them in a matrix 
offensive_data_2023 <- matrix(c(neurotic_2023, neuroticism_2023), nrow=2, byrow=TRUE)

#test chi-square and cramers v
chisq.test(x = offensive_data_2023)
cramers_v(x = offensive_data_2023)
