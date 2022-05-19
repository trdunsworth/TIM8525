# Package Installation
install.packages("tidyverse")
install.packages("ggpubr")
install.packages("rstatix")
install.packages("car")
install.packages("broom")
install.packages("Hmisc")
install.packages("psych")
install.packages("GGally")
install.packages("remotes")
remotes::install_github("jacobmaugoust/ULT")
install.packages("FSA")
install.packages("multcomp")
install.packages("emmeans")
install.packages("phia")
install.packages("RVAideMemoire")

# Library Management
library(tidyverse)
library(ggpubr)
library(rstatix)
library(car)
library(broom)
library(Hmisc)
library(psych)
library(GGally)
library(ULT)
library(FSA)
library(multcomp)
library(emmeans)
library(phia)
library(RVAideMemoire)

# Bring in the csv file for use
dfWk4 <- read_csv("D:\\8525\\Section2\\TIM8525.csv")

# Convert columns to factors
dfWk4$CollarColor <- as_factor(dfWk4$CollarColor)
dfWk4$Employment <- as_factor(dfWk4$Employment)
dfWk4$Country <- as_factor(dfWk4$Country)
dfWk4$Gender <- as_factor(dfWk4$Gender)
dfWk4$Age <- as_factor(dfWk4$Age)
dfWk4$Education <- as_factor(dfWk4$Education)

# Rename levels of factor columns
levels(dfWk4$CollarColor) <- c("Blue","Pink","Grey","White","Gold")
levels(dfWk4$Gender) <- c("Male", "Female")
levels(dfWk4$Employment) <- c("FullTime", "PartTime")
levels(dfWk4$Country) <- c("US", "Canada", "UK", "Ireland", "India", "Other")

# Column Removal
# Remove CountryOther since it has many missing values
dfWk4 <- subset(dfWk4, select = -c(CountryOther))
# Remove na values
dfWk4 <- dfWk4[!is.na(dfWk4$ValuesDomain),]
dfWk4 <- dfWk4[!is.na(dfWk4$MediatorDomain),]
dfWk4 <- dfWk4[!is.na(dfWk4$FulfillmentDomain),]
# Remove rows where the respondent gave the same value for all of the questions
dfWk4 <- dfWk4[!(dfWk4$ZeroVar == 1), ]

# First basic information
summary(df1)
describe(df1)
describe(df1$ValuesDomain)
describe(df1$MediatorDomain)
describe(df1$FulfillmentDomain)
mardia(df1$ValuesDomain)
mardia(df1$MediatorDomain)
mardia(df1$FulfillmentDomain)

# Basic Visualizations
hist(df1$ValuesDomain)
hist(df1$MediatorDomain)
hist(df1$FulfillmentDomain)

boxplot(df1$ValuesDomain)
boxplot(df1$MediatorDomain)
boxplot(df1$FulfillmentDomain)

qqnorm(df1$ValuesDomain)
qqnorm(df1$MediatorDomain)
qqnorm(df1$FulfillmentDomain)