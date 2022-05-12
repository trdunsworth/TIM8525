# Package Installation
install.packages("tidyverse")
install.packages("ggpubr")
install.packages("rstatix")
install.packages("car")
install.packages("broom")
install.packages("Hmisc")
install.packages("psych")

# Library Management
library(tidyverse)
library(ggpubr)
library(rstatix)
library(car)
library(broom)
library(Hmisc)
library(psych)

# Bring in the csv file for use
df1 <- read_csv("D:\\8525\\Section2\\TIM8525.csv")

# Convert columns to factors
df1$CollarColor <- as_factor(df1$CollarColor)
df1$Employment <- as_factor(df1$Employment)
df1$Country <- as_factor(df1$Country)
df1$Gender <- as_factor(df1$Gender)
df1$Age <- as_factor(df1$Age)
df1$Education <- as_factor(df1$Education)

# Rename levels of factor columns
levels(df1$CollarColor) <- c("Blue","Pink","Grey","White","Gold")
levels(df1$Gender) <- c("Male", "Female")
levels(df1$Employment) <- c("FullTime", "PartTime")
levels(df1$Country) <- c("US", "Canada", "UK", "Ireland", "India", "Other")

# First basaic information
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

# Normalcy Test
shapiro_test(df1$ValuesDomain)
shapiro_test(df1$MediatorDomain)
shapiro_test(df1$FulfillmentDomain)

# Column Removal
# Remove CountryOther since it has many missing values
df1 <- subset(df1, select = -c(CountryOther))
# Remove na values
df1 <- na.omit(df1)
# Remove rows where the respondent gave the same value for all of the questions
df1 <- df1[!(df1$ZeroVar == 1), ]

# Summarise variables by the independent variable
df1 %>% group_by(CollarColor) %>% summarise_at(c("ValuesDomain", "MediatorDomain", "FulfillmentDomain"), mean, na.rm=TRUE)
df1 %>% group_by(CollarColor) %>% summarise_at(c("ValuesDomain", "MediatorDomain", "FulfillmentDomain"), median, na.rm=TRUE)
df1 %>% group_by(CollarColor) %>% summarise_at(c("ValuesDomain", "MediatorDomain", "FulfillmentDomain"), sd, na.rm=TRUE)
df1 %>% group_by(CollarColor) %>% summarise_at(c("ValuesDomain", "MediatorDomain", "FulfillmentDomain"), IQR, na.rm=TRUE)
df1 %>% group_by(CollarColor) %>% summarise_at(c("ValuesDomain", "MediatorDomain", "FulfillmentDomain"), mad, na.rm=TRUE)

# Boxplot by Independent Variable
ggboxplot(
  df1, x = "CollarColor", y = c("ValuesDomain", "MediatorDomain", "FulfillmentDomain"), 
  merge = TRUE, palette = "lancet"
)

# Summary stats by independent variable
df1 %>%
  group_by(CollarColor) %>%
  get_summary_stats(ValuesDomain, MediatorDomain, FulfillmentDomain, type = "full")

# Check for outliers the quick way
df1 %>%
  group_by(Species) %>%
  mahalanobis_distance(-id) %>%
  filter(is.outlier == TRUE) %>%
  as.data.frame()
