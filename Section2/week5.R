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
install.packages("DiscriMiner")
install.packages("sur")
install.packages('DescTools',repos='http://cran.us.r-project.org')
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install('mixOmics')
install.packages("janitor")
install.packages("visdat")
install.packages("nlme")
install.packages("funModeling")
install.packages("inspectdf")
install.packages("dlookr")

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
library(DiscriMiner)
library(sur)
library(DescTools)
library(janitor)
library(visdat)
library(nlme)
library(funModeling)
library(inspectdf)
library(dlookr)

# Bring in the csv file for use
dfWk5 <- read_csv("D:\\8525\\Section2\\TIM8525.csv")
dfWk5 <- read_csv("C:\\Users\\tony.dunsworth\\OneDrive - City of Alexandria\\GitHub\\TIM8525\\Section2\\TIM8525.csv")
dfWk5 <- read_csv("/home/tonyd/Documents/GitHub/TIM8525/Section2/TIM8525.csv")

# Convert columns to factors
dfWk5$CollarColor <- as_factor(dfWk5$CollarColor)
dfWk5$Employment <- as_factor(dfWk5$Employment)
dfWk5$Country <- as_factor(dfWk5$Country)
dfWk5$Gender <- as_factor(dfWk5$Gender)
dfWk5$Age <- as_factor(dfWk5$Age)
dfWk5$Education <- as_factor(dfWk5$Education)

# Rename levels of factor columns
levels(dfWk5$CollarColor) <- c("Blue","Pink","Grey","White","Gold")
levels(dfWk5$Gender) <- c("Male", "Female")
levels(dfWk5$Employment) <- c("FullTime", "PartTime")
levels(dfWk5$Country) <- c("US", "Canada", "UK", "Ireland", "India", "Other")

# Column Removal
# Remove CountryOther since it has many missing values
dfWk5 <- subset(dfWk5, select = -c(CountryOther))
# Remove na values
dfWk5 <- dfWk5[!is.na(dfWk5$ValuesDomain),]
dfWk5 <- dfWk5[!is.na(dfWk5$MediatorDomain),]
dfWk5 <- dfWk5[!is.na(dfWk5$FulfillmentDomain),]
dfWk5 <- dfWk5[!is.na(dfWk5$CollarColor),]
dfWk5 <- dfWk5[!is.na(dfWk5$Gender),]
# Remove rows where the respondent gave the same value for all of the questions
dfWk5 <- dfWk5[!(dfWk5$ZeroVar == 1), ]
dfWk5 <- na.omit(dfWk5)

# EDA pieces
vis_dat(dfWk5, sort_type = FALSE)
vis_miss(dfWk5)
plot_num(dfWk5)
dfWk5 %>% inspect_cat %>% show_plot(high_cardinality = 1, col_palette = 1)
out1 <- diagnose_outlier(dfWk5)
plot_outlier(dfWk5)
bar_collar <- barplot(table(dfWk5$CollarColor))
bar_gender <- barplot(table(dfWk5$Gender))
bar_emp <- barplot(table(dfWk5$Employment))
bar_edu <- barplot(table(dfWk5$Education))
bar_country <- barplot(table(dfWk5$Country))
