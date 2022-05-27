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
install.packages("viridis")
install.packages("merTools")

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
library(viridis)
library(lme4)
library(merTools)

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
levels(dfWk5$Education) <- c("Less than HS", "Some HS", "HS Grad", "Some Uni", "Uni Grad", "Grad/Prof Degree")
levels(dfWk5$Age) <- c("18 to 24", "25 to 35", "36 to 45", "46 to 55", "56 to 65", "Over 65")

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
bar_collar <- barplot(table(dfWk5$CollarColor), col = c("#0000ff", "#ff99cc", "#c0c0c0", "#fffaf0", "#ffd700"))
bar_gender <- barplot(table(dfWk5$Gender), col = viridis(2))
bar_emp <- barplot(table(dfWk5$Employment), col = viridis(2))
bar_edu <- barplot(table(dfWk5$Education), col = viridis(6))
bar_country <- barplot(table(dfWk5$Country), col = plasma(6))

tabyl(dfWk5, CollarColor, Gender)
tabyl(dfWk5, CollarColor, Education)
tabyl(dfWk5, CollarColor, Country)

# Basic Visualizations
hist(dfWk5$ValuesDomain, col = viridis(15))
hist(dfWk5$MediatorDomain, col = viridis(8))
hist(dfWk5$FulfillmentDomain, col = viridis(20))

boxplot(dfWk5$ValuesDomain, col = "#1c5789")
boxplot(dfWk4a_new$MediatorDomain)
boxplot(dfWk4a_new$FulfillmentDomain)

qqnorm(dfWk5$ValuesDomain, col = viridis(1))
qqnorm(dfWk4a_new$MediatorDomain, col = viridis(1))
qqnorm(dfWk4a_new$FulfillmentDomain, col = viridis(1))

# Additional EDA
dfWk5 %>% group_by(CollarColor, Gender) %>% summarise_at(c("ValuesDomain", "MediatorDomain", "FulfillmentDomain"), mean, na.rm=TRUE)

# Normality Check
shapiro_test(dfWk5$ValuesDomain)
shapiro_test(dfWk5$MediatorDomain)
shapiro_test(dfWk5$FulfillmentDomain)

# Remove outliers to see if it normalizes data better
dfWk5a <- subset(dfWk5, dfWk5$ValuesDomain > 5)
dfWk5a <- subset(dfWk5a, dfWk5a$MediatorDomain > 5)
dfWk5a <- subset(dfWk5a, dfWk5a$FulfillmentDomain > 5)

# Second Normality Check
shapiro_test(dfWk5a$ValuesDomain)
shapiro_test(dfWk5a$MediatorDomain)
shapiro_test(dfWk5a$FulfillmentDomain)

out2 <- diagnose_outlier(dfWk5a)
plot_outlier(dfWk5a)

# HLM tests for the data
model1 <- gls(ValuesDomain~1, data = dfWk5, method = "ML", na.action = "na.omit")
summary(model1)
is.factor(dfWk5$Age)

model2 = lme(ValuesDomain~1, data = dfWk5, method = "ML", na.action = "na.omit", random = ~1|Age)
summary(model2)

anova(model1, model2)

model2.1<-lmer(ValuesDomain~1+(1|Age), REML = FALSE, data = dfWk5)
summary(model2.1)
ICC(outcome = "ValuesDomain", group = "Age", data = dfWk5)
confint(model2.1)

model3 = lme(ValuesDomain~ Gender, data = dfWk5, method = "ML", na.action = "na.omit", random = ~1|Age)
summary(model3)

anova(model3, model2)

model3.1 <-lmer(ValuesDomain~1+Gender+(1|Age), REML = FALSE, data = dfWk5)
summary(model3.1)

model4= lme(ValuesDomain~ Gender, data = dfWk5, method = "ML", na.action = "na.omit", random = ~Gender|Age, control = lmeControl(msMaxIter = 200))
summary(model4)

anova(model3, model4)

model3withBPD<-lme(ValuesDomain~Gender+Employment+Employment*Gender,
                   data = dfWk5, method = "ML", na.action = "na.omit", random = ~1|Age)
summary(model3withBPD)

anova(model3, model3withBPD)
