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

# Bring in the csv file for use
dfWk4 <- read_csv("D:\\8525\\Section2\\TIM8525.csv")
dfWk4 <- read_csv("C:\\Users\\tony.dunsworth\\OneDrive - City of Alexandria\\GitHub\\TIM8525\\Section2\\TIM8525.csv")
dfWk4 <- read_csv("/home/tonyd/Documents/GitHub/TIM8525/Section2/TIM8525.csv")

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
dfWk4 <- dfWk4[!is.na(dfWk4$CollarColor),]
dfWk4 <- dfWk4[!is.na(dfWk4$Gender),]
# Remove rows where the respondent gave the same value for all of the questions
dfWk4 <- dfWk4[!(dfWk4$ZeroVar == 1), ]

# First basic information
summary(dfWk4)
describe(dfWk4)
describe(dfWk4$ValuesDomain)
describe(dfWk4$MediatorDomain)
describe(dfWk4$FulfillmentDomain)
mardia(dfWk4$ValuesDomain)
mardia(dfWk4$MediatorDomain)
mardia(dfWk4$FulfillmentDomain)

# Basic Visualizations
hist(dfWk4$ValuesDomain)
hist(dfWk4$MediatorDomain)
hist(dfWk4$FulfillmentDomain)

boxplot(dfWk4$ValuesDomain)
boxplot(dfWk4$MediatorDomain)
boxplot(dfWk4$FulfillmentDomain)

qqnorm(dfWk4$ValuesDomain)
qqnorm(dfWk4$MediatorDomain)
qqnorm(dfWk4$FulfillmentDomain)

bar_collar <- barplot(table(dfWk4$CollarColor))
bar_gender <- barplot(table(dfWk4$Gender))

# Normality Check
shapiro_test(dfWk4$ValuesDomain)
shapiro_test(dfWk4$MediatorDomain)
shapiro_test(dfWk4$FulfillmentDomain)

# Outlier Identification
medV = median(dfWk4$ValuesDomain)
# subtract median from each value of x and get absolute deviation
abs_devV = abs(dfWk4$ValuesDomain-medV)
# get MAD
madV = 1.4826 * median(abs_devV)

# get threshold values for outliers
TminV = medV-(3*madV) 
TmaxV = medV+(3*madV) 

# find outlier
valuesOutlier <- dfWk4$ValuesDomain[which(dfWk4$ValuesDomain < TminV | dfWk4$ValuesDomain > TmaxV)]

max(valuesOutlier)
valuesOutlier

medM = median(dfWk4$MediatorDomain)
# subtract median from each value of x and get absolute deviation
abs_devM = abs(dfWk4$MediatorDomain-medM)
# get MAD
madM = 1.4826 * median(abs_devM)

# get threshold values for outliers
TminM = medM-(3*madM) 
TmaxM = medM+(3*madM) 

# find outlier
mediatorOutlier <- dfWk4$MediatorDomain[which(dfWk4$MediatorDomain < TminM | dfWk4$MediatorDomain > TmaxM)]

max(mediatorOutlier)
mediatorOutlier

medF = median(dfWk4$FulfillmentDomain)
# subtract median from each value of x and get absolute deviation
abs_devF = abs(dfWk4$FulfillmentDomain-medF)
# get MAD
madF = 1.4826 * median(abs_devF)

# get threshold values for outliers
TminF = medF-(3*madF) 
TmaxF = medF+(3*madF) 

# find outlier
fulfillmentOutlier <- dfWk4$FulfillmentDomain[which(dfWk4$FulfillmentDomain < TminF | dfWk4$FulfillmentDomain > TmaxF)]

max(fulfillmentOutlier)
fulfillmentOutlier

# Remove outliers from the data set. 

dfWk4 <- subset(dfWk4, dfWk4$ValuesDomain > max(valuesOutlier))
dfWk4 <- subset(dfWk4, dfWk4$MediatorDomain > max(mediatorOutlier))
dfWk4 <- subset(dfWk4, dfWk4$FulfillmentDomain > max(fulfillmentOutlier))

# Normalize the data
dfWk4a <- subset(dfWk4, select=c("Gender","CollarColor","ValuesDomain","MediatorDomain","FulfillmentDomain"))

dfWk4a_new <- dfWk4a %>% mutate_each_(list(~scale(.) %>% as.vector),
      vars = c("ValuesDomain","MediatorDomain","FulfillmentDomain"))
dep_var <- cbind(dfWk4a_new$ValuesDomain, dfWk4a_new$MediatorDomain, dfWk4a_new$FulfillmentDomain)

# First basic information
summary(dfWk4a_new)
describe(dfWk4a_new)
describe(dfWk4a_new$ValuesDomain)
describe(dfWk4a_new$MediatorDomain)
describe(dfWk4a_new$FulfillmentDomain)
mardia(dfWk4a_new$ValuesDomain)
mardia(dfWk4a_new$MediatorDomain)
mardia(dfWk4a_new$FulfillmentDomain)

# Basic Visualizations
hist(dfWk4a_new$ValuesDomain)
hist(dfWk4a_new$MediatorDomain)
hist(dfWk4a_new$FulfillmentDomain)

boxplot(dfWk4a_new$ValuesDomain)
boxplot(dfWk4a_new$MediatorDomain)
boxplot(dfWk4a_new$FulfillmentDomain)

qqnorm(dfWk4a_new$ValuesDomain)
qqnorm(dfWk4a_new$MediatorDomain)
qqnorm(dfWk4a_new$FulfillmentDomain)

bar_collar <- barplot(table(dfWk4a_new$CollarColor))
bar_gender <- barplot(table(dfWk4a_new$Gender))

dfWk4a_new %>% count(CollarColor,Gender)

# Normality Check
shapiro_test(dfWk4a_new$ValuesDomain)
shapiro_test(dfWk4a_new$MediatorDomain)
shapiro_test(dfWk4a_new$FulfillmentDomain)

# Summarise variables by the independent variable
dfWk4a_new %>% group_by(CollarColor) %>% summarise_at(c("ValuesDomain", "MediatorDomain", "FulfillmentDomain"), mean, na.rm=TRUE)
dfWk4a_new %>% group_by(CollarColor) %>% summarise_at(c("ValuesDomain", "MediatorDomain", "FulfillmentDomain"), median, na.rm=TRUE)
dfWk4a_new %>% group_by(CollarColor) %>% summarise_at(c("ValuesDomain", "MediatorDomain", "FulfillmentDomain"), sd, na.rm=TRUE)
dfWk4a_new %>% group_by(CollarColor) %>% summarise_at(c("ValuesDomain", "MediatorDomain", "FulfillmentDomain"), IQR, na.rm=TRUE)
dfWk4a_new %>% group_by(CollarColor) %>% summarise_at(c("ValuesDomain", "MediatorDomain", "FulfillmentDomain"), mad, na.rm=TRUE)

dfWk4a_new %>% group_by(Gender) %>% summarise_at(c("ValuesDomain", "MediatorDomain", "FulfillmentDomain"), mean, na.rm=TRUE)
dfWk4a_new %>% group_by(Gender) %>% summarise_at(c("ValuesDomain", "MediatorDomain", "FulfillmentDomain"), median, na.rm=TRUE)
dfWk4a_new %>% group_by(Gender) %>% summarise_at(c("ValuesDomain", "MediatorDomain", "FulfillmentDomain"), sd, na.rm=TRUE)
dfWk4a_new %>% group_by(Gender) %>% summarise_at(c("ValuesDomain", "MediatorDomain", "FulfillmentDomain"), IQR, na.rm=TRUE)
dfWk4a_new %>% group_by(Gender) %>% summarise_at(c("ValuesDomain", "MediatorDomain", "FulfillmentDomain"), mad, na.rm=TRUE)

# Boxplot by Independent Variable
ggboxplot(
  dfWk4a_new, x = "CollarColor", y = c("ValuesDomain", "MediatorDomain", "FulfillmentDomain"), 
  merge = TRUE, palette = "lancet"
)

ggboxplot(
  dfWk4a_new, x = "Gender", y = c("ValuesDomain", "MediatorDomain", "FulfillmentDomain"), merge = TRUE, palette = "lancet"
)

model_b <- lm(dep_var~CollarColor+Gender+CollarColor*Gender -1, data=dfWk4a_new)

# Summary stats by independent variable
dfWk4a_new %>%
  group_by(CollarColor) %>%
  get_summary_stats(ValuesDomain, MediatorDomain, FulfillmentDomain, type = "full")
dfWk4a_new %>%
  group_by(Gender) %>%
  get_summary_stats(ValuesDomain, MediatorDomain, FulfillmentDomain, type = "full")

# Establish the number of respondents per Collar Colour
dfWk4a_new %>% group_by(CollarColor) %>% summarise(N = n())
dfWk4a_new %>% group_by(Gender) %>% summarise(N = n())
dfWk4a_new %>% group_by(CollarColor, Gender) %>% summarise(N = n())

# Identify univariate outliers
dfWk4a_new %>% group_by(CollarColor, Gender) %>% identify_outliers(ValuesDomain)
dfWk4a_new %>% group_by(CollarColor, Gender) %>% identify_outliers(MediatorDomain)
dfWk4a_new %>% group_by(CollarColor, Gender) %>% identify_outliers(FulfillmentDomain)

# Remove additional outliers
valOut2 <- dfWk4a_new %>% group_by(CollarColor, Gender) %>% identify_outliers(ValuesDomain)
dfWk4a_new <- dfWk4a_new[!(dfWk4a_new$ValuesDomain %in% valOut2$ValuesDomain),]

medOut2 <- dfWk4a_new %>% group_by(CollarColor, Gender) %>% identify_outliers(MediatorDomain)
dfWk4a_new <- dfWk4a_new[!(dfWk4a_new$MediatorDomain %in% medOut2$MediatorDomain),]

fulOut2 <- dfWk4a_new %>% group_by(CollarColor, Gender) %>% identify_outliers(FulfillmentDomain)
dfWk4a_new <- dfWk4a_new[!(dfWk4a_new$FulfillmentDomain %in% fulOut2$FulfillmentDomain),]

# Check normality assumptions
dfWk4a_new %>% group_by(CollarColor) %>% shapiro_test(ValuesDomain, MediatorDomain, FulfillmentDomain) %>% arrange(variable)

ggqqplot(dfWk4a_new, "ValuesDomain", facet.by = "CollarColor",
         ylab = "Values Domain", ggtheme = theme_light())
ggqqplot(dfWk4a_new, "MediatorDomain", facet.by = "CollarColor",
         ylab = "Mediator Domain", ggtheme = theme_light())
ggqqplot(dfWk4a_new, "FulfillmentDomain", facet.by = "CollarColor",
         ylab = "Fulfillment Domain", ggtheme = theme_light())

# Check multivariate normality
dfWk4a_new %>%
  dplyr::select(ValuesDomain, MediatorDomain, FulfillmentDomain) %>%
  mshapiro_test()

dfWk4a_new %>% cor_test(ValuesDomain, MediatorDomain, FulfillmentDomain)
dep_vars2 <- c("ValuesDomain", "MediatorDomain", "FulfillmentDomain")
dfWk4a_new_dep <- dfWk4a_new[dep_vars2]
dfWk4a_new %>% cor_mat(ValuesDomain, MediatorDomain, FulfillmentDomain)

# Create scatterplot matrix
results2 <- dfWk4a_new %>% 
  dplyr::select(ValuesDomain, MediatorDomain, FulfillmentDomain, CollarColor, Gender) %>% 
  group_by(CollarColor) %>%
  doo(~ggpairs(.) + theme_light(), result = "plots")
results2
results2$plots

levenes.test(dfWk4a_new$ValuesDomain, dfWk4a_new$CollarColor)
levenes.test(dfWk4a_new$ValuesDomain, dfWk4a_new$Gender)

# Factorial Manova
attach(dfWk4a_new)
y <- cbind(dfWk4a_new$ValuesDomain, dfWk4a_new$MediatorDomain, dfWk4a_new$FulfillmentDomain)
summary(model_b)

manova_f <- Manova(model_b, test.statistic = "Pillai")
manova_f2 <- stats::manova(y ~ dfWk4a_new$CollarColor + dfWk4a_new$Gender + dfWk4a_new$CollarColor*dfWk4a_new$Gender)
summary(manova_f2)
summary.aov(manova_f2)

wk4_dda1 <- desDA(y, dfWk4a_new$CollarColor)
wk4_dda1

wk4_dda2 <- desDA(y, dfWk4a_new$CollarColor, covar = "total")
wk4_dda2

dfWk4a_new$f1 = wk4_dda1$scores[,1]
dfWk4a_new$f2 = wk4_dda1$scores[,2]
ggplot(data=dfWk4a_new, aes(x=f1, y=f2, colour=CollarColor)) +
  geom_hline(yintercept=0, colour="gray70") +
  geom_vline(xintercept=0, colour="gray70") +
  geom_text(aes(label=y), size=4)

#make this example reproducible
set.seed(42)

#Use 70% of dataset as training set and remaining 30% as testing set
sample <- sample(c(TRUE, FALSE), nrow(dfWk4a_new), replace=TRUE, prob=c(0.7,0.3))
train <- dfWk4a_new[sample, ]
test <- dfWk4a_new[!sample, ] 

model_c <- lda(CollarColor~., data=train)
model_c

#use LDA model to make predictions on test data
predicted <- predict(model_c, test)

names(predicted)
head(predicted$class)
head(predicted$posterior)
head(predicted$x)
mean(predicted$class==test$CollarColor)

#define data to plot
lda_plot <- cbind(train, predict(model_c)$x)

#create plot
ggplot(lda_plot, aes(LD1, LD2)) +
  geom_point(aes(color = CollarColor))
ggplot(lda_plot, aes(LD1, LD3)) +
  geom_point(aes(color = CollarColor))
ggplot(lda_plot, aes(LD1, LD4)) +
  geom_point(aes(color = CollarColor))
ggplot(lda_plot, aes(LD2, LD3)) +
  geom_point(aes(color = CollarColor))
ggplot(lda_plot, aes(LD2, LD4)) +
  geom_point(aes(color = CollarColor))
ggplot(lda_plot, aes(LD3, LD4)) +
  geom_point(aes(color = CollarColor))
