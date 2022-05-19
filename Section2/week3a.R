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

# Bring in the csv file for use
df1 <- read_csv("D:\\8525\\Section2\\TIM8525.csv")
df2 <- read_csv("D:\\8525\\Section2\\TIM8525.csv")
df1 <- read_csv("C:\\Users\\tony.dunsworth\\OneDrive - City of Alexandria\\GitHub\\TIM8525\\Section2\\TIM8525.csv")

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

# Column Removal
# Remove CountryOther since it has many missing values
df1 <- subset(df1, select = -c(CountryOther))
# Remove na values
df1 <- na.omit(df1)
# Remove rows where the respondent gave the same value for all of the questions
df1 <- df1[!(df1$ZeroVar == 1), ]

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

# Outlier Identification
medV = median(df1$ValuesDomain)
# subtract median from each value of x and get absolute deviation
abs_devV = abs(df1$ValuesDomain-medV)
# get MAD
madV = 1.4826 * median(abs_devV)

# get threshold values for outliers
TminV = medV-(3*madV) 
TmaxV = medV+(3*madV) 

# find outlier
valuesOutlier <- df1$ValuesDomain[which(df1$ValuesDomain < TminV | df1$ValuesDomain > TmaxV)]

min(valuesOutlier)
valuesOutlier

medM = median(df1$MediatorDomain)
# subtract median from each value of x and get absolute deviation
abs_devM = abs(df1$MediatorDomain-medM)
# get MAD
madM = 1.4826 * median(abs_devM)

# get threshold values for outliers
TminM = medM-(3*madM) 
TmaxM = medM+(3*madM) 

# find outlier
mediatorOutlier <- df1$MediatorDomain[which(df1$MediatorDomain < TminM | df1$MediatorDomain > TmaxM)]

min(mediatorOutlier)
mediatorOutlier

medF = median(df1$FulfillmentDomain)
# subtract median from each value of x and get absolute deviation
abs_devF = abs(df1$FulfillmentDomain-medF)
# get MAD
madF = 1.4826 * median(abs_devF)

# get threshold values for outliers
TminF = medF-(3*madF) 
TmaxF = medF+(3*madF) 

# find outlier
fulfillmentOutlier <- df1$FulfillmentDomain[which(df1$FulfillmentDomain < TminF | df1$FulfillmentDomain > TmaxF)]

min(fulfillmentOutlier)
fulfillmentOutlier

# Remove outliers from the dataset. 

df1 <- subset(df1, df1$ValuesDomain > 3.875)
df1 <- subset(df1, df1$MediatorDomain > 4.25)
df1 <- subset(df1, df1$FulfillmentDomain >= 4.28)

# Normalcy Test
shapiro_test(df1$ValuesDomain)
shapiro_test(df1$MediatorDomain)
shapiro_test(df1$FulfillmentDomain)


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

# Establish the number of respondents per Collar Colour
df1 %>% group_by(CollarColor) %>% summarise(N = n())

# Indetify univariate outliers
df1 %>% group_by(CollarColor) %>% identify_outliers(ValuesDomain)
df1 %>% group_by(CollarColor) %>% identify_outliers(MediatorDomain)
df1 %>% group_by(CollarColor) %>% identify_outliers(FulfillmentDomain)

# Attempt to detect multivariate outliers - Not working directly from here. Will have to find another way to run this later.
df1 %>%
  group_by(CollarColor) %>%
  mahalanobis_distance(-ResponseId) %>%
  filter(is.outlier == TRUE) %>%
  as.data.frame()

# Check normality assumptions
df1 %>% group_by(CollarColor) %>% shapiro_test(ValuesDomain, MediatorDomain, FulfillmentDomain) %>% arrange(variable)

ggqqplot(df1, "ValuesDomain", facet.by = "CollarColor",
         ylab = "Values Domain", ggtheme = theme_light())
ggqqplot(df1, "MediatorDomain", facet.by = "CollarColor",
         ylab = "Mediator Domain", ggtheme = theme_light())
ggqqplot(df1, "FulfillmentDomain", facet.by = "CollarColor",
         ylab = "Fulfillment Domain", ggtheme = theme_light())

# Check multivariate normality
df1 %>%
  select(ValuesDomain, MediatorDomain, FulfillmentDomain) %>%
  mshapiro_test()

df1 %>% cor_test(ValuesDomain, MediatorDomain, FulfillmentDomain)
dep_vars <- c("ValuesDomain", "MediatorDomain", "FulfillmentDomain")
df1_dep <- df1[dep_vars]
df1 %>% cor_mat(ValuesDomain, MediatorDomain, FulfillmentDomain)

# Create scatterplot matrix
results <- df1 %>% 
  select(ValuesDomain, MediatorDomain, FulfillmentDomain, CollarColor) %>% 
  group_by(CollarColor) %>%
  doo(~ggpairs(.) + theme_light(), result = "plots")
results
results$plots

# Homogenity of variance
df1 %>% 
  gather(key="variable", value = "value", ValuesDomain, MediatorDomain, FulfillmentDomain) %>%
  group_by(variable) %>%
  levene_test(value ~ CollarColor)

# MANOVA Test
model <- lm(cbind(ValuesDomain, MediatorDomain, FulfillmentDomain) ~ CollarColor, df1)
Manova(model, test.statistic = "Pillai") 

# Posthoc ANOVA two ways
grouped.data <- df1 %>% 
  gather(key="variable", value = "value", ValuesDomain, MediatorDomain, FulfillmentDomain) %>%
  group_by(variable)

# Welch One Way ANOVA
grouped.data %>% welch_anova_test(value ~ CollarColor)

# Kruskal Wallace Test
grouped.data %>% kruskal_test(value ~ CollarColor)

# Multiple Pair Wise Comparison
pwc <- df1 %>%
  gather(key = "variables", value = "value", ValuesDomain, MediatorDomain, FulfillmentDomain) %>%
  group_by(variables) %>%
  games_howell_test(value ~ CollarColor) %>%
  select(-estimate, -conf.low, -conf.high) # Remove details
pwc

pwc <- pwc %>% add_xy_position(x = "CollarColor")
test.label <- create_test_label(
  description = "MANOVA", statistic.text = quote(italic("F")),
  statistic = 3.94, p= "<0.0001", parameter = "12,3720",
  type = "expression", detailed = TRUE
)
ggboxplot(
  df1, x = "CollarColor", y = c("ValuesDomain", "MediatorDomain", "FulfillmentDomain"), 
  merge = TRUE, palette = "lancet"
) + 
  stat_pvalue_manual(
    pwc, hide.ns = TRUE, y.position = 0.15, 
    step.increase = 0.2, step.group.by = "variables",
    color = "variables"
  ) +
  labs(
    subtitle = test.label,
    caption = get_pwc_label(pwc, type = "expression")
  )

# Multivariate Kruskal-Wallis
df1mkw <- subset(df1, select = c(ValuesDomain, MediatorDomain, FulfillmentDomain))
multkw(y=df1mkw, df1$CollarColor)
