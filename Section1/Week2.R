df1 <- read_csv("D:\\8525\\Section1\\TIM8525.csv")

Employment <- as.factor(Employment)
Country <- as.factor(Country)
Gender <- as.factor(Gender)
Age <- as.factor(Age)
Education <- as.factor(Education)
CollarColor <- as.factor(CollarColor)
summary(df1)
dim(df1)
str(df1)

summary(ValuesDomain)
summary(MediatorDomain)
summary(FulfillmentDomain)
summary(Durationinseconds)

describe(ValuesDomain)
describe(MediatorDomain)
describe(FulfillmentDomain)
describe(Durationinseconds)


hist(Durationinseconds)
boxplot(Durationinseconds)
qqnorm(Durationinseconds)

df1 <- subset(df1, select = -c(CountryOther))

df1 <- na.omit(df1)

df1 <- df1[!(df1$ZeroVar == 1), ]

med = median(df1$Durationinseconds)
# subtract median from each value of x and get absolute deviation
abs_dev = abs(df1$Durationinseconds-med)
# get MAD
mad = 1.4826 * median(abs_dev)

# get threshold values for outliers
Tmin = med-(3*mad) 
Tmax = med+(3*mad) 

# find outlier
durationOutlier <- df1$Durationinseconds[which(df1$Durationinseconds < Tmin | df1$Durationinseconds > Tmax)]

min(durationOutlier)

df1 <- df1[(df1$Durationinseconds < 519),]

dim(df1)

sum(df1$ZeroVar == 1)

describe(df1$ValuesDomain)
describe(df1$MediatorDomain)
describe(df1$FulfillmentDomain)
describe(df1$Durationinseconds)

summary(df1$FulfillmentDomain)
summary(df1$MediatorDomain)
summary(df1$ValuesDomain)
summary(df1$Durationinseconds)

create_report(df1)

IQR(df1$Durationinseconds)
IQR(df1$ValuesDomain)
IQR(df1$FulfillmentDomain)
IQR(df1$MediatorDomain)

plot(as.factor(df1$Employment))
plot(as.factor(df1$Age))

plot(FulfillmentDomain ~ as.factor(df1$Employment), data=df1)
plot(FulfillmentDomain ~ as.factor(df1$Age), data=df1)

count(df1, 'Employment')
count(df1, 'Age')

ggscatter(
  df1, x = "FulfillmentDomain", y = "ValuesDomain",
  color = "CollarColor", add = "reg.line"
)+
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~"), color = CollarColor)
  )

df1 %>% anova_test(ValuesDomain ~ CollarColor*FulfillmentDomain)
df1 %>% anova_test(FulfillmentDomain ~ CollarColor*ValuesDomain)

ggscatter(
  df1, x="FulfillmentDomain", y="ValuesDomain",
  color="Education", add="reg.line"
)+
  stat_regline_equation(
    aes(label=paste(..eq.label.., ..rr.label.., sep="~~~~"), color=Education)
  )

df1 %>% anova_test(ValuesDomain ~ Education*FulfillmentDomain)
df1 %>% anova_test(FulfillmentDomain ~ Education*ValuesDomain)

model <- lm(ValuesDomain ~ FulfillmentDomain + Education, data = df1)

model.metrics <- augment(model) %>%
  select(-.hat, -.sigma, -.fitted) 
head(model.metrics, 3)

shapiro_test(model.metrics$.resid)

model.metrics %>% levene_test(.resid ~ as.factor(Education))

model.metrics %>% 
  filter(abs(.std.resid) > 3) %>%
  as.data.frame()

res.aov <- df1 %>% anova_test(ValuesDomain ~ FulfillmentDomain + Education)
get_anova_table(res.aov)

pwc <- df1 %>% 
  emmeans_test(
    FulfillmentDomain ~ Education, covariate = ValuesDomain,
    p.adjust.method = "bonferroni"
  )
pwc
get_emmeans(pwc)
