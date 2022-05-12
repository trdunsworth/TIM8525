dim(df1)

#create a bar chart
bar <- ggplot(df1, aes(Gender, ValuesDomain))
bar + stat_summary(fun.y = mean, geom = "bar", position ="dodge") + stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width = 0.90), width = 0.2) + labs(x = "Gender", y = "Values Score")

bar <- ggplot(df1, aes(Gender, MediatorDomain))
bar + stat_summary(fun.y = mean, geom = "bar", position ="dodge") + stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width = 0.90), width = 0.2) + labs(x = "Gender", y = "Mediator Score")

bar <- ggplot(df1, aes(Gender, FulfillmentDomain))
bar + stat_summary(fun.y = mean, geom = "bar", position ="dodge") + stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width = 0.90), width = 0.2) + labs(x = "Gender", y = "Fulfillment Score")


#Testing normality with a Q-Q plot
qqplot.time <- qplot(sample = df1$ValuesDomain, stat="qq") 
qqplot.time

#Testing normality with a Q-Q plot
qqplot.time <- qplot(sample = df1$MediatorDomain, stat="qq") 
qqplot.time

#Testing normality with a Q-Q plot
qqplot.time <- qplot(sample = df1$FulfillmentDomain, stat="qq") 
qqplot.time

#Histogram
myhistogram <- ggplot(df1, aes(FulfillmentDomain))
myhistogram + geom_histogram(aes(y = ..density..))
hist_values <- ggplot(df1, aes(FulfillmentDomain)) + theme(legend.position = "none") + geom_histogram(aes(y = ..density..), colour = "black", fill = "white") + labs(x="Values Score", y = "Density")
hist_values

#Create histograms for MediatorDomain and FulfillmentDomain: write code below.




#Creating separate dataframes for males and females and creating histograms
males<-subset(df1, Gender == 1)
females<-subset(df1, Gender == 2)

fullTime<-subset(df1, Employment == 1)
partTime<-subset(df1, Employment == 2)


hist.males <- ggplot(males, aes(FulfillmentDomain)) + geom_histogram(aes(y = ..density..), fill = "white", colour = "black", binwidth = 1) + labs(x = "Fulfillment Score", y = "Density") + stat_function(fun=dnorm, args=list(mean =  mean(males$FulfillmentDomain, na.rm = TRUE), sd = sd(males$FulfillmentDomain, na.rm = TRUE)), colour = "blue", size=1)
hist.males

hist.females <- ggplot(females, aes(FulfillmentDomain)) + geom_histogram(aes(y = ..density..), fill = "white", colour = "black", binwidth = 1) + labs(x = "Fulfillment Score", y = "Density") + stat_function(fun=dnorm, args=list(mean =  mean(females$FulfillmentDomain, na.rm = TRUE), sd = sd(females$FulfillmentDomain, na.rm = TRUE)), colour = "blue", size=1)
hist.females

hist.ft <- ggplot(fullTime, aes(FulfillmentDomain)) + geom_histogram(aes(y = ..density..), fill = "white", colour = "black", binwidth = 1) + labs(x = "Fulfillment Score", y = "Density") + stat_function(fun = dnorm, args = list(mean = mean(fullTime$FulfillmentDomain, na.rm = TRUE), sd = sd(fullTime$FulfillmentDomain, na.rm = TRUE)), colour = "blue", size=1)
hist.ft

hist.pt <- ggplot(partTime, aes(FulfillmentDomain)) + geom_histogram(aes(y = ..density..), fill = "white", colour = "black", binwidth = 1) + labs(x = "Fulfillment Score", y = "Density") + stat_function(fun = dnorm, args = list(mean = mean(partTime$FulfillmentDomain, na.rm = TRUE), sd = sd(partTime$FulfillmentDomain, na.rm = TRUE)), colour = "blue", size=1)
hist.pt

#Scatterplot
scatterplot(df1$ValuesDomain, df1$MediatorDomain)
scatterplot(df1$ValuesDomain, df1$FulfillmentDomain)
scatterplot(df1$FulfillmentDomain, df1$MediatorDomain)


#Testing statistical assumptions for parametric tests

#Shapiro-Wilk tests for normality
shapiro.test(df1$ValuesDomain)
by(df1$ValuesDomain, df1$Gender, shapiro.test)
by(df1$ValuesDomain, df1$Employment, shapiro.test)

shapiro.test(df1$FulfillmentDomain)
by(df1$FulfillmentDomain, df1$Age, shapiro.test)
by(df1$FulfillmentDomain, df1$Employment, shapiro.test)

#Create Shapiro-Wilk tests for the MediatorDomain and FulfillmentDomain variables: write your code below.


#Compute descritive statistics
describeBy(df1$ValuesDomain, df1$Gender)
describeBy(df1$FulfillmentDomain, df1$Employment)
describeBy(df1$FulfillmentDomain, df1$Age)

#Converting gender and employment variables to factors

df1$Gender <- as.factor(df1$Gender)
df1$Employment <- as.factor(df1$Employment)
df1$Age <- as.factor(df1$Age)
df1$Country <- as.factor(df1$Country)
df1$Education <- as.factor(df1$Education)
df1$CollarColor <- as.factor(df1$CollarColor)

#Levene's test for homogeneity of variance
leveneTest(df1$FulfillmentDomain, df1$Gender)
leveneTest(df1$FulfillmentDomain, df1$Employment)
leveneTest(df1$FulfillmentDomain, df1$Country)
leveneTest(df1$FulfillmentDomain, df1$CollarColor)
leveneTest(df1$FulfillmentDomain, df1$Education)
leveneTest(df1$FulfillmentDomain, df1$Age)


#Testing homogeneity of regression slopes (assuming Durationseconds is the covariate)
RegressionGender <- aov(df1$FulfillmentDomain ~ df1$MediatorDomain*df1$Gender)
summary(RegressionGender)

RegressionEmployment <- aov(df1$FulfillmentDomain ~ df1$MediatorDomain*df1$Employment)
summary(RegressionEmployment)

RegressionAge <- aov(df1$FulfillmentDomain ~ df1$MediatorDomain*df1$Age)
summary(RegressionAge)

df1$Gender <- as.factor(df1$Gender)
df1$Employment <- as.factor(df1$Employment)
df1$Age <- as.factor(df1$Age)

#View the results of the homogeneity of regression slopes
ancova(FulfillmentDomain ~ MediatorDomain + Gender, data=df1)
ancova(FulfillmentDomain ~ MediatorDomain + Employment, data=df1)
ancova(FulfillmentDomain ~ MediatorDomain + Age, data=df1)

#Run hypothesis/significance tests

#Factorial ANCOVAs
FactorialModel<-aov(FulfillmentDomain ~ Employment + Age + Employment*Age + MediatorDomain, data = df1)
Anova(FactorialModel, type="III")


#Plot results: hit enter when prompted, may be up to four prompts
plot(FactorialModel)

