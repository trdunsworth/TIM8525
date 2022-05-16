vd <- df1$ValuesDomain
md <- df1$MediatorDomain
fd <- df1$FulfillmentDomain

res.man1 <- manova(cbind(ValuesDomain, MediatorDomain, FulfillmentDomain) ~ CollarColor, data = df1)
summary(res.man1)

summary.aov(res.man1)
