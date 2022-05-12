dim(df1)

ggscatter(
  df1, x="ValuesDomain", y="FulfillmentDomain",
  facet.by = c("Age", "Employment"),
  short.panel.labs = FALSE
)+
  stat_smooth(method="loess", span=0.9)

df1 %>%
  anova_test(
    FulfillmentDomain ~ MediatorDomain + Employment + Age + 
      Employment*Age + MediatorDomain*Employment +
      MediatorDomain*Age + MediatorDomain*Age*Employment
  )

model2 <- lm(FulfillmentDomain ~ MediatorDomain + Age*Employment, data=df1)
model2.metrics <- augment(model) %>%
  select(-.hat, -.sigma, -.fitted)
head(model2.metrics)
shapiro_test(model2.metrics$.resid)

model2.metrics %>% 
  filter(abs(.std.resid) > 3) %>%
  as.data.frame()

res.aov <- df1 %>% 
  anova_test(FulfillmentDomain ~ MediatorDomain + Employment*Age)
get_anova_table(res.aov)

df1 %>%
  group_by(Age) %>%
  anova_test(FulfillmentDomain ~ MediatorDomain + Employment)

pwc <- df1 %>% 
  group_by(Age) %>%
  emmeans_test(
    FulfillmentDomain ~ Employment, covariate = MediatorDomain,
    p.adjust.method = "bonferroni"
  )
pwc %>% filter(Age == 3)

df1 %>%
  group_by(Employment) %>%
  anova_test(FulfillmentDomain ~ MediatorDomain + Age)

lp <- ggline(
  get_emmeans(pwc), x = "Age", y = "emmean", 
  color = "Employment", palette = "jco"
) +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high, color = Employment), 
    width = 0.1
  )

pwc <- pwc %>% add_xy_position(x = "Age", fun = "mean_se", step.increase = 0.2)
pwc.filtered <- pwc %>% filter(Age == 3)
lp + 
  stat_pvalue_manual(
    pwc.filtered, hide.ns = TRUE, tip.length = 0,
    bracket.size = 0
  ) +
  labs(
    subtitle = get_test_label(res.aov,  detailed = TRUE),
    caption = get_pwc_label(pwc)
  )