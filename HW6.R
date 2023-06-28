zinc.data = read.table("http://deanvossdraguljic.ietsandbox.net/DeanVossDraguljic/R-data/zinc.plating.txt", header = T)
xmean = mean(zinc.data$x)
zinc.data = within(zinc.data, {fV = factor(vendor); B = x - xmean})
options(contrasts = c("contr.sum", "contr.poly"))
modelzinc = lm(y ~ fV + B, data = zinc.data)
summary(modelzinc)
drop1(modelzinc, ~., test="F")
anova(modelzinc)

zinc.data = within(zinc.data, {pred=fitted(modelzinc); e=resid(modelzinc);z=e/sd(e); n=length(e); q=rank(e); nscore=qnorm((q-0.375)/(n+0.25))})
plot(z ~ x, data=zinc.data); abline(h=0)
plot(z ~ pred, data=zinc.data); abline(h=0)
plot(z ~ vendor, data=zinc.data); abline(h=0)
plot(z ~ nscore, data=zinc.data); qqline(zinc.data$z)

model2 = lm(formula = y ~ fV + B + fV:B, data=zinc.data)
anova(modelzinc, model2)

model3 = lm(formula = y ~ fV, data=zinc.data)
anova(modelzinc, model3)

zincnovamodel = lm(y ~ fV, data = zinc.data)
anova(zincnovamodel)
summary(zincnovamodel)
emmD = emmeans(zincnovamodel, ~ fV)
summary(contrast(emmD, method = "pairwise", adjust = "Scheffe"), infer=c(T,T))


library(emmeans)
emmC = emmeans(modelzinc, ~ fV)
summary(contrast(emmC, method = "pairwise", adjust = "Scheffe"), infer=c(T,T))
