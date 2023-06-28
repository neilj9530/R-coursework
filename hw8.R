lightdata = read.table("http://deanvossdraguljic.ietsandbox.net/DeanVossDraguljic/R-data/light.bulb.txt", header = T)
lightdata$trtmt = rep(1:6, 8)
lightdata$trtmt = as.factor(lightdata$trtmt)
lightmodel = lm(resist ~ block + trtmt + block*trtmt, data = lightdata)
anova(lightmodel)
summary(lightmodel)

library(emmeans)
lightX = emmeans(lightmodel, ~ trtmt + block)
summary(contrast(lightX, method = "pairwise", infer = c(T,T), level = 0.99))

lightX2 = emmeans(lightmodel, ~ trtmt)
summary(contrast(lightX2, method = "pairwise", infer = c(T,T), level = 0.995))

summary(lightmodel)
