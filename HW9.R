vgdata = read.table("http://deanvossdraguljic.ietsandbox.net/DeanVossDraguljic/R-data/video.game.txt", header = T)
vgdata = within(vgdata, {fRow = factor(order); fColumn = factor(day); fTrtmt = factor(trtmt)})

modelVG = lm(y ~ fRow + fColumn + fTrtmt, data = vgdata)
anova(modelVG)
summary(modelVG)

library(emmeans)
emVG = emmeans(modelVG, ~ fTrtmt)
summary(contrast(emVG, method = "pairwise", infer = c(T,T), level = 0.95))

vgdata = within(vgdata, {ypred = fitted(modelVG); e = resid(modelVG); z = e/sd(e); n = length(e); q = rank(e); nscore = qnorm((q-0.375)/(n+0.25))})
plot(z ~ ypred + fRow + fColumn + nscore, data = vgdata)
plot(y ~ ypred, data = vgdata)

summary(contrast(emVG, list(MvNM=c(1/3, 1/3, 1/3, -1/2, -1/2))), infer=c(T,T), level = 0.95)
summary(contrast(emVG, list(SvNS=c(1/4, 1/4, 1/4, 1/4, -1))), infer=c(T,T), level = 0.95)
