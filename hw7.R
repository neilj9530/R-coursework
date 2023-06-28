chemdata = read.table("http://deanvossdraguljic.ietsandbox.net/DeanVossDraguljic/R-data/chemical.txt", header = T)
trt = rep(1:8)
chemdata = within(chemdata, {fBlock = factor(block); fTrtmt = factor(trt)})
modelchem = lm(y ~ fBlock + fTrtmt, data = chemdata)
anova(modelchem)
summary(modelchem)

boxplot(y ~ block, data = chemdata, xlab = "Lab Block", ylab = "y")
boxplot(y ~ A, data = chemdata, xlab = "Acid Strength (80% and 90%)", ylab = "y")
boxplot(y ~ B, data = chemdata, xlab = "Reaction Time Allowed (15 and 30min)", ylab = "y")
boxplot(y ~ C, data = chemdata, xlab = "Temperature (50 and 70 Degrees Celsius)", ylab = "y")

library(emmeans)
emTrtmt = emmeans(modelchem, ~ fTrtmt)
summary(contrast(emTrtmt, method = "pairwise", infer = c(T,T), level = 0.99))

emBlock = emmeans(modelchem, ~ fBlock)
summary(contrast(emBlock, method = "pairwise", adjust = "scheffe", infer = c(T,T), level = 0.99))

modelchem2 = lm(y ~ fBlock + A + B + C, data = chemdata)
emAA = emmeans(modelchem2, ~ A)
summary(contrast(emAA, method = "pairwise", infer = c(T,T), level = 0.995))
emBB = emmeans(modelchem2, ~ B)
summary(contrast(emBB, method = "pairwise", infer = c(T,T), level = 0.995))
emCC = emmeans(modelchem2, ~ C)
summary(contrast(emCC, method = "pairwise", infer = c(T,T), level = 0.995))

anova(modelchem)
