str_weld = read.table("http://deanvossdraguljic.ietsandbox.net/DeanVossDraguljic/R-data/weld.strength.txt", header = TRUE)

str_weld = within(str_weld, {10*gage + time; fTC = factor(Trtmt); fA = factor(gage); fB = factor(time)})
summary(str_weld[,c("fA","fB","fTC","strength")])

options(contrasts = c("contr.sum","contr.poly"))
modelAB = aov(strength ~ fA + fB + fA:fB, data = str_weld)
anova(modelAB)

library(emmeans)
emAB = emmeans(modelAB, ~ fA:fB, CL = 0.98)
emAB

confint(emAB, level = 0.98)

summary(contrast(emAB, method="pairwise", adjust="tukey"), infer=c(T,T), level = 0.98)

emA = emmeans(modelAB, ~ fA)
confint(emA, level = 0.98)

summary(contrast(emA, method = "trt.vs.ctrl", adj="mvt", ref=1), infer=c(T,T), level = 0.98)

summary(contrast(emA, list(A3av12 =c(-1/2, -1/2, 1))), infer=c(T,T), level = 0.98)
