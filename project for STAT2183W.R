sentence <- c(24, 33.5, 25.5, 18, 18.5, 44.5, 38.5, 50.5, 12.5, 52, 20)
served <- c(8.75, 6.5, 6.5, 12.5, 11, 14.5, 20, 22, 1, 10.75, 1.5)
plea <- c("N","N","G","N","G","N","N","N","G","G","G")

crime.data <- data.frame(sentence, served, plea)
str(crime.data)
guilty.data<-subset(crime.data, crime.data$plea=="G")
notguilty.data<-subset(crime.data, crime.data$plea=="N")

wilcox.test(guilty.data$sentence, notguilty.data$sentence, alternative = "less")

wilcox.test(guilty.data$served, notguilty.data$served, alternative = "less")

var.test(guilty.data$sentence, notguilty.data$sentence)

var.test(guilty.data$served, notguilty.data$served)

