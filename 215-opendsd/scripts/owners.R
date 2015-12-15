d = read.csv('owners.csv')
d$count = NULL
d$int = c(1:50)
library(ggplot2)
ggplot(d,aes(TotalAmount,asr_impr)) + geom_text(aes(label=d$int),size=5)

cor.test(d$TotalAmount,d$asr_impr)

####owners_tot
df = read.csv('owners_tot.csv')
cor.test(df$TotalAmount,df$asr_impr)
ggplot(df,aes(TotalAmount,asr_impr)) + geom_point()
