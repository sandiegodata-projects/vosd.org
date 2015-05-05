d = read.csv('/Users/damoncrockett/vosd.org/215-opendsd/tables/fees_by_CPA_year.csv')
p = read.csv('/Users/damoncrockett/vosd.org/215-opendsd/tables/by_CPA_parcels.csv')

library(plyr)

bc = ddply(d[d$CPA!='nan',],c('CPA'),summarise,
      fees = sum(TotalAmount))

p$fees = bc$fees
p$fpp = p$fees/p$parcels

pp = p[p$parcels > 9999,]

library(ggplot2)
ggplot(pp,aes(parcels,fees)) + geom_point() + stat_smooth(method=lm) + geom_text(aes(label=community_name))
ggplot(pp,aes(asr_total,fpp)) + geom_point() + stat_smooth(method=lm) + geom_text(aes(label=community_name))
ggplot(pp,aes(asr_land,fpp)) + geom_point() + stat_smooth(method=lm) + geom_text(aes(label=community_name))
ggplot(pp,aes(asr_impr,fpp)) + geom_point() + stat_smooth(method=lm) + geom_text(aes(label=community_name))

attach(p)
cor.test(parcels,fees)
cor.test(asr_total,fpp)
cor.test(asr_land,fpp)
cor.test(asr_impr,fpp)

ggplot(pp,aes(lon,lat,size=asr_total,color=fpp)) + geom_point() +
  geom_text(aes(label=community_name),size=15,color='white') +
  scale_size_continuous(range=c(75,225)) +
  theme(panel.background = element_rect(fill = "black"),
        panel.grid.major = element_line(color = "black"),
        panel.grid.minor = element_line(color = "black"),
        plot.background = element_rect(fill = "black"),
        legend.position="none",
        axis.text = element_text(size=rel(0.7), color='white'),
        text = element_text(face='plain', color='white', size = 15))

write.csv(pp,'/Users/damoncrockett/vosd.org/215-opendsd/tables/major_cpa.csv')

ps = read.csv('/Users/damoncrockett/vosd.org/215-opendsd/tables/number_of_parcels_CPA.csv')
d$parcels = d$CPA
d$parcels = mapvalues(d$parcels,ps$community_name,ps$parcels)

d$parcels = as.numeric(as.character(d$parcels))
dp = subset(d,d$parcels > 9999)

write.csv(dp,'/Users/damoncrockett/vosd.org/215-opendsd/tables/major_CPA_year.csv')


profiles = read.csv('/Users/damoncrockett/vosd.org/215-opendsd/tables/CPA_profiles_2.csv')











































