dd = read.csv('approvals_parcels_CPA.csv')
str(dd)

library(ggplot2)
ggplot(dd,aes(asr_total,TimetoIssue)) + geom_point() +
  stat_smooth()

with(dd,cor.test(asr_total,TimetoIssue))

tti.cpa.lm = lm(data=dd, TimetoIssue~CPA)
summary(tti.cpa.lm)

tmp = rle(sort(c(as.character(dd$CPA))))
dd$CPA_count = tmp[[1]][match(dd$CPA, tmp[[2]])]

d = subset(dd,dd$CPA_count > 9000)
d$CPA = droplevels(d$CPA)

dD = subset(d,d$category=='Discretionary')
table(dD$CPA)

tti.cpa.lm.dD = lm(data=dD, TimetoIssue~CPA)
summary(tti.cpa.lm.dD)
ggplot(dD,aes(CPA,TimetoIssue)) + geom_boxplot()
ggplot(dD,aes(Longitude,Latitude,color=CPA)) + geom_point()

library(plyr)
tmp = dD[c('community_name','Latitude','Longitude','TimetoIssue')]
tmp_by_CPA = ddply(na.omit(tmp),c('community_name'), summarise,
                   median = median(TimetoIssue),
                   lat = median(Latitude),
                   lon = median(Longitude))
    
tmp_by_CPA = tmp_by_CPA[-1,]


p = read.csv('/Users/damoncrockett/vosd.org/215-opendsd/tables/by_CPA_parcels.csv')
ps = subset(p,p$parcels > 9000)

tmp_by_CPA$community_name = droplevels(tmp_by_CPA$community_name)
levels(tmp_by_CPA$community_name)

# okay this is awful hard-coding here...but R sucks for joins
tmp_by_CPA$val = c(492951,
                   220000,
                   300000,
                   227546,
                   354980,
                   646860,
                   245725,
                   197082,
                   217163,
                   295000,
                   320759,
                   327638.5,
                   288537,
                   125806,
                   266945,
                   320216)


ggplot(tmp_by_CPA[tmp_by_CPA$community_name!='Downtown',],
       aes(lon,lat,color=median,size=val)) + 
  geom_point() +
  geom_text(data=tmp_by_CPA[tmp_by_CPA$community_name!='Downtown',]
            ,aes(label=community_name),color='white',size=5) +
  scale_color_continuous(low='green',high='red') +
  scale_size_continuous(range=c(15,75)) +
  theme(panel.background = element_rect(fill = "black"),
        panel.grid.major = element_line(color = "black"),
        panel.grid.minor = element_line(color = "black"),
        plot.background = element_rect(fill = "black"),
        legend.position="none",
        axis.text = element_text(size=rel(0.7), color='white'),
        text = element_text(face='plain', color='white', size = 15))

