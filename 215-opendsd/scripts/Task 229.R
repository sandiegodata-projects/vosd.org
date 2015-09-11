setwd("~/vosd.org/215-opendsd")

df = read.csv('approval_master.csv')

anova(lm(data=df, TimetoIssue ~ factor(category)))
summary(lm(data=df, TimetoIssue ~ factor(category)))

anova(lm(data=df, TimetoIssue ~ factor(ApplicationYear)))
summary(lm(data=df, TimetoIssue ~ factor(ApplicationYear)))

anova(lm(data=df, TimetoIssue ~ factor(category) + factor(ApplicationYear)))

anova(lm(data=df, TimetoIssue ~ factor(ApplicationMonth)))
summary(lm(data=df, TimetoIssue ~ factor(ApplicationMonth)))


discretionary = subset(df, df$category == 'Discretionary') 
pm_count = rle(sort(c(as.character(discretionary$ProjectManager))))
discretionary$pm_count = pm_count[[1]][match(discretionary$ProjectManager, pm_count[[2]])]

discretionary_vets = subset(discretionary, discretionary$pm_count > 100) 

anova(lm(data=discretionary_vets, TimetoIssue ~ factor(ProjectManager)))
summary(lm(data=discretionary_vets, TimetoIssue ~ factor(ProjectManager)))


anova(lm(data=discretionary_vets, TimetoIssue ~ factor(Type) + factor(ApplicationYear) + factor(ProjectManager)))
summary(lm(data=discretionary_vets, TimetoIssue ~ factor(Type) + factor(ApplicationYear) + factor(ProjectManager)))






###################
## Bldg Maps ######
###################


bau = subset(df, df$Type == 'Building Permit')
bau = bau[c('ProjectId','TimetoIssue','Latitude','Longitude','ApplicationYear','IssueYear')]

df = subset(df, df$Longitude < -116.8)
bau = subset(bau, bau$Longitude < -116.8)


#length(unique(df$ProjectId))
#rev(sort(table(df$ProjectId)))[1:10]

#big.project = subset(df, df$ProjectId == 301767)
#big.project

project_count = rle(sort(c(as.character(bau$ProjectId))))
bau$project_count = project_count[[1]][match(bau$ProjectId, project_count[[2]])]



library(plyr)


north.south = bau
north.south['lat.bin'] = cut(north.south$Latitude, 2)

by.loc.ns = ddply(north.south, c('lat.bin'), 
                  summarize,
                  TimetoIssue = median(TimetoIssue),
                  Latitude = mean(Latitude),
                  Longitude = mean(Longitude),
                  Count = mean(project_count))



east.west = bau
east.west['lon.bin'] = cut(east.west$Longitude, 2)

by.loc.ew = ddply(east.west, c('lon.bin'), 
                  summarize,
                  TimetoIssue = median(TimetoIssue),
                  Latitude = mean(Latitude),
                  Longitude = mean(Longitude),
                  Count = mean(project_count))

four.bins = bau
four.bins['lat.bin'] = cut(four.bins$Latitude, 2)
four.bins['lon.bin'] = cut(four.bins$Longitude, 2)
four.bins['square'] = paste(as.character(four.bins$lat.bin), as.character(four.bins$lon.bin))

by.loc.4 = ddply(four.bins, c('square'), 
                  summarize,
                 TimetoIssue = median(TimetoIssue),
                 Latitude = mean(Latitude),
                 Longitude = mean(Longitude),
                 Count = mean(project_count))

eight.bins = bau
eight.bins['lat.bin'] = cut(eight.bins$Latitude, 4)
eight.bins['lon.bin'] = cut(eight.bins$Longitude, 2)
eight.bins['square'] = paste(as.character(eight.bins$lat.bin), as.character(eight.bins$lon.bin))

by.loc.8 = ddply(eight.bins, c('square'), 
                  summarize,
                 TimetoIssue = median(TimetoIssue),
                 Latitude = mean(Latitude),
                 Longitude = mean(Longitude),
                 Count = mean(project_count))

sixteen.bins = bau
sixteen.bins['lat.bin'] = cut(sixteen.bins$Latitude, 4)
sixteen.bins['lon.bin'] = cut(sixteen.bins$Longitude, 4)
sixteen.bins['square'] = paste(as.character(sixteen.bins$lat.bin), as.character(sixteen.bins$lon.bin))

by.loc.16 = ddply(sixteen.bins, c('square'), 
                  summarize,
                  TimetoIssue = median(TimetoIssue),
                  Latitude = mean(Latitude),
                  Longitude = mean(Longitude),
                  Count = mean(project_count))

thirtytwo.bins = bau
thirtytwo.bins['lat.bin'] = cut(thirtytwo.bins$Latitude, 8)
thirtytwo.bins['lon.bin'] = cut(thirtytwo.bins$Longitude, 4)
thirtytwo.bins['square'] = paste(as.character(thirtytwo.bins$lat.bin), as.character(thirtytwo.bins$lon.bin))

by.loc.32 = ddply(thirtytwo.bins, c('square'), 
                  summarize,
                  TimetoIssue = median(TimetoIssue),
                  Latitude = mean(Latitude),
                  Longitude = mean(Longitude),
                  Count = mean(project_count))

sixtyfour.bins = bau
sixtyfour.bins['lat.bin'] = cut(sixtyfour.bins$Latitude, 8)
sixtyfour.bins['lon.bin'] = cut(sixtyfour.bins$Longitude, 8)
sixtyfour.bins['square'] = paste(as.character(sixtyfour.bins$lat.bin), as.character(sixtyfour.bins$lon.bin))

by.loc.64 = ddply(sixtyfour.bins, c('square'), 
                  summarize,
                  TimetoIssue = median(TimetoIssue),
                  Latitude = mean(Latitude),
                  Longitude = mean(Longitude),
                  Count = mean(project_count))

onetwentyeight.bins = bau
onetwentyeight.bins['lat.bin'] = cut(onetwentyeight.bins$Latitude, 16)
onetwentyeight.bins['lon.bin'] = cut(onetwentyeight.bins$Longitude, 8)
onetwentyeight.bins['square'] = paste(as.character(onetwentyeight.bins$lat.bin), as.character(onetwentyeight.bins$lon.bin))

by.loc.128 = ddply(onetwentyeight.bins, c('square'), 
                  summarize,
                  TimetoIssue = median(TimetoIssue),
                  Latitude = mean(Latitude),
                  Longitude = mean(Longitude),
                  Count = mean(project_count))


twofiftysix.bins = bau
twofiftysix.bins['lat.bin'] = cut(twofiftysix.bins$Latitude, 16)
twofiftysix.bins['lon.bin'] = cut(twofiftysix.bins$Longitude, 16)
twofiftysix.bins['square'] = paste(as.character(twofiftysix.bins$lat.bin), as.character(twofiftysix.bins$lon.bin))

by.loc.256 = ddply(twofiftysix.bins, c('square'), 
                   summarize,
                   TimetoIssue = median(TimetoIssue),
                   Latitude = mean(Latitude),
                   Longitude = mean(Longitude),
                   Count = mean(project_count))



fivetwelve.bins = bau
fivetwelve.bins['lat.bin'] = cut(fivetwelve.bins$Latitude, 32)
fivetwelve.bins['lon.bin'] = cut(fivetwelve.bins$Longitude, 16)
fivetwelve.bins['square'] = paste(as.character(fivetwelve.bins$lat.bin), as.character(fivetwelve.bins$lon.bin))

by.loc.512 = ddply(fivetwelve.bins, c('square'), 
                   summarize,
                   TimetoIssue = median(TimetoIssue),
                   Latitude = mean(Latitude),
                   Longitude = mean(Longitude),
                   Count = mean(project_count))


tentwentyfour.bins = bau
tentwentyfour.bins['lat.bin'] = cut(tentwentyfour.bins$Latitude, 32)
tentwentyfour.bins['lon.bin'] = cut(tentwentyfour.bins$Longitude, 32)
tentwentyfour.bins['square'] = paste(as.character(tentwentyfour.bins$lat.bin), as.character(tentwentyfour.bins$lon.bin))

by.loc.1024 = ddply(tentwentyfour.bins, c('square'), 
                   summarize,
                   TimetoIssue = median(TimetoIssue),
                   Latitude = mean(Latitude),
                   Longitude = mean(Longitude),
                   Count = mean(project_count))


twentyfortyeight.bins = bau
twentyfortyeight.bins['lat.bin'] = cut(twentyfortyeight.bins$Latitude, 64)
twentyfortyeight.bins['lon.bin'] = cut(twentyfortyeight.bins$Longitude, 32)
twentyfortyeight.bins['square'] = paste(as.character(twentyfortyeight.bins$lat.bin), as.character(twentyfortyeight.bins$lon.bin))

by.loc.2048 = ddply(twentyfortyeight.bins, c('square'), 
                    summarize,
                    TimetoIssue = median(TimetoIssue),
                    Latitude = mean(Latitude),
                    Longitude = mean(Longitude),
                    Count = mean(project_count))






library(ggplot2)

ggplot(df, aes(x=Longitude, y=Latitude)) +
  geom_point(color='white',size=1) +
  scale_color_gradient(low = 'green', high = 'red') +
  theme(panel.background = element_rect(fill = "grey21"),
        panel.grid.major = element_line(color = "grey21"),
        panel.grid.minor = element_line(color = "grey21"),
        plot.background = element_rect(fill = "grey21"),
        legend.position="none",
        axis.text = element_text(size=rel(.7), color='white'),
        text = element_text(family='arial', face='plain', color='white', size = 14)) +
  labs(x='LONGITUDE',y='LATITUDE') +
  geom_point(data=by.loc.ns, aes(color=TimetoIssue), size=91, alpha = .42)



ggplot(df, aes(x=Longitude, y=Latitude)) +
  geom_point(color='white',size=1) +
  scale_color_gradient(low = 'green', high = 'red') +
  theme(panel.background = element_rect(fill = "grey21"),
        panel.grid.major = element_line(color = "grey21"),
        panel.grid.minor = element_line(color = "grey21"),
        plot.background = element_rect(fill = "grey21"),
        legend.position="none",
        axis.text = element_text(size=rel(.7), color='white'),
        text = element_text(family='arial', face='plain', color='white', size = 14)) +
  labs(x='LONGITUDE',y='LATITUDE') +
  geom_point(data=by.loc.ew, aes(color=TimetoIssue), size=63, alpha = .42)


ggplot(df, aes(x=Longitude, y=Latitude)) +
  geom_point(color='white',size=1) +
  scale_color_gradient(low = 'green', high = 'red') +
  theme(panel.background = element_rect(fill = "grey21"),
        panel.grid.major = element_line(color = "grey21"),
        panel.grid.minor = element_line(color = "grey21"),
        plot.background = element_rect(fill = "grey21"),
        legend.position="none",
        axis.text = element_text(size=rel(.7), color='white'),
        text = element_text(family='arial', face='plain', color='white', size = 14)) +
  labs(x='LONGITUDE',y='LATITUDE') +
  geom_point(data=by.loc.4, aes(color=TimetoIssue), size=63, alpha = .42)


ggplot(df, aes(x=Longitude, y=Latitude)) +
  geom_point(color='white',size=1) +
  scale_color_gradient(low = 'green', high = 'red') +
  theme(panel.background = element_rect(fill = "grey21"),
        panel.grid.major = element_line(color = "grey21"),
        panel.grid.minor = element_line(color = "grey21"),
        plot.background = element_rect(fill = "grey21"),
        legend.position="none",
        axis.text = element_text(size=rel(.7), color='white'),
        text = element_text(family='arial', face='plain', color='white', size = 14)) +
  labs(x='LONGITUDE',y='LATITUDE') +
  geom_point(data=by.loc.8, aes(color=TimetoIssue), size=56, alpha = .42)


ggplot(df, aes(x=Longitude, y=Latitude)) +
  geom_point(color='white',size=1) +
  scale_color_gradient(low = 'green', high = 'red') +
  theme(panel.background = element_rect(fill = "grey21"),
        panel.grid.major = element_line(color = "grey21"),
        panel.grid.minor = element_line(color = "grey21"),
        plot.background = element_rect(fill = "grey21"),
        legend.position="none",
        axis.text = element_text(size=rel(.7), color='white'),
        text = element_text(family='arial', face='plain', color='white', size = 14)) +
  labs(x='LONGITUDE',y='LATITUDE') +
  geom_point(data=by.loc.16, aes(color=TimetoIssue), size=42, alpha = .42)


ggplot(df, aes(x=Longitude, y=Latitude)) +
  geom_point(color='white',size=1) +
  scale_color_gradient(low = 'green', high = 'red') +
  theme(panel.background = element_rect(fill = "grey21"),
        panel.grid.major = element_line(color = "grey21"),
        panel.grid.minor = element_line(color = "grey21"),
        plot.background = element_rect(fill = "grey21"),
        legend.position="none",
        axis.text = element_text(size=rel(.7), color='white'),
        text = element_text(family='arial', face='plain', color='white', size = 14)) +
  labs(x='LONGITUDE',y='LATITUDE') +
  geom_point(data=by.loc.32, aes(color=TimetoIssue), size=42, alpha = .56)


ggplot(df, aes(x=Longitude, y=Latitude)) +
  geom_point(color='white',size=1) +
  scale_color_gradient(low = 'green', high = 'red') +
  theme(panel.background = element_rect(fill = "grey21"),
        panel.grid.major = element_line(color = "grey21"),
        panel.grid.minor = element_line(color = "grey21"),
        plot.background = element_rect(fill = "grey21"),
        legend.position="none",
        axis.text = element_text(size=rel(.7), color='white'),
        text = element_text(family='arial', face='plain', color='white', size = 14)) +
  labs(x='LONGITUDE',y='LATITUDE') +
  geom_point(data=by.loc.64, aes(color=TimetoIssue), size=21, alpha = .63)


ggplot(df, aes(x=Longitude, y=Latitude)) +
  geom_point(color='white',size=1) +
  scale_color_gradient(low = 'green', high = 'red') +
  theme(panel.background = element_rect(fill = "grey21"),
        panel.grid.major = element_line(color = "grey21"),
        panel.grid.minor = element_line(color = "grey21"),
        plot.background = element_rect(fill = "grey21"),
        legend.position="none",
        axis.text = element_text(size=rel(.7), color='white'),
        text = element_text(family='arial', face='plain', color='white', size = 14)) +
  labs(x='LONGITUDE',y='LATITUDE') +
  geom_point(data=by.loc.128, aes(color=TimetoIssue), size=21, alpha = .63)


ggplot(df, aes(x=Longitude, y=Latitude)) +
  geom_point(color='white',size=1) +
  scale_color_gradient(low = 'green', high = 'red') +
  theme(panel.background = element_rect(fill = "grey21"),
        panel.grid.major = element_line(color = "grey21"),
        panel.grid.minor = element_line(color = "grey21"),
        plot.background = element_rect(fill = "grey21"),
        legend.position="none",
        axis.text = element_text(size=rel(.7), color='white'),
        text = element_text(family='arial', face='plain', color='white', size = 14)) +
  labs(x='LONGITUDE',y='LATITUDE') +
  geom_point(data=by.loc.256, aes(color=TimetoIssue), size=14, alpha = .63)


ggplot(df, aes(x=Longitude, y=Latitude)) +
  geom_point(color='white',size=1) +
  scale_color_gradient(low = 'green', high = 'red') +
  theme(panel.background = element_rect(fill = "grey21"),
        panel.grid.major = element_line(color = "grey21"),
        panel.grid.minor = element_line(color = "grey21"),
        plot.background = element_rect(fill = "grey21"),
        legend.position="none",
        axis.text = element_text(size=rel(.7), color='white'),
        text = element_text(family='arial', face='plain', color='white', size = 14)) +
  labs(x='LONGITUDE',y='LATITUDE') +
  geom_point(data=by.loc.512, aes(color=TimetoIssue), size=14, alpha = .63)


ggplot(df, aes(x=Longitude, y=Latitude)) +
  geom_point(color='white',size=1) +
  scale_color_gradient(low = 'green', high = 'red') +
  theme(panel.background = element_rect(fill = "grey21"),
        panel.grid.major = element_line(color = "grey21"),
        panel.grid.minor = element_line(color = "grey21"),
        plot.background = element_rect(fill = "grey21"),
        legend.position="none",
        axis.text = element_text(size=rel(.7), color='white'),
        text = element_text(family='arial', face='plain', color='white', size = 14)) +
  labs(x='LONGITUDE',y='LATITUDE') +
  geom_point(data=by.loc.1024, aes(color=TimetoIssue), size=7, alpha = .77)


ggplot(df, aes(x=Longitude, y=Latitude)) +
  geom_point(color='white',size=1) +
  scale_color_gradient(low = 'green', high = 'red') +
  theme(panel.background = element_rect(fill = "grey21"),
        panel.grid.major = element_line(color = "grey21"),
        panel.grid.minor = element_line(color = "grey21"),
        plot.background = element_rect(fill = "grey21"),
        legend.position="none",
        axis.text = element_text(size=rel(.7), color='white'),
        text = element_text(family='arial', face='plain', color='white', size = 14)) +
  labs(x='LONGITUDE',y='LATITUDE') +
  geom_point(data=by.loc.2048, aes(color=TimetoIssue), size=7, alpha = .77)






























































rev(sort(table(df$IssueYear)))

four = bau[bau$IssueYear==2004,]
five = bau[bau$IssueYear==2005,]
six = bau[bau$IssueYear==2006,]
seven = bau[bau$IssueYear==2007,]
eight = bau[bau$IssueYear==2008,]
nine = bau[bau$IssueYear==2009,]
ten = bau[bau$IssueYear==2010,]
eleven = bau[bau$IssueYear==2011,]
twelve = bau[bau$IssueYear==2012,]
thirteen = bau[bau$IssueYear==2013,]
fourteen = bau[bau$IssueYear==2014,]

library(ggplot2)


ggplot(df, aes(x=Longitude, y=Latitude)) +
  geom_point(color='white',size=1) +
  theme(panel.background = element_rect(fill = "grey21"),
        panel.grid.major = element_line(color = "grey21"),
        panel.grid.minor = element_line(color = "grey21"),
        plot.background = element_rect(fill = "grey21"),
        legend.position="none",
        axis.text = element_text(size=rel(.7), color='white'),
        text = element_text(family='arial', face='plain', color='white', size = 14)) +
  labs(x='LONGITUDE',y='LATITUDE', title='2004 BUILDING PERMITS') +
  geom_point(data=four, color='red', size=14, alpha=0.014)


##DOWNTOWN PERMITS <START>

downtown = subset(df, df$Longitude > -117.175 & df$Longitude < -117.135 & df$Latitude > 32.7 & df$Latitude < 32.725)
downtown_bau = subset(downtown, downtown$Type == 'Building Permit')
downtown_bau = downtown_bau[c('ProjectId','TimetoIssue','Latitude','Longitude','ApplicationYear','IssueYear')]


four = downtown_bau[downtown_bau$IssueYear==2004,]
five = downtown_bau[downtown_bau$IssueYear==2005,]
six = downtown_bau[downtown_bau$IssueYear==2006,]
seven = downtown_bau[downtown_bau$IssueYear==2007,]
eight = downtown_bau[downtown_bau$IssueYear==2008,]
nine = downtown_bau[downtown_bau$IssueYear==2009,]
ten = downtown_bau[downtown_bau$IssueYear==2010,]
eleven = downtown_bau[downtown_bau$IssueYear==2011,]
twelve = downtown_bau[downtown_bau$IssueYear==2012,]
thirteen = downtown_bau[downtown_bau$IssueYear==2013,]
fourteen = downtown_bau[downtown_bau$IssueYear==2014,]


ggplot(downtown, aes(x=Longitude, y=Latitude)) +
  geom_point(color='white',size=1) +
  theme(panel.background = element_rect(fill = "grey21"),
        panel.grid.major = element_line(color = "grey21"),
        panel.grid.minor = element_line(color = "grey21"),
        plot.background = element_rect(fill = "grey21"),
        legend.position="none",
        axis.text = element_text(size=rel(.7), color='white'),
        text = element_text(family='arial', face='plain', color='white', size = 14)) +
  labs(x='LONGITUDE',y='LATITUDE', title='2013 Downtown Building Permits') + 
  geom_point(data=thirteen, color='red', size=3, alpha = .88)




##DOWNTOWN PERMITS <END>





zero=subset(df, df$DaystoIssue == 0)

rev(sort(table(zero$Type)))
rev(sort(table(df$Type)))



summary(lm(data=df, SecondstoIssue~Type))
summary(lm(data=df, DaystoIssue~Latitude))
summary(lm(data=df, DaystoIssue~Latitude+Longitude))

shaw = subset(df, df$IssuedBy == 'Shaw Jr, Andy')


library(ggplot2)
qplot(df$SecondstoIssue[df$SecondstoIssue < 60])
length(df$SecondstoIssue[df$DaystoIssue ==0])
qplot(subset(df$DaystoIssue, df$DaystoIssue > 0 & df$DaystoIssue < 100), binwidth=1)

ggplot(subset(df, df$DaystoIssue > 0 & df$DaystoIssue < 500), aes(x=DaystoIssue)) +
  geom_histogram(fill='white', color='black', binwidth=1) +
  scale_x_continuous(breaks = round(seq(min(df$DaystoIssue), max(df$DaystoIssue), by = 10),1))


ggplot(df, aes(x=factor(df$appY))) + 
  geom_histogram(fill='white', color='black', binwidth=1)

ggplot(df, aes(x=factor(df$issY))) + 
  geom_histogram(fill='white', color='black', binwidth=1)

ggplot(df, aes(x=factor(df$appM))) + 
  geom_histogram(fill='white', color='black', binwidth=1)

ggplot(df, aes(x=factor(df$issM))) + 
  geom_histogram(fill='white', color='black', binwidth=1)

ggplot(df, aes(x=factor(df$appD))) + 
  geom_histogram(fill='white', color='black', binwidth=1)

ggplot(df, aes(x=factor(df$issD))) + 
  geom_histogram(fill='white', color='black', binwidth=1)

ggplot(df, aes(x=factor(df$appday))) + 
  geom_histogram(fill='white', color='black', binwidth=1)

ggplot(df, aes(x=factor(df$issday))) + 
  geom_histogram(fill='white', color='black', binwidth=1)

ggplot(df, aes(x=factor(df$apph))) + 
  geom_histogram(fill='white', color='black', binwidth=1)

ggplot(df, aes(x=factor(df$issh))) + 
  geom_histogram(fill='white', color='black', binwidth=1)


df[df$DaystoIssue==349,]

mean(df$DaystoIssue)
mean(df$DaystoIssue[df$DaystoIssue > 0])

summary(df$DaystoIssue)






holder_count = rle(sort(c(as.character(df$PermitHolder))))
df$holder_count = holder_count[[1]][match(df$PermitHolder, holder_count[[2]])]

tmp = data.frame(rev(sort(table(df$PermitHolder))))
tmp[1:42,]

big.devs = df[df$holder_count > 322,]

35290/216691

summary(lm(data=big.devs, DaystoIssue~PermitHolder))
anova(lm(data=big.devs, DaystoIssue~PermitHolder+Type))

big.devs[order(big.devs$DaystoIssue), c(7,13)]


library(plyr)

by.dev.type = ddply(big.devs, c('PermitHolder','Type'), 
               summarize,
               mean.dti = mean(DaystoIssue),
               h.count = mean(holder_count))

by.dev = ddply(big.devs, c('PermitHolder'), 
                    summarize,
                    mean.dti = mean(DaystoIssue),
                    h.count = mean(holder_count))


options("scipen"=100, 'digits'=4)
by.dev.type[order(by.dev.type$mean.dti),]


unique(big.devs$PermitHolder)

df['big.dev'] = df$holder_count > 322


anova(lm(data=df, DaystoIssue~Type+big.dev))
summary(lm(data=df, DaystoIssue~big.dev))

anova(lm(data=big.devs, DaystoIssue~PermitHolder+Type))
summary(lm(data=big.devs, DaystoIssue~holder_count))

big.devs['lat.bin'] = cut(big.devs$Latitude, 8)
big.devs['lon.bin'] = cut(big.devs$Longitude, 4)
big.devs['square'] = paste(as.character(big.devs$lat.bin), as.character(big.devs$lon.bin))

anova(lm(data=big.devs, DaystoIssue~PermitHolder+square+Type))




type_count = rle(sort(c(as.character(df$Type))))
df$type_count = type_count[[1]][match(df$Type, type_count[[2]])]

ggplot(df, aes(reorder(Type, type_count))) +
  geom_histogram(fill='white', color='black')

sort(table(df$Type))

ggplot(df[df$DaystoIssue > 0,], aes(x=factor(Type), y=DaystoIssue)) +
  geom_boxplot()




individual_count = rle(sort(c(as.character(df$IssuedBy))))
df$individual_count = individual_count[[1]][match(df$IssuedBy, individual_count[[2]])]

ggplot(df, aes(reorder(IssuedBy, individual_count))) +
  geom_histogram(fill='white', color='black')





df['zero'] = df$DaystoIssue==0
summary(lm(data=df, zero~Type))



ggplot(subset(df, df$DaystoIssue > 0 & df$DaystoIssue < 500), aes(x=DaystoIssue)) +
  geom_histogram(fill='white', color='black', binwidth=1) +
  scale_x_continuous(breaks = round(seq(min(df$DaystoIssue), max(df$DaystoIssue), by = 10),1))





df['lat.bin'] = cut(df$Latitude, 8)
df['lon.bin'] = cut(df$Longitude, 4)
df['square'] = paste(as.character(df$lat.bin), as.character(df$lon.bin))

df['square'] = as.factor(df$square)

anova(lm(data=df, DaystoIssue~Type+square))



#### Building TTI By Month

bau = read.csv('bau_month.csv')
df = read.csv('month_time_CLEANED.csv')


df$Month_L = c(1:139)

library(ggplot2)
ggplot(data=df[df$Month_L > 103,], aes(x=Month_L, y=Median)) + geom_line()

bau$app_year_month = factor(bau$app_year_month)

qplot(bau$TimetoIssue[bau$app_year_month == '201201'], binwidth=10) + 
  xlim(0,900) + 
  ylim(0,100)


qplot(bau$TimetoIssue[bau$app_year_month == '201202'], binwidth=10) + 
  xlim(0,900) + 
  ylim(0,100)

qplot(bau$TimetoIssue[bau$app_year_month == '201203'], binwidth=10) + 
  xlim(0,900) + 
  ylim(0,100)

qplot(bau$TimetoIssue[bau$app_year_month == '201204'], binwidth=10) + 
  xlim(0,900) + 
  ylim(0,100)

qplot(bau$TimetoIssue[bau$app_year_month == '201205'], binwidth=10) + 
  xlim(0,900) + 
  ylim(0,100)

qplot(bau$TimetoIssue[bau$app_year_month == '201206'], binwidth=10) + 
  xlim(0,900) + 
  ylim(0,100)

qplot(bau$TimetoIssue[bau$app_year_month == '201207'], binwidth=10) + 
  xlim(0,900) + 
  ylim(0,100)

qplot(bau$TimetoIssue[bau$app_year_month == '201208'], binwidth=10) + 
  xlim(0,900) + 
  ylim(0,100)

qplot(bau$TimetoIssue[bau$app_year_month == '201209'], binwidth=10) + 
  xlim(0,900) + 
  ylim(0,100)

qplot(bau$TimetoIssue[bau$app_year_month == '201210'], binwidth=10) + 
  xlim(0,900) + 
  ylim(0,100)

qplot(bau$TimetoIssue[bau$app_year_month == '201211'], binwidth=10) + 
  xlim(0,900) + 
  ylim(0,100)

qplot(bau$TimetoIssue[bau$app_year_month == '201212'], binwidth=10) + 
  xlim(0,900) + 
  ylim(0,100)

qplot(bau$TimetoIssue[bau$app_year_month == '201301'], binwidth=10) + 
  xlim(0,900) + 
  ylim(0,100)

qplot(bau$TimetoIssue[bau$app_year_month == '201302'], binwidth=10) + 
  xlim(0,900) + 
  ylim(0,100)

qplot(bau$TimetoIssue[bau$app_year_month == '201303'], binwidth=10) + 
  xlim(0,900) + 
  ylim(0,100)

qplot(bau$TimetoIssue[bau$app_year_month == '201304'], binwidth=10) + 
  xlim(0,900) + 
  ylim(0,100)

qplot(bau$TimetoIssue[bau$app_year_month == '201305'], binwidth=10) + 
  xlim(0,900) + 
  ylim(0,100)

qplot(bau$TimetoIssue[bau$app_year_month == '201306'], binwidth=10) + 
  xlim(0,900) + 
  ylim(0,100)

qplot(bau$TimetoIssue[bau$app_year_month == '201307'], binwidth=10) + 
  xlim(0,900) + 
  ylim(0,100)

qplot(bau$TimetoIssue[bau$app_year_month == '201308'], binwidth=10) + 
  xlim(0,900) + 
  ylim(0,100)

qplot(bau$TimetoIssue[bau$app_year_month == '201309'], binwidth=10) + 
  xlim(0,900) + 
  ylim(0,100)

qplot(bau$TimetoIssue[bau$app_year_month == '201310'], binwidth=10) + 
  xlim(0,900) + 
  ylim(0,100)

qplot(bau$TimetoIssue[bau$app_year_month == '201311'], binwidth=10) + 
  xlim(0,900) + 
  ylim(0,100)

qplot(bau$TimetoIssue[bau$app_year_month == '201312'], binwidth=10) + 
  xlim(0,900) + 
  ylim(0,100)

qplot(bau$TimetoIssue[bau$app_year_month == '201401'], binwidth=10) + 
  xlim(0,900) + 
  ylim(0,100)

qplot(bau$TimetoIssue[bau$app_year_month == '201402'], binwidth=10) + 
  xlim(0,900) + 
  ylim(0,100)

qplot(bau$TimetoIssue[bau$app_year_month == '201403'], binwidth=10) + 
  xlim(0,900) + 
  ylim(0,100)

qplot(bau$TimetoIssue[bau$app_year_month == '201404'], binwidth=10) + 
  xlim(0,900) + 
  ylim(0,100)

qplot(bau$TimetoIssue[bau$app_year_month == '201405'], binwidth=10) + 
  xlim(0,900) + 
  ylim(0,100)

qplot(bau$TimetoIssue[bau$app_year_month == '201406'], binwidth=10) + 
  xlim(0,900) + 
  ylim(0,100)

qplot(bau$TimetoIssue[bau$app_year_month == '201407'], binwidth=10) + 
  xlim(0,900) + 
  ylim(0,100)

qplot(bau$TimetoIssue[bau$app_year_month == '201408'], binwidth=10) + 
  xlim(0,900) + 
  ylim(0,100)

qplot(bau$TimetoIssue[bau$app_year_month == '201409'], binwidth=10) + 
  xlim(0,900) + 
  ylim(0,100)

qplot(bau$TimetoIssue[bau$app_year_month == '201410'], binwidth=10) + 
  xlim(0,900) + 
  ylim(0,100)

max(bau$TimetoIssue[bau$app_year_month == '201410'])






































































































































