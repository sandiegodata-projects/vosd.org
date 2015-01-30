df = read.csv('~/Desktop/DSD_time_to_issue.csv')
df$X = NULL

################
## BI party ####
################

library(plyr)

rev(sort(table(df$Type)))

bau = subset(df, df$Type == 'Combination Building Permit' | df$Type == 'Building Permit')

north.south = bau
north.south['lat.bin'] = cut(north.south$Latitude, 2)

by.loc.ns = ddply(north.south, c('lat.bin'), 
                  summarize,
                  mean.dti = mean(DaystoIssue),
                  Latitude = mean(Latitude),
                  Longitude = mean(Longitude))



east.west = bau
east.west['lon.bin'] = cut(east.west$Longitude, 2)

by.loc.ew = ddply(east.west, c('lon.bin'), 
                  summarize,
                  mean.dti = mean(DaystoIssue),
                  Latitude = mean(Latitude),
                  Longitude = mean(Longitude))

four.bins = bau
four.bins['lat.bin'] = cut(four.bins$Latitude, 2)
four.bins['lon.bin'] = cut(four.bins$Longitude, 2)
four.bins['square'] = paste(as.character(four.bins$lat.bin), as.character(four.bins$lon.bin))

by.loc.4 = ddply(four.bins, c('square'), 
                  summarize,
                  mean.dti = mean(DaystoIssue),
                 Latitude = mean(Latitude),
                 Longitude = mean(Longitude))

eight.bins = bau
eight.bins['lat.bin'] = cut(eight.bins$Latitude, 4)
eight.bins['lon.bin'] = cut(eight.bins$Longitude, 2)
eight.bins['square'] = paste(as.character(eight.bins$lat.bin), as.character(eight.bins$lon.bin))

by.loc.8 = ddply(eight.bins, c('square'), 
                  summarize,
                  mean.dti = mean(DaystoIssue),
                 Latitude = mean(Latitude),
                 Longitude = mean(Longitude))

sixteen.bins = bau
sixteen.bins['lat.bin'] = cut(sixteen.bins$Latitude, 4)
sixteen.bins['lon.bin'] = cut(sixteen.bins$Longitude, 4)
sixteen.bins['square'] = paste(as.character(sixteen.bins$lat.bin), as.character(sixteen.bins$lon.bin))

by.loc.16 = ddply(sixteen.bins, c('square'), 
                  summarize,
                  mean.dti = mean(DaystoIssue),
                  Latitude = mean(Latitude),
                  Longitude = mean(Longitude))

thirtytwo.bins = bau
thirtytwo.bins['lat.bin'] = cut(thirtytwo.bins$Latitude, 8)
thirtytwo.bins['lon.bin'] = cut(thirtytwo.bins$Longitude, 4)
thirtytwo.bins['square'] = paste(as.character(thirtytwo.bins$lat.bin), as.character(thirtytwo.bins$lon.bin))

by.loc.32 = ddply(thirtytwo.bins, c('square'), 
                  summarize,
                  mean.dti = mean(DaystoIssue),
                  Latitude = mean(Latitude),
                  Longitude = mean(Longitude))

sixtyfour.bins = bau
sixtyfour.bins['lat.bin'] = cut(sixtyfour.bins$Latitude, 8)
sixtyfour.bins['lon.bin'] = cut(sixtyfour.bins$Longitude, 8)
sixtyfour.bins['square'] = paste(as.character(sixtyfour.bins$lat.bin), as.character(sixtyfour.bins$lon.bin))

by.loc.64 = ddply(sixtyfour.bins, c('square'), 
                  summarize,
                  mean.dti = mean(DaystoIssue),
                  Latitude = mean(Latitude),
                  Longitude = mean(Longitude))


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
  labs(x='LONGITUDE',y='LATITUDE') +
  geom_point(data=by.loc.64, aes(color=mean.dti), size=14)












rev(sort(table(df$issY)))

four = bau[bau$issY==2004,]
five = bau[bau$issY==2005,]
six = bau[bau$issY==2006,]
seven = bau[bau$issY==2007,]
eight = bau[bau$issY==2008,]
nine = bau[bau$issY==2009,]
ten = bau[bau$issY==2010,]
eleven = bau[bau$issY==2011,]
twelve = bau[bau$issY==2012,]
thirteen = bau[bau$issY==2013,]
fourteen = bau[bau$issY==2014,]

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


##a





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








david = subset(df, df$IssuedBy == 'Saborio, David')
qplot(david$Type)

ggplot(df, aes(x=Longitude, y= Latitude)) +
  geom_point(color='white', size=1) +
  geom_point(data=david, color='lightblue', size=2)







