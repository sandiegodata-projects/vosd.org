d = read.csv('/Users/damoncrockett/vosd.org/215-opendsd/tables/by_CPA_parcels.csv')
d = subset(d,d$parcels > 9000)
library(ggplot2)
ggplot(d,aes(lon,lat)) +
  geom_point(data=d,aes(color=asr_impr,size=asr_land)) +
  geom_text(data=d,aes(label=community_name),size=9,color='white') +
  scale_color_continuous(low='red',high='green') +
  scale_size_continuous(range=c(20,100)) +
  theme(panel.background = element_rect(fill = "black"),
        panel.grid.major = element_line(color = "black"),
        panel.grid.minor = element_line(color = "black"),
        plot.background = element_rect(fill = "black"),
        legend.position="none",
        axis.text = element_text(size=rel(0.7), color='white'),
        text = element_text(face='plain', color='white', size = 15))
