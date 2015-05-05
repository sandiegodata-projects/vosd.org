library(ggplot2)

dd = read.csv('approvals_parcels_CPA.csv')

d = read.csv('tti_by_CPA.csv')

ggplot(dd[dd$Longitude < -116.9,],aes(Longitude,Latitude)) + 
  geom_point(color='white',size=1) +
  theme(panel.background = element_rect(fill = "grey"),
        panel.grid.major = element_line(color = "grey"),
        panel.grid.minor = element_line(color = "grey"),
        plot.background = element_rect(fill = "grey"),
        legend.position="none",
        axis.text = element_text(size=rel(0.7), color='white'),
        text = element_text(face='plain', color='white', size = 15)) +
  labs(x='LON',y='LAT', title='DISCRETIONARY TIME TO ISSUE BY COMMUNITY PLAN AREA') +
  geom_point(data=d[d$TimetoIssue < 2000,],aes(color=TimetoIssue,size=count)) +
  scale_size_continuous(range = c(10,25)) +
  geom_text(data=d,aes(label=CPA),color='black',size=3)


