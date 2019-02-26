library(tidyverse)
library(gridExtra)
library(grid)

trains_raw <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-26/full_trains.csv")

# Scatter of whether time of trip is related to number of late arrivals/departures (by season)
df1 <- trains_raw %>% 
  mutate(month = gsub("1","Winter", month)) %>% 
  mutate(month = gsub("2","Winter", month)) %>% 
  mutate(month = gsub("3","Spring", month)) %>% 
  mutate(month = gsub("4","Spring", month)) %>% 
  mutate(month = gsub("5","Spring", month)) %>% 
  mutate(month = gsub("6","Summer", month)) %>% 
  mutate(month = gsub("7","Summer", month)) %>% 
  mutate(month = gsub("8","Summer", month)) %>% 
  mutate(month = gsub("9","Fall", month)) %>% 
  mutate(month = gsub("10*","Fall", month)) %>% 
  mutate(month = gsub("11*","Fall", month)) %>% 
  mutate(month = gsub("12*","Winter", month)) %>% 
  mutate(month = gsub("Winter0","Winter",month)) %>% 
  mutate(month = gsub("WinterWinter","Winter",month)) %>% 
  rename(TripTime = journey_time_avg) %>% 
  rename(DeptLate = num_late_at_departure) %>% 
  rename(ArrLate = num_arriving_late) %>% 
  rename(TotTrip = total_num_trips) %>% 
  mutate(TotLate = DeptLate + ArrLate) %>% 
  mutate(NormDeptLate = (DeptLate/TotTrip)*100) %>% 
  mutate(NormArrLate = (ArrLate/TotTrip)*100)

#Scatter plot by month
p5 <- df1 %>% 
  ggplot(aes(x=month,y=NormDeptLate))+
  geom_violin(aes(fill=month)) +
  coord_cartesian(ylim=c(0,100))+
  labs(title="Late Departures",x="Season",y="Normalized Late Departures (% of Total)") +
  scale_y_continuous(expand = c(0,0)) +
  theme(legend.title = element_blank(),
        legend.key = element_rect(fill=NA),
        panel.background = element_blank(),
        axis.title = element_text(size = rel(1)),
        axis.title.y = element_text(angle=90,vjust=2),
        axis.title.x = element_text(vjust = -0.2),
        axis.text = element_text(),
        axis.line = element_line(colour="black"),
        axis.ticks = element_line(),
        panel.grid.major = element_line(colour="#f0f0f0"),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.key.size = unit(0.2,"cm"),
        legend.text=element_text(size=16),
        legend.spacing.x = unit(0.25,'cm'),
        plot.margin = unit(c(10,5,5,5),"mm"),
        plot.title = element_text(face="bold",size=rel(1.5),hjust=0.5))
p5

p6 <- df1 %>% 
  ggplot(aes(x=month,y=NormArrLate))+
  geom_violin(aes(fill = month))+
  coord_cartesian(ylim=c(0,100))+
  labs(title="Late Arrivals",x="Season",y="Normalized Late Arrivals (% of Total)") +
  scale_y_continuous(expand = c(0,0)) +
  theme(legend.title = element_blank(),
        legend.key = element_rect(fill=NA),
        panel.background = element_blank(),
        axis.title = element_text(size = rel(1)),
        axis.title.y = element_text(angle=90,vjust=2),
        axis.title.x = element_text(vjust = -0.2),
        axis.text = element_text(),
        axis.line = element_line(colour="black"),
        axis.ticks = element_line(),
        panel.grid.major = element_line(colour="#f0f0f0"),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.key.size = unit(0.2,"cm"),
        legend.text=element_text(size=16),
        legend.spacing.x = unit(0.25,'cm'),
        plot.margin = unit(c(10,5,5,5),"mm"),
        plot.title = element_text(face="bold",size=rel(1.5),hjust=0.5))
p6

g_legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend <- g_legend(p5)

grid.arrange(arrangeGrob(p5+theme(legend.position="none"),p6+theme(legend.position="none"),nrow=1),
             mylegend,nrow=2,heights=c(10,1),
             top = textGrob("French Trains: 'Lateness' by the numbers",gp=gpar(fontsize=24,font=1)))