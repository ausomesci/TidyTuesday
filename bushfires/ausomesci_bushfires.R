## Tidy Tuesday January 7th, 2020
# Australian bush fires

library(tidyverse)
library(lubridate)
library(ggsci)
library(scales)
library(gridExtra)

#Bring in data
rainfall <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-07/rainfall.csv')
temperature <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-07/temperature.csv')

#Wrangle temperature and rainfall to summarize into years and cities
df.t <- temperature %>% 
  mutate(year = year(date)) %>% 
  mutate(month = month(date)) %>% 
  mutate(day = day(date)) %>% 
  filter(city_name == "PERTH" | city_name == "MELBOURNE" | city_name == "SYDNEY" |
           city_name == "BRISBANE") %>% 
  filter(temp_type == "max") %>% 
  group_by(year, city_name) %>% 
  summarize(temp = mean(temperature, na.rm=TRUE))
  
df.r <- rainfall %>% 
  filter(city_name == "Perth" | city_name == "Melbourne" | city_name == "Sydney" |
           city_name == "Brisbane") %>% 
  group_by(year, city_name) %>% 
  summarize(rain = mean(rainfall, na.rm = TRUE))

# Just needed to check what the hexadecimal codes for the NEJM palette were
nejm_pal <- pal_nejm("default", alpha = 1)(4)
show_col(nejm_pal)

#Density plot of temperatures + 2019 indicator line
t19 <- df.t %>% 
  filter(year == 2019)
density.temp <- ggplot(df.t, aes(x = temp))+
  geom_density(aes(fill = city_name), alpha = 0.5)+
  scale_fill_nejm(name = "City")+
  geom_vline(xintercept = t19$temp, lwd = 1.3, color = c("#BC3C29FF","#0072B5FF","#E18727FF","#20854EFF"))+
  scale_x_continuous(name = "Temperature (°C)", limits = c(15,30))+
  scale_y_continuous(name = "Density")+
  theme(text = element_text(size = 18),
        panel.background = element_blank(),
        panel.grid.major = element_line(colour="#f0f0f0"),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour="black"),
        legend.position = "bottom")

#Density plot of rainfall + 2019 indicator line
r19 <- df.r %>%
  filter(year == 2019)
density.rain <- ggplot(df.r, aes(x = rain))+
  geom_density(aes(fill = city_name), alpha = 0.5)+
  scale_fill_nejm(name = "City")+
  geom_vline(xintercept = r19$rain, lwd = 1.3, color = c("#BC3C29FF","#0072B5FF","#E18727FF","#20854EFF"))+
  scale_x_continuous(name = "Rainfall (mm)", limits = c(0,10))+
  scale_y_continuous(name = "Density")+
  annotate("text", label = "| 2019 values", x = 8, y=1.0, size = 5)+
  theme(text = element_text(size = 18),
        panel.background = element_blank(),
        panel.grid.major = element_line(colour="#f0f0f0"),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour="black"),
        legend.position = "bottom")

# Function to extract common legend from plots
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend<-g_legend(density.temp)

# Use gridExtra::grid.arrange to combine both plots and common legend
# Export as pdf
pdf("ausomesci_bushfires.pdf", width = 10, height = 7)
grid.arrange(arrangeGrob(density.temp+theme(legend.position = 0),
             density.rain+theme(legend.position = 0), nrow=1),
             mylegend, nrow = 2, heights = c(10,1))
dev.off()

