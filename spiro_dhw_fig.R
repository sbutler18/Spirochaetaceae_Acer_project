library(tidyverse)
library(ggplot2)
library(ggpubr)


#  Spiro DHW plots
# Sterling Butler 
#.  NOAA - AOML - CIMAS

setwd("/Users/sterling.butler/Downloads")

DHW <- read_csv("kbnTemps.csv")

#temp plot 

temp_plot <- ggplot(DHW, aes(x=date, y=temp))+
  geom_line( color="#B22", size=.3)+
  labs(x="Month", y="Temp (°C)", title = "Temperature at Key Biscayne Nursery 2023")+
  theme_classic()+
  scale_y_continuous(
    limits = c(26, 32))+
    geom_hline( yintercept = 30 , linetype = "dashed", color = "black")+
  geom_vline(xintercept = as.POSIXct(
      c("2023-06-15 12:00:03", "2023-09-08 12:00:03"),
      tz = "UTC"),color = "black",size = .5)+
  annotate("text", x = as.POSIXct(c("2023-06-24 12:00:03", "2023-09-17 12:00:03")), y = 26.2, label = c("T1", "T2"), 
           color = "black",  vjust = -0.5)+
  annotate(
    geom = "curve", x =as.POSIXct(c("2023-06-21 12:00:03", "2023-09-14 12:00:03")), y = 26.6, xend = as.POSIXct(c("2023-06-15 16:00:03", "2023-09-08 16:00:03")),
    yend = 26.2, curvature = .3, arrow = arrow(length = unit(2, "mm"))
  )+
  theme(axis.text = element_text(size=12))+
  theme(title = element_text(size=12))

temp_plot

#dhw plot 

dhw_plot <- ggplot(DHW, aes(x=date, y=dhws))+
  geom_line( color="#B22", size=.4)+
  labs(x="Month", y="DHWs", title = "Degree Heating Weeks (DHWs) at Key Biscayne Nursery 2023")+
  theme_classic()+
  geom_vline(xintercept = as.POSIXct(
  c("2023-06-15 12:00:03", "2023-09-08 12:00:03"),
  tz = "UTC"), color = "black", size = .5)+
  annotate("text", x = as.POSIXct(c("2023-06-24 12:00:03", "2023-09-17 12:00:03")), y = .9, label = c("T1", "T2"), 
           color = "black",  vjust = -0.5)+
  annotate(
    geom = "curve", x =as.POSIXct(c("2023-06-21 12:00:03", "2023-09-14 12:00:03")), y = 1.6, xend = as.POSIXct(c("2023-06-15 16:00:03", "2023-09-08 16:00:03")),
    yend = 1, curvature = .3, arrow = arrow(length = unit(2, "mm"))
  )+
  theme(axis.text = element_text(size=12))+
  theme(title = element_text(size=12))
  
  

fig <- ggarrange( temp_plot, dhw_plot,
  ncol = 1, nrow = 2,
  align = "v")

fig

setwd("/Users/sterling.butler/Desktop")

ggsave(filename = "Supp_fig_dhws&temp.png", plot = fig, device = "png", 
  width = 8, height = 5, units = "in")



#mean temperature

t1 <- DHW %>% filter(date>="2023-06-15 01:00:03" & date<= "2023-06-15 23:00:03" ) %>% select(rollingTemp) 

mean(t1$rollingTemp)


t2 <- DHW %>% filter(date>= "2023-09-08 01:00:03" & date<= "2023-09-08 23:00:03") %>% select(rollingTemp)

mean(t2$rollingTemp)

  
