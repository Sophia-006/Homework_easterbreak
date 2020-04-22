#Working with data BOM

library(tidyverse)
library(cowplot)

Bom <-  read_csv ("Data/BOM_data.csv")

#Perth station ID 9225

Bom_perth <- filter(Bom, Station_number == 9225) %>%  
  separate(Temp_min_max, into= c('min', 'max'), sep='/') %>% 
  filter(min!= '-', max!= '-', Rainfall != '-', Solar_exposure!="-")

Bom_perth$min<-as.numeric(Bom_perth$min)
Bom_perth$max<-as.numeric(Bom_perth$max)
Bom_perth$Rainfall<-as.numeric(Bom_perth$Rainfall)
Bom_perth$Solar_exposure<-as.numeric(Bom_perth$Solar_exposure)

# 3 plots relation max_temp vs min_temp, rainfall & solar exp
#Plot1 max vs min
Max_min <- Bom_perth %>% 
  ggplot (aes(x=max, y=min))+
  geom_point(shape =20, colour='magenta4')+
  labs(title= "Temperature", 
       x='Maximum',
       y= 'Minimum') 

#Plot2 max vs rainfall
Max_rainfall <- Bom_perth %>% 
  ggplot (aes(x=max, y=Rainfall))+
  geom_point(shape =20, colour='blue1')+
  labs(title= "Temperature max vs Rainfall", 
       x='Maximum Temperature',
       y= 'Rainfall') 

#Plot3 max vs solar exp
Max_solar <- Bom_perth %>% 
  ggplot (aes(x=max, y=Solar_exposure))+
  geom_point(shape =20, colour='gold4')+
  labs(title= "Temperature max vs Solar exposure", 
       x='Maximum Temperature',
       y= 'Solar exposure')

plot_grid(Max_min, Max_rainfall, Max_solar,labels = 'auto' )

#Question 2
#Display four measurements in a single plot
#use additional aesthetic mappings

Perth_4m <- Bom_perth %>% 
  ggplot (aes(x=max, y=Solar_exposure, colour= min, size= Rainfall))+
  geom_point(shape =20, alpha= 0.3)+
  labs(x='Maximum Temperature',
       y= 'Solar exposure',
       size= 'Rainfall',
       colour= 'min') +
  theme(plot.title= element_text(face= "bold", size=12),
         axis.title = element_text(size=10,face='bold'),
         legend.position = "bottom")

#Combined plots- Question 3

Perth_4plot <- plot_grid(Max_min, Max_rainfall, Max_solar,Perth_4m, labels = 'auto' )

ggsave(filename="Results/combined_Perth.png", 
       plot=Perth_4plot, 
       width=23, height=20, dpi=300, units="cm")

#Calculate avarage monthly rainfall for each station- Question 4

Bom_rainfall <- Bom %>%  filter( Rainfall != '-') 
Bom_rainfall$Rainfall<-as.numeric(Bom_rainfall$Rainfall)
Bom_rainfall$Station_number<-as.character(Bom_rainfall$Station_number)

Bom_group <- Bom_rainfall %>% group_by(Station_number, Month) %>% 
  summarise(meanrain= mean(Rainfall))

Bom_stations <-  read_csv ("Data/BOM_stations.csv") %>% 
  gather(key= 'Station_number', value = 'data', -info) %>% 
  spread(key='info', value= 'data')

Bom_join <- left_join(Bom_group, Bom_stations, by= "Station_number")

plotQ4 <- Bom_join %>% 
  ggplot( aes(x= Month, y=meanrain, group= Station_number, colour=name))+
  geom_line()+
  facet_wrap(~state)+
  labs(x='Month',
       y= 'Rain',
       colour = 'State',
       title= 'Rainfall across Australia')+
  theme(strip.background = element_blank(),
        panel.grid.minor= element_blank(), #element_line(size=1)
        axis.title = element_text(size=12,face='bold'),
        legend.position = 'top',
        legend.text = element_text(size=8),
        legend.background = element_rect(fill= 'lightblue', size = 0.5))


ggsave(filename="Results/Rainfall_Aus.png", 
       plot=plotQ4) 
       

