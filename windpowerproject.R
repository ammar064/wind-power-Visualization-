#=====================================
#install packages
#=====================================
install.packages("cartography")#This package helps to design cartographic 
install.packages("sf")#represents simple features as records in a data.frame with a geometry list-column
install.packages("tidyverse")
install.packages("dplyr")#use this package  to filter data 
install.packages("readxl")#use this package  to read the execl in r studio
install.packages("rgdal")#use this package  to load shapefiles and plot
library(rgdal)
library(readxl)
library(cartography)
library(sf)
library(tidyverse)
library(dplyr)

setwd("C:/Users/Ammar/Desktop/wind power project")

#=====================================
#load and read our data
#=====================================
counties<-st_read("Landkreise_RLP.shp")
View(counties)

windPower<-st_read("Windkraftanlagen.shp")
View(windPower)

#filter our data we need only data of Rheinland-Pfalz
Rhein_windPower<- filter(windPower, Bundesland =="Rheinland-Pfalz")

colnames(Rhein_windPower)
colnames(counties)

#plot our observation points in the map 
plot(st_geometry(counties ))
points(Rhein_windPower$Längengra,Rhein_windPower$Breitengra, col="red", pch=1, cex = 0.5)


#=====================================
#another way to create map
#=====================================
my_data <- read_excel("Idkreis.xlsx")

#check our columns names
colnames(my_data)
colnames(counties)

#merge Data in mapdata

mapdata<-merge(counties, my_data, by="ldkreis")
View(mapdata)
names(mapdata)

#plot mapdata  

plot(st_geometry(mapdata))
choroLayer(x=mapdata, var="Anzahl", method="quantile", nclass=6)
layoutLayer(title="the wind power plants in Rheinland-Pfalz", tabtitle = TRUE, frame=TRUE, scale=6)

#using ggplot package

ggplot(data=mapdata)+geom_sf(aes(fill=Anzahl), color="white")+
  scale_fill_viridis_c(option = "viridis", trans = "sqrt")+
  xlab("Longitute")+ ylab("latitude")+
  ggtitle("the wind power plants in Rheinland-Pfalz")

points<-cbind(mapdata, st_coordinates(st_centroid(mapdata$geometry)))

#adding backgraund , distrit names and nambers of wind power plants

library(ggthemes)
ggplot(data = points) +
  geom_sf(aes(fill=Anzahl), color="black", size=0.2) +
  scale_fill_viridis_c(option = "viridis", trans = "sqrt")+
  geom_text(data= points,aes(x=X, y=Y, label=paste(ldkreis)),
            color = "white", size=1.5, fontface = "italic", angle=0, vjust=-1, check_overlap = FALSE) +
  geom_text(data= points,aes(x=X, y=Y, label=paste(Anzahl)),
            color = "white", size=2.0, fontface = "bold", angle=0, vjust=+1, check_overlap = FALSE) +
  ggtitle("The wind power plants in Rheinland-Pfalz")+xlab("Longitude")+ylab("Latitude")+
  
  theme(
    panel.background = element_rect(fill='lightblue', size=0.5, 
                                    linetype=0, colour='lightblue'),
    plot.title = element_text(color="black", size=16, hjust=0.5, face="bold.italic"),
    axis.title.x = element_text(color="blue", size=10, face="bold"),
    axis.title.y = element_text(color="red", size=10, face="bold")
  )

ggsave("map2.pdf", width=6, height=6, dpi='screen')
