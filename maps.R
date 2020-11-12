library(usmap)
library(dplyr)
library(tidyverse)
library(drat)
library(hurricaneexposuredata)
library(hurricaneexposure)
library(ggplot2)
library(maps)
library(tmap)
library(leaflet)
library(sf)
library(sp)
library(lubridate)

#import data
disaster <- read_csv("DisasterDeclarationsSummaries.csv")
disaster$fipsStateCode <- as.numeric(disaster$fipsStateCode)
disaster$fipsCountyCode <- as.numeric(disaster$fipsCountyCode)
disaster$fips <- 1000*disaster$fipsStateCode+disaster$fipsCountyCode

#combine the fips with maps data
##county
data(county.fips)
Map=st_as_sf(map('county',plot=F,fill=T))
colnames(county.fips)[2]=colnames(Map)[1]
Map=left_join(Map,county.fips,'ID')

##state
data(state.fips)
map=st_as_sf(map('state',plot = F,fill = T))
colnames(state.fips)[6]=colnames(map)[1]
map=left_join(map,state.fips,'ID')
map[20,2:6]<-c(25,22,1,1,"MA")
map[21,2:6]<-c(26,23,2,3,"MI")
map[31,2:6]<-c(36,33,1,2,"NY")
map[32,2:6]<-c(37,34,3,5,"NC")
map[45,2:6]<-c(51,49,3,5,"VA")
map[46,2:6]<-c(53,50,4,9,"WA")
colnames(map)[2] <- "stateNumberCode"
map$stateNumberCode <- as.numeric(map$stateNumberCode)

#select hurricane
hurricane <- filter(disaster,incidentType=="Hurricane")
hurricane$beginyear <- year(hurricane$declarationDate)
colnames(hurricane)[16] <- "stateNumberCode" 
hurricane <- filter(hurricane,beginyear>2008&beginyear<2019)
hurricane_GroupByfips<- hurricane %>% group_by(femaDeclarationString,stateNumberCode)%>%summarise(count = n())
hurricanecount <- right_join(map,hurricane_GroupByfips,"stateNumberCode")
hurricanecount <- hurricanecount %>% group_by(ID)%>%summarise(count=n())


hurricane_group<- group_by(hurricane, fips)
hurricane_GroupByfips<- summarise(hurricane_group,count = n())
hurricane_GroupByfips<- hurricane_GroupByfips[order(hurricane_GroupByfips$count,decreasing=F),]
hurricanecount2 <- right_join(Map,hurricane_GroupByfips,"fips")


#use leaflet
pal <- colorNumeric("Blues",domain=hurricanecount$count)
pop <- paste("State:",hurricanecount$ID,"<br/>",
             "count:",hurricanecount$count,"<br/>")
mapCounty = map("county", fill = TRUE, plot = FALSE)
mapStates = map("state", fill = TRUE, plot = FALSE)
leaflet(hurricanecount)%>%
  addTiles()%>%
  addPolygons(data = hurricanecount,fillColor=~pal(hurricanecount$count), 
              stroke = T,popup=pop,fillOpacity = 0.5,weight = 0.5,
              color = "gray")%>%
  addLegend("bottomright",values = ~count,pal = pal,
            title = "count of huarricane disaster")

pal2 <- colorNumeric("Blues",domain=hurricanecount2$count)
pop2 <- paste("State:",hurricanecount2$ID,"<br/>",
             "count:",hurricanecount2$count,"<br/>")
leaflet(hurricanecount2)%>%
  addTiles()%>%
  addPolygons(data = hurricanecount2,fillColor=~pal2(hurricanecount2$count), 
              stroke = T,popup=pop2,fillOpacity = 0.5,weight = 0.5,color = "gray")%>%
  addLegend("bottomright",values = ~count,pal = pal2,
            title = "count of hurricane disaster",opacity = 1)


#use ggplot2
ggplot()+
  ggtitle("hurricane")+
  geom_polygon(data=mapCounty, aes(x=long, y=lat, group=group),
               color="snow3", fill="white") +
  geom_sf(data=hurricanecount,mapping=aes(fill=count),color="gray65",size=0.1)+
  geom_polygon(data=mapStates, aes(x=long, y=lat, group=group),
               color="black", fill="lightgray",  size = 0.4, alpha = .3) +
  scale_color_brewer(name="count of hurricane disaster",direction = -1)


#by time
ggplot(data = hurricane)+geom_bar(aes(factor(year(incidentBeginDate)),fill=state))+
  labs(x="year",title = "The number of hurricane in each year")
ggplot(data = hurricane)+geom_bar(aes(factor(month(incidentBeginDate)),fill=state))+
  labs(x="month",title = "The number of hurricane in each month")
ggplot(data = hurricane)+geom_histogram(aes(as.duration(incidentEndDate-incidentBeginDate)/604800))+
  labs(x="lasting weeks",title = "count of lasting weeks")

