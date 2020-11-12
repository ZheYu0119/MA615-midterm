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
library(readr)
library(kableExtra)

#import data
PAFPD <- read_csv("PublicAssistanceFundedProjectsDetails.csv")
PAFPD$beginyear <- year(PAFPD$declarationDate)
PAFPD_0918 <- filter(PAFPD,beginyear > 2008 & beginyear < 2019 & incidentType == "Hurricane")
PAFPD_0918$stateNumberCode <- as.numeric(PAFPD_0918$stateNumberCode)
PAFPD_0918$countyCode <- as.numeric(PAFPD_0918$countyCode)
PAFPD_0918$fips <- 1000*PAFPD_0918$stateNumberCode+PAFPD_0918$countyCode
PAFPD_0918 <- filter(PAFPD_0918,projectAmount>=0)

#combine the fips with maps data
##county
data(county.fips)
mapcounty=st_as_sf(map('county',plot=F,fill=T))
colnames(county.fips)[2]=colnames(mapcounty)[1]
mapcounty=left_join(mapcounty,county.fips,'ID')

##state
data(state.fips)
mapstate=st_as_sf(map('state',plot = F,fill = T))
colnames(state.fips)[6]=colnames(mapstate)[1]
mapstate=left_join(mapstate,state.fips,'ID')
mapstate[20,2:6]<-c(25,22,1,1,"MA")
mapstate[21,2:6]<-c(26,23,2,3,"MI")
mapstate[31,2:6]<-c(36,33,1,2,"NY")
mapstate[32,2:6]<-c(37,34,3,5,"NC")
mapstate[45,2:6]<-c(51,49,3,5,"VA")
mapstate[46,2:6]<-c(53,50,4,9,"WA")
colnames(mapstate)[2] <- "stateNumberCode"
mapstate$stateNumberCode<- as.numeric(mapstate$stateNumberCode)

#combine data
#by state
hurricane_group<- group_by(PAFPD_0918, stateNumberCode)
h_GroupByfips<- summarise(hurricane_group,sumcost = sum(projectAmount))
h_GroupByfips<- h_GroupByfips[order(h_GroupByfips$sumcost,decreasing=F),]
hcount_s <- right_join(mapstate,h_GroupByfips,"stateNumberCode")

#by county
hurricane_group <- filter(PAFPD_0918,countyCode!=0)
hurricane_group<- group_by(hurricane_group, fips)
h_GroupByfips<- summarise(hurricane_group,sumcost = sum(projectAmount))
h_GroupByfips<- h_GroupByfips[order(h_GroupByfips$sumcost,decreasing=F),]
hcount_c <- right_join(mapcounty,h_GroupByfips,"fips")
hcount_c <- filter(hcount_c,is.na(ID)==F)
hcount_c2 <- filter(hcount_c,sumcost<200000000)
hcount_c3 <- filter(hcount_c,sumcost>200000000)
#leaflet
#state
pal <- colorNumeric("YlOrRd",domain=hcount_s$sumcost)
pop <- paste("State:",hcount_s$ID,"<br/>",
             "estimated total cost:",hcount_s$sumcost,"$","<br/>")
leaflet(hcount_s)%>%
  addTiles()%>%
  addPolygons(data = hcount_s,fillColor=~pal(hcount_s$sumcost), 
              stroke = T,popup=pop,fillOpacity = 0.5,weight = 0.5,
              color = "gray")%>%
  addLegend("bottomright",values = ~sumcost,pal = pal,
            title = "estimated total cost")

#county
#higher than 200,000,000
pal2 <- colorNumeric("PuRd",domain=hcount_c3$sumcost)
pop2 <- paste("County:",hcount_c3$ID,"<br/>",
             "estimated total cost:",hcount_c3$sumcost,"$","<br/>")
leaflet(hcount_c3)%>%
  addTiles()%>%
  addPolygons(data = hcount_c3,fillColor=~pal2(hcount_c3$sumcost), 
              stroke = T,popup=pop2,fillOpacity = 0.7,weight = 0.5,
              color = "gray")%>%
  addLegend("bottomright",values = ~sumcost,pal = pal2,
            title = "estimated total cost")

#under 200,000,000
pal3 <- colorNumeric("PuRd",domain=hcount_c2$sumcost)
pop3 <- paste("County:",hcount_c2$ID,"<br/>",
              "estimated total cost:",hcount_c2$sumcost,"$","<br/>")
leaflet(hcount_c2)%>%
  addTiles()%>%
  addPolygons(data = hcount_c2,fillColor=~pal3(hcount_c2$sumcost), 
              stroke = T,popup=pop2,fillOpacity = 0.7,weight = 0.5,
              color = "gray")%>%
  addLegend("bottomright",values = ~sumcost,pal = pal3,
            title = "estimated total cost")

kable(hcount_c3)
