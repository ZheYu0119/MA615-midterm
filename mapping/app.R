library(shiny)
library(usmap)
library(dplyr)
library(tidyverse)
library(drat)
library(ggplot2)
library(maps)
library(tmap)
library(leaflet)
library(sf)
library(sp)
library(lubridate)
library(readr)
library(kableExtra)
library(shinydashboard)
library(gridExtra)

#import data
hurricane <- read.csv("hurricane.csv",header = T)
PAFPD_0918 <- read.csv("PAFPD_0918.csv",header = T)

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

#number

##by state
hurricanecount <- right_join(mapstate,hurricane,"stateNumberCode")
hurricanecount <- hurricanecount %>% 
  group_by(ID,beginyear,femaDeclarationString)%>%
  summarise(count=n(),.groups = 'drop')%>%
  group_by(ID,beginyear)%>%summarise(count=n(),.groups='drop')

##by county
hurricane_group<- group_by(hurricane, fips, beginyear)
hurricane_GroupByfips<- summarise(hurricane_group,count = n(),.groups = 'drop')
hurricanecount2 <- right_join(mapcounty,hurricane_GroupByfips,"fips")
hurricanecount2 <- filter(hurricanecount2,is.na(ID)==F)

##fund

#by state
hurricane_group<- PAFPD_0918%>%group_by(beginyear, stateNumberCode)%>%
  summarise(sumcost = sum(projectAmount),.groups = 'drop')
hcount_s <- right_join(mapstate,hurricane_group,"stateNumberCode")
hcount_s <- filter(hcount_s,is.na(ID)==F)
##by county
hurricane_group <- filter(PAFPD_0918,countyCode!=0)
hurricane_group<- hurricane_group%>%group_by(beginyear, fips)%>%
  summarise(sumcost = sum(projectAmount),.groups = 'drop')
hcount_c <- right_join(mapcounty,hurricane_group,"fips")
hcount_c <- filter(hcount_c,is.na(ID)==F)
hcount_c2 <- filter(hcount_c,sumcost<200000000)
hcount_c3 <- filter(hcount_c,sumcost>200000000)

#leaflet

##numstate
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

##numcounty
pal2 <- colorNumeric("Blues",domain=hurricanecount2$count)
pop2 <- paste("State:",hurricanecount2$ID,"<br/>",
              "count:",hurricanecount2$count,"<br/>")
leaflet(hurricanecount2)%>%
  addTiles()%>%
  addPolygons(data = hurricanecount2,fillColor=~pal2(hurricanecount2$count), 
              stroke = T,popup=pop2,fillOpacity = 0.5,weight = 0.5,color = "gray")%>%
  addLegend("bottomright",values = ~count,pal = pal2,
            title = "count of hurricane disaster",opacity = 1)


##fundstate
pal <- colorNumeric("YlOrRd",domain=hcount_s$sumcost)
pop <- paste("State:",hcount_s$ID,"<br/>",
             "estimated total cost:",hcount_s$sumcost,"$","<br/>")
fundstate <- leaflet(hcount_s)%>%
  addTiles()%>%
  addPolygons(data = hcount_s,fillColor=~pal(hcount_s$sumcost), 
              stroke = T,popup=pop,fillOpacity = 0.5,weight = 0.5,
              color = "gray")%>%
  addLegend("bottomright",values = ~sumcost,pal = pal,
            title = "estimated total cost")

##fundcounty
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


##1
statesum <- PAFPD_0918 %>% group_by(state) %>% summarize(sum = sum(projectAmount),.groups='drop')
p1.1 <- ggplot(data = statesum[rev(order(statesum$sum))[1:10],], mapping = aes(x = state, y = sum, fill = state)) + 
  geom_bar(stat = "identity") + 
  guides(fill = F) + 
  ylab("sum of projectAmount") + 
  ggtitle("Barplot of the top 10 projectAmount in states") + 
  theme(plot.title = element_text(hjust = 0.5))+coord_flip()
##barplot of the other 22 projectAmount in states
p1.2 <- ggplot(data = statesum[rev(order(statesum$sum))[11:27],], mapping = aes(x = state, y = sum, fill = state)) + 
  geom_bar(stat = "identity") + 
  guides(fill = F) + 
  ylab("sum of projectAmount") + 
  ggtitle("Barplot of projectAmount for other states") + 
  theme(plot.title = element_text(hjust = 0.5))+coord_flip()


##2

yearsum <- PAFPD_0918 %>% group_by(beginyear) %>% summarize(sum = sum(projectAmount),.groups='drop')
##barplot of the top 2 projectAmount in years
p2.1 <- ggplot(data = yearsum[rev(order(yearsum$sum))[1:2],], mapping = aes(x = factor(beginyear), y = sum ,fill = factor(beginyear))) + 
  geom_bar(stat = "identity") + 
  guides(fill = F) + 
  xlab("year") + 
  ylab("sum of projectAmount") + 
  ggtitle("Barplot of the top 2 projectAmount in years") + 
  theme(plot.title = element_text(hjust = 0.5))
##barplot of the other projectAmount in years
p2.2 <- ggplot(data = yearsum[rev(order(yearsum$sum))[3:5],], mapping = aes(x = factor(beginyear), y = sum, fill = factor(beginyear))) + 
  geom_bar(stat = "identity") + 
  guides(fill = F) + 
  xlab("year") + 
  ylab("sum of projectAmount") + 
  ggtitle("Barplot of the No.3-No.5 for each year") + 
  theme(plot.title = element_text(hjust = 0.5))
p2.3 <- ggplot(data = yearsum[rev(order(yearsum$sum))[6:8],], mapping = aes(x = factor(beginyear), y = sum, fill = factor(beginyear))) + 
  geom_bar(stat = "identity") + 
  guides(fill = F) + 
  xlab("year") + 
  ylab("sum of projectAmount") + 
  ggtitle("Barplot of the other projectAmount for each year") + 
  theme(plot.title = element_text(hjust = 0.5))

##3

sizesum <- PAFPD_0918 %>% group_by(projectSize) %>% summarize(sum = sum(projectAmount),.groups='drop')
sizesum
##barplot of projectAmount for different projectSize
p3.1 <- ggplot(data = sizesum, mapping = aes(x = factor(projectSize), y = sum, fill = factor(projectSize))) + 
  geom_bar(stat = "identity") + 
  guides(fill = F) + 
  xlab("projectSize") + 
  ylab("sum of projectAmount") + 
  ggtitle("projectAmount for different projectSize") + 
  theme(plot.title = element_text(hjust = 0.5))

sizecount <- PAFPD_0918 %>% group_by(projectSize) %>% summarize(count = n(),.groups='drop')
p3.2 <- ggplot(data = sizecount, mapping = aes(x = factor(projectSize), y = count, fill = factor(projectSize))) + 
  geom_bar(stat = "identity") + 
  guides(fill = F) + 
  xlab("projectSize") + 
  ylab("count of projectAmount") + 
  ggtitle("count for different projectSize") + 
  theme(plot.title = element_text(hjust = 0.5))
grid.arrange(p3.1,p3.2,ncol=2)

##4
rank10<-data.frame(county=1:9,costs=1:9)
rank10$county <- hcount_c3$ID
rank10$costs <- hcount_c3$sumcost
rank10 <- rank10[order(rank10$costs,decreasing=T),]
rank10$rank <- 1:9


# Define UI for application that draws a histogram
ui <- dashboardPage(

    # Application title
    dashboardHeader(title="Mapping of hurricane"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Number of hurricane disaster",
                 menuSubItem("Mapping",tabName = "nohd"),
                 menuSubItem("Graphs",tabName = "numeda")),
        menuItem("Costs on hurricane disaster",
                 menuSubItem("Mapping",tabName = "fohd"),
                 menuSubItem("Graphs",tabName = "fundeda"))
      )
    ),
    dashboardBody(
      tabItems(
        ##first item
        tabItem(
          tabName = "nohd",
          fluidRow(
            tabBox(width = 8,
                title = "Mapping",
                id = "tabset1",
                tabPanel("State level",
                         leafletOutput("statecount",height = 650)),
                tabPanel("County level",
                         leafletOutput("countycount",height = 650))),
           box(width = 4,
              title = "Controls",
              status = "warning",
              solidHeader = TRUE,
              footer = "If whole world map is showed, that means the state and year 
                you choosed has no hurricane disaster.",
              sliderInput("range","Range of years",min=2009,max=2018,
                          value = c(2009, 2018)),
              selectInput("State",
                          label = "Choose a state to display",
                          choices = c("All","Alabama"="alabama","Arizona"="arizona","Arkansas"="arkansas",
                               "California"="california","Colorado"="colorado","Connecticut"="connecticut",
                               "Delaware"="delaware","District of columbia"="district of columbia",
                               "Florida"="florida","Georgia"="georgia","Idaho"="idaho",
                               "Illinois"="illinois","Indiana"="indiana","Iowa"="iowa","Kansas"="kansas",
                               "kentucky"="kentucky","Louisiana"="louisiana","Maine"="maine",
                               "Maryland"="maryland","Massachusetts"="massachusetts",
                               "Michigan"="michigan","Minnesota"="minnesota",
                               "Mississippi"="mississippi","Missouri"="missouri","Montana"="montana",
                               "Nebraska"="nebraska","Nevada"="nevada",
                               "New Hampshire"="new hampshire","New Jersey"="new jersey",
                               "New Mexico"="new mexico","New York"="new york",
                               "North Carolina"="north carolina","North Dakota"="north dakota",
                               "Ohio"="ohio","Oklahoma"="oklahoma","Oregon"="oregon",
                               "Pennsylvania"="pennsylvania","Rhode Island"="rhode island",
                               "South Carolina"="south carolina","South Dakota"="south dakota",
                               "Tennessee"="tennessee","Texas"="texas","Utah"="utah",
                               "Vermont"="vermont","Virginia"="virginia","Washington"="washington",
                               "West Virginia"="west virginia","Wisconsin"="wisconsin",
                               "Wyoming"="wyoming"))
            ),
            box(width = 4,
              title = "Tips",background = "olive", 
              solidHeader = TRUE,collapsible = TRUE,
                div("1.Click map to see more information."),
                div("2.If you choosed a state, switch from state level and county level to see different scale."),
                div("3.Click '-' to hide Tips"))
          )#fluidrow
        ),#tabitem
        ##second item
        tabItem(
          tabName = "numeda",
          fluidRow(
            box(width = 4,
                title = "The number of hurricane in each year",
                plotOutput("numy"),
                uiOutput("fornumy")
            ),
            box(width = 4,
                title = "The number of hurricane in each month",
                plotOutput("numm"),
                sliderInput("rangenumm","Range of years",min=2009,max=2018,
                            value = c(2009, 2018)),
                uiOutput("fornumm")
            ),
            box(width = 4,
                title = "Lasting time of hurricane",    
                plotOutput("time"),
                sliderInput("rangetime","Range of years",min=2009,max=2018,
                            value = c(2009, 2018)),
                uiOutput("fortime")
            )
          )
        ),
        #third item
        tabItem(
          tabName = "fohd",
          fluidRow(
            tabBox(width = 8,
                   title = "Mapping",
                   id = "tabset2",
                   tabPanel("State level",
                            leafletOutput("statefund",height = 650)),
                   tabPanel("County level",
                            leafletOutput("countyfund",height = 650))
              ),
            box(width = 4,
              title = "Control", status = "warning", solidHeader = TRUE,
              footer = "If whole world map is showed, that means the state and year 
                you choosed has no hurricane disaster.",
              sliderInput("range1","Range of years",min=2009,max=2018,
                          value = c(2009, 2018)),
              selectInput("State2",
                          label = "Choose a state to display",
                          choices = c("All","Alabama"="alabama","Arizona"="arizona","Arkansas"="arkansas",
                                      "California"="california","Colorado"="colorado","Connecticut"="connecticut",
                                      "Delaware"="delaware","District of columbia"="district of columbia",
                                      "Florida"="florida","Georgia"="georgia","Idaho"="idaho",
                                      "Illinois"="illinois","Indiana"="indiana","Iowa"="iowa","Kansas"="kansas",
                                      "kentucky"="kentucky","Louisiana"="louisiana","Maine"="maine",
                                      "Maryland"="maryland","Massachusetts"="massachusetts",
                                      "Michigan"="michigan","Minnesota"="minnesota",
                                      "Mississippi"="mississippi","Missouri"="missouri","Montana"="montana",
                                      "Nebraska"="nebraska","Nevada"="nevada",
                                      "New Hampshire"="new hampshire","New Jersey"="new jersey",
                                      "New Mexico"="new mexico","New York"="new york",
                                      "North Carolina"="north carolina","North Dakota"="north dakota",
                                      "Ohio"="ohio","Oklahoma"="oklahoma","Oregon"="oregon",
                                      "Pennsylvania"="pennsylvania","Rhode Island"="rhode island",
                                      "South Carolina"="south carolina","South Dakota"="south dakota",
                                      "Tennessee"="tennessee","Texas"="texas","Utah"="utah",
                                      "Vermont"="vermont","Virginia"="virginia","Washington"="washington",
                                      "West Virginia"="west virginia","Wisconsin"="wisconsin",
                                      "Wyoming"="wyoming")
             )#selectinput
            ),#box
            box(width = 4,
                title = "Tips",background = "olive",
                solidHeader = TRUE,collapsible = TRUE,
                div("The 'Cost' here means the estimated total cost of the Public Assistance grant project in dollars, without administrative costs. This amount is based on the damage survey
                    And in the 'Grapgh' part we use 'projectAmount' to stand for it."),
                div("1.Click map to see more information."),
                div("2.If you choosed a state, switch from state level and county level to see different scale."),
                div("3.Click '-' to hide Tips")
                ),
            box(width = 4,
                title = "Top 9 counties",
                footer = "For the top 9 are too large so they are not plotted in map.",
                tableOutput("top")
            )
        )#fluidrow
      ),
      ##4th item
      tabItem(title = "dataplots",
        tabName = "fundeda",
        fluidRow(
          box(width = 8,
              title = "Sum of projectAmount for each state",
              plotOutput("f.1")
          ),
          box(width = 4,
              title = "sum of projectAmount for different projectSize",
              plotOutput("f.3")
          ),
          
          box(width = 8,
              title = "sum of projectAmount for each year",
              plotOutput("f.2")
          ),
          box(width = 4,
              title = "sum of projectAmount for different damageCategory",
              plotOutput("f.4")
            
          )
        )
      )#second tabitem
    )#tabitems
    

        
  )#dashboardbody
)#dashboardpage


# Define server logic required to draw a histogram
server <- function(input, output) {
    
  #state level number
    output$statecount <- renderLeaflet({
      hurricanecount <- hurricanecount%>%
        filter(beginyear>=input$range[1]&beginyear<=input$range[2])%>%
        group_by(ID)%>%summarise(count=sum(count),.groups = 'drop')
      hurricanecount$count <- as.integer(hurricanecount$count)
      pal <- colorNumeric("Blues",domain=hurricanecount$count)
      pop <- paste("State:",hurricanecount$ID,"<br/>",
                   "count:",hurricanecount$count,"<br/>")
      if(input$State=="All"){
       leaflet(hurricanecount)%>%
         addTiles()%>%
         addPolygons(data = hurricanecount,fillColor=~pal(hurricanecount$count), 
                     stroke = T,popup=pop,fillOpacity = 0.5,weight = 0.5,
                     color = "gray")%>%
         addLegend("bottomright",values = ~count,pal = pal,
                   title = "count of huarricane disaster")
        }else{
                     hurricanecount <- filter(hurricanecount,hurricanecount$ID==input$State)
                     hurricanecount$count <- as.integer(hurricanecount$count)
                     pal <- colorNumeric("Blues",domain=hurricanecount$count)
                     pop <- paste("State:",hurricanecount$ID,"<br/>",
                                  "count:",hurricanecount$count,"<br/>")
                     leaflet(hurricanecount)%>%
                       addTiles()%>%
                       addPolygons(data = hurricanecount,fillColor=~pal(hurricanecount$count), 
                                   stroke = T,popup=pop,fillOpacity = 0.5,weight = 0.5,
                                   color = "gray")
                   }
    })
    
    
    #county level number
    output$countycount <- renderLeaflet({
      hurricanecount2 <- hurricanecount2%>%
        filter(beginyear>=input$range[1]&beginyear<=input$range[2])%>%
        group_by(ID)%>%summarise(count=sum(count),.groups='drop')%>%
        separate(ID,into = c("state","county"),sep = ",")
      pal2 <- colorNumeric("Blues",domain=hurricanecount2$count)
      pop2 <- paste("county:",hurricanecount2$county,"<br/>",
                    "count:",hurricanecount2$count,"<br/>")
      if(input$State=="All"){
        leaflet(hurricanecount2)%>%
          addTiles()%>%
          addPolygons(data = hurricanecount2,fillColor=~pal2(hurricanecount2$count), 
                      stroke = T,popup=pop2,fillOpacity = 0.5,weight = 0.5,color = "gray")%>%
          addLegend("bottomright",values = ~count,pal = pal2,
                    title = "count of hurricane disaster",opacity = 1)
        }else{
                      hurricanecount2 <- filter(hurricanecount2,hurricanecount2$state==input$State)
                      pal2 <- colorNumeric("Blues",domain=hurricanecount2$count)
                      pop2 <- paste("county:",hurricanecount2$county,"<br/>",
                                    "count:",hurricanecount2$count,"<br/>")
                      numcounty <- leaflet(hurricanecount2)%>%
                        addTiles()%>%
                        addPolygons(data = hurricanecount2,fillColor=~pal2(hurricanecount2$count), 
                                    stroke = T,popup=pop2,fillOpacity = 0.5,weight = 0.5,color = "gray")%>%
                        addLegend("bottomright",values = ~count,pal = pal2,
                                  title = "count of hurricane disaster",opacity = 1)
                    }
      
    })
    
    
    #state level fund
    output$statefund <- renderLeaflet({
      hcount_s <- hcount_s%>%filter(beginyear>=input$range1[1]&beginyear<=input$range1[2])%>%
        group_by(ID)%>%summarise(sumcost=sum(sumcost),.groups = 'drop')
      pal4 <- colorNumeric("YlOrRd",domain=hcount_s$sumcost)
      pop4 <- paste("State:",hcount_s$ID,"<br/>",
                    "estimated total cost:",hcount_s$sumcost,"$","<br/>")
      if(input$State2=="All"){
      leaflet(hcount_s)%>%
        addTiles()%>%
        addPolygons(data = hcount_s,fillColor=~pal4(hcount_s$sumcost), 
                    stroke = T,popup=pop4,fillOpacity = 0.5,weight = 0.5,
                    color = "gray")%>%
        addLegend("bottomright",values = ~sumcost,pal = pal4,
                  title = "estimated total cost")
      }else{
                    
                    hcount_s <- filter(hcount_s, hcount_s$ID==input$State2)
                    pal4 <- colorNumeric("YlOrRd",domain=hcount_s$sumcost)
                    pop4 <- paste("State:",hcount_s$ID,"<br/>",
                                  "estimated total cost:",hcount_s$sumcost,"$","<br/>")
                    leaflet(hcount_s)%>%
                      addTiles()%>%
                      addPolygons(data = hcount_s,fillColor=~pal4(hcount_s$sumcost), 
                                  stroke = T,popup=pop4,fillOpacity = 0.5,weight = 0.5,
                                  color = "gray")
                  }
    })
    
  
    #county level fund
    output$countyfund <- renderLeaflet({
      hcount_c2<-hcount_c2%>%
        filter(beginyear>=input$range1[1]&beginyear<=input$range1[2])%>%
        group_by(ID)%>%summarise(sumcost=sum(sumcost),.groups = 'drop')%>%
        separate(ID,into = c("state","county"),sep = ",")
      pal3 <- colorNumeric("YlOrRd",domain=hcount_c2$sumcost)
      pop3 <- paste("County:",hcount_c2$county,"<br/>",
                    "estimated total cost:",hcount_c2$sumcost,"Dollars","<br/>")
      if(input$State2=="All"){
      fundcounty <- leaflet(hcount_c2)%>%
        addTiles()%>%
        addPolygons(data = hcount_c2,fillColor=~pal3(hcount_c2$sumcost), 
                    stroke = T,popup=pop3,fillOpacity = 0.7,weight = 0.5,
                    color = "gray")%>%
        addLegend("bottomright",values = ~sumcost,pal = pal3,
                  title = "Estimated total cost")}else{
                    hcount_c2<-filter(hcount_c2,state==input$State2)
                    pal3 <- colorNumeric("YlOrRd",domain=hcount_c2$sumcost)
                    pop3 <- paste("County:",hcount_c2$county,"<br/>",
                                  "estimated total cost:",hcount_c2$sumcost,"Dollars","<br/>")
                    leaflet(hcount_c2)%>%
                      addTiles()%>%
                      addPolygons(data = hcount_c2,fillColor=~pal3(hcount_c2$sumcost), 
                                  stroke = T,popup=pop3,fillOpacity = 0.5,weight = 0.5,
                                  color = "gray")%>%
                      addLegend("bottomright",values = ~sumcost,pal = pal3,
                                title = "Estimated total cost")
                  }
    })
    
    output$numy <- renderPlot({
      #hurricane <- filter(hurricane,year(incidentBeginDate)>=input$rangenumy&year(incidentBeginDate)<=input$rangenumy)
      ggplot(data = hurricane)+geom_bar(aes(factor(year(incidentBeginDate)),fill=state))+
        labs(x="year",title = "The number of hurricane in each year")
    })
    
    
    output$numm <- renderPlot({
      hurricanem <- filter(hurricane,beginyear>=input$rangenumm[1]&beginyear<=input$rangenumm[2])
      ggplot(data = hurricanem)+geom_bar(aes(factor(month(incidentBeginDate)),fill=state))+
        labs(x="month",title = "The number of hurricane in each month")
    })
    
    output$time  <- renderPlot({
      hurricanet <- filter(hurricane,beginyear>=input$rangetime[1]&beginyear<=input$rangetime[2])
      hurricanet$incidentBeginDate <- ymd_hms(hurricanet$incidentBeginDate)
      hurricanet$incidentEndDate <- ymd_hms(hurricanet$incidentEndDate)
      ggplot(data = hurricanet)+geom_histogram(aes(as.duration(incidentEndDate-incidentBeginDate)/604800))+
        labs(x="lasting weeks",title = "count of lasting weeks")
    })
    
    output$top <- renderTable({
      rank10
    })
    
    output$fornumy <- renderUI({
      p("We can see that the number of hurricane disaster in 2017 is the highest. And 2011, 2012, 2018 are 
        at the same middle level. 2016 is quite lower than the years mentioned before. But 2010 and 2009 
        are much small than other years. For 2009, there are only 2, we are not sure it is because missing some records,
        or it is the truth.")
    })
    
    output$fornumm <- renderUI({
      p("It is clear that majority of hurricanes happened during month 8-10, but there are also small number of
         hurricane at Feb, Mar and Nov.")
    })
    
    output$fortime <- renderUI({
      p("Most hurricane would last no more than 2 weeks. But still some hurricane lasted 4 or more wee")
    })
    
    output$f.1 <- renderPlot({
      grid.arrange(p1.1,p1.2,ncol=2)
    })
    
    output$f.2 <- renderPlot({
      grid.arrange(p2.1,p2.2,p2.3,ncol=3)
      
    })
    
    output$f.3 <- renderPlot({
      grid.arrange(p3.1,p3.2,ncol=2)
    })
    
    output$f.4 <- renderPlot({
      ##sum of projectAmount for different damageCategory
      dccsum <- PAFPD_0918 %>% group_by(dcc) %>% summarize(sum = sum(projectAmount),.groups='drop')
      dccsum[rev(order(dccsum$sum)),]
      ##Barplot of projectAmount for different damageCategory
      ggplot(data = dccsum, mapping = aes(x = dcc, y = sum, fill = dcc)) + 
        geom_bar(stat = "identity") + 
        guides(fill=guide_legend(title=NULL)) + 
        scale_fill_discrete(labels=c("A-Debris Utilities", "B-Protective Measures", "C-Roads and Bridges", "D-Water Control Facilities", "E-Public Buildings", "F-Public Utilities", "G-Recreational or Other", "Z-State Management")) + 
        xlab("damageCatrgory") + 
        ylab("sum of projectAmount") + 
        ggtitle("Barplot of projectAmount for different damageCategory") + 
        theme(plot.title = element_text(hjust = 0.5))
    })
}
# Run the application 
shinyApp(ui = ui, server = server)
