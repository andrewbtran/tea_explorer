
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(tidycensus)
library(tigris)
library(censusapi)
source("key.R")
library(tidyverse)
library(ggmap)
library(viridis)

census_api_key(key)

shinyServer(function(input, output) {

  output$choose_proj <- renderUI({
    state_abb <- input$state 
    proj_list <- filter(teas, state==state_abb)
    
    selectInput("project", "Choose a project:", 
                choices = proj_list$selector)
  })
  
  
  output$Plot <- renderPlot({
    state_abb <- input$state 
    
    proj_name <- input$project
    #filter(teas, selector==proj_name)
    
    unemployment <- get_acs(geography="tract", endyear=2015, variables= c("B23025_005E", "B23025_002E"),  state=state_abb)
    unemployment$moe <- NULL
    unemployment <- spread(unemployment, variable, estimate)
    unemployment$per_un <- round(unemployment[,4]/unemployment[,3]*100,2)
    unemployment$rank <- rank(unemployment$per_un)
    unemployment$per_un <- unemployment$per_un$B23025_005
    single_proj <- filter(teas, selector==proj_name)
    tracts <- unlist(single_proj$tracts_list)
    #tracts <- c("34017004400", "34017004500")
    #unemployment <- data.frame(unemployment)
    unemployment <- filter(unemployment, GEOID %in% tracts)
    names(unemployment)[names(unemployment) == 'GEOID'] <- 'id'
    
    state_map <- tracts(state_abb, cb=F)
    state_map <- fortify(state_map, region="GEOID")
    state_map <- left_join(state_map, unemployment)
    state_map <- filter(state_map, !is.na(rank))
    
    addy <- geocode(location=single_proj$project_address, output="latlon", source="google")
    location <- c(addy$lat, addy$lon)
    
    map <- ggplot()
    #nj_map <- nj_map + geom_polygon(data=nj_hf, aes(x=long, y=lat, group=group), fill=NA, color="black", size=.1)
    map <- map + geom_polygon(data=state_map, aes(x=long, y=lat, group=group, fill=per_un), color="black", size=.5)
    #nj_map <- nj_map + facet_wrap(~year)
    map <- map + coord_map() 
    map <- map + scale_fill_viridis(option = "inferno", direction=-1, name = "Unemployment rate")
    map <- map + scale_color_viridis(option = "inferno", direction=-1)
    map <- map + theme_nothing(legend=TRUE) 
    map <- map + labs(x=NULL, y=NULL, title= proj_name)
    map <- map + theme(panel.grid.major = element_line(colour = NA))
    map <- map + theme(text = element_text(size=15))
    map <- map + theme(plot.title=element_text(face="bold", hjust=.4))
    map <- map + theme(plot.subtitle=element_text(face="italic", size=9, margin=margin(l=20)))
    map <- map + theme(plot.caption=element_text(size=12, margin=margin(t=12), color="#7a7d7e", hjust=0))
    map <- map + theme(legend.key.size = unit(1, "cm"))
    #map <- map + annotate("segment", x = -74.063644, xend = -74.035, y = 40.734330, yend = 40.734330, colour = "tomato", size=.5) 
    map <- map + annotate("point", x = addy$lon, y = addy$lat, colour = "red", size = 1) 
    #map <- map + annotate("text", x = -74.01, y = 40.734330, label = "1 Journal Square", size=3, colour="gray30") 
    
    print(map)
  })
  
  
  output$table <- renderDataTable({
    proj_name <- input$project
    filter(teas, selector==proj_name)

    unemployment <- get_acs(geography="tract", endyear=2015, variables= c("B23025_005E", "B23025_002E"),  state=state_abb)
    unemployment$moe <- NULL
    unemployment <- spread(unemployment, variable, estimate)
    unemployment$per_un <- round(unemployment[,4]/unemployment[,3]*100,2)
    unemployment$rank <- rank(unemployment$per_un)
    unemployment$per_un <- unemployment$per_un$B23025_005
    single_proj <- filter(teas, selector==proj_name)
    tracts <- unlist(single_proj$tracts_list)
    #tracts <- c("34017004400", "34017004500")
    #unemployment <- data.frame(unemployment)
    unemployment <- filter(unemployment, GEOID %in% tracts)
    un_rate <- round(sum(unemployment$B23025_005) / sum(unemployment$B23025_002) *100,2)
    unemployment$NAME <- gsub(",.*", "", unemployment$NAME)
    select(unemployment, tract=NAME, `percent unemployment`=per_un, rank)
  })
  
})
