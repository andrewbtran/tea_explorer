
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(tidycensus)
library(tigris)
#library(censusapi)
#source("key.R")
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
    
    #unemployment <- get_acs(geography="tract", endyear=2015, variables= c("B23025_005E", "B23025_002E"),  state=state_abb)
    #unemployment$moe <- NULL
    #unemployment <- spread(unemployment, variable, estimate)
    
    
    single_proj <- filter(teas, selector==proj_name)
    tracts <- unlist(single_proj$tracts_list)
    county_only <- data.frame(substr(tracts, 1,5))
    colnames(county_only) <- "tract_name"
    county_only <- county_only %>%
      group_by(tract_name) %>%
      summarize(count=n()) %>%
      arrange(desc(count)) %>%
      slice(1)
    county_only <- as.character(county_only$tract_name)
    
    county_unemployment <- filter(states_counties_tracts, state_county == county_only)
    county_unemployment$per_un <- round(county_unemployment[,10]/county_unemployment[,11]*100,2)
    county_unemployment$rank <- rank(county_unemployment$per_un)
    county_unemployment$per_un <- county_unemployment$per_un$B23025_005
    names(county_unemployment)[names(county_unemployment) == 'tract_full'] <- 'id'
    
    # DO SOMETHING ABOUT THE ZERO UNEMPLOYMENT/POPULATION
      
    unemployment <- filter(states_counties_tracts, tract_full %in% tracts)
    unemployment$per_un <- round(unemployment[,10]/unemployment[,11]*100,2)
    #unemployment$rank <- rank(unemployment$per_un)
    unemployment$per_un <- unemployment$per_un$B23025_005
    #tracts <- c("34017004400", "34017004500")
    #unemployment <- data.frame(unemployment)
    names(unemployment)[names(unemployment) == 'tract_full'] <- 'id'
    
    # IMPORT MAP HERE NOWWWW
    
    state_map <- readOGR("sm_shapes", county_only)
    
    #state_map <- tracts(state_abb, cb=F)
    state_map <- fortify(state_map, region="GEOID")
    state_map <- left_join(state_map, unemployment)
    state_map <- filter(state_map, !is.na(per_un))
    
    #addy <- geocode(location=single_proj$project_address, output="latlon", source="google")
    #location <- c(addy$lat, addy$lon)
    
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
    map <- map + annotate("point", x = single_proj$lon, y = single_proj$lat, colour = "red", size = 1) 
    #map <- map + annotate("text", x = -74.01, y = 40.734330, label = "1 Journal Square", size=3, colour="gray30") 
    
    print(map)
  })
  
  
  output$table <- renderDataTable({
    proj_name <- input$project
    filter(teas, selector==proj_name)

    single_proj <- filter(teas, selector==proj_name)
    tracts <- unlist(single_proj$tracts_list)
    
    unemployment <- filter(states_counties_tracts, tract_full %in% tracts)
    unemployment$per_un <- round(unemployment[,10]/unemployment[,11]*100,2)
    #unemployment$rank <- rank(unemployment$per_un)
    unemployment$per_un <- unemployment$per_un$B23025_005
    
    #tracts <- c("34017004400", "34017004500")
    #unemployment <- data.frame(unemployment)
    #unemployment <- filter(unemployment, GEOID %in% tracts)
    un_rate <- round(sum(unemployment$B23025_005) / sum(unemployment$B23025_002) *100,2)
    unemployment$NAME <- gsub(",.*", "", unemployment$NAME)
    
    county_only <- data.frame(substr(tracts, 1,5))
    colnames(county_only) <- "tract_name"
    county_only <- county_only %>%
      group_by(tract_name) %>%
      summarize(count=n()) %>%
      arrange(desc(count)) %>%
      slice(1)
    county_only <- as.character(county_only$tract_name)
    
    county_unemployment <- filter(states_counties_tracts, state_county == county_only)
    county_unemployment$per_un <- round(county_unemployment[,10]/county_unemployment[,11]*100,2)
    county_unemployment$rank <- rank(county_unemployment$per_un)
    county_unemployment$per_un <- county_unemployment$per_un$B23025_005
    county_unemployment <- select(county_unemployment, tract_full, rank)
    
    unemployment <- left_join(unemployment, county_unemployment)
    
    select(unemployment, tract=tract_name, `percent unemployment`=per_un, rank)
  })
  
})
