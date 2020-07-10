#Necessary Packages for App
library(shiny)
library(shinythemes)
library(lubridate)
library(plyr)
library(tidyverse)
library(readr)

###The code for Ohio Analysis (module 03)###
Ohio_data <- read_rds("data/statewide.rds")
Ohio_data <- filter(Ohio_data, !is.na(county_name), !is.na(subject_race))
Ohio_data <- Ohio_data %>%
    mutate(
        citation_issued = NA
    )
Ohio_data <-Ohio_data%>%
    mutate(year = year(date))

race_Ohio_data <- Ohio_data %>%
    group_by(subject_race) %>%
    summarise(
        arrest_rate = mean(arrest_made, na.rm = TRUE),
        search_rate = mean(search_conducted, na.rm = TRUE),
        warning_rate = mean(warning_issued, na.rm = TRUE)
    )
###The code for Columbus Analysis(module 01)###
Columbus_data <- read_rds("data/columbus.rds")

Columbus_data <- filter(Columbus_data, !is.na(subject_race))

Columbus_data <-Columbus_data%>%
  mutate(year = year(date))

race_Columbus_data <- Columbus_data %>%
  group_by(subject_race) %>%
  summarise(
    arrest_rate = mean(arrest_made, na.rm = TRUE),
    search_rate = mean(search_conducted, na.rm = TRUE),
    warning_rate = mean(warning_issued, na.rm = TRUE)
  )

###The code needed for creating the data frame for Spatial Analysis
###Order necessary due to package conflicts

Ohio_data$county_name <- gsub("\\s*\\w*$", "", Ohio_data$county_name)
Ohio_data$county_name <- toupper(Ohio_data$county_name)

race_rates <- Ohio_data %>%
    group_by(county_name, subject_race) %>%
    summarise(
        arrest_rate = mean(arrest_made, na.rm = TRUE),
        warning_rate = mean(warning_issued, na.rm = TRUE),
        search_rate = mean(search_conducted, na.rm = TRUE)
    )

race_rates_bw <- filter(race_rates, subject_race == "black" | subject_race == "white")
race_rates_b <- filter(race_rates_bw, subject_race == "black")
race_rates_w <- filter(race_rates_bw, subject_race == "white")
rm(race_rates_bw)
race_rates_bw <- merge(race_rates_b, race_rates_w, by = "county_name")
race_rates_bw$arrest_rate_Delta <- race_rates_bw$arrest_rate.x - race_rates_bw$arrest_rate.y
race_rates_bw$search_rate_Delta <- race_rates_bw$search_rate.x - race_rates_bw$search_rate.y
race_rates_bw$warning_rate_Delta <- race_rates_bw$warning_rate.x - race_rates_bw$warning_rate.y


#For generating the colopleth maps. helpers.R uses the maps and mapproj packages in R
library(maps)
library(mapproj)
# helpers.R is an R script that can help you make choropleth maps
library(rgeos)
library(rgdal)
library(maptools)
library(ggrepel)
library(scales)

###Generates Shape Files for Spatial Analysis
oh_shp <- readOGR("data/REFER_COUNTY.shp")
oh_shp <- fortify(oh_shp, region = "COUNTY") #shape file for map
#Generates County Labels
county_labels <- ddply(oh_shp, .(id), summarize, clat = mean(lat), clong = mean(long))


server <- function(input, output, session) {


        ###First module 01: "Columbus Analaysis"
        observeEvent(input$picture1, {
            updateTabsetPanel(session, "module01",
                              selected = input$picture1
            )
        })

       #for arrest_rate
        output$plot1 <- renderPlot({
            y_input <- switch(input$picture1,
                        "arrest_rate" = race_Columbus_data$arrest_rate,
                        "search_rate" = race_Columbus_data$search_rate,
                        "warning_rate" = race_Columbus_data$warning_rate
            )
            title <- switch(input$picture1,
                            "arrest_rate" = "Arrest Rate",
                            "search_rate" = "Search Rate",
                            "warning_rate" = "Warning Rate"
            )
             ggplot(race_Columbus_data, aes(x = subject_race, y = y_input,  fill = subject_race)) +
                 geom_bar(stat = "identity", position = position_dodge()) +
                 labs(title = c("Racial Disparities in ", title ,"in Columbus"),
                      x = "Driver Race",
                      y = input$picture1,
                      fill = "Driver Race")



                        # ggplot...
            # Code for testing ggplot
            #ggplot(data = mpg) +
                #geom_point(mapping = aes(x = displ, y = hwy, color = class))

        })
        ###Second module 02: "Difference Blakck and white Analysis"
        observeEvent(input$picture2, {
            updateTabsetPanel(session, "module02",
                              selected = input$picture2
            )
        })

        #for arrest_rate
        output$plot21 <- renderPlot({

            ##The switch button to change the value of input of y for ggplot
            rate <- switch(input$picture2,
                           "arrest_rate" = race_rates_bw$arrest_rate_Delta,
                           "search_rate" = race_rates_bw$search_rate_Delta,
                           "warning_rate" = race_rates_bw$warning_rate_Delta
            )

            Difference <- switch(input$picture2,
                                 "arrest_rate" = race_rates_bw$arrest_rate_Delta,
                                 "search_rate" = race_rates_bw$search_rate_Delta,
                                 "warning_rate" = race_rates_bw$warning_rate_Delta
            )

            quantity <- race_rates_bw$arrest_rate_Delta
            ggplot() + geom_map(data = race_rates_bw, aes(map_id = county_name, fill = Difference),
                                map = oh_shp) +
                expand_limits(
                    x = oh_shp$long -1,
                    y = oh_shp$lat) +
                scale_fill_gradient2(
                    low = "red",
                    mid = "white",
                    midpoint = median(rate),
                    high = scales::muted("blue"),
                    limits = c(min(rate), max(rate))) +
                geom_text_repel( aes(x = clong, y = clat, label = id),
                                 data = county_labels,
                                 size = 3,
                                 point.padding = NA) +
                theme_minimal() +
                theme(axis.text.x=element_blank(),
                      axis.text.y = element_blank())+
                labs(title = "Difference in Black & White Rates")

        })


        ##Third module 03: "Ohio State Analysis"
        #@input:
        ##plot1 is the output obejct from mainPanel;
        ##'input$picture3' is the selection made from user, arrest_rate, search_rate, warning_rate

        observeEvent(input$picture3, {
            updateTabsetPanel(session, "module03",
                              selected = input$picture3
            )
        })

        #for arrest_rate
        output$plot31 <- renderPlot({
            y <- switch(input$picture3,
                        "arrest_rate" = race_Ohio_data$arrest_rate,
                        "search_rate" = race_Ohio_data$search_rate,
                        "warning_rate" = race_Ohio_data$warning_rate
            )
            title <- switch(input$picture3,
                            "arrest_rate" = "Arrest Rate",
                            "search_rate" = "Search Rate",
                            "warning_rate" = "Warning Rate"
            )
            ggplot(race_Ohio_data, aes(x = subject_race, y, fill = subject_race)) +
                geom_bar(stat = "identity", position = position_dodge()) +
                labs(title = c("Racial Disparities in ", title ,"in Ohio"),
                     x = "Driver Race",
                     y = input$picture3,
                     fill = "Driver Race")

        })

        ###"Forth Module: "Spatial Analysis update"###

        observeEvent(input$picture4, {
            updateTabsetPanel(session, "module04",
                              selected = input$picture4
                              )
            })



        output$plot41 <- renderPlot({

            race <- switch(input$picture4,
                           "asian" = "asian/pacific islander",
                           "black" = "black",
                           "hispanic" = "hispanic",
                           "other" =  "other/unknown",
                           "white" = "white"
            )
            quantity <- filter(race_rates, subject_race == race)$arrest_rate
            ggplot() + geom_map(data = filter(race_rates, subject_race == race), aes(map_id = county_name, fill = arrest_rate),
                                map = oh_shp) +
                expand_limits(
                    x = oh_shp$long -1,
                    y = oh_shp$lat) +
                scale_fill_gradient2(
                    low = "red",
                    mid = "white",
                    midpoint = median(race_rates$arrest_rate),
                    high = scales::muted("blue"),
                    limits = c(min(race_rates$arrest_rate), max(race_rates$arrest_rate)+.01)) +
                geom_text_repel( aes(x = clong, y = clat, label = id),
                                 data = county_labels,
                                 size = 3,
                                 point.padding = NA) +
                theme_minimal() +
                theme(axis.text.x=element_blank(),
                      axis.text.y = element_blank())+
                labs(title = "Selected Race Arrest Rate Distribution",
                     fill = "Arrest Rate")

            ##Generating  the graph

            # ggplot...
            #Code for testing ggplot
            # ggplot(data = mpg) +
            #     geom_point(mapping = aes(x = displ, y = hwy, color = class))
        })

        output$plot42 <- renderPlot({

            race <- switch(input$picture4,
                           "asian" = "asian/pacific islander",
                           "black" = "black",
                           "hispanic" = "hispanic",
                           "other" =  "other/unknown",
                           "white" = "white"
            )
            quantity <- filter(race_rates, subject_race == race)$search_rate
            ggplot() + geom_map(data = filter(race_rates, subject_race == race), aes(map_id = county_name, fill = search_rate),
                                map = oh_shp) +
                expand_limits(
                    x = oh_shp$long -1,
                    y = oh_shp$lat) +
                scale_fill_gradient2(
                    low = "red",
                    mid = "white",
                    midpoint = median(race_rates$search_rate),
                    high = scales::muted("blue"),
                    limits = c(min(race_rates$search_rate), max(race_rates$search_rate)+.01)) +
                geom_text_repel( aes(x = clong, y = clat, label = id),
                                 data = county_labels,
                                 size = 3,
                                 point.padding = NA) +
                theme_minimal() +
                theme(axis.text.x=element_blank(),
                      axis.text.y = element_blank())+
                labs(title = "Selected Race Search Rate Distribution",
                     fill = "Search Rate")
            ##Genearting  the graph

            # ggplot...
            #Code for testing ggplot
            # ggplot(data = mpg) +
            #     geom_point(mapping = aes(x = displ, y = hwy, color = class))
        })

        output$plot43 <- renderPlot({

            race <- switch(input$picture4,
                           "asian" = "asian/pacific islander",
                           "black" = "black",
                           "hispanic" = "hispanic",
                           "other" =  "other/unknown",
                           "white" = "white"
            )
            quantity <- filter(race_rates, subject_race == race)$warning_rate
            ggplot() + geom_map(data = filter(race_rates, subject_race == race), aes(map_id = county_name, fill = warning_rate),
                                map = oh_shp) +
                expand_limits(
                    x = oh_shp$long -1,
                    y = oh_shp$lat) +
                scale_fill_gradient2(
                    low = "red",
                    mid = "white",
                    midpoint = median(race_rates$warning_rate),
                    high = scales::muted("blue"),
                    limits = c(min(race_rates$warning_rate), max(race_rates$warning_rate)+.01)) +
                geom_text_repel( aes(x = clong, y = clat, label = id),
                                 data = county_labels,
                                 size = 3,
                                 point.padding = NA) +
                theme_minimal() +
                theme(axis.text.x=element_blank(),
                      axis.text.y = element_blank())+
                labs(title = "Selected Race Warning Rate Distribution",
                     fill = "Warning Rate")

            ##Genearting  the graph

            # ggplot...
            #Code for testing ggplot
            # ggplot(data = mpg) +
            #     geom_point(mapping = aes(x = displ, y = hwy, color = class))
        })

        #Showing current time(Optional)
        output$currentTime <- renderText({
            invalidateLater(1000, session) #1000 milisecond = 1 sec
            paste("The current time is", Sys.time())
        })



        ###5th module 05:
        observeEvent(input$picture5, {
          updateTabsetPanel(session, "module05",
                            selected = input$picture5
          )
        })

        output$table5 <- renderTable({

          if(input$picture5 == "arrest_made"){


          Ohio_data %>%
            group_by(county_name) %>%
            summarise(arrest_rate = percent(mean(arrest_made, na.rm = TRUE))) %>%
            arrange(desc(arrest_rate) )%>%
            top_n(input$picture52)

          }
          else if(input$picture5 == "search_conducted"){
            Ohio_data %>%
              group_by(county_name) %>%
              summarise(search_rate = percent(mean(search_conducted, na.rm = TRUE))) %>%
              arrange(desc(search_rate) )%>%
              top_n(input$picture52)
          }
          else if(input$picture5 == "warning_issued"){
            Ohio_data %>%
              group_by(county_name) %>%
              summarise(warning_rate = percent(mean(warning_issued, na.rm = TRUE))) %>%
              arrange(desc(warning_rate) )%>%
              top_n(input$picture52)
          }
        }
        )
    }#End of server