library(shiny)
library(shinythemes)
ui <- tagList(
        shinythemes::themeSelector(),
        #h2(textOutput("currentTime")),

        navbarPage(
             theme = shinytheme("flatly"),  # <--- To use a theme, uncomment this
            "Group 3: The Stanford Open Policing Project",

            ##First module 01: "Columbus Analysis"
            tabPanel("Columbus analysis",

                     sidebarPanel(
                       "Outcome Type",
                       selectInput("picture1", "Select a type of metric: ",
                                   choices = c("arrest_rate", "search_rate", "warning_rate"),
                                   selected = "arrest_rate"
                       ),
                       ##TD: Add description for drop down manu(CJ), don't forget to have a common beforehand
                         h3("Analysis: Zhaohong Wang, Description: Cornelius Johnson"),
                         h4("These graphs provide insights into racial disparities within Columbus. From a
                            light inspection, it's clear that hispanics face inordinately high search and
                            arrest rates during stops, with black drivers also facing comparable rates.
                            This is valuable information when compared to the Ohio Analysis graphs, and
                            suggests that there may be some differences in the treatment of minority
                            drivers in urban areas as opposed to Ohio as a whole. ")

                     ),#For sidebarPanel

                     mainPanel(
                         tabsetPanel(id = "module01",
                                     tabPanel("Type of rate",
                                              # value = "1",
                                              value = "",
                                               plotOutput(outputId = "plot1", height = "720", width = "950"),
                                              hr(),
                                              h4("Stop Outcome by Race.")
                                              )
                                     )#For tabsetPanel
            )#For mainPanel
            ),#For tabPanel


            ##Second module 02: "Spatial Analysis"
            tabPanel("Black and White Analysis",

                     sidebarPanel(
                         selectInput("picture2", "Select a type of metric: ",
                                     choices = c("arrest_rate", "search_rate", "warning_rate"),
                                     selected = "arrest_rate"#default value
                         ),
                         ##TD: Need explanation for spatial analysis(CJ)
                         h3("Analysis: Cornelius Johnson, Description: Cornelius Johnson"),
                         h4("This is a chloropleth of Ohio, detailing differences in stop outcome between black
                            and white drivers. Shades closer to light red depict no difference, darker red
                            depicts a higher rate for whites and darker blue depicts a higher rate for blacks. Note
                            the changing magnitudes of differences for different outcome types. Specifically, arrest
                            differences are small but certainly noticeable, and differences increase as severity of outcome
                            decreases. This suggests that, while there are significant differences between blacks and
                            whites, these differences are localized (tending toward one region of Ohio over another) and
                            concentrated in minimally severe types, like warnings/citations instead of arrests/searches.")
                     ),#For sidebarPanel

                     mainPanel(
                         tabsetPanel(id = "module02", ##The id must declared as Literal String
                                     tabPanel("Type of rate",
                                              value = "arrest_rate",
                                              plotOutput(outputId = "plot21", height = "720", width = "950")
                                     )

                         )#For tabsetPanel
                     )#For mainPanel
            ),#For tabPanel

            ##Third module 03: "Ohio Analysis"
            tabPanel("Ohio Analysis",

                     sidebarPanel(
                         "Outcome Type",
                         selectInput("picture3", "Select a type of metric: ",
                                     choices = c("arrest_rate", "search_rate", "warning_rate"),
                                     selected = "arrest_rate"
                         ),

                         h3("Analysis: Zhaohong Wang, Description: Cornelius Johnson"),
                         h4("Analysis of Ohio racial disparities. Note that black drivers face substantially
                            higher arrest rates than drivers of other races, and both black and hispanic
                            drivers have their vehicles searched at much higher rates than other races.
                            However, warning rates are much more equal among races. This suggests that there
                            may be some type of discriminatory practice amongst state patrols on an aggregate
                            level, as blacks and especially hispanics are minorities within the state. ")

                         ##TD: Add description for drop down manu(CJ), don't forget to have a common beforehand

                     ),#For sidebarPanel
                     mainPanel(
                         tabsetPanel(id = "module03",
                                     tabPanel("Type of rate",
                                              value = "arrest_rate",
                                              plotOutput(outputId = "plot31", height = "720", width = "950")
                                     ) #For tabPanel

                         )#For tabsetPanel

                     )#For mainPanel
            ),#For tabPanel(Ohio analysis), end

            ##Fourth module 04: "Spatial Analysis Update"
            tabPanel("Spatial Analysis",

                     sidebarPanel(
                         selectInput("picture4", "Choose a type of race:",
                                     choices = c("asian", "black", "hispanic", "other" ,"white"),
                                     selected = "asian"#default value
                         ),

                         h3("Analysis: Cornelius Johnson, Description: Cornelius Johnson"),
                         h4("This second chloropleth breaks Ohio down at the county level and allows you
                            to see stop type distributions for every race. As before, consider the graph
                            with an ordinal interpretation - darker blue shades indicate higher levels of each
                            stop type, white depicts a more 'average' value, while darker red shades depict
                            lower than normal rates. Legends are consistent across races with respect to
                            type type. Trends from previous graphs are persistent - hispanics face much higher
                            rates of search and arrest within central Ohio than any other race. Blacks face
                            higher rates of arrest and search than Asians and Whites in Ohio overall, with no
                            clear patterns besides slight elevations in Northeast and South Ohio counties.
                            Thus, this graph provides strong insight to the differing racial distributions for
                            each type of stop within different counties. ")
                         ##TD: add an explanation for your spacial analysis, and might what diferent race could do(CJ)
                     ),#For sidebarPanel

                     mainPanel(
                         tabsetPanel(id = "module04",
                                     #Panel 1
                                     tabPanel("Arrest Rate",
                                              value = "arrest_rate"
                                              ,plotOutput(outputId = "plot41", height = "720", width = "950")
                                     ),
                                     # Panel 2
                                     tabPanel("Search Rate",
                                              value = "search_rate"
                                              ,plotOutput(outputId = "plot42", height = "720", width = "950")
                                     ),
                                     #Panel 3
                                     tabPanel("Warning Rate",
                                              value = "warning_rate"
                                              ,plotOutput(outputId = "plot43", height = "720", width = "950")
                                     )#End of all tabPanel
                         )#For tabsetPanel
                     )#For mainPanel

            ),#For tabPanel(Spatial Analysis Update), end

         tabPanel("County's rate",
                  sidebarPanel(
                    "Outcome Type",
                    selectInput("picture5", "Select a type of metric: ",
                                choices = c("arrest_made", "search_conducted", "warning_issued"),
                                selected = "arrest_rate"
                    ),
                    numericInput("picture52", "Number of observations to view", value = 15, min = 1, max = 30)
                  ),
                  mainPanel(

                            tabsetPanel( id = "module05",
                               tabPanel(  "Type of outcome",
                                     value = "Type of outcome",
                                     tableOutput("table5")
                                     )

                            )
                  )
                  ) # For module 5
        )#For navBarpage
    )