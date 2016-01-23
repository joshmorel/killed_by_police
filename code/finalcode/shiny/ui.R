require(shiny)
require(ggvis)
thecounted_and_crime <- read.csv("data/thecounted_and_crime.csv",stringsAsFactors = FALSE)
#thecounted_and_crime_labels <- read.csv("data/the.csv",stringsAsFactors = FALSE)
states <- thecounted_and_crime$state
states <- c("USA",states)
states <- setNames(states,c("United States",thecounted_and_crime$state_name))

shinyUI(fluidPage(
        titlePanel(title ="People Killed by Police in the United States - 2015"),
        fluidRow(
                column(8,
                       tabsetPanel(
                               tabPanel("Plot",
                                        ggvisOutput("ggvisplot"),
                                        wellPanel(
                                        fluidRow(h4("Plot controls"),
                                               column(4,
                                                        selectInput("xvar","Select x-variable", choices=c("Killed by Police per 100k" = "killedbypolice2015_per100k",
                                                                                         "Murder & Non-neg. Manslaughter per 100k" = "murder_nonnegligent_manslaughter2014_per100k",
                                                                                         "Violent Crime per 100k" = "violent_crime2014_per100k",
                                                                                         "Police Officers Employed per 100k" = "police_officers2014_per100k"),
                                                   selected="killedbypolice2015_per100k")
                                                   ),
                                               column(4,
                                                      selectInput("yvar","Select y-variable", choices=c("No y variable (dot-plot)" = "",
                                                                                         "Killed by Police per 100k" = "killedbypolice2015_per100k",
                                                                                         "Murder & Non-neg. Manslaughter per 100k" = "murder_nonnegligent_manslaughter2014_per100k",
                                                                                         "Violent Crime per 100k" = "violent_crime2014_per100k",
                                                                                         "Police Officers Employed per 100k" = "police_officers2014_per100k"),
                                                   selected="")
                                               ),
                                               column(4,
                                                      radioButtons("includeDC","Include Distrinct of Columbia? (outlier)",choice = list("Yes","No"),selected="Yes")
                                               )
                                        )
                                )
                               ),
                               tabPanel("Map",verbatimTextOutput("something")),
                                tabPanel("Data",verbatimTextOutput("hello"))
                               
                               
                )
                               
                ),
                column(4,
                        wellPanel(
                                h3("State violence & police statistics"),
                                selectInput("state","See state stats", choices=states,selected="USA"),
                                tableOutput("tbl_out")
                        )
                )
        
        )
))
