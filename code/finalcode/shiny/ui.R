require(shiny)
require(ggvis)
thecounted_and_crime <- read.csv("../../data/thecounted_and_crime.csv",stringsAsFactors = FALSE)
states <- thecounted_and_crime$state
states <- c("USA",states)
states <- setNames(states,c("United States",thecounted_and_crime$state_name))

shinyUI(fluidPage(
        titlePanel(title ="People Killed by Police in the United States - 2015"),
        fluidRow(
                column(8,
                       ggvisOutput("ggvisplot")
                       
                ),
                column(4,
                        wellPanel(
                                h5("x count"),
                                textOutput("xcnt"),
                                h5("y count"),
                                textOutput("ycnt"),
                                h3("State stats"),
                                selectInput("state","See state stats", choices=states,selected="USA"),
                                #uiOutput("ggvisplot_ui"),
                                tableOutput("tbl_out")
                                #textOutput("mydata")
                                #tableOutput("mydata")
                        )
                ),
        fluidRow(
                column(12,
                       #Upon selection of xvar, state, count and related rate will be passed to data.frame which will be plotted
                       selectInput("xvar","Select x-variable", choices=c("Killed by Police" = "killedbypolice2015",
                                                                         "Violent Crime" = "violent_crime2014"),
                                                                         selected="killedbypolice2015"),
                       selectInput("yvar","Select y-variable", choices=c("",
                                                                        "Killed by Police" = "killedbypolice2015",
                                                                         "Violent Crime" = "violent_crime2014"),
                                   selected="")
                       
                )
        ),
        fluidRow(
                column(12,
                       tableOutput("tbl2")
                )
        )
        
        )
))
