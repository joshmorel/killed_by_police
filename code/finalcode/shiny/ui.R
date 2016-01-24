require(shiny)
require(ggvis)
require(leaflet)
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
                                        actionButton('helpButton','Help'),
                                        uiOutput("helpBox"),
                                        ggvisOutput("ggvisplot")),
                               tabPanel("Map",leafletOutput("countedmap")),
                                tabPanel("Data",
                                         hr(),
                                         downloadButton(outputId="dltbl",label="Download state statistics"),
                                         downloadButton(outputId="dlcodebook",label="Download code book")
                                         )
                        ),
                       hr(),
                       includeMarkdown("description.md")
                )
                ,
                column(4,
                        wellPanel(
                                selectInput("xvar","Select x-variable", choices=c("Killed by Police per 100k" = "killedbypolice2015_per100k",
                                                                                  "Murder & Non-neg. Manslaughter per 100k" = "murder_nonnegligent_manslaughter2014_per100k",
                                                                                  "Violent Crime per 100k" = "violent_crime2014_per100k",
                                                                                  "Police Officers Employed per 100k" = "police_officers2014_per100k"),
                                                selected="killedbypolice2015_per100k",
                                #Selectize inputs display on-top of ggvis popups, without a solution, this is best option 
                                selectize = FALSE)
                                ,
                                selectInput("yvar","Select y-variable", choices=c("No y variable (dot-plot)" = "none",
                                                                         "Killed by Police per 100k" = "killedbypolice2015_per100k",
                                                                         "Murder & Non-neg. Manslaughter per 100k" = "murder_nonnegligent_manslaughter2014_per100k",
                                                                         "Violent Crime per 100k" = "violent_crime2014_per100k",
                                                                         "Police Officers Employed per 100k" = "police_officers2014_per100k"),
                                selected="none",selectize=FALSE),
                               radioButtons("includeDC","Include Distrinct of Columbia? (outlier)",choice = list("Yes","No"),selected="Yes"),
                                selectInput("state","State violence & police statistics", choices=states,selected="USA",selectize=FALSE),
                                tableOutput("tbl_out")
                        )
                )
        )
))
