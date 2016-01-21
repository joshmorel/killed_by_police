library(shiny)
thecounted_and_crime <- read.csv("../../data/thecounted_and_crime.csv",stringsAsFactors = FALSE)
states <- thecounted_and_crime$state
states <- c("USA",states)
states <- setNames(states,c("United States",thecounted_and_crime$state_name))

library(shiny)
shinyUI(fluidPage(
        #Fluid pages scale their components in realtime to fill available browser width.
        titlePanel(title ="People Killed by Police in the United States - 2015"),
        # Sidebar layout creates a layout with a sidebar and main area
        sidebarLayout(position="right",
                      sidebarPanel(h3("State stats"),
                                   selectInput("state","See state stats", choices=states,selected="USA"),
                                   p("State name"),
                                   uiOutput("state_name"),
                                   p("Number killed by police"),
                                   uiOutput("killedbypolice"),
                                   p("blah"),
                                   uiOutput("ggvisplot_ui"),
                                   tableOutput("tbl_out")

                                   #textOutput("mydata")
                                   #tableOutput("mydata")
                      ),
                      mainPanel(
                              tabsetPanel(type="tab",
                                          tabPanel("Frontpage",
                                                   ggvisOutput("ggvisplot")

                                          ),
                                          tabPanel("Summary",
                                                   textOutput("something")
                                                   #tableOutput("mydata")
                                          ),
                                          tabPanel("Data",
                                                   downloadButton(outputId="dltbl",label="Download table"),
                                                   tableOutput("mydata")
                                          ),
                                          tabPanel("Plot",
                                                   plotOutput("myplot"),
                                                   downloadButton(outputId="dlplot",label="Download plot")
                                          )
                              )
                      )
        )
))
