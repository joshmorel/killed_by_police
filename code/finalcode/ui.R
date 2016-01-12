library(shiny)
thecounted_and_crime = read.csv("../../data/thecounted_and_crime.csv",stringsAsFactors = FALSE)


shinyUI(
        #fluidPage(
        #        fluidRow(
        #                dataTableOutput(outputId="table")
        #        )
        #)
        pageWithSidebar(
        headerPanel("The Counted - People Killed by Police in the U.S. in 2015"),
        sidebarPanel(
                radioButtons("plotchoice","Explore Data",
                             c("Descriptive" = "descriptive",
                               "Modeled" = "modeled",
                               "User-supplied X vs Y" = "xvsy")
                ),
                radioButtons("dc","Include DC?\n(DC is outlier for many variables)",
                             c("Yes" = TRUE,
                               "No" = FALSE)
                ),
                selectInput("gg_x", "X-variable for User-supplied X vs Y scatter plot:",
                            choices = c("Violent Crime per 100k Population" = "violent_crime2014_per100k",
                                        "Murder & Non Negligent Manslaughter per 100k Population" = "murder_nonnegligent_manslaughter2014_per100k",
                                        "Police Officers Employed - per 100k Population" = "police_officers2014_per100k",
                                        "Black or African American - Percent of Population" = "population_black2014_percent",
                                        "Killed by Police - per 100k Population" = "killedbypolice2015_per100k")
                            
                ),
                selectInput("gg_y", "Y-variable for User-supplied X vs Y scatter plot:",
                            choices = c("Killed by Police per 100k Population" = "killedbypolice2015_per100k",
                                        "Violent Crime per 100k Population" = "violent_crime2014_per100k",
                                        "Murder & Non Negligent Manslaughter per 100k Population" = "murder_nonnegligent_manslaughter2014_per100k",
                                        "Police Officers Employed - per 100k Population" = "police_officers2014_per100k",
                                        "Black or African American - Percent of Population" = "population_black2014_percent")
                ),
                
                selectInput("state", "Select state to see stats:",
                            choices = thecounted_and_crime$state
                )
                
        ),
        mainPanel(
             plotOutput('gg_points')   
        )
)
)
