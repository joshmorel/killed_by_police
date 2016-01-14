require(shiny)
require(rCharts)


shinyUI(fluidPage(
        titlePanel(title ="rCharts Leaflet example"),
        sidebarLayout(position="left",
                      sidebarPanel(
                              numericInput(inputId = 'lat',label='Enter the latitude',value=44.56,min=-90,max=90,step=1),
                              numericInput(inputId = 'long',label='Enter the longitude',value=-80.45,min=-180,max=180,step=1)
                      ),
                      mainPanel(
                              tabsetPanel(type="tab",
                                          tabPanel("Choropleth",
                                                   # Show output is rCharts function, need to specify library, leaflet in this case
                                                   h3("Number of People Killed by Police per 100,000 in the US in 2015"),
                                                   showOutput("choropleth", "datamaps")
                                          ),
                                          tabPanel("DetailMap",
                                                   # Show output is rCharts function, need to specify library, leaflet in this case
                                                   showOutput("mymap", "leaflet")
                                          )
                              )                              
                              

                      )
                      
        )
)
)