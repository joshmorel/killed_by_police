require(shiny)
require(rCharts)
require(data.table)
require(RColorBrewer)

combined_state_stats = fread("../../data/combined_state_stats.csv",showProgress = FALSE)

# Discretize the number killed by police per 100k, labels are max of quantile range
fillkey = round(quantile(combined_state_stats$killedbypolice2015_per100k,seq(0, 1, 1/5)),2)
fillkey = fillkey[-1]
combined_state_stats[,fillKey := cut(combined_state_stats$killedbypolice2015_per100k,
                                     5,labels=fillkey)]

# Format pop-up in html format with numbers used in calculation for contextual information  
combined_state_stats[,popup:= paste("<strong>Number Killed By Police</strong>:",
                                    as.character(killedbypolice2015),
                                    "<br><strong>Population</strong>:",
                                    prettyNum(combined_state_stats$population2014,big.mark=',',scientific=FALSE))]


# DataMaps requires lists of lists, where each top level list item is named with 
#state abbreviation, with each state as itself a list with data attributes

killed_for_datamap <- combined_state_stats[,.(State=state,fillKey,popup)]

payload = toJSONArray2(killed_for_datamap, json=FALSE)
names(payload) = lapply(payload2, '[[', 'State')




shinyServer(
        function(input,output) {
                lat <- reactive({
                        as.numeric(input$lat)
                })
                long <- reactive({
                        as.numeric(input$long)
                })
                # renderMap is rCharts function for integration with Shiny
                output$mymap <- renderMap({
                        latlong = c(lat(),long())
                        map <- Leaflet$new()
                        map$setView(latlong, zoom = 8)
                        map$marker(latlong,bindPopup="<p> You are here now!</p>")
                        map
                })
                
                output$choropleth <- renderMap({
                        map <- Datamaps$new()
                        map$set(
                                dom = 'chart_1',
                                scope = 'usa',
                                fills = fills,
                                data = payload,
                                legend = TRUE,
                                labels = TRUE,
                                geographyConfig  = list(
                                        popupTemplate = "#! function(geography, data){
    return '<div class=hoverinfo><strong>' + geography.properties.name + '</strong><br>' + data.popup + '</div>';
                } !#"
                                )
                        )
                        map
                })
                
        }
)