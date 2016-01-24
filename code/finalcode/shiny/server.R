require(shiny)
require(ggvis)
require(dplyr)
require(leaflet)

# State-level data
thecounted_and_crime <- read.csv("data/thecounted_and_crime.csv",stringsAsFactors = FALSE)
thecounted_and_crime_usa <- read.csv("data/thecounted_and_crime_usa.csv",stringsAsFactors = FALSE)
thecounted_and_crime_labels <- read.csv("data/killed_by_police_code_book.csv",stringsAsFactors = FALSE)
rownames(thecounted_and_crime) <- thecounted_and_crime$state
thecounted_and_crime$id <- 1:nrow(thecounted_and_crime)
Statistic <-  c("State", "State Name", "Population (2014)", "Police Officers (2014)", "Police Officers per 100k Pop.",
                "Killed by Police (2015)", "Killed by Police per 100k Pop.", "Murder & Non-neg. Manslaughter (2014)", "Murder & Non-neg. Manslaughter per 100k Pop.",
                "Violent Crime (2014)", "Violent Crime per 100k Pop.")

# Individual-level geocoded data for map
thecounted_geocoded <- read.csv("data/thecounted_geocoded.csv",stringsAsFactors = FALSE)
thecountedurl <- "http://theguardian.com/thecounted/list"
thecounted_geocoded$url <- with(thecounted_geocoded,paste(thecountedurl,paste(gsub("\\s","-",tolower(name)),as.character(uid),sep="-"),sep="#"))
thecounted_geocoded$popup <- with(thecounted_geocoded,paste("<p><a target=\"_blank\" href=\"",url,"\">",name,"</a><br>",
                                                            age,", ",gender,", ",raceethnicity,"<br>",
                                                            city,", ",state,"<br>",
                                                            "Death By: ",classification,"<br>",
                                                            "Armed With: ",armed,"</p>",
                                                            sep=""))


shinyServer(
        # Add session when using updateSelectInput
        function(input, output,session) {
                #For display of statistics in side panel, defaults to United States totals
                update_side_table <- function(stateIn="USA") {
                        if(stateIn=="USA") {
                                Value <- select(thecounted_and_crime_usa, state, state_name,population2014, police_officers2014,police_officers2014_per100k,killedbypolice2015,killedbypolice2015_per100k,murder_nonnegligent_manslaughter2014,murder_nonnegligent_manslaughter2014_per100k,violent_crime2014,violent_crime2014_per100k)
                        }
                        else {
                                Value <- filter(thecounted_and_crime,state==stateIn) %>% select(state, state_name,population2014, police_officers2014,police_officers2014_per100k,killedbypolice2015,killedbypolice2015_per100k,murder_nonnegligent_manslaughter2014,murder_nonnegligent_manslaughter2014_per100k,violent_crime2014,violent_crime2014_per100k)
                        }
                        Value <- as.character(t(Value))
                        thecounted_and_crime_display <- data.frame(Statistic,Value)
                        output$tbl_out <- renderTable({
                                thecounted_and_crime_display
                        })
                }
                
                output$helpBox <- renderUI({
                        #Action button starts at 0 and increments by one, so display with each second click 
                        if (input$helpButton %% 2) {
                                helpText("Explore stats related to violence, crime & police. \
                                         Choose between dot-plot (just x-variable) or scatter plot (add y variables). \
                                         Size in dot-plot represents the numerator for the displayed rate (e.g. Number Killed for Killed by Police per 100k).\
                                         Hover over each state to see more stats, and click to update the complete table to the right. \
                                        Click help again to close this text.")
                        } else return()
                })

                plotData <- reactive({
                        if(input$yvar=="none"){
                                xlabel <- thecounted_and_crime_labels[thecounted_and_crime_labels$variable==input$xvar,]
                                d <- thecounted_and_crime[,c("state",input$xvar,xlabel$numerator,"id")]
                                if(input$includeDC=="No") {
                                        d <- d[d$state!="DC",]
                                }
                                d$xlabel <- xlabel$label
                                names(d) <- c("state","xvar","xcount","id","xlabel")
                                d
                        }
                        else {
                                xlabel <- thecounted_and_crime_labels[thecounted_and_crime_labels$variable==input$xvar,]
                                ylabel <- thecounted_and_crime_labels[thecounted_and_crime_labels$variable==input$yvar,]
                                d <- thecounted_and_crime[,c("state",input$xvar,input$yvar,xlabel$numerator,"id")]
                                if(input$includeDC=="No") {
                                        d <- d[d$state!="DC",]
                                }
                                d$xlabel <- xlabel$label
                                d$ylabel <- ylabel$label
                                names(d) <- c("state","xvar","yvar","xcount","id","xlabel","ylabel")
                                d
                        }
                })

                tip_value <- function(x) {
                        if(is.null(x$id)) return(NULL)
                        r <- thecounted_and_crime[thecounted_and_crime$id == x$id,]
                        paste0("<b>",r$state_name,"</b> --> click to view more stats",
                               "<br>People killed by police: ",r$killedbypolice2015,
                               "<br>Murder & non-negligent homicide: ",r$murder_nonnegligent_manslaughter2014,
                               "<br>Violent crime: ",formatC(r$violent_crime2014, format="d",big.mark=','),
                               "<br>Police officers employed: ",formatC(r$police_officers2014, format="d",big.mark=','),
                               "<br>Total population: ",formatC(r$population2014, format="d",big.mark=','),
                               collapse="")
                }
                
                update_selection <- function(data,location,session){
                        # Prevent action when clicking on smoother 
                        if(is.null(data$id)) return(NULL)
                        cat(as.character(names(data)))
                        r <- thecounted_and_crime[thecounted_and_crime$id == data$id,]
                        updateSelectInput(session
                                          ,"state"
                                          ,selected=r$state)
                }
                render_dotplot <- function(plotData) {
                        #ggvis_0.4.2 has bug where scaling of points is not done correctly, so creating workaround hack
                        size_scaler <- (min(plotData$xcount)+1)/3
                        size_max <- max(plotData$xcount)
                        xlabel <- plotData$xlabel[1]
                        plotData %>% ggvis(~xvar,~reorder(state,-xvar),key := ~id) %>%
                                layer_points(fillOpacity := .5,fillOpacity.hover:=.7,
                                             size:=~xcount/size_scaler,
                                             stroke:="red",strokeWidth:=0,
                                             strokeWidth.hover:=10) %>%
                                add_axis("y",title= "State") %>%
                                add_axis("x",title=xlabel) %>%
                                add_tooltip(tip_value,"hover") %>%
                                handle_click(update_selection) %>%
                                set_options(width = "auto", height = "500", resizable=FALSE) 
                }
                render_scatterplot <- function(plotData) {
                       xlabel <- plotData$xlabel[1]
                       ylabel <- plotData$ylabel[1]
                       plotData %>% ggvis(~xvar,~yvar) %>%
                               layer_smooths(se=TRUE) %>%
                                add_axis("y",title=ylabel) %>%
                               add_axis("x",title=xlabel) %>%
                               layer_points(fillOpacity := .5,fillOpacity.hover:=.7,fill.hover:="red",key := ~id) %>%
                               add_tooltip(tip_value,"hover") %>%
                               handle_click(update_selection) %>%
                               set_options(width = "auto", height = "600", resizable=FALSE)
                }
                #}

                reactive({
                        if(input$yvar=="none") {
                                render_dotplot(plotData())
                        }
                        else {
                                render_scatterplot(plotData())       
                        }
                        # In shiny apps, need to register ggvis observers
                        # and tell shiny where to put the controls
                }) %>% bind_shiny("ggvisplot")
                
                # Listens for any change of input$state and updates table accordingly 
                observeEvent(input$state, {
                        update_side_table(input$state)
                })
                
                output$countedmap <- renderLeaflet({

                        leaflet(thecounted_geocoded) %>% addTiles('http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png', 
                                                                  attribution='Map tiles by <a href=\"http://stamen.com">Stamen Design</a>, <a href\"http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a>  &mdash; Map data &copy; <a href=\"http://www.openstreetmap.org/copyright">OpenStreetMap</a>') %>%
                                addCircles(lng = ~long, lat=~lat, popup=~popup, weight = 3, radius=100, 
                                           color="#ffa500", stroke = TRUE, fillOpacity = 0.8)  %>% 
                                setView(lng=-96.35,lat=37.50, zoom = 3)
                }
                )
                
                
                output$dltbl <- downloadHandler(
                        filename="thecounted_and_crime.csv",
                        content=function(file) {
                                write.csv(thecounted_and_crime,file,row.names = FALSE)
                        }
                )
                
                output$dlcodebook <- downloadHandler(
                        filename="thecounted_and_crime_codebook.csv",
                        content=function(file) {
                                write.csv(thecounted_and_crime_labels,file,row.names = FALSE)
                        }
                )
                
                
        }
)
