require(shiny)
require(ggvis)
require(dplyr)

thecounted_and_crime <- read.csv("data/thecounted_and_crime.csv",stringsAsFactors = FALSE)
thecounted_and_crime_usa <- read.csv("data/thecounted_and_crime_usa.csv",stringsAsFactors = FALSE)
thecounted_and_crime_labels <- read.csv("data/killed_by_police_code_book.csv",stringsAsFactors = FALSE)
rownames(thecounted_and_crime) <- thecounted_and_crime$state
thecounted_and_crime$id <- 1:nrow(thecounted_and_crime)


Statistic <-  c("State", "State Name", "Population (2014)", "Police Officers (2014)", "Police Officers per 100k Pop.",
                "Killed by Police (2015)", "Killed by Police per 100k Pop.", "Murder & Non-neg. Manslaughter (2014)", "Murder & Non-neg. Manslaughter per 100k Pop.",
                "Violent Crime (2014)", "Violent Crime per 100k Pop.")


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
                
                plotData <- reactive({
                        if(input$yvar==""){
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
                        paste0(r$state_name," --> click to view more stats",collapse="")
                }
                
                update_selection <- function(data,location,session){
                        # Prevent action when clicking on smoother 
                        if(is.null(data$id)) return(NULL)
                        cat(as.character(data))
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
                                layer_points(fillOpacity := .5,fillOpacity.hover:=.9,fill.hover:="red",size:=~xcount/size_scaler,size.hover:=size_max/size_scaler*1.1) %>%
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
                               layer_points(fillOpacity := .5,fillOpacity.hover:=.9,fill.hover:="red",key := ~id) %>%
                               add_tooltip(tip_value,"hover") %>%
                               handle_click(update_selection) %>%
                               set_options(width = "auto", height = "600", resizable=FALSE)
                }
                #}

                reactive({
                        if(input$yvar=="") {
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
                
                
        }
)
