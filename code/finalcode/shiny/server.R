require(shiny)
require(ggvis)
require(dplyr)

thecounted_and_crime <- read.csv("../../data/thecounted_and_crime.csv",stringsAsFactors = FALSE)
thecounted_and_crime_usa <- read.csv("../../data/thecounted_and_crime_usa.csv",stringsAsFactors = FALSE)
rownames(thecounted_and_crime) <- thecounted_and_crime$state
thecounted_and_crime$id <- 1:nrow(thecounted_and_crime)


Statistic <-  c("State", "State Name", "Population (2014)", "Police Officers (2014)", "Police Officers per 100k Pop.",
                "Killed by Police (2015)", "Killed by Police per 100k Pop.", "Murder & Non-neg. Manslaughter (2014)", "Murder & Non-neg. Manslaughter per 100k Pop.",
                "Violent Crime (2014)", "Violent Crime per 100k Pop.")


shinyServer(
        # Add session when using updateSelectInput
        function(input, output,session) {
                x <- 0
                y <- 0

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
                                cols <- grep(input$xvar,colnames(thecounted_and_crime),value=TRUE)
                                d <- thecounted_and_crime[,c("state",cols,"id")]
                                names(d) <- c("state","count","rate","id")
                                d
                        }
                })
                
                output$tbl2 <- renderTable({plotData()})

                
                tip_value <- function(x) {
                        if(is.null(x)) return(NULL)
                        r <- thecounted_and_crime[thecounted_and_crime$id == x$id,]
                        paste0(r$state_name," --> click to view more stats",collapse="")
                }
                
                update_selection <- function(data,location,session){
                        if(is.null(data)) return(NULL)
                        cat(as.character(data))
                        r <- thecounted_and_crime[thecounted_and_crime$id == data$id,]
                        updateSelectInput(session
                                          ,"state"
                                          ,selected=r$state)
                        y <<- y + 1
                        output$ycnt <- renderText({y})
                }
                render_dotplot <- function(plotData) {
                        

                        #ggvis_0.4.2 has bug where scaling of points is not done correctly, so creating workaround hack
                        size_scaler <- (min(plotData$count)+1)/3
                        size_max <- max(plotData$count)
                        plotData %>% ggvis(~rate,~reorder(state,-rate),key := ~id) %>%
                                layer_points(fillOpacity := .5,fillOpacity.hover:=.9,fill.hover:="red",size:=~count/size_scaler,size.hover:=size_max/size_scaler*1.1) %>%
                                add_axis("y",title= "State") %>%
                                add_axis("x",title="rate") %>%
                                add_tooltip(tip_value,"hover") %>%
                                handle_click(update_selection) %>%
                                set_options(width = "auto", height = "600", resizable=FALSE) 

                        
                        #size_scaler <- (min(thecounted_and_crime$killedbypolice2015)+1)/3
                        
                        #thecounted_and_crime %>% ggvis(~killedbypolice2015_per100k,~reorder(state,-killedbypolice2015_per100k),key := ~id) %>%
                        #        layer_points(fillOpacity := .5,fillOpacity.hover:=.9,fill.hover:="red",size:=~killedbypolice2015/size_scaler,size.hover:=300) %>%
                        #        add_axis("y",title= "State") %>%
                        #        add_axis("x",title="Killed by Police per 100k") %>%
                        #        add_tooltip(tip_value,"hover") %>%
                        #        handle_click(update_selection) %>%
                        #        set_options(width = "auto", height = "600", resizable=FALSE)
                        #)
                }
                
                # The default initial plot
                reactive({
                        render_dotplot(plotData())
                        # In shiny apps, need to register ggvis observers
                        # and tell shiny where to put the controls
                }) %>% bind_shiny("ggvisplot")
                
                # Listens for any change of input$state and updates table accordingly 
                observeEvent(input$state, {
                        x <<- x + 1
                        output$xcnt <- renderText({x})
                        update_side_table(input$state)
                })
                
                
        }
)


#glm.killed <- glm(killedbypolice2015 ~ log(police_officers2014) + log(violent_crime2014) + log(population_black2014),offset=log(population2014),family="quasipoisson",data=thecounted_and_crime)

#thecounted_and_crime$killedbypolice2015_estimated = glm.killed$fitted.values
#thecounted_and_crime$killedbypolice2015_residual = resid(glm.killed)

#lab = c("State","State Name","Population - Total","Population - Black or African Americans","Killed by Police - Number", "Violent Crime - Number", "Murder & Non-negligent Manslaughter","Police Officers Employed","Black or African American - Percent of Population","Killed by Police - per 100k Population","Violent Crime - per 100k Population","Murder & Non-negligent Manslaughter - per 100k Population","Police Officers Employed - per 100k Population","People Killed by Police - Estimated","People Killed by Police - Residuals")
#thecounted_and_crime_labels = setNames(lab,colnames(thecounted_and_crime))

#killed_estimates <- data.frame(estimated = glm.killed$fitted.values,residuals = resid(glm.killed),
#                               lower = lower,
#                               upper = upper,
#                               actual = thecounted_and_crime$killedbypolice2015,
#                               state = thecounted_and_crime$state
#)
#killed_estimates$actual_above_estimate = ifelse(killed_estimates$actual > killed_estimates$estimated,"Actual > Estimated","Estimated > Actual")
#killed_estimates$actual_above_estimate = factor(killed_estimates$actual_above_estimate)
#killed_estimates$actual_estimated_ratio = killed_estimates$actual/killed_estimates$estimated

#output$stateSelected <- renderPrint({t(subset(thecounted_and_crime,state==input$state))})

#selectedData <- reactive({
#        if(input$dc){
#                thecounted_and_crime
#        }
#        else {
#                subset(thecounted_and_crime,state!="DC")
#        }
#})

#output$x <- renderPrint({input$gg_x})