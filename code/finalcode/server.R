require(shiny)
require(ggvis)
require(dplyr)
require(MASS)

shinyServer(
        # Add session when using updateSelectInput
        function(input, output,session) {
                x <- 0
                y <- 0

                thecounted_and_crime <- read.csv("../../data/thecounted_and_crime.csv",stringsAsFactors = FALSE)
                rownames(thecounted_and_crime) <- thecounted_and_crime$state
                thecounted_and_crime$hovered <- factor(0,levels=c(0,1))
                thecounted_and_crime$clicked <- factor(0,levels=c(0,1))
                thecounted_and_crime_usa <- read.csv("../../data/thecounted_and_crime_usa.csv",stringsAsFactors = FALSE)
                
                #For display of key statistics in side panel
                Statistic <-  c("State", "State Name", "Population (2014)", "Police Officers (2014)", "Police Officers per 100k Pop.",
                                "Killed by Police (2015)", "Killed by Police per 100k Pop.", "Murder & Non-neg. Manslaughter (2014)", "Murder & Non-neg. Manslaughter per 100k Pop.",
                                "Violent Crime (2014)", "Violent Crime per 100k Pop.")
                
                Value <- select(thecounted_and_crime_usa, state, state_name,population2014, police_officers2014,police_officers2014_per100k,killedbypolice2015,killedbypolice2015_per100k,murder_nonnegligent_manslaughter2014,murder_nonnegligent_manslaughter2014_per100k,violent_crime2014,violent_crime2014_per100k)
                Value <- as.character(t(Value))
                thecounted_and_crime_display <- data.frame(Statistic,Value)
                
                
                
                thecounted_and_crime$id <- 1:nrow(thecounted_and_crime)
                
                tip_value <- function(x) {
                        if(is.null(x)) return(NULL)
                        r <- thecounted_and_crime[thecounted_and_crime$id == x$id,]
                        paste0(r$state_name,":<br />",r$killedbypolice2015," killed by police",collapse="")
                }
                
                tip_click <- function(x) {
                        #if(is.null(x)) return(NULL)
                        cat(paste0("\n",as.character(x)),collapse="")
                        #z <- x[1,5]
                        updateSelectInput(session,"state",selected = "CA")
                        y <<- y + 1
                        output$ycnt <- renderText({y})
                        #return(r)
                        
                }
                
                update_selection = function(data,location,session){
                        if(is.null(data)) return(NULL)
                        cat(paste0("\n",as.character(data)))
                        updateSelectInput(session
                                          ,"state"
                                          ,selected=data[1,5])
                        
                        y <<- y + 1
                        output$ycnt <- renderText({y})
                        
                }

                render_dotplot <- function(thecounted_and_crime) {
                        thecounted_and_crime %>% ggvis(~killedbypolice2015_per100k,~reorder(state,-killedbypolice2015_per100k),size=~killedbypolice2015,fill=~clicked,key := ~id) %>%
                                layer_points() %>%
                                add_axis("y",title= "State") %>%
                                add_axis("x",title="Killed by Police per 100k") %>%
                                add_legend("size",title="Number Killed") %>%
                                hide_legend("fill") %>%
                                add_tooltip(tip_value,"hover") %>%
                                handle_click(update_selection) #{
                                        #cat("\n-----\n",as.character(data))
                                        #updateSelectInput(session,"state",selected = data[1,5])
                                        #cat(paste0("\n",data[1,5],collapse=""))
                                        #y <<- y + 1
                                        #output$ycnt <- renderText({y})
                                        
                                #}
                        #)
                }
                
                # The default initial plot
                

                reactive({
                        render_dotplot(thecounted_and_crime)
                        # In shiny apps, need to register ggvis observers
                        # and tell shiny where to put the controls
                }) %>% bind_shiny("ggvisplot")
                
                # Listens for any change of input$state and updates table accordingly 
                observeEvent(input$state, {

                        x <<- x + 1
                        output$xcnt <- renderText({x})
                        if(input$state=="USA") {
                                Value <- select(thecounted_and_crime_usa, state, state_name,population2014, police_officers2014,police_officers2014_per100k,killedbypolice2015,killedbypolice2015_per100k,murder_nonnegligent_manslaughter2014,murder_nonnegligent_manslaughter2014_per100k,violent_crime2014,violent_crime2014_per100k)
                        }
                        else {
                                Value <- filter(thecounted_and_crime,state==input$state) %>% select(state, state_name,population2014, police_officers2014,police_officers2014_per100k,killedbypolice2015,killedbypolice2015_per100k,murder_nonnegligent_manslaughter2014,murder_nonnegligent_manslaughter2014_per100k,violent_crime2014,violent_crime2014_per100k)
                        }
                        Value <- as.character(t(Value))
                        thecounted_and_crime_display <- data.frame(Statistic,Value)
                        output$tbl_out <- renderTable({
                                thecounted_and_crime_display
                        })
                        
                        if(input$state!="USA") {
                                #thecounted_and_crime <- thecounted_and_crime
                                thecounted_and_crime$clicked <- factor(0,levels=c(0,1))
                                thecounted_and_crime[input$state,"clicked"] <- 1

                                reactive({
                                        render_dotplot(thecounted_and_crime)
                                        # In shiny apps, need to register ggvis observers
                                        # and tell shiny where to put the controls
                                #}) %>% bind_shiny("ggvisplot")
                                
                                }) %>% bind_shiny("ggvisplot")
                                
                        }
                        
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