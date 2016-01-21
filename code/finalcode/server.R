library(shiny)
library(ggplot2)
library(ggvis)
library(MASS)

thecounted_and_crime <- read.csv("../../data/thecounted_and_crime.csv",stringsAsFactors = FALSE)

shinyServer(
        # Add session when using updateSelectInput
        function(input, output,session) {

                thecounted_and_crime$id <- 1:nrow(thecounted_and_crime)
                
                tip_value <- function(x) {
                        if(is.null(x)) return(NULL)
                        r <- thecounted_and_crime[thecounted_and_crime$id == x$id,]
                        paste0(r$state_name,":<br />",r$killedbypolice2015," killed by police",collapse="")
                }
                
                tip_click <- function(x) {
                        if(is.null(x)) return(NULL)
                        r <- thecounted_and_crime[thecounted_and_crime$id == x$id,]
                        updateSelectInput(session,"state",selected = r$state)
                        output$state_name <- renderUI({
                                r$state_name
                        })
                        output$killedbypolice <- renderUI({
                                as.character(r$killedbypolice2015)
                        })
                        return("State Stats Updated")
                }
                
                thecounted_and_crime %>% ggvis(~killedbypolice2015_per100k,~reorder(state,-killedbypolice2015_per100k),
                                               size=~killedbypolice2015,key := ~id) %>%
                        layer_points() %>%
                        add_axis("y",title= "State") %>%
                        add_axis("x",title="Killed by Police per 100k") %>%
                        add_legend("size",title="Number Killed") %>%
                        add_tooltip(tip_value,"hover") %>%
                        add_tooltip(tip_click,"click") %>%
                        # In shiny apps, need to register observers
                        # and tell shiny where to put the controls
                        bind_shiny("ggvisplot","ggvisplot_ui")                

                tbl <- reactive({ subset(thecounted_and_crime,state==input$state)})
                
                output$tbl_out <- renderTable({
                        tbl()[, c("state", "killedbypolice2015")]
                })
                
                x <- reactive({input$state})
                
#                  output$mydata <- renderText({
#                         
#                         
#                         thecounted_and_crime_us <- data.frame(state = "USA",state_name="United States",population2014=sum(thecounted_and_crime$population2014),stringsAsFactors = FALSE)
#                         mytable <- as.data.frame(t(thecounted_and_crime_us),stringsAsFactors = FALSE)
#                         colnames(mytable) <- "Statistics"
#                         mytable[1,]
#                 })
                
                output$state_name <- renderText({x()})

 

                

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