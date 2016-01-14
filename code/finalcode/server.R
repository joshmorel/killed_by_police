library(shiny)
library(ggplot2)
thecounted_and_crime = read.csv("../../data/thecounted_and_crime.csv",stringsAsFactors = FALSE)
glm.killed <- glm(killedbypolice2015 ~ log(police_officers2014) + log(violent_crime2014) + log(population_black2014),offset=log(population2014),family="quasipoisson",data=thecounted_and_crime)

thecounted_and_crime$killedbypolice2015_estimated = glm.killed$fitted.values
thecounted_and_crime$killedbypolice2015_residual = resid(glm.killed)

lab = c("State","State Name","Population - Total","Population - Black or African Americans","Killed by Police - Number", "Violent Crime - Number", "Murder & Non-negligent Manslaughter","Police Officers Employed","Black or African American - Percent of Population","Killed by Police - per 100k Population","Violent Crime - per 100k Population","Murder & Non-negligent Manslaughter - per 100k Population","Police Officers Employed - per 100k Population","People Killed by Police - Estimated","People Killed by Police - Residuals")
thecounted_and_crime_labels = setNames(lab,colnames(thecounted_and_crime))

killed_estimates <- data.frame(estimated = glm.killed$fitted.values,residuals = resid(glm.killed),
                               lower = lower,
                               upper = upper,
                               actual = thecounted_and_crime$killedbypolice2015,
                               state = thecounted_and_crime$state
)
killed_estimates$actual_above_estimate = ifelse(killed_estimates$actual > killed_estimates$estimated,"Actual > Estimated","Estimated > Actual")
killed_estimates$actual_above_estimate = factor(killed_estimates$actual_above_estimate)
killed_estimates$actual_estimated_ratio = killed_estimates$actual/killed_estimates$estimated



shinyServer(
        function(input, output) {
                output$stateSelected <- renderPrint({t(subset(thecounted_and_crime,state==input$state))})

                selectedData <- reactive({
                        if(input$dc){
                                thecounted_and_crime
                        }
                        else {
                                subset(thecounted_and_crime,state!="DC")
                        }
                })
                
                output$x <- renderPrint({input$gg_x})

                output$gg_points <- renderPlot({
                        if(input$plotchoice == "descriptive") {
                                ggplot(thecounted_and_crime,aes(x=killedbypolice2015_per100k,y=reorder(state,killedbypolice2015_per100k))) + 
                                        geom_segment(aes(yend = state), size=1,xend = 0, colour ="#D2D2F0",guide=FALSE) +
                                        geom_point(aes(size=killedbypolice2015)) + 
                                        scale_colour_brewer(palette ="Set1", guide = FALSE) + 
                                        theme_bw() + 
                                        theme(panel.grid.major.y = element_blank()) + 
                                        ggtitle("Rate of People Killed by Police in the U.S. in 2015 by State") +
                                        labs(x = "Killed per 100k Population",y = "State", size="Number Killed\nby Police") +
                                        theme(axis.text.y = element_text(hjust = grid::unit(c(-1, 0), "points")))
                        }
                        else if(input$plotchoice == "modeled"){
                                ggplot(killed_estimates,aes(x=log(estimated),y=reorder(state,actual_estimated_ratio),shape=actual_above_estimate)) + 
                                        geom_point(aes(color="blue")) + 
                                        geom_point(dat=killed_estimates,aes(x=log(actual),y=state,colour="red")) +
                                        labs(x = "Natural Log of Estimated People Killed", y = "State") + 
                                        geom_errorbarh(aes(xmin = log(lower),xmax=log(upper)),colour ="#D2D2F0") +
                                        scale_colour_brewer(palette ="Set1", guide = FALSE) + theme_bw() + theme(panel.grid.major.y = element_blank()) + 
                                        facet_grid(actual_above_estimate ~  .,scales= "free_y",space="free_y") +
                                        ggtitle("Rate of People Killed by Police in the U.S. in 2015 by State") + 
                                        guides(shape = FALSE) +
                                        theme(legend.position="top") +
                                        scale_colour_manual(name = 'Number Killed by Police',guide = 'legend',
                                                            values =c('blue'='blue','red'='red'),
                                                            labels = c('Estimated','Actual')) +
                                        theme(axis.text.y = element_text(hjust = grid::unit(c(-1, 0), "points")))
                                
                        }
                        else {
                                ggplot(dat = selectedData(),aes_string(x=input$gg_x,y=input$gg_y,size="killedbypolice2015",label="state")) + 
                                        geom_smooth(method = "lm",show_guide=FALSE) + 
                                        geom_point(alpha=.5) + 
                                        geom_text(size=3) +
                                        labs(x = thecounted_and_crime_labels[input$gg_x], y=thecounted_and_crime_labels[input$gg_y], size="Number Killed\nby Police")
                                
                        }
                })

        }
)